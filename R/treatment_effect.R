# =============================================================================
# Treatment effect summaries for mixgpd_te_fit
#   - ATE: posterior mean/sd/CI of E[Y(1)] - E[Y(0)]
#   - QTE: posterior mean/sd/CI of Q_tau(Y(1)) - Q_tau(Y(0))
#   - QATE: alias of QTE (many people use both terms loosely)
# Currently implemented for:
#   - Unconditional Gamma DP mixtures (kernel="gamma", tail="none")
#     where draws contain: w[j], shape[j], scale[j]
# =============================================================================

#' Coerce MCMC draws to a matrix (iterations x parameters)
#' @keywords internal
.as_mcmc_matrix <- function(x) {
  # x can be:
  # - matrix
  # - coda::mcmc / mcmc.list
  # - list containing $mcmc_draws
  if (is.list(x) && !is.null(x$mcmc_draws)) x <- x$mcmc_draws

  if (is.matrix(x)) return(x)

  if (inherits(x, "mcmc")) {
    return(as.matrix(x))
  }
  if (inherits(x, "mcmc.list")) {
    mats <- lapply(x, as.matrix)
    return(do.call(rbind, mats))
  }

  stop("Cannot coerce MCMC draws to a matrix.", call. = FALSE)
}

#' Extract ordered Gamma DP mixture parameters from a draw-matrix
#' Returns W, Shape, Scale as matrices M x K
#' @keywords internal
.extract_gamma_dp_params <- function(draws, renormalize_weights = TRUE) {
  cn <- colnames(draws)

  w_cols  <- grep("^w\\[",     cn, value = TRUE)
  sh_cols <- grep("^shape\\[", cn, value = TRUE)
  sc_cols <- grep("^scale\\[", cn, value = TRUE)

  if (!length(w_cols))  stop("Cannot find 'w[j]' in MCMC draws.", call. = FALSE)
  if (!length(sh_cols)) stop("Cannot find 'shape[j]' in MCMC draws.", call. = FALSE)
  if (!length(sc_cols)) stop("Cannot find 'scale[j]' in MCMC draws.", call. = FALSE)

  w_j  <- as.integer(sub("^w\\[([0-9]+)\\]$", "\\1", w_cols))
  sh_j <- as.integer(sub("^shape\\[([0-9]+)\\]$", "\\1", sh_cols))
  sc_j <- as.integer(sub("^scale\\[([0-9]+)\\]$", "\\1", sc_cols))

  w_cols  <- w_cols[order(w_j)]
  sh_cols <- sh_cols[order(sh_j)]
  sc_cols <- sc_cols[order(sc_j)]

  # sanity: same component indices
  if (!identical(sort(w_j), sort(sh_j)) || !identical(sort(w_j), sort(sc_j))) {
    stop("Mismatch of component indices across w/shape/scale columns.", call. = FALSE)
  }

  W     <- as.matrix(draws[, w_cols,  drop = FALSE])
  Shape <- as.matrix(draws[, sh_cols, drop = FALSE])
  Scale <- as.matrix(draws[, sc_cols, drop = FALSE])

  # enforce positivity where needed (defensive)
  Shape <- pmax(Shape, .Machine$double.eps)
  Scale <- pmax(Scale, .Machine$double.eps)
  W     <- pmax(W, 0)

  if (renormalize_weights) {
    rs <- rowSums(W)
    rs[rs <= 0] <- 1
    W <- W / rs
  }

  list(W = W, Shape = Shape, Scale = Scale, K = ncol(W), M = nrow(W))
}

#' Mixture CDF for one draw (Gamma mixture)
#' @keywords internal
.gamma_mix_cdf_1draw <- function(x, W, Shape, Scale) {
  # W,Shape,Scale are numeric vectors length K
  sum(W * stats::pgamma(x, shape = Shape, scale = Scale))
}

#' Robust uniroot quantile solver for one draw (Gamma mixture)
#' Uses increasing upper bound until CDF(upper) >= p
#' @keywords internal
.qgamma_mix_1draw <- function(p, W, Shape, Scale, y_max_hint = 1) {
  # defensive: clamp p
  p <- min(max(p, 0), 1)
  if (p == 0) return(0)

  # Start upper bound based on data scale
  upper <- max(1, y_max_hint)
  upper <- upper * 5

  F_upper <- .gamma_mix_cdf_1draw(upper, W, Shape, Scale)

  # Expand upper until we bracket p
  it <- 0L
  while (is.finite(F_upper) && F_upper < p && it < 80L) {
    upper <- upper * 2
    if (upper > 1e12) break
    F_upper <- .gamma_mix_cdf_1draw(upper, W, Shape, Scale)
    it <- it + 1L
  }

  if (!is.finite(F_upper) || F_upper < p) {
    stop("Failed to bracket quantile (upper bound too small even after expansion).", call. = FALSE)
  }

  # root of F(x) - p on [0, upper]
  f <- function(x) .gamma_mix_cdf_1draw(x, W, Shape, Scale) - p
  stats::uniroot(f, lower = 0, upper = upper, tol = 1e-10, maxiter = 5000L)$root
}

# =============================================================================
# Public generics
# =============================================================================

#' Average Treatment Effect (ATE)
#' @export
ate <- function(object, ...) UseMethod("ate")

#' Quantile Treatment Effect (QTE)
#' @export
qte <- function(object, ...) UseMethod("qte")

#' Quantile Average Treatment Effect (QATE)
#' (Alias of QTE in this package)
#' @export
qate <- function(object, ...) UseMethod("qate")

# =============================================================================
# Methods for mixgpd_te_fit
# =============================================================================

#' @export
ate.mixgpd_te_fit <- function(object, level = 0.95, renormalize_weights = TRUE, ...) {
  stopifnot(inherits(object, "mixgpd_te_fit"))

  d1 <- .as_mcmc_matrix(object$fit_trt)
  d0 <- .as_mcmc_matrix(object$fit_con)

  # only implemented for unconditional gamma dp draws that have w/shape/scale
  p1 <- .extract_gamma_dp_params(d1, renormalize_weights = renormalize_weights)
  p0 <- .extract_gamma_dp_params(d0, renormalize_weights = renormalize_weights)

  # E[Gamma(shape, scale)] = shape * scale
  Ey1 <- rowSums(p1$W * (p1$Shape * p1$Scale))
  Ey0 <- rowSums(p0$W * (p0$Shape * p0$Scale))

  te <- Ey1 - Ey0

  alpha <- (1 - level) / 2
  out <- matrix(
    c(
      mean(te),
      stats::sd(te),
      stats::quantile(te, probs = alpha,     names = FALSE),
      stats::quantile(te, probs = 1 - alpha, names = FALSE)
    ),
    nrow = 1,
    dimnames = list("ATE", c("mean", "sd", "lower", "upper"))
  )

  out
}

#' @export
qte.mixgpd_te_fit <- function(object,
                              probs = c(0.1, 0.5, 0.9),
                              level = 0.95,
                              renormalize_weights = TRUE,
                              ...) {
  stopifnot(inherits(object, "mixgpd_te_fit"))

  probs <- as.numeric(probs)
  probs <- sort(unique(probs))
  if (any(probs < 0 | probs > 1)) stop("'probs' must be in [0,1].", call. = FALSE)

  d1 <- .as_mcmc_matrix(object$fit_trt)
  d0 <- .as_mcmc_matrix(object$fit_con)

  p1 <- .extract_gamma_dp_params(d1, renormalize_weights = renormalize_weights)
  p0 <- .extract_gamma_dp_params(d0, renormalize_weights = renormalize_weights)

  M <- min(p1$M, p0$M)

  # use observed maxima as bracketing hints
  y1_max <- if (!is.null(object$spec_trt$Y)) max(object$spec_trt$Y, na.rm = TRUE) else 1
  y0_max <- if (!is.null(object$spec_con$Y)) max(object$spec_con$Y, na.rm = TRUE) else 1

  alpha <- (1 - level) / 2

  out <- matrix(NA_real_, nrow = length(probs), ncol = 4)
  rownames(out) <- paste0("tau=", probs)
  colnames(out) <- c("mean", "sd", "lower", "upper")

  # compute draw-level QTEs for each tau, then summarize
  for (ii in seq_along(probs)) {
    tau <- probs[ii]

    te_draw <- numeric(M)

    for (m in seq_len(M)) {
      q1 <- .qgamma_mix_1draw(tau, p1$W[m, ], p1$Shape[m, ], p1$Scale[m, ], y_max_hint = y1_max)
      q0 <- .qgamma_mix_1draw(tau, p0$W[m, ], p0$Shape[m, ], p0$Scale[m, ], y_max_hint = y0_max)
      te_draw[m] <- q1 - q0
    }

    out[ii, "mean"]  <- mean(te_draw)
    out[ii, "sd"]    <- stats::sd(te_draw)
    out[ii, "lower"] <- stats::quantile(te_draw, probs = alpha,     names = FALSE)
    out[ii, "upper"] <- stats::quantile(te_draw, probs = 1 - alpha, names = FALSE)
  }

  out
}

#' @export
summary.mixgpd_te_fit <- function(object,
                                  probs = c(0.1, 0.5, 0.9),
                                  ...) {

  out <- list(
    call    = object$call,
    formula = object$formula,
    A_name  = object$A_name,

    # group fits (so users can inspect them)
    fit_trt = object$fit_trt,
    fit_con = object$fit_con
  )

  # effects
  out$ate <- try(ate(object), silent = TRUE)
  out$qte <- try(qte(object, probs = probs), silent = TRUE)

  class(out) <- "summary_mixgpd_te_fit"
  out
}

#' @export
print.summary_mixgpd_te_fit <- function(x, ...) {
  cat("Treatment-effect fit (DPmixGPD)\n")
  cat("A column:", x$A_name, "\n")
  cat("Formula :", deparse(x$formula), "\n\n")

  cat("ATE:\n")
  if (inherits(x$ate, "try-error")) cat("  <not available>\n\n")
  else print(x$ate)

  cat("\nQTE:\n")
  if (inherits(x$qte, "try-error")) cat("  <not available>\n")
  else print(x$qte)

  invisible(x)
}

#' @export
plot.mixgpd_te_fit <- function(x,
                               effect = c("avg", "quantile"),
                               probs = seq(0.1, 0.9, by = 0.05),
                               ...) {

  effect <- match.arg(effect)

  if (effect == "avg") {
    a <- ate(x)
    y  <- a["ATE", "mean"]
    lo <- a["ATE", "lower"]
    hi <- a["ATE", "upper"]

    plot(1, y,
         xlim = c(0.5, 1.5),
         ylim = range(c(lo, hi)),
         xaxt = "n",
         xlab = "",
         ylab = "ATE",
         main = "Average Treatment Effect")
    axis(1, at = 1, labels = "ATE")
    segments(1, lo, 1, hi)
    abline(h = 0, lty = 2)
    return(invisible(a))
  }

  q <- qte(x, probs = probs)
  tau <- as.numeric(sub("^tau=", "", rownames(q)))

  plot(tau, q[, "mean"], type = "l",
       xlab = expression(tau),
       ylab = "QTE(tau)",
       main = "Quantile Treatment Effect")
  lines(tau, q[, "lower"], lty = 2)
  lines(tau, q[, "upper"], lty = 2)
  abline(h = 0, lty = 3)

  invisible(q)
}


