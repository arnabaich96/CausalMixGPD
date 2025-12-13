#' @export
predict.mixgpd_fit <- function(object,
                               newdata,
                               type = c("density", "cdf", "sample", "quantile"),
                               probs = c(0.1, 0.5, 0.9),
                               n_samples = 1000L,
                               alpha = NULL,
                               ...) {

  type <- match.arg(type)

  if (type == "quantile") {
    probs <- sort(as.numeric(probs))
    if (any(probs <= 0 | probs >= 1)) stop("'probs' must be in (0,1).", call. = FALSE)

    pars <- .extract_gamma_dp_params(object)
    Q <- .q_gamma_dp_over_draws(probs, pars$W, pars$Shape, pars$Scale,
                                lower = 0, upper0 = 1, expand = 2, max_expand = 80)

    # Summaries across draws
    a <- if (is.null(alpha)) 0.05 else alpha
    out <- rbind(
      mean  = colMeans(Q, na.rm = TRUE),
      sd    = apply(Q, 2, stats::sd, na.rm = TRUE),
      lower = apply(Q, 2, stats::quantile, probs = a/2, na.rm = TRUE),
      upper = apply(Q, 2, stats::quantile, probs = 1 - a/2, na.rm = TRUE)
    )

    return(t(out))
  }

  # keep your existing density/cdf/sample logic here (they can later call the same core pieces)
  stop("Only type='quantile' shown here; plug into your existing predict().", call. = FALSE)
}


# internal: robust quantile inversion via bracketing + uniroot
.invert_cdf_uniroot <- function(cdf_fun,
                                tau,
                                lower = 0,
                                upper = NULL,
                                max_expand = 60L,
                                expand_factor = 2,
                                tol = 1e-8) {
  tau <- as.numeric(tau)
  if (!is.finite(tau) || tau < 0 || tau > 1) stop("'tau' must be in [0,1].", call. = FALSE)

  # handle edges
  if (tau == 0) return(lower)
  if (tau == 1) return(Inf)

  # initial upper
  if (is.null(upper) || !is.finite(upper) || upper <= lower) {
    # start with something not-cute: 1 if data are tiny, else 2*lower+1
    upper <- max(1, 2 * lower + 1)
  }

  # function to root-find
  f_root <- function(x) cdf_fun(x) - tau

  fL <- f_root(lower)
  if (!is.finite(fL)) stop("CDF at lower bound is not finite.", call. = FALSE)

  fU <- f_root(upper)
  if (!is.finite(fU)) {
    # try nudging upper up if CDF fails at upper
    fU <- f_root(upper * expand_factor)
    upper <- upper * expand_factor
  }

  # expand upper until bracketed (fL <= 0 and fU >= 0)
  it <- 0L
  while (is.finite(fU) && fU < 0 && it < max_expand) {
    upper <- upper * expand_factor
    fU <- f_root(upper)
    it <- it + 1L
  }

  if (!is.finite(fU)) {
    stop("CDF became non-finite while bracketing quantile.", call. = FALSE)
  }
  if (fU < 0) {
    stop("Failed to bracket quantile (upper bound too small even after expansion).", call. = FALSE)
  }

  # uniroot in bracket
  uniroot(f_root, lower = lower, upper = upper, tol = tol)$root
}
#' @export
predict.mixgpd_te_fit <- function(object,
                                  newdata = NULL,
                                  type = c("quantile"),
                                  probs = c(0.1, 0.5, 0.9),
                                  alpha = object$alpha %||% 0.05,
                                  ...) {

  type <- match.arg(type)
  if (type != "quantile") {
    stop("Currently only type='quantile' is implemented for TE objects.", call. = FALSE)
  }

  # predict groupwise quantiles
  q1 <- predict(object$fit_trt, newdata = newdata, type = "quantile", probs = probs, ...)
  q0 <- predict(object$fit_con, newdata = newdata, type = "quantile", probs = probs, ...)

  # expect rows aligned as q{prob}
  if (!identical(rownames(q1), rownames(q0))) {
    stop("Treatment/control quantile rownames do not match.", call. = FALSE)
  }

  # QTE summaries (approx using posterior draws would be better later;
  # for now we combine mean/sd assuming independence is NOT correct,
  # so we should compute from draws if available.)
  out <- q1
  out[, ] <- NA_real_

  # If your quantile predictor already returns posterior draw summaries only,
  # we need draw-level QTE. Here's the quick safe version:
  out[, "mean"]  <- q1[, "mean"] - q0[, "mean"]
  out[, "sd"]    <- sqrt(q1[, "sd"]^2 + q0[, "sd"]^2)   # conservative-ish
  out[, "lower"] <- q1[, "lower"] - q0[, "upper"]
  out[, "upper"] <- q1[, "upper"] - q0[, "lower"]

  attr(out, "treatment_quantiles") <- q1
  attr(out, "control_quantiles")   <- q0

  out
}


#' Average Treatment Effect (ATE) for a fitted TE object
#'
#' @export
ate <- function(object, ...) UseMethod("ate")

#' @export
ate.mixgpd_te_fit <- function(object, level = 0.95, ...) {
  stopifnot(inherits(object, "mixgpd_te_fit"))

  d1 <- .as_mcmc_matrix(object$fit_trt)
  d0 <- .as_mcmc_matrix(object$fit_con)

  # Extract Gamma-mixture mean per draw:
  # E[Y] = sum_j w_j * (shape_j * scale_j)
  .mean_gamma_mix <- function(draws) {
    cn <- colnames(draws)
    w_cols  <- grep("^w\\[", cn, value = TRUE)
    sh_cols <- grep("^shape\\[", cn, value = TRUE)
    sc_cols <- grep("^scale\\[", cn, value = TRUE)

    if (!length(w_cols) || !length(sh_cols) || !length(sc_cols)) {
      stop("ATE currently implemented for unconditional Gamma DP (requires w[j], shape[j], scale[j]).",
           call. = FALSE)
    }

    W  <- as.matrix(draws[, w_cols,  drop = FALSE])
    Sh <- as.matrix(draws[, sh_cols, drop = FALSE])
    Sc <- as.matrix(draws[, sc_cols, drop = FALSE])

    mu <- Sh * Sc
    as.numeric(rowSums(W * mu))
  }

  Ey1 <- .mean_gamma_mix(d1)
  Ey0 <- .mean_gamma_mix(d0)

  te <- Ey1 - Ey0

  alpha <- (1 - level) / 2
  out <- c(
    mean  = mean(te),
    sd    = stats::sd(te),
    lower = stats::quantile(te, probs = alpha,     names = FALSE),
    upper = stats::quantile(te, probs = 1 - alpha, names = FALSE)
  )

  structure(out, class = "mixgpd_te_summary")
}

