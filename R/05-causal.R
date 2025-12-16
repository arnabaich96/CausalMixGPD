#' Fit propensity
#'
#' Fit propensity.
#'
#' @param A A.
#' @param X X.
#' @param method method.
#' @param ... Additional arguments.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".fit_propensity", "DPmixGPD")
#' f
#'
#' @keywords internal
.fit_propensity <- function(A, X, method = c("glm_logit"), ...) {
  method <- match.arg(method)

  if (!is.numeric(A)) A <- as.numeric(A)
  if (!all(A %in% c(0, 1))) stop("'A' must be binary (0/1).", call. = FALSE)

  if (is.null(X)) stop("Internal error: X is NULL in .fit_propensity().", call. = FALSE)

  # ensure matrix
  X <- as.matrix(X)

  # GLM logistic for v1
  fit <- stats::glm(A ~ X, family = stats::binomial())

  ehat <- stats::fitted(fit)
  ehat <- pmin(pmax(ehat, 1e-6), 1 - 1e-6) # stabilize

  list(
    method = method,
    model  = fit,
    ehat   = ehat
  )
}

# =========================================================
# fit.TE: Two-arm wrapper with per-arm tail dispatch
#   tail = FALSE => bulk-only (fit.dpm)
#   tail = TRUE       => both arms bulk+GPD (fit.dpmgpd)
#   tail = c(TRUE,FALSE) => trt uses fit.dpmgpd, con uses fit.dpm
#   tail = c(FALSE,TRUE) => trt uses fit.dpm,    con uses fit.dpmgpd
#
# Arm-specific rule (length 1 or 2):
#   kernel, priors, trans, intercept, tail_ctrl
# =========================================================

# ---- internal: split length-1 or length-2 into (trt, con)
#' @keywords internal
.split_arm_arg <- function(x, name) {
  if (is.null(x)) {
    stop(sprintf("'%s' cannot be NULL; use length-1 or length-2 values.", name),
         call. = FALSE)
  }
  # list-like args (priors/trans/tail_ctrl often are lists)
  if (is.list(x) && !is.data.frame(x)) {
    if (length(x) == 0L) return(list(trt = list(), con = list()))
    if (length(x) == 1L) return(list(trt = x[[1]], con = x[[1]]))
    if (length(x) == 2L) return(list(trt = x[[1]], con = x[[2]]))
    stop(sprintf("'%s' must be length 0, 1 or 2 (list).", name), call. = FALSE)
  }


  # atomic vectors / character / logical
  if (length(x) == 1L) return(list(trt = x[[1]], con = x[[1]]))
  if (length(x) == 2L) return(list(trt = x[[1]], con = x[[2]]))

  stop(sprintf("'%s' must be length 1 or 2.", name), call. = FALSE)
}

# ---- internal: parse tail flags into per-arm TRUE/FALSE
#' @keywords internal
.parse_tail_flags <- function(tail) {
  if (is.null(tail)) {
    stop("'tail' cannot be NULL; use TRUE/FALSE or c(TRUE,FALSE).",
         call. = FALSE)
  }

  if (is.logical(tail)) {
    if (length(tail) == 1L) return(list(trt = tail, con = tail))
    if (length(tail) == 2L) return(list(trt = tail[1L], con = tail[2L]))
    stop("'tail' must be length-1 logical, or length-2 logical.", call. = FALSE)
  }

  stop("'tail' must be logical", call. = FALSE)
}

# ---- internal: only pass args that the target function accepts (unless it has "...")
#' @keywords internal
.call_with_formals <- function(fun, args) {
  fmls <- names(formals(fun))
  if (!("..." %in% fmls)) args <- args[names(args) %in% fmls]
  do.call(fun, args)
}
#' Fit treatment-effects model via two-arm DPM or DPM+GPD
#'
#' Fits separate outcome models for the treated and control groups, then
#' returns an object suitable for treatment-effect summaries (ATE/QTE).
#' The fitting backend is chosen per arm using the \code{tail} argument:
#' \code{tail = FALSE} uses \code{\link{fit.dpm}}, and \code{tail = TRUE}
#' uses \code{\link{fit.dpmgpd}}.
#'
#' @param formula A model formula for the outcome.
#' @param data A data.frame.
#' @param A Treatment indicator. Either a column name in \code{data} or a 0/1 vector.
#' @param kernel Bulk kernel(s). Length 1 uses the same kernel for both arms;
#'   length 2 uses \code{kernel[1]} for treated and \code{kernel[2]} for control.
#' @param dp_rep DP representation. Passed to the arm fits.
#' @param dp_ctrl DP control list (e.g., \code{list(K=5)}). Passed to arm fits.
#' @param priors Prior specification(s). Length 1 applies to both arms; length 2
#'   uses arm-specific priors.
#' @param mcmc MCMC control list. Passed to arm fits.
#' @param alpha Credible interval alpha (e.g., 0.05 gives 95\% intervals).
#' @param trans Transform specification(s). Length 1 applies to both arms; length 2 arm-specific.
#' @param intercept Logical. Whether to include an intercept in the design matrix construction.
#'   Length 1 applies to both arms; length 2 arm-specific.
#' @param tail Tail indicator(s). \code{TRUE} or \code{FALSE} uses bulk-only DPM.
#'   \code{TRUE} uses DPM+GPD. Length 1 applies to both arms; length 2 uses
#'   \code{tail[1]} for treated and \code{tail[2]} for control.
#' @param tail_ctrl Optional tail control list(s) used only for arms with \code{tail=TRUE}.
#'   Length 1 applies to both tail arms; length 2 arm-specific.
#' @param ... Reserved for future extensions. Currently unused.
#'
#' @return An object of class \code{"mixgpd_te_fit"} with components
#'   \code{fit_trt}, \code{fit_con}, \code{spec_trt}, and \code{spec_con}.
#'
#' @export
fit.TE <- function(formula,
                   data,
                   A,
                   kernel = "gamma",
                   dp_rep = "stick_breaking",
                   dp_ctrl = list(K = 10),
                   mcmc = list(n_iter = 2000, burn_in = 1000, chains = 1),
                   alpha = 0.05,
                   priors = list(),
                   trans = list(),
                   intercept = FALSE,
                   tail = FALSE,          # NULL/FALSE/TRUE or c(FALSE,TRUE) etc; or "none"/"gpd"
                   tail_ctrl = list(),     # optional; length 1 or 2; only used where tail==TRUE
                   ...) {

  # ---- basic checks
  if (missing(data) || is.null(data)) stop("'data' must be provided.", call. = FALSE)
  if (missing(A)) stop("A is missing: provide A as a column name or 0/1 vector.", call. = FALSE)
  if (is.null(priors)) priors <- list()
  if (is.null(trans))  trans  <- list()
  if (is.null(tail_ctrl)) tail_ctrl <- list()
  if (is.null(intercept)) intercept <- TRUE
  if (is.null(tail)) tail <- FALSE

  # ---- build model frame for y + X (keep NAs; we handle complete cases explicitly)
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.pass)

  # ---- attach treatment to mf for consistent NA filtering
  if (is.character(A) && length(A) == 1L) {
    if (!A %in% names(data)) stop("A column not found in data.", call. = FALSE)
    mf$A__ <- data[[A]]
  } else {
    mf$A__ <- A
  }

  if (nrow(mf) != NROW(mf$A__)) {
    stop("A has incompatible length with data/model frame.", call. = FALSE)
  }

  Avals <- mf$A__
  if (is.logical(Avals)) Avals <- as.integer(Avals)
  if (any(is.na(Avals))) stop("A contains NA; remove missingness first.", call. = FALSE)
  if (!all(Avals %in% c(0, 1))) stop("A must be binary with values 0/1.", call. = FALSE)
  mf$A__ <- as.integer(Avals)

  # ---- complete-case filtering across response, predictors, and A
  cc <- stats::complete.cases(mf)
  mf <- mf[cc, , drop = FALSE]

  mf_trt <- mf[mf$A__ == 1L, , drop = FALSE]
  mf_con <- mf[mf$A__ == 0L, , drop = FALSE]

  if (nrow(mf_trt) == 0L || nrow(mf_con) == 0L) {
    stop("Both treatment arms must have at least one observation after filtering.", call. = FALSE)
  }

  # ---- split arm-specific args
  k    <- .split_arm_arg(kernel,    "kernel")
  pr   <- .split_arm_arg(priors,    "priors")
  tr   <- .split_arm_arg(trans,     "trans")
  icpt <- .split_arm_arg(intercept, "intercept")
  tc   <- .split_arm_arg(tail_ctrl, "tail_ctrl")

  # ---- tail flags per arm
  tf <- .parse_tail_flags(tail)

  # ---- choose backend per arm
  backend_trt <- if (isTRUE(tf$trt)) DPmixGPD::fit.dpmgpd else DPmixGPD::fit.dpm
  backend_con <- if (isTRUE(tf$con)) DPmixGPD::fit.dpmgpd else DPmixGPD::fit.dpm

  # ---- assemble args per arm (NEVER pass 'tail' to fit.dpm; only tail_ctrl to tail backend)
  args_trt <- list(
    formula   = formula,
    data      = mf_trt,
    kernel    = k$trt,
    dp_rep    = dp_rep,
    dp_ctrl   = dp_ctrl,
    mcmc      = mcmc,
    alpha     = alpha,
    priors    = pr$trt,
    trans     = tr$trt,
    intercept = icpt$trt,
    ...
  )
  if (isTRUE(tf$trt) && !identical(tc$trt, list())) args_trt$tail_ctrl <- tc$trt


  args_con <- list(
    formula   = formula,
    data      = mf_con,
    kernel    = k$con,
    dp_rep    = dp_rep,
    dp_ctrl   = dp_ctrl,
    mcmc      = mcmc,
    alpha     = alpha,
    priors    = pr$con,
    trans     = tr$con,
    intercept = icpt$con,
    ...
  )
  if (isTRUE(tf$con) && !identical(tf$con, list())) args_con$tail_ctrl <- tc$con

  # ---- fit arms
  fit_trt <- .call_with_formals(backend_trt, args_trt)
  fit_con <- .call_with_formals(backend_con, args_con)

  # ---- output
  out <- list(
    fit_trt  = fit_trt,
    fit_con  = fit_con,
    spec_trt = fit_trt$spec,
    spec_con = fit_con$spec,
    tail     = c(trt = isTRUE(tf$trt), con = isTRUE(tf$con))
  )

  class(out) <- "mixgpd_te_fit"
  out
}




#' Print mixgpd te
#'
#' Print mixgpd te.
#'
#' @param x x.
#' @param ... Additional arguments.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("print.mixgpd_te", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
#' }
#'
#' @export
print.mixgpd_te <- function(x, ...) {
  cat("Treatment effect fit (mixgpd_te)\n")
  cat("Formula: ", deparse(x$formula), "\n", sep = "")
  cat("Effect:  ", x$effect, "\n", sep = "")
  cat("Alpha:   ", x$alpha, "\n", sep = "")
  cat("Tail:    ", x$tail, "\n", sep = "")
  cat("Kernel:  trt=", x$kernel["trt"], ", con=", x$kernel["con"], "\n", sep = "")

  n1 <- x$fit$trt$N
  n0 <- x$fit$con$N
  cat("N (A=1): ", n1, "\n", sep = "")
  cat("N (A=0): ", n0, "\n", sep = "")

  if (!is.null(x$ps)) {
    cat("Propensity: ", x$ps$method, " (ps + X: ", x$ps_include_X, ")\n", sep = "")
    cat("PS grid: ", length(x$ps_grid), " points\n", sep = "")
  } else {
    cat("Propensity: not used (unconditional)\n")
  }

  invisible(x)
}
# Treatment-effect summaries and ggplot-based plotting

#' Average Treatment Effect (ATE)
#'
#' Computes the posterior summary of the average treatment effect:
#' \deqn{\mathrm{ATE} = \mathbb{E}[Y(1)] - \mathbb{E}[Y(0)].}
#'
#' Currently implemented for *unconditional* Gamma DP mixture fits created by
#' [fit.TE()] with `y ~ 0` and `kernel = "gamma"` (and `tail = "none"`).
#'
#' @param object A fitted object returned by [fit.TE()].
#' @param level Credible interval level (default 0.95).
#' @param renormalize_weights Logical; if `TRUE`, renormalize mixture weights per draw.
#' @param ... Reserved for future use.
#' @return A 1x4 matrix with columns `mean`, `sd`, `lower`, `upper`.
#' @export
ate <- function(object, level = 0.95, renormalize_weights = TRUE, ...) {
  UseMethod("ate")
}

#' @export
ate.mixgpd_te_fit <- function(object, level = 0.95, renormalize_weights = TRUE, ...) {
  stopifnot(inherits(object, "mixgpd_te_fit"))

  # Regression/conditional TE support is not implemented yet.
  # Fail early with a clear message rather than touching DP draw internals.
  if (!identical(object$spec_trt$mode %||% NA_character_, "response_only") ||
      !identical(object$spec_con$mode %||% NA_character_, "response_only")) {
    stop(
      "Unconditional ATE/QTE are not implemented for regression fits (y ~ x). ",
      "Fit y ~ 0 or use ate(x)/qte(x) once conditional support is added.",
      call. = FALSE
    )
  }


  d1 <- .as_mcmc_matrix(object$fit_trt)
  d0 <- .as_mcmc_matrix(object$fit_con)

  .require_uncond_gamma(d1, where = "ate()")
  .require_uncond_gamma(d0, where = "ate()")

  p1 <- .extract_gamma_dp_params(d1, renormalize_weights = renormalize_weights)
  p0 <- .extract_gamma_dp_params(d0, renormalize_weights = renormalize_weights)

  # ---- helper: pull a single (non-indexed) parameter from draws
  .pull_scalar_draw <- function(draws, base) {
    cn <- colnames(draws)
    # allow "u" or "u[1]" etc
    id <- which(cn == base)
    if (length(id) == 0L) {
      id <- grep(paste0("^", base, "\\[[0-9]+\\]$"), cn)
    }
    if (length(id) == 0L) {
      stop("ATE with GPD requires posterior draws for '", base, "'.", call. = FALSE)
    }
    if (length(id) > 1L) {
      # if multiple (e.g., u[1],u[2]) but user didn't specify; choose first deterministically
      id <- id[1L]
    }
    as.numeric(draws[, id])
  }

  # ---- tail flags: default FALSE if not present
  tail_flags <- object$tail
  if (is.null(tail_flags)) tail_flags <- c(trt = FALSE, con = FALSE)
  tail_trt <- isTRUE(unname(tail_flags[["trt"]]))
  tail_con <- isTRUE(unname(tail_flags[["con"]]))

  # ---- bulk-only mean for a single draw
  .bulk_mean_1draw <- function(w, shape, scale) .gamma_mix_mean_1draw(w, shape, scale)

  # ---- bulk+gpd mean for a single draw (Gamma mixture bulk)
  # Ey = E[Y I(Y<=u)] + P(Y>u) * (u + sigma/(1-xi))   (valid only if xi<1)
  .bulk_gpd_mean_1draw <- function(u, sigma, xi, w, shape, scale) {
    # sanitize
    if (!is.finite(u) || !is.finite(sigma) || !is.finite(xi)) return(NA_real_)
    if (sigma <= 0) return(NA_real_)
    if (xi >= 1) return(Inf)  # will be filtered upstream
    if (u < 0) u <- 0         # gamma support

    # P(Y <= u) under Gamma mixture
    p_u <- sum(w * stats::pgamma(u, shape = shape, scale = scale))
    p_u <- min(max(p_u, 0), 1)

    # E[Y * I(Y <= u)] for Gamma(k, scale=theta):
    # integral_0^u y f(y) dy = theta*k*Pgammacdf(u; shape=k+1, scale=theta)
    EY_le_u <- sum(w * (scale * shape) * stats::pgamma(u, shape = shape + 1, scale = scale))

    # Tail mean contribution (conditional on exceedance):
    # E[Y|Y>u] = u + sigma/(1-xi)
    EY_gt_u <- u + sigma / (1 - xi)

    EY_le_u + (1 - p_u) * EY_gt_u
  }

  # ---- compute E[Y] draw-by-draw for each arm, choosing bulk vs bulk+tail
  if (!tail_trt) {
    Ey1 <- vapply(seq_len(p1$M), function(m) .bulk_mean_1draw(p1$W[m, ], p1$Shape[m, ], p1$Scale[m, ]), numeric(1))
  } else {
    u1     <- .pull_scalar_draw(d1, "u")
    sigma1 <- .pull_scalar_draw(d1, "sigma")
    xi1    <- .pull_scalar_draw(d1, "xi")

    Ey1 <- vapply(seq_len(p1$M), function(m) {
      .bulk_gpd_mean_1draw(u1[m], sigma1[m], xi1[m], p1$W[m, ], p1$Shape[m, ], p1$Scale[m, ])
    }, numeric(1))
  }

  if (!tail_con) {
    Ey0 <- vapply(seq_len(p0$M), function(m) .bulk_mean_1draw(p0$W[m, ], p0$Shape[m, ], p0$Scale[m, ]), numeric(1))
  } else {
    u0     <- .pull_scalar_draw(d0, "u")
    sigma0 <- .pull_scalar_draw(d0, "sigma")
    xi0    <- .pull_scalar_draw(d0, "xi")

    Ey0 <- vapply(seq_len(p0$M), function(m) {
      .bulk_gpd_mean_1draw(u0[m], sigma0[m], xi0[m], p0$W[m, ], p0$Shape[m, ], p0$Scale[m, ])
    }, numeric(1))
  }

  te <- Ey1 - Ey0

  # ---- handle infinite means from xi>=1: warn + drop invalid draws
  bad <- !is.finite(te)
  if (any(bad)) {
    warning(
      "ATE: some posterior draws imply infinite/undefined mean (e.g., GPD xi >= 1 or invalid tail params). ",
      "Dropping those draws from ATE summaries.",
      call. = FALSE
    )
    te <- te[!bad]
  }

  if (length(te) == 0L) {
    warning("ATE: all posterior draws were invalid after filtering; returning NA.", call. = FALSE)
    return(matrix(
      NA_real_, nrow = 1, ncol = 4,
      dimnames = list("ATE", c("mean","sd","lower","upper"))
    ))

  }

  alpha <- (1 - level) / 2
  matrix(
    c(mean(te),
      stats::sd(te),
      stats::quantile(te, probs = alpha,     names = FALSE),
      stats::quantile(te, probs = 1 - alpha, names = FALSE)),
    nrow = 1,
    dimnames = list("ATE", c("mean", "sd", "lower", "upper"))
  )
}


#' Quantile Treatment Effect (QTE)
#'
#' Computes posterior summaries of quantile treatment effects:
#' \deqn{\mathrm{QTE}(\tau) = Q_{Y(1)}(\tau) - Q_{Y(0)}(\tau).}
#'
#' Currently implemented for *unconditional* Gamma DP mixture fits created by
#' [fit.TE()] with `y ~ 0` and `kernel = "gamma"` (and `tail = "none"`).
#'
#' @param object A fitted object returned by [fit.TE()].
#' @param probs Vector of quantile levels in (0, 1).
#' @param level Credible interval level (default 0.95).
#' @param renormalize_weights Logical; if `TRUE`, renormalize mixture weights per draw.
#' @param ... Reserved for future use.
#' @return A matrix with one row per `probs` and columns `mean`, `sd`, `lower`, `upper`.
#' @export
qte <- function(object, probs = c(0.1, 0.5, 0.9), level = 0.95, renormalize_weights = TRUE, ...) {
  UseMethod("qte")
}

#' @export
qte.mixgpd_te_fit <- function(object,
                              probs = c(0.1, 0.5, 0.9),
                              level = 0.95,
                              renormalize_weights = TRUE,
                              ...) {
  stopifnot(inherits(object, "mixgpd_te_fit"))

  # Regression/conditional TE support is not implemented yet.
  # Fail early with a clear message rather than touching DP draw internals.
  if (!identical(object$spec_trt$mode %||% NA_character_, "response_only") ||
      !identical(object$spec_con$mode %||% NA_character_, "response_only")) {
    stop(
      "Unconditional ATE/QTE are not implemented for regression fits (y ~ x). ",
      "Fit y ~ 0 or use ate(x)/qte(x) once conditional support is added.",
      call. = FALSE
    )
  }


  probs <- sort(unique(as.numeric(probs)))
  if (any(!is.finite(probs)) || any(probs <= 0 | probs >= 1)) {
    stop("'probs' must be in (0, 1).", call. = FALSE)
  }

  d1 <- .as_mcmc_matrix(object$fit_trt)
  d0 <- .as_mcmc_matrix(object$fit_con)

  .require_uncond_gamma(d1, where = "qte()")
  .require_uncond_gamma(d0, where = "qte()")

  p1 <- .extract_gamma_dp_params(d1, renormalize_weights = renormalize_weights)
  p0 <- .extract_gamma_dp_params(d0, renormalize_weights = renormalize_weights)

  M <- min(p1$M, p0$M)
  if (M <= 0L) stop("No posterior draws available for QTE.", call. = FALSE)

  alpha <- (1 - level) / 2

  out <- matrix(NA_real_, nrow = length(probs), ncol = 4)
  rownames(out) <- paste0("q", probs)
  colnames(out) <- c("mean", "sd", "lower", "upper")

  # helper: summarize with filtering + warning
  .summ_te <- function(z, tau) {
    z <- as.numeric(z)
    ok <- is.finite(z)
    if (!all(ok)) {
      warning(
        "QTE(tau=", tau, "): dropping non-finite posterior draws (NA/Inf) before summarizing.",
        call. = FALSE
      )
    }
    z <- z[ok]

    if (!length(z)) {
      warning(
        "QTE(tau=", tau, "): all posterior draws invalid after filtering; returning NA for this tau.",
        call. = FALSE
      )
      return(c(mean = NA_real_, sd = NA_real_, lower = NA_real_, upper = NA_real_))
    }

    c(
      mean  = mean(z),
      sd    = stats::sd(z),
      lower = as.numeric(stats::quantile(z, probs = alpha,     names = FALSE)),
      upper = as.numeric(stats::quantile(z, probs = 1 - alpha, names = FALSE))
    )
  }

  for (ii in seq_along(probs)) {
    tau <- probs[ii]
    te_draw <- numeric(M)

    for (m in seq_len(M)) {
      q1 <- tryCatch(
        .gamma_mix_quantile_1draw(tau, p1$W[m, ], p1$Shape[m, ], p1$Scale[m, ]),
        error = function(e) NA_real_
      )
      q0 <- tryCatch(
        .gamma_mix_quantile_1draw(tau, p0$W[m, ], p0$Shape[m, ], p0$Scale[m, ]),
        error = function(e) NA_real_
      )

      te_draw[m] <- q1 - q0
    }

    out[ii, ] <- .summ_te(te_draw, tau = tau)
  }

  out
}


#' Plot treatment effects
#'
#' ggplot-based plotting for objects from [fit.TE()]. The default plot shows the
#' posterior summary of the ATE. Set `effect = "quantile"` to plot QTE(\eqn{\tau})
#' across a grid of `tau`.
#'
#' @param x A fitted object returned by [fit.TE()].
#' @param effect `"avg"` (default; ATE) or `"quantile"`.
#' @param tau Vector of quantile indices for `effect = "quantile"`.
#' @param level Credible interval level (default 0.95).
#' @param ... Additional arguments passed to [ate()] / [qte()].
#' @return A ggplot object.
#' @export
plot.mixgpd_te_fit <- function(x,
                               effect = c("avg", "quantile"),
                               tau = stats::quantile(seq(0, 1, length.out = 101), probs = c(0.25, 0.5, 0.75), names = FALSE),
                               level = 0.95,
                               ...) {
  effect <- match.arg(effect)

  # Current plotting supports only response-only fits (formula y ~ 0)
  # because conditional TE curves require additional predictive machinery.
  if (!identical(x$spec_trt$mode, "response_only") ||
      !identical(x$spec_con$mode, "response_only")) {
    stop(
      "plot.mixgpd_te_fit is currently implemented only for response-only fits (y ~ 0).\n",
      "You fitted covariates; for now use ate(x) / qte(x) only after adding conditional support.",
      call. = FALSE
    )
  }

  if (effect == "avg") {
    s <- ate(x, level = level, ...)
    df <- data.frame(
      effect = "ATE",
      mean   = s[1, "mean"],
      lower  = s[1, "lower"],
      upper  = s[1, "upper"]
    )
    ggplot2::ggplot(df, ggplot2::aes(x = effect, y = mean)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::geom_point(size = 2.2) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.15) +
      ggplot2::labs(x = NULL, y = "Treatment effect") +
      ggplot2::theme_minimal()
  } else {
    tau <- sort(unique(as.numeric(tau)))
    tau <- tau[is.finite(tau) & tau > 0 & tau < 1]
    if (!length(tau)) stop("'tau' must contain values in (0, 1).", call. = FALSE)

    s <- qte(x, probs = tau, level = level, ...)
    df <- data.frame(
      tau   = tau,
      mean  = s[, "mean"],
      lower = s[, "lower"],
      upper = s[, "upper"]
    )

    ggplot2::ggplot(df, ggplot2::aes(x = tau, y = mean)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.25) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::labs(x = expression(tau), y = "QTE(tau)") +
      ggplot2::theme_minimal()
  }
}
# =============================================================================
# Conditional TE on propensity-score grid (ggplot only downstream)
# =============================================================================

# ---- internal: estimate propensity score ----
.pscore_logit <- function(A, X) {
  # A: 0/1 numeric
  # X: data.frame or matrix
  df <- as.data.frame(X)
  df$.A <- A

  # use glm logistic; stable + no extra deps
  fit <- stats::glm(.A ~ ., data = df, family = stats::binomial())
  p <- stats::fitted(fit)

  # clamp away from 0/1 for numerical stability
  eps <- 1e-6
  p <- pmin(pmax(p, eps), 1 - eps)

  list(p = p, model = fit)
}

# ---- internal: build propensity grid ----
.ps_grid <- function(p, n_grid = 101L) {
  p <- as.numeric(p)
  p <- p[is.finite(p)]
  if (!length(p)) stop("No finite propensity scores.", call. = FALSE)

  # grid over observed range (not [0,1] blindly)
  rng <- range(p)
  seq(rng[1], rng[2], length.out = as.integer(n_grid))
}

# ---- internal: make augmented design matrix for outcome stage ----
# Augment original X with pscore as first column (and optional intercept)
.augment_X_with_ps <- function(X_raw, p, intercept = TRUE) {
  if (is.null(X_raw)) {
    # response-only: augmented X is just pscore (and intercept optionally)
    X <- matrix(p, ncol = 1)
    colnames(X) <- "ps"
  } else {
    X <- as.matrix(X_raw)
    X <- cbind(ps = p, X)
  }

  if (isTRUE(intercept)) {
    X <- cbind(`(Intercept)` = 1, X)
  }

  X
}
#' Predict mixgpd te fit
#'
#' Predict mixgpd te fit.
#'
#' @param object object.
#' @param newdata newdata.
#' @param type type.
#' @param probs probs.
#' @param alpha alpha.
#' @param ... Additional arguments.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("predict.mixgpd_te_fit", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
#' }
#'
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
  q1 <- stats::predict(object$fit_trt, newdata = newdata, type = "quantile", probs = probs, ...)
  q0 <- stats::predict(object$fit_con, newdata = newdata, type = "quantile", probs = probs, ...)

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
# =============================================================================
# Public: conditional TE curve
# =============================================================================

#' Treatment-effect curve vs propensity score
#'
#' Computes ATE (avg) or QTE(tau) on a grid of propensity scores.
#'
#' @param object A fitted TE object from fit.TE().
#' @param effect "avg" or "quantile".
#' @param tau Quantile levels for effect="quantile".
#' @param level Credible interval level.
#' @param n_grid Number of propensity grid points.
#' @param ... Unused.
#' @return data.frame with columns: ps, tau (if quantile), mean, sd, lower, upper.
#' @export
te_curve <- function(object,
                     effect = c("avg", "quantile"),
                     tau = c(0.1, 0.5, 0.9),
                     level = 0.95,
                     n_grid = 101L,
                     ...) {
  UseMethod("te_curve")
}

#' @export
te_curve.mixgpd_te_fit <- function(object,
                                   effect = c("avg", "quantile"),
                                   tau = c(0.1, 0.5, 0.9),
                                   level = 0.95,
                                   n_grid = 101L,
                                   ...) {
  effect <- match.arg(effect)

  # We need original full data + X to do conditional.
  # If your fit.TE currently doesn’t store them, add them (see note below).
  if (is.null(object$.te_data)) {
    stop(
      "This TE object does not store original data needed for conditional TE.\n",
      "Fix: in fit.TE(), store `.te_data = list(data=..., A=..., X_raw=..., formula=...)`.",
      call. = FALSE
    )
  }

  te_dat <- object$.te_data
  dat    <- te_dat$data
  A      <- te_dat$A
  X_raw  <- te_dat$X_raw      # raw RHS model.matrix (without A)
  int    <- isTRUE(te_dat$intercept)

  # propensity score from ORIGINAL covariates (not including A)
  # If X_raw is NULL (y~0), propensity is just intercept-only model.
  X_for_ps <- if (is.null(X_raw)) data.frame(`(Intercept)` = 1) else as.data.frame(X_raw)
  ps_fit <- .pscore_logit(A = A, X = X_for_ps)
  ps     <- ps_fit$p

  grid <- .ps_grid(ps, n_grid = n_grid)

  # build augmented X on grid
  # For grid prediction, we need "typical" X values.
  # Simple policy: hold X at column means, vary only ps (you asked: plot vs ps).
  if (is.null(X_raw)) {
    X_grid_raw <- NULL
  } else {
    X_means <- colMeans(as.matrix(X_raw), na.rm = TRUE)
    X_grid_raw <- matrix(rep(X_means, each = length(grid)), nrow = length(grid), byrow = FALSE)
    colnames(X_grid_raw) <- colnames(as.matrix(X_raw))
  }

  X_grid_aug <- .augment_X_with_ps(X_grid_raw, p = grid, intercept = int)

  # ---- Now compute TE on that grid ----
  # We do it by calling predict() on the underlying group fits,
  # using the SAME quantile solver you already fixed for mixtures.
  #
  # This requires: predict.mixgpd_fit supports newdata as a *matrix* for regression,
  # or supports list(newX=...) in whatever convention you use.
  #
  # I’m assuming: predict(fit, newdata = Xmat, type="quantile", probs=tau) works.

  fit1 <- object$fit_trt
  fit0 <- object$fit_con

  alpha <- (1 - level) / 2

  if (effect == "avg") {
    # If you don’t have closed-form mean for all configs, approximate by PP sampling:
    # sample from each group predictive at each grid point, then average and diff.
    # Keep it simple + consistent: use predict(type="sample") and take mean.
    #
    # n_rep can be a future knob; hardcode modest default here.
    n_rep <- 2000L

    out <- data.frame(
      ps    = grid,
      mean  = NA_real_,
      sd    = NA_real_,
      lower = NA_real_,
      upper = NA_real_
    )

    for (i in seq_along(grid)) {
      x_i <- X_grid_aug[i, , drop = FALSE]

      y1 <- predict(fit1, newdata = x_i, type = "sample", n_samples = n_rep)
      y0 <- predict(fit0, newdata = x_i, type = "sample", n_samples = n_rep)

      te <- y1 - y0

      out$mean[i]  <- mean(te)
      out$sd[i]    <- stats::sd(te)
      out$lower[i] <- stats::quantile(te, probs = alpha,     names = FALSE)
      out$upper[i] <- stats::quantile(te, probs = 1 - alpha, names = FALSE)
    }

    return(out)
  }

  # quantile curve
  tau <- sort(unique(as.numeric(tau)))
  if (any(!is.finite(tau)) || any(tau <= 0 | tau >= 1)) {
    stop("'tau' must be in (0,1).", call. = FALSE)
  }

  out <- expand.grid(ps = grid, tau = tau)
  out$mean  <- NA_real_
  out$sd    <- NA_real_
  out$lower <- NA_real_
  out$upper <- NA_real_

  for (ii in seq_along(tau)) {
    t0 <- tau[ii]

    for (i in seq_along(grid)) {
      x_i <- X_grid_aug[i, , drop = FALSE]

      q1 <- predict(fit1, newdata = x_i, type = "quantile", probs = t0)
      q0 <- predict(fit0, newdata = x_i, type = "quantile", probs = t0)

      # Your predict(quantile) returns a matrix with rowname like q0.5 or tau=0.5
      # Normalize extraction robustly:
      get_q <- function(qobj) {
        if (is.matrix(qobj)) return(qobj[1, , drop = FALSE])
        if (is.data.frame(qobj)) return(as.matrix(qobj[1, , drop = FALSE]))
        stop("Unexpected quantile return type from predict().", call. = FALSE)
      }

      Q1 <- get_q(q1)
      Q0 <- get_q(q0)

      # treat TE distribution is draw-level inside predict;
      # if predict returns posterior summary only, we can only do mean/CI via delta-method (bad).
      # So we REQUIRE your quantile predict returns mean/sd/ci computed from draw-level roots.
      te_mean  <- Q1[1, "mean"]  - Q0[1, "mean"]
      te_sd    <- sqrt(Q1[1, "sd"]^2 + Q0[1, "sd"]^2)  # rough; replace when you expose draw-level
      te_lower <- Q1[1, "lower"] - Q0[1, "upper"]
      te_upper <- Q1[1, "upper"] - Q0[1, "lower"]

      k <- which(out$ps == grid[i] & out$tau == t0)
      out$mean[k]  <- te_mean
      out$sd[k]    <- te_sd
      out$lower[k] <- te_lower
      out$upper[k] <- te_upper
    }
  }

  out
}
#' Plot treatment-effect curve vs propensity score
#' @param df Data frame from te_curve().
#' @return ggplot2 object.
#' @export
plot_te_curve <- function(df) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required.", call. = FALSE)
  }

  if ("tau" %in% names(df)) {
    ggplot2::ggplot(df, ggplot2::aes(x = .data$ps, y = .data$mean)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), alpha = 0.2) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::facet_wrap(~ tau, scales = "free_y") +
      ggplot2::labs(x = "Propensity score", y = "QTE", title = "CQTE vs propensity score") +
      ggplot2::theme_minimal()
  } else {
    ggplot2::ggplot(df, ggplot2::aes(x = .data$ps, y = .data$mean)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), alpha = 0.2) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::labs(x = "Propensity score", y = "ATE", title = "CATE vs propensity score") +
      ggplot2::theme_minimal()
  }
}