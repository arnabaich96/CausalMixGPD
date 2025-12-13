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

#' @keywords internal
.build_X_aug <- function(X, ehat, include_X = TRUE) {
  X <- as.matrix(X)
  ehat <- as.numeric(ehat)

  if (length(ehat) != nrow(X)) {
    stop("Length mismatch: ehat vs nrow(X).", call. = FALSE)
  }

  if (include_X) {
    X_aug <- cbind(ps = ehat, X)
  } else {
    X_aug <- matrix(ehat, ncol = 1L)
    colnames(X_aug) <- "ps"
  }

  X_aug
}
#' Fit treatment effects model (two-group DP mixture engine)
#'
#' @description
#' Fit a causal treatment effects model by estimating the density of the response
#' separately for the treatment and control groups using Dirichlet process mixtures.
#' This allows for the estimation of heterogeneous treatment effects, such as the
#' Average Treatment Effect (ATE) and Quantile Treatment Effects (QTE), without
#' assuming parametric forms for the response distributions.
#'
#' @param formula A formula specifying the outcome model, e.g. \code{y ~ x1 + x2}.
#'   The covariates on the RHS are used to model the parameters of the kernel
#'   distributions in both groups.
#' @param data A \code{data.frame} containing the variables referenced in \code{formula}
#'   and the treatment variable \code{A}.
#' @param A A string specifying the name of the binary treatment column in \code{data},
#'   or a numeric/integer vector of the same length as \code{data} containing 0s and 1s.
#' @param kernel Character vector specifying the bulk kernel for the mixture components.
#'   If length 1, the same kernel is used for both groups. If length 2, the first
#'   kernel is used for the treatment group and the second for control.
#'   Options: \code{"gamma"}, \code{"lognormal"}, \code{"normal"}, \code{"laplace"},
#'   \code{"inverse_gaussian"}, \code{"amoroso"}, \code{"pareto"}.
#' @param tail Character string specifying the tail behavior.
#'   \code{"none"} uses the pure DP mixture; \code{"gpd"} adds a Generalized Pareto
#'   tail for heavy-tailed extrapolation (not yet fully active in the engine).
#' @param dp_rep Character string selecting the Dirichlet process representation,
#'   either \code{"stick_breaking"} (truncated) or \code{"crp"} (Chinese Restaurant Process).
#' @param dp_ctrl List of control arguments for the DP representation,
#'   such as \code{K} (truncation level) when \code{dp_rep = "stick_breaking"}.
#' @param priors Optional list of prior hyperparameters for the mixture components.
#' @param tail_priors Optional list of prior hyperparameters for the GPD tail (if \code{tail = "gpd"}).
#' @param trans Optional list describing transformations to apply to covariates
#'   (e.g., centering/scaling).
#' @param mcmc List of MCMC control settings, typically including
#'   \code{n_iter}, \code{burn_in}, \code{thin}, and \code{chains}.
#' @param alpha Nominal level used for summarizing posterior quantities
#'   (e.g., \code{alpha = 0.05} for 95\% credible intervals).
#' @param intercept Logical; if \code{TRUE} (default), an intercept is added to the
#'   design matrix for the covariate-dependent parameters.
#' @param seed Optional integer to set the random seed for reproducibility.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return An object of class \code{"mixgpd_te_fit"} containing:
#'   \itemize{
#'     \item \code{call}: The matched call.
#'     \item \code{formula}: The model formula.
#'     \item \code{A_name}: Name of the treatment variable.
#'     \item \code{spec_trt}, \code{spec_con}: Model specifications for treatment and control groups.
#'     \item \code{fit_trt}, \code{fit_con}: The fitted \code{mixgpd_fit} objects for each group.
#'     \item \code{alpha}: The significance level.
#'   }
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' N <- 200
#' dat <- data.frame(
#'   A = rbinom(N, 1, 0.5),
#'   x = rnorm(N)
#' )
#' # Treatment group shifts mean
#' dat$y <- ifelse(dat$A == 1,
#'   rgamma(N, shape = 5, scale = 1),
#'   rgamma(N, shape = 2, scale = 1)
#' )
#'
#' fit <- fit.TE(
#'   y ~ x,
#'   data = dat,
#'   A = "A",
#'   kernel = "gamma",
#'   dp_rep = "stick_breaking",
#'   dp_ctrl = list(K = 10),
#'   mcmc = list(n_iter = 1000, burn_in = 500)
#' )
#'
#' print(fit)
#' plot(fit, effect = "quantile")
#' }
#'
#' @export
fit.TE <- function(formula,
                   data,
                   A,
                   kernel = c("gamma", "lognormal", "normal", "laplace", "inverse_gaussian", "amoroso", "pareto"),
                   tail = c("none", "gpd"),
                   dp_rep = c("stick_breaking", "crp"),
                   dp_ctrl = list(K = 5),
                   priors = list(),
                   tail_priors = list(),
                   trans = list(),
                   mcmc = list(),
                   alpha = 0.05,
                   intercept = TRUE,
                   seed = NULL,
                   ...) {
  if (!is.null(seed)) set.seed(seed)

  formula <- stats::as.formula(formula)
  tail <- match.arg(tail)
  dp_rep <- match.arg(dp_rep)

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.", call. = FALSE)
  }

  # ---- extract Y and RHS design like fit.dpm ----
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.omit)
  Y <- stats::model.response(mf)

  tt <- stats::terms(formula, data = mf)

  mm <- try(stats::model.matrix(tt, data = mf), silent = TRUE)
  if (inherits(mm, "try-error")) {
    stop("Could not build model matrix from formula. Check RHS variables are in 'data'.",
      call. = FALSE
    )
  }

  # remove default intercept column from model.matrix; we handle intercept via helper
  if ("(Intercept)" %in% colnames(mm)) {
    mm_noint <- mm[, setdiff(colnames(mm), "(Intercept)"), drop = FALSE]
  } else {
    mm_noint <- mm
  }

  X_raw <- if (ncol(mm_noint) == 0L) NULL else mm_noint

  # ensure X is always a matrix when not NULL, and optionally add intercept when p==1
  X_use <- .prepare_design_matrix(X_raw, intercept = intercept)

  # ---- get A aligned to rows used in mf (NA handling) ----
  idx <- as.integer(rownames(mf))

  A_use <- NULL
  if (is.character(A) && length(A) == 1L) {
    if (!A %in% names(data)) stop("A column '", A, "' not found in data.", call. = FALSE)
    A_use <- data[[A]][idx]
  } else {
    if (length(A) != nrow(data)) stop("If A is a vector, it must have length nrow(data).", call. = FALSE)
    A_use <- A[idx]
  }

  # coerce A to {0,1}
  if (anyNA(A_use)) stop("Treatment A has missing values (after NA dropping from formula rows).", call. = FALSE)

  if (is.logical(A_use)) A_use <- as.integer(A_use)
  if (is.factor(A_use)) {
    if (nlevels(A_use) != 2L) stop("A must be binary (2-level factor).", call. = FALSE)
    A_use <- as.integer(A_use == levels(A_use)[2L])
  }
  A_use <- as.integer(A_use)
  if (!all(A_use %in% c(0L, 1L))) stop("A must be binary coded as 0/1 (or coercible).", call. = FALSE)

  N <- length(Y)
  if (length(A_use) != N) stop("Internal error: A length mismatch after row filtering.", call. = FALSE)
  if (sum(A_use == 1L) == 0L || sum(A_use == 0L) == 0L) {
    stop("Both treatment groups must have at least one observation.", call. = FALSE)
  }

  # ---- kernel rule: length 1 => same kernel both groups; length 2 => (treat, control) ----
  if (length(kernel) == 1L) {
    ker_trt <- ker_con <- match.arg(kernel, choices = c("gamma", "lognormal", "normal", "laplace", "inverse_gaussian", "amoroso", "pareto"))
  } else if (length(kernel) == 2L) {
    ker_trt <- match.arg(kernel[1L], choices = c("gamma", "lognormal", "normal", "laplace", "inverse_gaussian", "amoroso", "pareto"))
    ker_con <- match.arg(kernel[2L], choices = c("gamma", "lognormal", "normal", "laplace", "inverse_gaussian", "amoroso", "pareto"))
  } else {
    stop("'kernel' must have length 1 or 2 (treat, control).", call. = FALSE)
  }

  # ---- build two specs (treat/control) and run engines (you’ll wire this) ----
  spec_trt <- build_model_spec(
    Y = Y[A_use == 1L],
    X = if (is.null(X_use)) NULL else X_use[A_use == 1L, , drop = FALSE],
    kernel = ker_trt,
    tail = tail,
    dp_rep = dp_rep,
    priors = priors,
    dp_ctrl = dp_ctrl,
    trans = trans,
    alpha = alpha
  )

  spec_con <- build_model_spec(
    Y = Y[A_use == 0L],
    X = if (is.null(X_use)) NULL else X_use[A_use == 0L, , drop = FALSE],
    kernel = ker_con,
    tail = tail,
    dp_rep = dp_rep,
    priors = priors,
    dp_ctrl = dp_ctrl,
    trans = trans,
    alpha = alpha
  )

  fit_trt <- run_mcmc_engine(spec_trt, mcmc)
  fit_con <- run_mcmc_engine(spec_con, mcmc)

  structure(
    list(
      call = match.call(),
      formula = formula,
      A_name = if (is.character(A) && length(A) == 1L) A else NULL,
      spec_trt = spec_trt,
      spec_con = spec_con,
      fit_trt = fit_trt,
      fit_con = fit_con,
      alpha = alpha
    ),
    class = "mixgpd_te_fit"
  )
}

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

#' @export
plot.mixgpd_te <- function(x, what = c("ps"), ...) {
  what <- match.arg(what)

  if (what == "ps") {
    if (is.null(x$ps)) stop("No propensity scores were fit (unconditional model).", call. = FALSE)
    hist(x$ps$ehat, breaks = 30, main = "Estimated propensity scores", xlab = "e_hat")
    return(invisible(NULL))
  }

  invisible(NULL)
}
# ============================================================================
# Internal: TE draws for unconditional Gamma DP mixtures (kernel = "gamma")
# ============================================================================

.as_draws_matrix <- function(x) {
  if (is.matrix(x)) {
    return(x)
  }
  if (inherits(x, "mcmc") || inherits(x, "mcmc.list")) {
    return(as.matrix(x))
  }
  if (is.data.frame(x)) {
    return(as.matrix(x))
  }
  stop("Cannot coerce draws to matrix.", call. = FALSE)
}

.extract_gamma_uncond_dp <- function(draws_mat) {
  cn <- colnames(draws_mat)

  w_cols <- grep("^w\\[", cn, value = TRUE)
  sh_cols <- grep("^shape\\[", cn, value = TRUE)
  sc_cols <- grep("^scale\\[", cn, value = TRUE)

  if (length(w_cols) == 0L) stop("Cannot find 'w[j]' in MCMC draws.", call. = FALSE)
  if (length(sh_cols) == 0L) stop("Cannot find 'shape[j]' in MCMC draws.", call. = FALSE)
  if (length(sc_cols) == 0L) stop("Cannot find 'scale[j]' in MCMC draws.", call. = FALSE)

  # order by component index
  w_j <- as.integer(sub("^w\\[([0-9]+)\\]$", "\\1", w_cols))
  sh_j <- as.integer(sub("^shape\\[([0-9]+)\\]$", "\\1", sh_cols))
  sc_j <- as.integer(sub("^scale\\[([0-9]+)\\]$", "\\1", sc_cols))

  w_cols <- w_cols[order(w_j)]
  sh_cols <- sh_cols[order(sh_j)]
  sc_cols <- sc_cols[order(sc_j)]

  w_j <- sort(w_j)
  sh_j <- sort(sh_j)
  sc_j <- sort(sc_j)

  if (!identical(w_j, sh_j) || !identical(w_j, sc_j)) {
    stop("Component index mismatch among w/shape/scale columns.", call. = FALSE)
  }

  list(
    W     = draws_mat[, w_cols, drop = FALSE],
    Shape = draws_mat[, sh_cols, drop = FALSE],
    Scale = draws_mat[, sc_cols, drop = FALSE],
    K     = length(w_cols),
    M     = nrow(draws_mat)
  )
}

.gamma_mix_mean <- function(w, shape, scale) {
  # mean(Gamma(shape, scale)) = shape * scale
  sum(w * (shape * scale))
}

.gamma_mix_cdf <- function(y, w, shape, scale) {
  # pgamma uses shape+scale or shape+rate; we use scale
  sum(w * stats::pgamma(y, shape = shape, scale = scale))
}

.gamma_mix_quantile_uniroot <- function(tau, w, shape, scale,
                                        lower = 0, upper = NULL,
                                        tol = 1e-8, maxiter = 200) {
  stopifnot(length(tau) == 1L, is.finite(tau), tau > 0, tau < 1)

  # pick an automatic upper bracket if not given
  if (is.null(upper)) {
    # a conservative start: max component quantile * 2
    upper <- max(stats::qgamma(tau, shape = shape, scale = scale)) * 2
    if (!is.finite(upper) || upper <= 0) upper <- 1
  }

  # expand upper until CDF(upper) >= tau
  Fu <- .gamma_mix_cdf(upper, w, shape, scale)
  iter_expand <- 0L
  while (Fu < tau && iter_expand < 60L) {
    upper <- upper * 2
    Fu <- .gamma_mix_cdf(upper, w, shape, scale)
    iter_expand <- iter_expand + 1L
  }
  if (Fu < tau) {
    stop("Failed to bracket quantile (upper bound too small even after expansion).", call. = FALSE)
  }

  f <- function(x) .gamma_mix_cdf(x, w, shape, scale) - tau
  stats::uniroot(f, lower = lower, upper = upper, tol = tol, maxiter = maxiter)$root
}

# ============================================================================
# Internal: compute TE draws (ATE / CQTE) for unconditional Gamma DP only
# ============================================================================

.compute_te_draws <- function(fit_trt,
                              fit_con,
                              effect = c("average", "quantile"),
                              tau = c(0.25, 0.5, 0.75),
                              n_rep = 1000L,
                              seed = NULL) {
  effect <- match.arg(effect)
  n_rep <- as.integer(n_rep)
  if (n_rep <= 0L) stop("'n_rep' must be positive.", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)

  d_trt <- .as_draws_matrix(fit_trt$mcmc_draws)
  d_con <- .as_draws_matrix(fit_con$mcmc_draws)

  p_trt <- .extract_gamma_uncond_dp(d_trt)
  p_con <- .extract_gamma_uncond_dp(d_con)

  # sample posterior iterations (with replacement)
  it_trt <- sample.int(p_trt$M, n_rep, replace = TRUE)
  it_con <- sample.int(p_con$M, n_rep, replace = TRUE)

  if (effect == "average") {
    out <- numeric(n_rep)
    for (s in seq_len(n_rep)) {
      m1 <- it_trt[s]
      m0 <- it_con[s]
      w1 <- as.numeric(p_trt$W[m1, ])
      sh1 <- as.numeric(p_trt$Shape[m1, ])
      sc1 <- as.numeric(p_trt$Scale[m1, ])
      w0 <- as.numeric(p_con$W[m0, ])
      sh0 <- as.numeric(p_con$Shape[m0, ])
      sc0 <- as.numeric(p_con$Scale[m0, ])

      out[s] <- .gamma_mix_mean(w1, sh1, sc1) - .gamma_mix_mean(w0, sh0, sc0)
    }
    return(data.frame(
      iter   = seq_len(n_rep),
      value  = out,
      effect = "average",
      tau    = NA_real_
    ))
  }

  # quantile effect (CQTE): invert mixture CDF with uniroot
  tau <- sort(as.numeric(tau))
  if (any(!is.finite(tau)) || any(tau <= 0 | tau >= 1)) {
    stop("'tau' must be within (0, 1).", call. = FALSE)
  }

  res_list <- vector("list", length(tau))
  for (k in seq_along(tau)) {
    tk <- tau[k]
    out <- numeric(n_rep)

    for (s in seq_len(n_rep)) {
      m1 <- it_trt[s]
      m0 <- it_con[s]
      w1 <- as.numeric(p_trt$W[m1, ])
      sh1 <- as.numeric(p_trt$Shape[m1, ])
      sc1 <- as.numeric(p_trt$Scale[m1, ])
      w0 <- as.numeric(p_con$W[m0, ])
      sh0 <- as.numeric(p_con$Shape[m0, ])
      sc0 <- as.numeric(p_con$Scale[m0, ])

      q1 <- .gamma_mix_quantile_uniroot(tk, w1, sh1, sc1)
      q0 <- .gamma_mix_quantile_uniroot(tk, w0, sh0, sc0)

      out[s] <- q1 - q0
    }

    res_list[[k]] <- data.frame(
      iter   = seq_len(n_rep),
      value  = out,
      effect = "quantile",
      tau    = tk
    )
  }

  do.call(rbind, res_list)
}
# ============================================================================
# S3 summary method for TE fits
# ============================================================================

#' @export
summary.mixgpd_te_fit <- function(object,
                                  effect = c("average", "quantile"),
                                  tau = c(0.25, 0.5, 0.75),
                                  level = 0.95,
                                  n_rep = 1000,
                                  ...) {
  effect <- match.arg(effect)
  alpha <- 1 - level

  draws <- .compute_te_draws(
    fit_trt = object$fit_trt,
    fit_con = object$fit_con,
    effect  = effect,
    tau     = tau,
    n_rep   = n_rep
  )

  if (effect == "average") {
    est <- mean(draws$value)
    sdv <- stats::sd(draws$value)
    ci <- stats::quantile(draws$value, probs = c(alpha / 2, 1 - alpha / 2))

    out <- data.frame(
      effect = "ATE",
      estimate = est,
      sd = sdv,
      ci_lower = ci[1],
      ci_upper = ci[2],
      level = level
    )

    rownames(out) <- NULL
    return(out)
  }

  # CQTE summary by tau
  out <- do.call(rbind, lapply(split(draws, draws$tau), function(d) {
    ci <- stats::quantile(d$value, probs = c(alpha / 2, 1 - alpha / 2))
    data.frame(
      tau = unique(d$tau),
      estimate = mean(d$value),
      sd = stats::sd(d$value),
      ci_lower = ci[1],
      ci_upper = ci[2],
      level = level
    )
  }))

  rownames(out) <- NULL
  out
}
#' @export
print.mixgpd_te_fit <- function(x, ...) {
  cat("Treatment effect fit (DPmixGPD)\n")
  cat("Formula: ")
  print(x$formula)
  cat("A:", x$A_name, "\n")
  cat("Treatment kernel:", x$spec_trt$kernel, " | tail:", x$spec_trt$tail, "\n")
  cat("Control   kernel:", x$spec_con$kernel, " | tail:", x$spec_con$tail, "\n")
  cat("N_treat =", x$spec_trt$N, "  N_control =", x$spec_con$N, "\n")
  invisible(x)
}
#' @export
plot.mixgpd_te_fit <- function(x,
                               effect = c("average", "quantile"),
                               tau = c(0.25, 0.5, 0.75),
                               level = 0.95,
                               n_rep = 1000,
                               xlab = NULL,
                               ylab = NULL,
                               main = NULL,
                               show_points = TRUE,
                               show_band = TRUE,
                               lwd = 2,
                               ...) {
  effect <- match.arg(effect)

  if (effect == "average") {
    s <- summary(x, effect = "average", level = level, n_rep = n_rep)

    if (is.null(main)) main <- "Average Treatment Effect (ATE)"
    if (is.null(xlab)) xlab <- ""
    if (is.null(ylab)) ylab <- "Effect"

    # simple 1D "dot + interval" plot
    plot(NA,
      xlim = c(0.5, 1.5),
      ylim = range(c(s$ci_lower, s$ci_upper, s$estimate), finite = TRUE),
      xaxt = "n", xlab = xlab, ylab = ylab, main = main
    )

    axis(1, at = 1, labels = "ATE")

    # CI line
    segments(1, s$ci_lower, 1, s$ci_upper, lwd = lwd)
    # end caps
    segments(0.95, s$ci_lower, 1.05, s$ci_lower, lwd = lwd)
    segments(0.95, s$ci_upper, 1.05, s$ci_upper, lwd = lwd)

    if (show_points) points(1, s$estimate, pch = 19, cex = 1.2)

    mtext(sprintf("level = %.2f", s$level), side = 3, line = 0.2, cex = 0.85)
    return(invisible(s))
  }

  # CQTE curve
  s <- summary(x, effect = "quantile", tau = tau, level = level, n_rep = n_rep)
  s <- s[order(s$tau), , drop = FALSE]

  if (is.null(main)) main <- "Conditional Quantile Treatment Effect (CQTE)"
  if (is.null(xlab)) xlab <- expression(tau)
  if (is.null(ylab)) ylab <- "Effect"

  ylim <- range(c(s$ci_lower, s$ci_upper, s$estimate), finite = TRUE)

  plot(s$tau, s$estimate,
    type = "n", ylim = ylim,
    xlab = xlab, ylab = ylab, main = main
  )

  # CI band as vertical segments (base-R friendly)
  if (show_band) {
    segments(s$tau, s$ci_lower, s$tau, s$ci_upper)
  }

  # estimate curve
  lines(s$tau, s$estimate, lwd = lwd)

  if (show_points) points(s$tau, s$estimate, pch = 19, cex = 0.7)

  abline(h = 0, lty = 2)
  mtext(sprintf("level = %.2f", level), side = 3, line = 0.2, cex = 0.85)

  invisible(s)
}
