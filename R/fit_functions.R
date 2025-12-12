# User-facing fitting functions -----------------------------------------------

#' Fit DP mixture model without GPD tail (Y/X interface)
#' @export
fit_mixgpd_xy <- function(
  Y,
  X        = NULL,
  kernel   = c("gamma", "lognormal", "normal", "laplace", "inverse_gaussian", "amoroso", "pareto"),
  dp_rep   = c("stick_breaking", "crp"),
  dp_ctrl  = list(),
  priors   = list(),
  trans    = list(),
  mcmc     = list(),
  alpha    = 0.05,
  seed     = NULL
) {
  kernel <- match.arg(kernel)
  dp_rep <- match.arg(dp_rep)

  if (!is.null(seed)) set.seed(seed)

  mode <- if (is.null(X)) "response_only" else "regression"

  spec <- build_model_spec_xy(
    Y      = Y,
    X      = X,
    mode   = mode,
    kernel = kernel,
    dp_rep = dp_rep,
    dp_ctrl = dp_ctrl,
    priors  = priors,
    trans   = trans,
    tail    = "none"
  )

  res <- run_mcmc_engine(spec, mcmc)

  structure(
    list(
      call       = match.call(),
      spec       = spec,
      mcmc_draws = res$mcmc_draws,
      mcmc_info  = res$mcmc_info,
      alpha      = alpha
    ),
    class = "mixgpd_fit"
  )
}

#' Fit DP mixture model without GPD tail (formula interface)
#' @export
fit_mixgpd <- function(
  formula,
  data,
  kernel   = c("gamma", "lognormal", "normal", "laplace", "inverse_gaussian", "amoroso", "pareto"),
  dp_rep   = c("stick_breaking", "crp"),
  dp_ctrl  = list(),
  priors   = list(),
  trans    = list(),
  mcmc     = list(),
  alpha    = 0.05,
  seed     = NULL
) {
  kernel <- match.arg(kernel)
  dp_rep <- match.arg(dp_rep)

  data <- as.data.frame(data)
  mf   <- stats::model.frame(formula, data = data)
  Y    <- stats::model.response(mf)

  tt          <- stats::terms(formula, data = data)
  term_labels <- attr(tt, "term.labels")
  has_int     <- attr(tt, "intercept") == 1
  n_terms     <- length(term_labels)

  mode_formula <- if (n_terms == 0 && !has_int) {
    "response_only"     # y ~ 0
  } else if (n_terms == 0 && has_int) {
    stop("Formula 'y ~ 1' (intercept-only) is not supported. Use 'y ~ 0' (unconditional) or include covariates.")
  } else {
    "regression"
  }

  y_name  <- all.vars(formula)[1L]
  x_names <- setdiff(colnames(mf), y_name)

  if (mode_formula == "regression" && length(x_names) == 0L) {
    stop("No covariates found in data. Use ", y_name, " ~ 0 for an unconditional model.")
  }

  if (mode_formula == "response_only") {
    X <- NULL
  } else {
    X <- stats::model.matrix(tt, data = mf)
  }

  fit_mixgpd_xy(
    Y      = Y,
    X      = X,
    kernel = kernel,
    dp_rep = dp_rep,
    dp_ctrl = dp_ctrl,
    priors  = priors,
    trans   = trans,
    mcmc    = mcmc,
    alpha   = alpha,
    seed    = seed
  )
}

#' Fit DP mixture model with GPD tail (Y/X interface)
#' @export
fit_mixgpd_gpd_xy <- function(
  Y,
  X        = NULL,
  kernel   = c("gamma", "lognormal", "normal", "laplace", "inverse_gaussian", "amoroso", "pareto"),
  dp_rep   = c("stick_breaking", "crp"),
  dp_ctrl  = list(),
  priors   = list(),
  trans    = list(),
  mcmc     = list(),
  alpha    = 0.05,
  seed     = NULL
) {
  kernel <- match.arg(kernel)
  dp_rep <- match.arg(dp_rep)

  if (!is.null(seed)) set.seed(seed)

  mode <- if (is.null(X)) "response_only" else "regression"

  spec <- build_model_spec_xy(
    Y      = Y,
    X      = X,
    mode   = mode,
    kernel = kernel,
    dp_rep = dp_rep,
    dp_ctrl = dp_ctrl,
    priors  = priors,
    trans   = trans,
    tail    = "gpd"
  )

  res <- run_mcmc_engine(spec, mcmc)

  structure(
    list(
      call       = match.call(),
      spec       = spec,
      mcmc_draws = res$mcmc_draws,
      mcmc_info  = res$mcmc_info,
      alpha      = alpha
    ),
    class = "mixgpd_fit"
  )
}

#' Fit DP mixture model with GPD tail (formula interface)
#' @export
fit_mixgpd_gpd <- function(
  formula,
  data,
  kernel   = c("gamma", "lognormal", "normal", "laplace", "inverse_gaussian", "amoroso", "pareto"),
  dp_rep   = c("stick_breaking", "crp"),
  dp_ctrl  = list(),
  priors   = list(),
  trans    = list(),
  mcmc     = list(),
  alpha    = 0.05,
  seed     = NULL
) {
  kernel <- match.arg(kernel)
  dp_rep <- match.arg(dp_rep)

  data <- as.data.frame(data)
  mf   <- stats::model.frame(formula, data = data)
  Y    <- stats::model.response(mf)

  tt          <- stats::terms(formula, data = data)
  term_labels <- attr(tt, "term.labels")
  has_int     <- attr(tt, "intercept") == 1
  n_terms     <- length(term_labels)

  mode_formula <- if (n_terms == 0 && !has_int) {
    "response_only"
  } else if (n_terms == 0 && has_int) {
    stop("Formula 'y ~ 1' (intercept-only) is not supported. Use 'y ~ 0' (unconditional) or include covariates.")
  } else {
    "regression"
  }

  y_name  <- all.vars(formula)[1L]
  x_names <- setdiff(colnames(mf), y_name)

  if (mode_formula == "regression" && length(x_names) == 0L) {
    stop("No covariates found in data. Use ", y_name, " ~ 0 for an unconditional model.")
  }

  if (mode_formula == "response_only") {
    X <- NULL
  } else {
    X <- stats::model.matrix(tt, data = mf)
  }

  fit_mixgpd_gpd_xy(
    Y      = Y,
    X      = X,
    kernel = kernel,
    dp_rep = dp_rep,
    dp_ctrl = dp_ctrl,
    priors = priors,
    trans  = trans,
    mcmc   = mcmc,
    alpha  = alpha,
    seed   = seed
  )
}
#' @export
summary.mixgpd_fit <- function(object,
                               probs = c(0.025, 0.5, 0.975),
                               ...) {
  draws <- object$mcmc_draws

  if (is.null(draws)) {
    stop("No MCMC draws stored in 'object$mcmc_draws'.", call. = FALSE)
  }
  if (!is.matrix(draws)) {
    stop("'object$mcmc_draws' must be a matrix (iterations x parameters).", call. = FALSE)
  }

  means <- colMeans(draws)
  sds   <- apply(draws, 2L, stats::sd)
  qs    <- t(apply(draws, 2L, stats::quantile, probs = probs))

  out <- cbind(
    mean = means,
    sd   = sds,
    qs
  )

  res <- list(
    call      = object$call,
    kernel    = object$kernel %||% object$spec$kernel,
    tail      = object$tail   %||% object$spec$tail,
    mode      = object$mode   %||% object$spec$mode,
    alpha     = object$alpha  %||% object$spec$alpha,
    summary   = out,
    probs     = probs
  )

  class(res) <- "summary.mixgpd_fit"
  res
}

#' @export
print.summary.mixgpd_fit <- function(x, digits = 3, ...) {
  cat("Summary of DP mixture fit\n")
  cat("  Kernel:", x$kernel, "\n")
  cat("  Tail:  ", x$tail,   "\n")
  cat("  Mode:  ", x$mode,   "\n")
  cat("  Alpha: ", x$alpha,  "\n\n")

  print(round(x$summary, digits = digits))
  invisible(x)
}
