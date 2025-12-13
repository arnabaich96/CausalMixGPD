# User-facing fitting functions -----------------------------------------------

#' Fit DP mixture model without GPD tail (Y/X interface)
#'
#' @description
#' A matrix-interface alternative to \code{\link{fit_mixgpd}}. Fits a truncated
#' Dirichlet process mixture model for a univariate response \code{Y} using
#' a positive-support kernel (Gamma, lognormal, etc.) or real-valued kernel (Normal, Laplace),
#' optionally regressing kernel parameters on covariates \code{X}.
#'
#' @param Y Numeric vector of response observations. Must be compatible with
#'   the support of the chosen \code{kernel} (e.g., positive for \code{"gamma"}).
#' @param X Numeric matrix or data.frame of covariates. If \code{NULL}, an
#'   unconditional mixture model is fit. Rows must match \code{Y}.
#' @param kernel Character string selecting the bulk kernel for the mixture components.
#'   One of \code{"gamma"}, \code{"lognormal"}, \code{"normal"}, \code{"laplace"},
#'   \code{"inverse_gaussian"}, \code{"amoroso"}, or \code{"pareto"}.
#' @param dp_rep Character string selecting the Dirichlet process representation,
#'   either \code{"stick_breaking"} (truncated) or \code{"crp"} (Chinese Restaurant Process).
#' @param dp_ctrl List of control arguments for the DP representation,
#'   such as \code{K} (truncation level) when \code{dp_rep = "stick_breaking"}.
#' @param priors Optional list of prior hyperparameters.
#' @param trans Optional list of transformations to apply to covariates.
#' @param mcmc List of MCMC control settings (\code{n_iter}, \code{burn_in}, etc.).
#' @param alpha Nominal level used for summarizing posterior quantities (e.g., 0.05).
#' @param seed Optional integer to set the random seed.
#'
#' @return An object of class \code{"mixgpd_fit"} containing the model specification,
#'   MCMC draws, and summary information.
#'
#' @examples
#' \dontrun{
#' y <- rgamma(100, 2, 1)
#' # Unconditional model
#' fit <- fit_mixgpd_xy(y, kernel = "gamma", dp_ctrl = list(K = 5))
#' summary(fit)
#' }
#'
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
#'
#' @description
#' A unified formula interface for fitting Dirichlet process mixture models
#' with various kernels, but without a Generalized Pareto tail. This function
#' parses the formula to create the response vector and design matrix, then
#' delegates to the engine.
#'
#' @param formula A formula specifying the model. Use \code{y ~ 0} for unconditional
#'   density estimation, or \code{y ~ x} for density regression.
#' @param data A \code{data.frame} containing the variables in \code{formula}.
#' @param kernel Character string selecting the bulk kernel (e.g., \code{"gamma"}, \code{"normal"}).
#' @param dp_rep Character string selecting the DP representation (\code{"stick_breaking"} or \code{"crp"}).
#' @param dp_ctrl List of control arguments for the DP (e.g., truncation \code{K}).
#' @param priors Optional list of prior hyperparameters.
#' @param trans Optional list of covariate transformations.
#' @param mcmc List of MCMC settings (\code{n_iter}, \code{burn_in}, \code{chains}, etc.).
#' @param alpha Nominal level for credible intervals (default 0.05).
#' @param seed Optional integer for reproducibility.
#'
#' @return An object of class \code{"mixgpd_fit"}.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(y = rgamma(100, 2, 1), x = rnorm(100))
#' fit <- fit_mixgpd(y ~ x, data = dat, kernel = "gamma")
#' print(fit)
#' }
#'
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
#'
#' @description
#' A matrix-interface alternative to \code{\link{fit_mixgpd_gpd}}. Fits a
#' semiparametric model combining a Dirichlet process mixture for the bulk
#' distribution and a Generalized Pareto Distribution (GPD) for the tail.
#'
#' @param Y Numeric vector of response observations.
#' @param X Numeric matrix or data.frame of covariates, or \code{NULL} for an unconditional model.
#' @param kernel Character string selecting the bulk kernel.
#' @param dp_rep Character string selecting the DP representation.
#' @param dp_ctrl List of control arguments for the DP.
#' @param priors Optional list of prior hyperparameters for the bulk and tail parameters.
#' @param trans Optional list of covariate transformations.
#' @param mcmc List of MCMC control settings.
#' @param alpha Nominal level for credible intervals.
#' @param seed Optional integer for reproducibility.
#'
#' @return An object of class \code{"mixgpd_fit"} with \code{tail = "gpd"}.
#'
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
#'
#' @description
#' A unified formula interface for fitting bulk-tail models. This combines
#' a Dirichlet process mixture for the bulk with a Generalized Pareto tail
#' for extreme values.
#'
#' @param formula A formula specifying the model (e.g. \code{y ~ x} or \code{y ~ 0}).
#' @param data A \code{data.frame} containing variables in the formula.
#' @param kernel Character string selecting the bulk kernel.
#' @param dp_rep Character string selecting the DP representation.
#' @param dp_ctrl List of control arguments for the DP.
#' @param priors Optional list of prior hyperparameters.
#' @param trans Optional list of covariate transformations.
#' @param mcmc List of MCMC control settings.
#' @param alpha Nominal level for credible intervals.
#' @param seed Optional integer for reproducibility.
#'
#' @return An object of class \code{"mixgpd_fit"} with \code{tail = "gpd"}.
#'
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
