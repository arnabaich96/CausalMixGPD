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
fit_mixgpd <- function(formula,
                       data,
                       kernel = c("gamma","lognormal","normal","laplace","invgauss","amoroso"),
                       tail   = c("none", "gpd"),
                       dp_rep = c("stick_breaking","crp"),
                       dp_ctrl = list(),
                       priors  = list(),
                       trans   = list(),
                       mcmc    = list(),
                       alpha   = 0.05) {

  kernel <- match.arg(kernel)
  tail   <- match.arg(tail)
  dp_rep <- match.arg(dp_rep)

  mf <- stats::model.frame(formula, data = data)
  y  <- stats::model.response(mf)
  X  <- model.matrix(stats::terms(mf), mf)

  # if you want y ~ 0 to mean "no covariates":
  tt        <- stats::terms(formula)
  rhs_terms <- attr(tt, "term.labels")
  intercept <- attr(tt, "intercept")
  if (length(rhs_terms) == 0L && intercept == 0L) {
    X_use <- NULL              # response-only
  } else {
    X_use <- X
  }

  spec <- build_model_spec(
    Y       = y,
    X       = X_use,
    kernel  = kernel,
    tail    = tail,
    dp_rep  = dp_rep,
    priors  = priors,
    dp_ctrl = dp_ctrl,
    trans   = trans,
    alpha   = alpha
  )

  res <- run_mcmc_engine(spec, mcmc)

  alpha_use <- alpha
  if (is.null(alpha_use)) alpha_use <- 0.05
  param_summary <- .summarize_mcmc(res$mcmc_draws, alpha_use)

  structure(
    list(
      call          = match.call(),
      spec          = spec,
      mcmc_draws    = res$mcmc_draws,
      mcmc_info     = res$mcmc_info,
      alpha         = alpha,
      param_summary = param_summary
    ),
    class = "mixgpd_fit"
  )
}
