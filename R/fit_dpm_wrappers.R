#' Fit a Dirichlet process mixture model without a GPD tail
#'
#' @description
#' Fit a truncated Dirichlet process mixture model for a univariate
#' response using a positive-support kernel (Gamma, lognormal, etc.)
#' with optional covariates on the right-hand side of a formula.
#' The model can be used for unconditional density estimation
#' (`y ~ 0`) or for covariate-dependent density regression (`y ~ x1 + x2`).
#'
#' @param formula A formula specifying the model, e.g. `y ~ x1 + x2`
#'   for regression or `y ~ 0` for an unconditional model with no covariates.
#' @param data A `data.frame` containing the variables referenced
#'   in `formula`.
#' @param kernel Character string selecting the bulk kernel for the
#'   mixture components. One of
#'   `"gamma"`, `"lognormal"`, `"normal"`, `"laplace"`,
#'   `"inverse_gaussian"`, `"amoroso"`, or `"pareto"`.
#' @param dp_rep Character string selecting the Dirichlet process
#'   representation, either `"stick_breaking"` (truncated stick-breaking)
#'   or `"crp"` (Chinese restaurant process–style allocation).
#' @param dp_ctrl List of control arguments for the DP representation,
#'   such as `K` (truncation level) when `dp_rep = "stick_breaking"`.
#' @param priors Optional list of prior hyperparameters. The list
#'   structure depends on the chosen kernel and DP representation.
#' @param mcmc List of MCMC control settings, typically including
#'   elements such as `n_iter`, `burn_in`, `thin`, and `chains`.
#' @param alpha Nominal level used for summarizing posterior quantities
#'   (e.g., `alpha = 0.05` for 95% credible intervals).
#' @param trans Optional list describing transformations to apply
#'   to covariates (e.g., centering/scaling). The exact structure is
#'   resolved internally.
#' @param intercept Logical; if `TRUE` (default) an intercept column
#'   is included when there is at least one covariate. When there is
#'   only a single covariate, setting `intercept = TRUE` causes a
#'   column of ones to be prepended so that the design matrix has
#'   `(1, x)` rather than just `x`.
#'
#' @return An object of class `"mixgpd_fit"` containing:
#'   \itemize{
#'     \item `call` – the matched call.
#'     \item `formula`, `kernel`, `tail`, `dp_rep`, `priors`, `dp_ctrl`,
#'           `trans`, `alpha` – model specification.
#'     \item `spec` – internal model specification list passed to the
#'           MCMC engine.
#'     \item `N` – sample size.
#'     \item `x_range` – covariate ranges used for extrapolation checks.
#'     \item `mcmc` – MCMC configuration and diagnostics.
#'     \item `mcmc_draws` – posterior draws as a matrix
#'           (iterations × parameters).
#'   }
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' dat <- data.frame(
#'   y  = rgamma(200, shape = 2, scale = 1),
#'   x1 = rnorm(200),
#'   x2 = rnorm(200)
#' )
#'
#' # Unconditional density estimation
#' fit0 <- fit.dpm(y ~ 0, data = dat,
#'                 kernel = "gamma",
#'                 dp_rep = "stick_breaking",
#'                 dp_ctrl = list(K = 5),
#'                 mcmc = list(n_iter = 1000, burn_in = 500))
#'
#' # Density regression with covariates
#' fit1 <- fit.dpm(y ~ x1 + x2, data = dat,
#'                 kernel = "gamma",
#'                 dp_rep = "stick_breaking",
#'                 dp_ctrl = list(K = 5),
#'                 mcmc = list(n_iter = 1000, burn_in = 500))
#'
#' print(fit1)
#' summary(fit1)
#' }
#'
#' @export
fit.dpm <- function(formula,
                    data,
                    kernel = c("gamma", "lognormal", "normal",
                               "laplace", "inverse_gaussian", "amoroso", "pareto"),
                    dp_rep = c("stick_breaking", "crp"),
                    dp_ctrl = list(K = 5),
                    priors  = list(),
                    mcmc    = list(),
                    alpha   = 0.05,
                    trans   = list(),
                    intercept = TRUE) {

  formula <- stats::as.formula(formula)
  kernel  <- match.arg(kernel)
  dp_rep  <- match.arg(dp_rep)

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.", call. = FALSE)
  }

  ## ---- 1) Formula/data consistency check ----
  .check_formula_data(formula, data)

  ## ---- 2) Build model frame, extract Y and X ----
  mf <- stats::model.frame(formula, data = data)
  Y  <- stats::model.response(mf)

  ## terms object encodes intercept + covariates on RHS
  tt <- stats::terms(formula, data = mf)

  # model.matrix() for RHS; may have 0 columns (e.g., y ~ 0)
  mm <- try(stats::model.matrix(tt, data = mf), silent = TRUE)
  if (inherits(mm, "try-error")) {
    stop("Could not build model matrix from formula. Check that RHS variables are in 'data'.",
         call. = FALSE)
  }

  # Drop the intercept column if present; we handle intercept explicitly via `intercept` arg
  if ("(Intercept)" %in% colnames(mm)) {
    mm_noint <- mm[, setdiff(colnames(mm), "(Intercept)"), drop = FALSE]
  } else {
    mm_noint <- mm
  }

  # Case 1: no real covariates at all: y ~ 0 or y ~ 1
  if (ncol(mm_noint) == 0L) {
    X_raw <- NULL
  } else {
    X_raw <- mm_noint
  }

  # Always pass through the helper so we enforce:
  # - NULL stays NULL
  # - vector -> matrix
  # - if intercept = TRUE and only one column, prepend a column of 1s
  X_use <- .prepare_design_matrix(X_raw, intercept = intercept)

  N <- length(Y)

  ## ---- 3) Data-level checks ----
  .check_missing(Y, X_use)
  .check_kernel_support(Y, kernel)
  .check_dp_ctrl(dp_ctrl, N)
  .check_mcmc(mcmc)

  ## optional: store ranges of covariates for predict() extrapolation warnings
  x_range <- NULL
  if (!is.null(X_use)) {
    x_df <- as.data.frame(X_use)
    x_range <- lapply(x_df, range, finite = TRUE)
  }

  ## ---- 4) Build model spec ----
  spec <- build_model_spec(
    Y       = Y,
    X       = X_use,
    kernel  = kernel,
    tail    = "none",
    dp_rep  = dp_rep,
    dp_ctrl = dp_ctrl,
    priors  = priors,
    trans   = trans,
    alpha   = alpha
  )

  ## ---- 5) Dispatch to MCMC engine ----
  engine_out <- run_mcmc_engine(spec, mcmc)

  res <- list(
    call        = match.call(),
    formula     = formula,
    kernel      = kernel,
    tail        = "none",
    dp_rep      = dp_rep,
    priors      = priors,
    dp_ctrl     = dp_ctrl,
    trans       = trans,
    alpha       = alpha,
    spec        = spec,
    N           = N,
    x_range     = x_range,
    mcmc        = engine_out$mcmc_info,
    mcmc_draws  = engine_out$mcmc_draws
  )

  class(res) <- "mixgpd_fit"
  res
}



#' Fit a Dirichlet process mixture with a GPD tail
#'
#' @description
#' Fit a semiparametric bulk–tail model combining a Dirichlet process
#' mixture for the central part of the distribution with a generalized
#' Pareto tail for exceedances above a covariate-dependent threshold.
#' The interface mirrors [fit.dpm()] but sets `tail = "gpd"`.
#'
#' @param formula A formula specifying the model, e.g. `y ~ x1 + x2`
#'   for regression or `y ~ 0` for an unconditional model.
#' @param data A `data.frame` containing the variables referenced
#'   in `formula`.
#' @param kernel Character string selecting the bulk kernel for the
#'   mixture components. One of
#'   `"gamma"`, `"lognormal"`, `"normal"`, `"laplace"`,
#'   `"inverse_gaussian"`, `"amoroso"`, or `"pareto"`.
#' @param dp_rep Character string selecting the Dirichlet process
#'   representation, either `"stick_breaking"` or `"crp"`.
#' @param dp_ctrl List of control arguments for the DP representation,
#'   such as `K` (truncation level) when `dp_rep = "stick_breaking"`.
#' @param priors Optional list of prior hyperparameters for the bulk,
#'   tail, and DP parameters.
#' @param mcmc List of MCMC control settings, typically including
#'   elements such as `n_iter`, `burn_in`, `thin`, and `chains`.
#' @param alpha Nominal level used for summarizing posterior quantities
#'   (e.g., `alpha = 0.05` for 95\% credible intervals).
#' @param trans Optional list describing transformations to apply
#'   to covariates, resolved internally.
#' @param intercept Logical; if `TRUE` (default) an intercept column
#'   is included when there is at least one covariate, with the same
#'   behavior as in [fit.dpm()].
#'
#' @return An object of class `"mixgpd_fit"` with the same structure
#' as [fit.dpm()], but with `tail = "gpd"` in the `spec` slot and
#' additional tail parameters in `mcmc_draws`.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' dat <- data.frame(
#'   y  = rgamma(200, shape = 2, scale = 1),
#'   x1 = rnorm(200)
#' )
#'
#' fit_tail <- fit.dpmgpd(
#'   y ~ x1,
#'   data   = dat,
#'   kernel = "gamma",
#'   dp_rep = "stick_breaking",
#'   dp_ctrl = list(K = 5),
#'   mcmc   = list(n_iter = 1000, burn_in = 500)
#' )
#'
#' print(fit_tail)
#' summary(fit_tail)
#' }
#'
#' @export
fit.dpmgpd <- function(formula,
                       data,
                       kernel      = c("gamma", "lognormal", "normal",
                                       "laplace", "inverse_gaussian", "amoroso", "pareto"),
                       dp_rep      = c("stick_breaking", "crp"),
                       dp_ctrl     = list(K = 5),
                       priors      = list(),
                       tail_priors = list(),
                       mcmc        = list(),
                       alpha       = 0.05,
                       trans       = list(),
                       intercept   = TRUE) {

  formula <- stats::as.formula(formula)
  kernel  <- match.arg(kernel)
  dp_rep  <- match.arg(dp_rep)

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.", call. = FALSE)
  }

  ## ---- 1) Formula/data consistency check ----
  .check_formula_data(formula, data)

  ## ---- 2) Build model frame, extract Y and X ----
  mf <- stats::model.frame(formula, data = data)
  Y  <- stats::model.response(mf)

  tt <- stats::terms(formula, data = mf)

  mm <- try(stats::model.matrix(tt, data = mf), silent = TRUE)
  if (inherits(mm, "try-error")) {
    stop("Could not build model matrix from formula. Check that RHS variables are in 'data'.",
         call. = FALSE)
  }

  if ("(Intercept)" %in% colnames(mm)) {
    mm_noint <- mm[, setdiff(colnames(mm), "(Intercept)"), drop = FALSE]
  } else {
    mm_noint <- mm
  }

  if (ncol(mm_noint) == 0L) {
    X_raw <- NULL
  } else {
    X_raw <- mm_noint
  }

  X_use <- .prepare_design_matrix(X_raw, intercept = intercept)

  N <- length(Y)

  ## ---- 3) Data-level checks ----
  .check_missing(Y, X_use)
  .check_kernel_support(Y, kernel)
  .check_dp_ctrl(dp_ctrl, N)
  .check_mcmc(mcmc)

  ## ---- 4) merge tail priors into priors ----
  if (length(tail_priors)) {
    priors <- modifyList(priors, list(tail = tail_priors))
  }

  ## optional: ranges for extrapolation warnings later
  x_range <- NULL
  if (!is.null(X_use)) {
    x_df <- as.data.frame(X_use)
    x_range <- lapply(x_df, range, finite = TRUE)
  }

  ## ---- 5) Build model spec (with tail = "gpd") ----
  spec <- build_model_spec(
    Y       = Y,
    X       = X_use,
    kernel  = kernel,
    tail    = "gpd",
    dp_rep  = dp_rep,
    dp_ctrl = dp_ctrl,
    priors  = priors,
    trans   = trans,
    alpha   = alpha
  )

  ## ---- 6) Run MCMC engine ----
  engine_out <- run_mcmc_engine(spec, mcmc)

  res <- list(
    call        = match.call(),
    formula     = formula,
    kernel      = kernel,
    tail        = "gpd",
    dp_rep      = dp_rep,
    priors      = priors,
    dp_ctrl     = dp_ctrl,
    trans       = trans,
    alpha       = alpha,
    spec        = spec,
    N           = N,
    x_range     = x_range,
    mcmc        = engine_out$mcmc_info,
    mcmc_draws  = engine_out$mcmc_draws
  )

  class(res) <- "mixgpd_fit"
  res
}

