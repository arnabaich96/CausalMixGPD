#' Fit a Dirichlet process mixture model (no tail)
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
    N           = N,
    x_range     = x_range,
    mcmc        = engine_out$mcmc_info,
    mcmc_draws  = engine_out$mcmc_draws
  )

  class(res) <- "mixgpd_fit"
  res
}



#' Fit a Dirichlet process mixture + GPD tail model
#'
#' @export
fit.dpmgpd <- function(formula,
                       data,
                       kernel = c("gamma", "lognormal", "normal",
                                  "laplace", "inverse_gaussian", "amoroso"),
                       dp_rep = c("stick_breaking", "crp"),
                       dp_ctrl = list(K = 5),
                       priors      = list(),
                       tail_priors = list(),
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

  ## terms object encodes intercept + RHS structure
  tt <- stats::terms(formula, data = mf)

  # model.matrix() for RHS; may have 0 columns (e.g., y ~ 0)
  mm <- try(stats::model.matrix(tt, data = mf), silent = TRUE)
  if (inherits(mm, "try-error")) {
    stop("Could not build model matrix from formula. ",
         "Check that RHS variables are present in 'data'.",
         call. = FALSE)
  }

  # Drop the intercept column if present; we control intercept via `intercept`
  if ("(Intercept)" %in% colnames(mm)) {
    mm_noint <- mm[, setdiff(colnames(mm), "(Intercept)"), drop = FALSE]
  } else {
    mm_noint <- mm
  }

  # Case: no true covariates (y ~ 0 or y ~ 1)
  if (ncol(mm_noint) == 0L) {
    X_raw <- NULL
  } else {
    X_raw <- mm_noint
  }

  # Standardized design-matrix handling:
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
  # (keep any additional checks you already have here)

  ## optional: store ranges of covariates for predict() extrapolation warnings
  x_range <- NULL
  if (!is.null(X_use)) {
    x_df <- as.data.frame(X_use)
    x_range <- lapply(x_df, range, finite = TRUE)
  }

  ## ---- 4) Build model spec (bulk + GPD tail) ----
  spec <- build_model_spec(
    Y          = Y,
    X          = X_use,
    kernel     = kernel,
    tail       = "gpd",
    dp_rep     = dp_rep,
    dp_ctrl    = dp_ctrl,
    priors     = priors,
    tail_priors = tail_priors,
    trans      = trans,
    alpha      = alpha
  )

  ## ---- 5) Dispatch to MCMC engine ----
  engine_out <- run_mcmc_engine(spec, mcmc)

  ## ---- 6) Wrap result ----
  res <- list(
    call        = match.call(),
    formula     = formula,
    kernel      = kernel,
    tail        = "gpd",
    dp_rep      = dp_rep,
    priors      = priors,
    tail_priors = tail_priors,
    dp_ctrl     = dp_ctrl,
    trans       = trans,
    alpha       = alpha,
    N           = N,
    x_range     = x_range,
    mcmc        = engine_out$mcmc_info,
    mcmc_draws  = engine_out$mcmc_draws
    # add any other slots you were already returning (coef, fitted, etc.)
  )

  class(res) <- "mixgpd_fit"
  res
}
