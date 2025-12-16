#' Fit dpm
#'
#' Fit dpm.
#'
#' @param formula formula.
#' @param data data.
#' @param kernel kernel.
#' @param dp_rep dp_rep.
#' @param dp_ctrl dp_ctrl.
#' @param priors priors.
#' @param mcmc mcmc.
#' @param alpha alpha.
#' @param trans trans.
#' @param intercept intercept.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("fit.dpm", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
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
  if (is.null(priors) || (is.list(priors) && length(priors) == 0L)) priors <- list()
  if (is.null(trans)  || (is.list(trans)  && length(trans)  == 0L)) trans  <- list()

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
  .check_dp_ctrl(dp_ctrl, N, dp_rep = dp_rep)
  .check_mcmc(mcmc)

  ## optional: store ranges of covariates for stats::predict() extrapolation warnings
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
#' Fit dpmgpd
#'
#' Fit dpmgpd.
#'
#' @param formula formula.
#' @param data data.
#' @param kernel kernel.
#' @param dp_rep dp_rep.
#' @param dp_ctrl dp_ctrl.
#' @param priors priors.
#' @param mcmc mcmc.
#' @param alpha alpha.
#' @param trans trans.
#' @param intercept intercept.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("fit.dpmgpd", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
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
  if (is.null(priors) || (is.list(priors) && length(priors) == 0L)) priors <- list()
  if (is.null(trans)  || (is.list(trans)  && length(trans)  == 0L)) trans  <- list()

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
  .check_dp_ctrl(dp_ctrl, N, dp_rep = dp_rep)
  .check_mcmc(mcmc)

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
# User-facing fitting functions -----------------------------------------------
#' Fit mixgpd xy
#'
#' Fit mixgpd xy.
#'
#' @param Y Y.
#' @param X X.
#' @param kernel kernel.
#' @param dp_rep dp_rep.
#' @param dp_ctrl dp_ctrl.
#' @param priors priors.
#' @param trans trans.
#' @param mcmc mcmc.
#' @param alpha alpha.
#' @param seed seed.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("fit_mixgpd_xy", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
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
#' Fit mixgpd
#'
#' Fit mixgpd.
#'
#' @param formula formula.
#' @param data data.
#' @param kernel kernel.
#' @param dp_rep dp_rep.
#' @param dp_ctrl dp_ctrl.
#' @param priors priors.
#' @param trans trans.
#' @param mcmc mcmc.
#' @param alpha alpha.
#' @param seed seed.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("fit_mixgpd", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
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
#' Fit mixgpd gpd xy
#'
#' Fit mixgpd gpd xy.
#'
#' @param Y Y.
#' @param X X.
#' @param kernel kernel.
#' @param dp_rep dp_rep.
#' @param dp_ctrl dp_ctrl.
#' @param priors priors.
#' @param trans trans.
#' @param mcmc mcmc.
#' @param alpha alpha.
#' @param seed seed.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("fit_mixgpd_gpd_xy", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
#' }
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
#' Fit mixgpd gpd
#'
#' Fit mixgpd gpd.
#'
#' @param formula formula.
#' @param data data.
#' @param kernel kernel.
#' @param dp_rep dp_rep.
#' @param dp_ctrl dp_ctrl.
#' @param priors priors.
#' @param trans trans.
#' @param mcmc mcmc.
#' @param alpha alpha.
#' @param seed seed.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("fit_mixgpd_gpd", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
#' }
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

# small helper: null-coalescing
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Print summary of a MixGPD fit
#'
#' @param x An object of class `summary.mixgpd_fit`.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.mixgpd_fit <- function(x, ...) {
  # Try to pull key bits from top level, then from spec
  kernel <- x$kernel %||% (x$spec$kernel %||% NA_character_)
  tail   <- x$tail   %||% (x$spec$tail   %||% NA_character_)
  mode   <- x$mode   %||% (x$spec$mode   %||% NA_character_)
  alpha  <- x$alpha  %||% NA_real_

  # sample size if we stored it
  N <- x$N %||% x$spec$N %||% NA_integer_

  # truncation K if present
  K <- x$spec$dp_ctrl$K %||% NA_integer_

  cat("Dirichlet process mixture model\n")
  if (!is.na(N)) cat("N:      ", N, "\n", sep = "")
  if (!is.na(kernel)) cat("Kernel: ", kernel, "\n", sep = "")
  if (!is.na(tail) && tail != "none") {
    cat("Tail:   ", tail, " (GPD)\n", sep = "")
  } else if (!is.na(tail)) {
    cat("Tail:   none\n")
  }
  if (!is.na(mode)) cat("Mode:   ", mode, "\n", sep = "")
  if (!is.na(K))    cat("DP K:   ", K, "\n", sep = "")
  if (!is.na(alpha)) cat("Alpha:  ", alpha, "\n", sep = "")

  invisible(x)
}



# internal helper: summarize MCMC draws into param table
#' Summarize mcmc
#'
#' Summarize mcmc.
#'
#' @param mcmc_draws mcmc_draws.
#' @param alpha alpha.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".summarize_mcmc", "DPmixGPD")
#' f
#'
#' @keywords internal
.summarize_mcmc <- function(mcmc_draws, alpha = 0.05) {
  # Accept a variety of formats:
  #  - coda::mcmc.list
  #  - single coda::mcmc
  #  - plain list of matrices
  #  - single matrix

  if (inherits(mcmc_draws, "mcmc.list")) {
    chain_list <- mcmc_draws
  } else if (inherits(mcmc_draws, "mcmc")) {
    chain_list <- list(mcmc_draws)
  } else if (is.list(mcmc_draws)) {
    # assume each element is an iterations x parameters matrix
    chain_list <- lapply(mcmc_draws, coda::as.mcmc)
  } else if (is.matrix(mcmc_draws)) {
    chain_list <- list(coda::as.mcmc(mcmc_draws))
  } else {
    stop(
      "'object$mcmc_draws' must be a matrix, mcmc, mcmc.list, or list of matrices.",
      call. = FALSE
    )
  }

  # stack all chains
  combined <- do.call(rbind, chain_list)
  if (!is.matrix(combined)) {
    combined <- as.matrix(combined)
  }

  pnames <- colnames(combined)
  if (is.null(pnames)) {
    pnames <- paste0("param_", seq_len(ncol(combined)))
    colnames(combined) <- pnames
  }

  lower <- alpha / 2
  upper <- 1 - alpha / 2

  means <- colMeans(combined, na.rm = TRUE)
  sds   <- apply(combined, 2L, stats::sd, na.rm = TRUE)
  q_lo  <- apply(combined, 2L, stats::quantile, probs = lower, names = FALSE, na.rm = TRUE)
  q_hi  <- apply(combined, 2L, stats::quantile, probs = upper, names = FALSE, na.rm = TRUE)

  out <- data.frame(
    parameter = pnames,
    mean      = as.numeric(means),
    sd        = as.numeric(sds),
    q_lo      = as.numeric(q_lo),
    q_hi      = as.numeric(q_hi),
    row.names = NULL
  )

  out
}

#' Summarize a MixGPD fit
#'
#' @param object A fitted MixGPD object.
#' @param ... Additional arguments (ignored).
#' @return An object of class `summary.mixgpd_fit`.
#' @export
summary.mixgpd_fit <- function(object, ...) {
  # use stored alpha if available, else default to 0.05
  alpha <- object$alpha %||% 0.05
  .summarize_mcmc(object$mcmc_draws, alpha = alpha)
}
#' Print summary mixgpd fit
#'
#' Print summary mixgpd fit.
#'
#' @param x x.
#' @param ... Additional arguments.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("print.summary.mixgpd_fit", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
#' }
#'
#' @export
print.summary.mixgpd_fit <- function(x, ...) {
  cat("Summary of mixgpd_fit object\n")
  cat("Kernel:", x$spec$kernel, "\n")
  cat("Tail:  ", x$spec$tail, "\n")
  cat("Mode:  ", x$spec$mode, "\n")
  cat("Alpha: ", x$alpha, "\n\n")
  print(x$param_summary, row.names = FALSE)
  invisible(x)
}
#' Coef mixgpd fit
#'
#' Coef mixgpd fit.
#'
#' @param object object.
#' @param ... Additional arguments.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("coef.mixgpd_fit", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
#' }
#'
#' @export
coef.mixgpd_fit <- function(object, ...) {
  alpha <- object$alpha
  if (is.null(alpha)) alpha <- 0.05

  if (is.null(object$param_summary)) {
    ps <- .summarize_mcmc(object$mcmc_draws, alpha)
  } else {
    ps <- object$param_summary
  }

  out <- cbind(estimate = ps$mean, sd = ps$sd)
  rownames(out) <- ps$param
  out
}
#' Fitted mixgpd fit
#'
#' Fitted mixgpd fit.
#'
#' @param object object.
#' @param ... Additional arguments.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("fitted.mixgpd_fit", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
#' }
#'
#' @export
fitted.mixgpd_fit <- function(object, ...) {
  # Fitted values for original data: posterior mean only
  preds <- predict.mixgpd_fit(object, newdata = NULL)
  preds$mean
}
#' As mcmc
#'
#' As mcmc.
#'
#' @param object object.
#' @param ... Additional arguments.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("as_mcmc", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
#' }
#'
#' @export
as_mcmc <- function(object, ...) {
  UseMethod("as_mcmc")
}
#' As mcmc mixgpd fit
#'
#' As mcmc mixgpd fit.
#'
#' @param object object.
#' @param ... Additional arguments.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("as_mcmc.mixgpd_fit", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
#' }
#'
#' @export
as_mcmc.mixgpd_fit <- function(object, ...) {
  object$mcmc_draws
}
#' Mcmc ggdiag
#'
#' Mcmc ggdiag.
#'
#' @param object object.
#' @param what what.
#' @param ... Additional arguments.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("mcmc_ggdiag", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
#' }
#'
#' @export
mcmc_ggdiag <- function(object, what = c("trace", "density"), ...) {
  what <- match.arg(what)

  mcmc_obj <- object$mcmc_draws
  if (is.null(mcmc_obj)) stop("No `mcmc_draws` found in `object`.", call. = FALSE)

  # Support common containers: coda::mcmc.list, matrix, data.frame
  if (inherits(mcmc_obj, "mcmc.list")) {
    m <- as.matrix(mcmc_obj)
  } else if (inherits(mcmc_obj, "mcmc")) {
    m <- as.matrix(mcmc_obj)
  } else if (is.data.frame(mcmc_obj)) {
    m <- as.matrix(mcmc_obj)
  } else {
    m <- as.matrix(mcmc_obj)
  }

  if (ncol(m) == 0L) stop("`mcmc_draws` has no columns.", call. = FALSE)

  df <- as.data.frame(m)
  df$.iter <- seq_len(nrow(df))

  if (what == "trace") {
    long <- stats::reshape(df,
                          varying = setdiff(names(df), ".iter"),
                          v.names = "value",
                          timevar = "param",
                          times = setdiff(names(df), ".iter"),
                          direction = "long")
    long$.iter <- long$.iter
    long$param <- factor(long$param)

    p <- ggplot2::ggplot(long, ggplot2::aes(x = .iter, y = value)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~ param, scales = "free_y") +
      ggplot2::labs(x = "Iteration", y = "Value", title = "MCMC trace plots") +
      ggplot2::theme_minimal()

    return(p)
  }

  # density
  dens_list <- lapply(setdiff(names(df), ".iter"), function(nm) {
    d <- stats::density(df[[nm]])
    data.frame(x = d$x, y = d$y, param = nm, stringsAsFactors = FALSE)
  })
  dens <- do.call(rbind, dens_list)
  dens$param <- factor(dens$param)

  p <- ggplot2::ggplot(dens, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ param, scales = "free") +
    ggplot2::labs(x = NULL, y = "Density", title = "MCMC marginal densities") +
    ggplot2::theme_minimal()

  p
}
# ============================================================================
# Gamma DP mixture predictive helpers (package-native; no qTotal/rTotal names)
# ============================================================================
#'
#' Extract gamma dp params uncond.
#'
#' @param object object.
#' @return See details.
#' @examples
#' f <- getFromNamespace(".extract_gamma_dp_params_uncond", "DPmixGPD")
#' f
#' @keywords internal
.extract_gamma_dp_params_uncond <- function(object) {
  draws <- .as_mcmc_matrix(object)
  cn    <- colnames(draws)

  # ---- component counts ----
  shape_cols <- grep("^shape\\[", cn, value = TRUE)
  if (length(shape_cols) == 0L) {
    stop("No 'shape[j]' columns found in MCMC draws for Gamma DP.", call. = FALSE)
  }
  K <- length(shape_cols)

  # order by component index
  shape_j <- as.integer(sub("^shape\\[([0-9]+)\\]$", "\\1", shape_cols))
  shape_cols <- shape_cols[order(shape_j)]
  shape_j    <- sort(shape_j)

  # unconditional scale[j] is expected
  scale_cols <- grep("^scale\\[", cn, value = TRUE)
  if (length(scale_cols) == 0L) {
    stop("No 'scale[j]' columns found in MCMC draws for unconditional Gamma DP.", call. = FALSE)
  }
  scale_j <- as.integer(sub("^scale\\[([0-9]+)\\]$", "\\1", scale_cols))
  scale_cols <- scale_cols[order(scale_j)]
  scale_j    <- sort(scale_j)

  if (!identical(shape_j, scale_j)) {
    stop("Mismatch between 'shape[j]' and 'scale[j]' component indices.", call. = FALSE)
  }

  # weights: prefer w[j] if present, else build from v[j]
  w_cols <- grep("^w\\[", cn, value = TRUE)
  if (length(w_cols) > 0L) {
    w_j <- as.integer(sub("^w\\[([0-9]+)\\]$", "\\1", w_cols))
    w_cols <- w_cols[order(w_j)]
    w_j    <- sort(w_j)
    if (!identical(w_j, shape_j)) {
      stop("Mismatch between 'w[j]' and 'shape[j]' indices.", call. = FALSE)
    }
    W <- draws[, w_cols, drop = FALSE]
  } else {
    v_cols <- grep("^v\\[", cn, value = TRUE)
    if (length(v_cols) == 0L) {
      stop("No 'w[j]' or 'v[j]' columns found in MCMC draws.", call. = FALSE)
    }
    v_j <- as.integer(sub("^v\\[([0-9]+)\\]$", "\\1", v_cols))
    v_cols <- v_cols[order(v_j)]
    V <- draws[, v_cols, drop = FALSE]

    # we expect K-1 stick breaks; if you monitored K by mistake, drop extras
    if (ncol(V) >= K) {
      V <- V[, 1:(K - 1L), drop = FALSE]
    } else if (ncol(V) != K - 1L) {
      stop("For K components, expected K-1 'v[j]' stick-breaking terms.", call. = FALSE)
    }

    M <- nrow(V)
    W <- matrix(NA_real_, nrow = M, ncol = K)
    colnames(W) <- paste0("w[", 1:K, "]")

    for (m in seq_len(M)) {
      v_m <- as.numeric(V[m, ])
      w_m <- numeric(K)
      prod_term <- 1
      for (j in seq_len(K - 1L)) {
        w_m[j] <- v_m[j] * prod_term
        prod_term <- prod_term * (1 - v_m[j])
      }
      w_m[K] <- prod_term
      W[m, ] <- w_m
    }
  }

  # ---- pull shape/scale ----
  Shape <- draws[, shape_cols, drop = FALSE]
  Scale <- draws[, scale_cols, drop = FALSE]

  # ---- CRITICAL FIX: renormalize weights per draw ----
  # If weights don't sum to 1 (numerical or model-definition issues),
  # force them onto the simplex. This prevents quantile bracketing failure.
  rs <- rowSums(W)
  bad <- !is.finite(rs) | rs <= 0
  if (any(bad)) {
    stop("Non-finite or non-positive mixture weight sums encountered in MCMC draws.", call. = FALSE)
  }
  W <- W / rs

  # (optional) tiny negative cleanup
  W[W < 0] <- 0
  W <- W / rowSums(W)

  list(
    W      = W,        # M x K
    Shape  = Shape,    # M x K
    Scale  = Scale,    # M x K
    n_iter = nrow(draws),
    K      = K
  )
}


# mixture CDF at x for one draw-row
#' @noRd
.gamma_mix_cdf_1 <- function(x, w, shape, scale) {
  sum(w * stats::pgamma(x, shape = shape, scale = scale))
}
#' @noRd
.gamma_mix_pdf_1 <- function(x, w, shape, scale) {
  sum(w * stats::dgamma(x, shape = shape, scale = scale))
}



# ============================================================================
# stats::predict() for unconditional Gamma DP mixtures
# ============================================================================
#' @noRd
.gamma_mix_cdf <- function(x, w, shape, scale) {
  x <- as.numeric(x)
  vapply(x, function(xx) .gamma_mix_cdf_1(xx, w=w, shape=shape, scale=scale), numeric(1))
}

#' @noRd
.gamma_mix_pdf <- function(x, w, shape, scale) {
  x <- as.numeric(x)
  vapply(x, function(xx) .gamma_mix_pdf_1(xx, w=w, shape=shape, scale=scale), numeric(1))
}
.gamma_mix_density_1draw <- function(x, w, shape, scale) {
  sum(w * stats::dgamma(x, shape = shape, scale = scale))
}

.gamma_mix_cdf_1draw <- function(x, w, shape, scale) {
  sum(w * stats::pgamma(x, shape = shape, scale = scale))
}
#' @noRd
.gamma_mix_quantile_1draw <- function(p, w, shape, scale,
                                      tol = 1e-10,
                                      max_expand = 60L,
                                      expand_factor = 2) {
  p <- as.numeric(p)[1L]
  if (!is.finite(p) || p <= 0 || p >= 1) return(NA_real_)

  w <- as.numeric(w); shape <- as.numeric(shape); scale <- as.numeric(scale)
  if (length(w) == 0L || length(shape) != length(w) || length(scale) != length(w)) return(NA_real_)
  if (any(!is.finite(shape)) || any(shape <= 0)) return(NA_real_)
  if (any(!is.finite(scale)) || any(scale <= 0)) return(NA_real_)

  w[!is.finite(w) | w < 0] <- 0
  sw <- sum(w)
  if (!is.finite(sw) || sw <= 0) return(NA_real_)
  w <- w / sw

  mix_cdf <- function(x) {
    if (!is.finite(x) || x < 0) return(0)
    sum(w * stats::pgamma(x, shape = shape, scale = scale))
  }
  f <- function(x) mix_cdf(x) - p

  lo <- 0
  flo <- f(lo)
  if (!is.finite(flo)) flo <- -p

  # NA-safe upper hint from component quantiles
  q_hint <- suppressWarnings(stats::qgamma(p, shape = shape, scale = scale))
  q_hint <- q_hint[is.finite(q_hint) & q_hint >= 0]
  y_max_hint <- if (length(q_hint)) max(q_hint) else NA_real_

  ok_hint <- is.numeric(y_max_hint) &&
    length(y_max_hint) == 1L &&
    !is.na(y_max_hint) &&
    is.finite(y_max_hint) &&
    y_max_hint > 0

  hi <- if (ok_hint) max(10 * y_max_hint, 1) else 1

  fhi <- f(hi)
  k <- 0L
  while ((is.na(fhi) || !is.finite(fhi) || fhi < 0) && k < max_expand) {
    hi <- hi * expand_factor
    fhi <- f(hi)
    k <- k + 1L
  }

  if (is.na(fhi) || !is.finite(fhi) || fhi < 0) return(NA_real_)

  out <- tryCatch(
    stats::uniroot(f, lower = lo, upper = hi, tol = tol)$root,
    error = function(e) NA_real_
  )
  out
}


#' @noRd
.gamma_mix_mean_1draw <- function(w, shape, scale) {
  sum(w * (shape * scale))
}
#' @noRd
.extract_gamma_draws <- function(draws) {
  draws <- as.matrix(draws)
  cn <- colnames(draws)

  shape_cols <- grep("^shape\\[", cn, value = TRUE)
  scale_cols <- grep("^scale\\[", cn, value = TRUE)

  if (length(shape_cols) == 0L || length(scale_cols) == 0L) {
    stop("Gamma draws must contain shape[j] and scale[j] columns.", call. = FALSE)
  }

  # Order by component index
  comp_index <- function(x) as.integer(sub(".*\\[([0-9]+)\\].*", "\\1", x))
  shape_cols <- shape_cols[order(comp_index(shape_cols))]
  scale_cols <- scale_cols[order(comp_index(scale_cols))]

  Shape <- draws[, shape_cols, drop = FALSE]
  Scale <- draws[, scale_cols, drop = FALSE]

  # weights: either w[j] directly or stick-breaking v[j]
  w_cols <- grep("^w\\[", cn, value = TRUE)
  v_cols <- grep("^v\\[", cn, value = TRUE)

  if (length(w_cols) > 0L) {
    w_cols <- w_cols[order(comp_index(w_cols))]
    W <- draws[, w_cols, drop = FALSE]
  } else if (length(v_cols) > 0L) {
    v_cols <- v_cols[order(comp_index(v_cols))]
    V <- draws[, v_cols, drop = FALSE]
    K <- ncol(Shape)
    if (ncol(V) < (K - 1L)) {
      stop("Stick-breaking draws must include v[1:(K-1)].", call. = FALSE)
    }
    V <- V[, seq_len(K - 1L), drop = FALSE]
    M <- nrow(V)
    W <- matrix(NA_real_, nrow = M, ncol = K)
    prod_tail <- rep(1, M)
    for (j in seq_len(K - 1L)) {
      W[, j] <- V[, j] * prod_tail
      prod_tail <- prod_tail * (1 - V[, j])
    }
    W[, K] <- prod_tail
  } else {
    stop("Draws must contain either w[j] or v[j] columns for mixture weights.", call. = FALSE)
  }

  # Normalize defensively
  rs <- rowSums(W)
  rs[!is.finite(rs) | rs <= 0] <- NA_real_
  W <- W / rs

  list(W = W, Shape = Shape, Scale = Scale)
}
#' @noRd
#'
#'
#'

#' @noRd
.extract_gamma_dp_params <- function(draws, K = NULL, renormalize_weights = TRUE) {
  if (!is.matrix(draws)) draws <- as.matrix(draws)
  cn <- colnames(draws)

  w_cols  <- grep("^w\\[",     cn, value = TRUE)
  sh_cols <- grep("^shape\\[", cn, value = TRUE)
  sc_cols <- grep("^scale\\[", cn, value = TRUE)

  if (!length(w_cols))  stop("Cannot find mixture weights 'w[j]' in MCMC draws.", call. = FALSE)
  if (!length(sh_cols)) stop("Cannot find 'shape[j]' in MCMC draws.", call. = FALSE)
  if (!length(sc_cols)) stop("Cannot find 'scale[j]' in MCMC draws.", call. = FALSE)

  # order by component index
  get_j <- function(cols, pat) as.integer(sub(pat, "\\1", cols))
  wj  <- get_j(w_cols,  "^w\\[([0-9]+)\\]$")
  shj <- get_j(sh_cols, "^shape\\[([0-9]+)\\]$")
  scj <- get_j(sc_cols, "^scale\\[([0-9]+)\\]$")

  o_w  <- order(wj);  o_sh <- order(shj); o_sc <- order(scj)

  w_cols  <- w_cols[o_w];  wj  <- wj[o_w]
  sh_cols <- sh_cols[o_sh]; shj <- shj[o_sh]
  sc_cols <- sc_cols[o_sc]; scj <- scj[o_sc]

  if (!identical(wj, shj) || !identical(wj, scj)) {
    stop("Mismatch between component indices across w/shape/scale columns.", call. = FALSE)
  }

  # Optional user-specified truncation. This is mainly used by
  # conditional effect helpers where K is already known.
  if (!is.null(K)) {
    K <- as.integer(K)
    if (!is.finite(K) || K < 1L) stop("K must be a positive integer.")
    if (K > length(w_cols)) stop("K exceeds available mixture components in draws.")
    w_cols  <- w_cols[seq_len(K)]
    sh_cols <- sh_cols[seq_len(K)]
    sc_cols <- sc_cols[seq_len(K)]
  }

  W  <- draws[, w_cols,  drop = FALSE]
  Sh <- draws[, sh_cols, drop = FALSE]
  Sc <- draws[, sc_cols, drop = FALSE]

  if (isTRUE(renormalize_weights)) {
    rs <- rowSums(W)
    bad <- !is.finite(rs) | rs <= 0
    if (any(bad)) stop("Non-finite or non-positive mixture weight sums encountered.", call. = FALSE)
    W <- W / rs
    W[W < 0] <- 0
    W <- W / rowSums(W)
  }

  list(W = W, Shape = Sh, Scale = Sc, M = nrow(draws), K = ncol(W))
}
#' Predict mixgpd fit
#'
#' Predict mixgpd fit.
#'
#' @param object object.
#' @param newdata newdata.
#' @param type type.
#' @param probs probs.
#' @param n_samples n_samples.
#' @param level level of significance
#' @param renormalize_weights renormalize_weights(default: TRUE).
#' @param ... Additional arguments.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("predict.mixgpd_fit", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
#' }
#'
#' @export
predict.mixgpd_fit <- function(object,
                               newdata = NULL,
                               type = c("quantile", "density", "cdf", "sample"),
                               probs = c(0.1, 0.5, 0.9),
                               n_samples = 1000L,
                               level = NULL,
                               renormalize_weights = TRUE,
                               ...) {

  type <- match.arg(type)
  draws <- object$mcmc_draws
  if (is.null(draws)) stop("No MCMC draws stored in object$mcmc_draws.", call. = FALSE)

  # DPMGPD objects share the same class but predictive quantities for the
  # bulk+tail model are not wired into predict() yet.
  if (!is.null(object$tail) && !identical(object$tail, "none")) {
    stop("predict() is currently implemented for DPM (tail='none') only.", call. = FALSE)
  }

  # current implementation is unconditional gamma-only
  if (!identical(object$spec$kernel, "gamma")) {
    stop("predict.mixgpd_fit currently supports kernel='gamma' only.", call. = FALSE)
  }

  draws <- as.matrix(draws)
  cn <- colnames(draws)

  shape_cols <- grep("^shape\\[", cn, value = TRUE)
  scale_cols <- grep("^scale\\[", cn, value = TRUE)

  if (length(shape_cols) == 0L || length(scale_cols) == 0L) {
    stop("Draws must contain shape[j] and scale[j] columns.", call. = FALSE)
  }

  comp_index <- function(x) as.integer(sub(".*\\[([0-9]+)\\].*", "\\1", x))
  shape_cols <- shape_cols[order(comp_index(shape_cols))]
  scale_cols <- scale_cols[order(comp_index(scale_cols))]

  Shape <- draws[, shape_cols, drop = FALSE]
  Scale <- draws[, scale_cols, drop = FALSE]

  w_cols <- grep("^w\\[", cn, value = TRUE)
  v_cols <- grep("^v\\[", cn, value = TRUE)

  if (length(w_cols) > 0L) {
    w_cols <- w_cols[order(comp_index(w_cols))]
    W <- draws[, w_cols, drop = FALSE]
  } else if (length(v_cols) > 0L) {
    v_cols <- v_cols[order(comp_index(v_cols))]
    V <- draws[, v_cols, drop = FALSE]
    K <- ncol(Shape)
    if (ncol(V) < (K - 1L)) stop("Need v[1:(K-1)] for stick-breaking.", call. = FALSE)

    V <- V[, seq_len(K - 1L), drop = FALSE]
    M <- nrow(V)
    W <- matrix(NA_real_, nrow = M, ncol = K)

    prod_tail <- rep(1, M)
    for (j in seq_len(K - 1L)) {
      W[, j] <- V[, j] * prod_tail
      prod_tail <- prod_tail * (1 - V[, j])
    }
    W[, K] <- prod_tail
  } else {
    stop("Draws must contain either w[j] or v[j] for mixture weights.", call. = FALSE)
  }

  # sanitize / renormalize
  W[!is.finite(W) | W < 0] <- 0
  rs <- rowSums(W)
  bad_w <- !is.finite(rs) | rs <= 0
  if (any(bad_w)) rs[bad_w] <- NA_real_
  if (isTRUE(renormalize_weights)) W <- W / rs

  # drop garbage rows (prevents Inf/NA densities and quantile bracketing crashes)
  ok_row <- is.finite(rowSums(W)) &
    apply(Shape, 1, function(z) all(is.finite(z) & z > 0)) &
    apply(Scale, 1, function(z) all(is.finite(z) & z > 0))

  if (!any(ok_row)) stop("All posterior draws are invalid after filtering.", call. = FALSE)

  W <- W[ok_row, , drop = FALSE]
  Shape <- Shape[ok_row, , drop = FALSE]
  Scale <- Scale[ok_row, , drop = FALSE]

  M <- nrow(W)
  K <- ncol(W)

  alpha <- object$alpha %||% 0.05
  if (!is.null(level)) alpha <- 1 - level

  # ------------------------------------------------------------------
  # density / cdf: require y grid in newdata
  # ------------------------------------------------------------------
  if (type %in% c("density", "cdf")) {
    if (is.null(newdata)) {
      stop("newdata must include y values for type='density'/'cdf'.", call. = FALSE)
    }

    if (is.data.frame(newdata)) {
      x <- if ("y" %in% names(newdata)) newdata[["y"]] else newdata[[1L]]
    } else if (is.matrix(newdata)) {
      x <- newdata[, 1L]
    } else {
      x <- newdata
    }

    x <- as.numeric(x)

    out <- numeric(length(x))
    for (i in seq_along(x)) {
      xi <- x[i]
      if (!is.finite(xi) || xi < 0) {
        out[i] <- if (type == "density") 0 else 0
        next
      }

      if (type == "density") {
        vals <- vapply(seq_len(M), function(m) {
          sum(W[m, ] * stats::dgamma(xi, shape = Shape[m, ], scale = Scale[m, ]))
        }, numeric(1))
      } else {
        vals <- vapply(seq_len(M), function(m) {
          sum(W[m, ] * stats::pgamma(xi, shape = Shape[m, ], scale = Scale[m, ]))
        }, numeric(1))
      }

      vals <- vals[is.finite(vals)]
      out[i] <- if (length(vals)) mean(vals) else if (type == "density") 0 else NA_real_
    }

    if (type == "density") out[!is.finite(out)] <- 0
    return(out)
  }

  # ------------------------------------------------------------------
  # sample: does NOT require newdata
  # ------------------------------------------------------------------
  if (type == "sample") {
    n_samples <- as.integer(n_samples)
    if (length(n_samples) != 1L || n_samples <= 0L) {
      stop("n_samples must be a positive integer.", call. = FALSE)
    }

    ys <- numeric(n_samples)
    m_idx <- sample.int(M, size = n_samples, replace = TRUE)
    for (s in seq_len(n_samples)) {
      m <- m_idx[s]
      j <- sample.int(K, size = 1L, prob = W[m, ])
      ys[s] <- stats::rgamma(1L, shape = Shape[m, j], scale = Scale[m, j])
    }
    return(ys)
  }

  # ------------------------------------------------------------------
  # quantile: does NOT require newdata
  # ------------------------------------------------------------------
  probs <- as.numeric(probs)
  if (any(!is.finite(probs)) || any(probs <= 0 | probs >= 1)) {
    stop("probs must be in (0,1).", call. = FALSE)
  }

  qdraws <- matrix(NA_real_, nrow = length(probs), ncol = M)
  rownames(qdraws) <- paste0("q", probs)

  for (i in seq_along(probs)) {
    p <- probs[i]
    qdraws[i, ] <- vapply(seq_len(M), function(m) {
      tryCatch(
        .gamma_mix_quantile_1draw(p, W[m, ], Shape[m, ], Scale[m, ]),
        error = function(e) NA_real_
      )
    }, numeric(1))
  }

  summ <- cbind(
    mean  = apply(qdraws, 1, function(z) mean(z[is.finite(z)], na.rm = TRUE)),
    sd    = apply(qdraws, 1, function(z) stats::sd(z[is.finite(z)], na.rm = TRUE)),
    lower = apply(qdraws, 1, function(z) stats::quantile(z[is.finite(z)],
                                                         probs = alpha/2,
                                                         na.rm = TRUE,
                                                         names = FALSE)),
    upper = apply(qdraws, 1, function(z) stats::quantile(z[is.finite(z)],
                                                         probs = 1 - alpha/2,
                                                         na.rm = TRUE,
                                                         names = FALSE))
  )

  summ
}



# internal: robust quantile inversion via bracketing + uniroot
#' Invert cdf uniroot
#'
#' Invert cdf uniroot.
#'
#' @param cdf_fun cdf_fun.
#' @param tau tau.
#' @param lower lower.
#' @param upper upper.
#' @param max_expand max_expand.
#' @param expand_factor expand_factor.
#' @param tol tol.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".invert_cdf_uniroot", "DPmixGPD")
#' f
#'
#' @keywords internal
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
  stats::uniroot(f_root, lower = lower, upper = upper, tol = tol)$root
}
