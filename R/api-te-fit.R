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
#' Fit TE
#'
#' Fit TE.
#'
#' @param formula formula.
#' @param data data.
#' @param A A.
#' @param kernel kernel.
#' @param tail tail.
#' @param dp_rep dp_rep.
#' @param dp_ctrl dp_ctrl.
#' @param priors priors.
#' @param trans trans.
#' @param mcmc mcmc.
#' @param alpha alpha.
#' @param intercept intercept.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("fit.TE", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
#' }
#'
#' @export
fit.TE <- function(formula,
                   data,
                   A,
                   kernel = c("gamma", "lognormal", "normal",
                              "laplace", "inverse_gaussian", "amoroso", "pareto"),
                   tail   = c("none", "gpd"),
                   dp_rep = c("stick_breaking", "crp"),
                   dp_ctrl = list(K = 5),
                   priors  = list(),
                   mcmc    = list(),
                   alpha   = 0.05,
                   trans   = list(),
                   intercept = TRUE) {

  # ---- 0) Inputs ----
  formula <- stats::as.formula(formula)

  # kernel can be length 1 (shared) or length 2 (treat, control)
  if (length(kernel) == 1L) {
    kernel_trt <- kernel_con <- match.arg(kernel, choices = kernel)
  } else if (length(kernel) == 2L) {
    kernel_trt <- match.arg(kernel[1], choices = kernel)
    kernel_con <- match.arg(kernel[2], choices = kernel)
  } else {
    stop("'kernel' must be length 1 (shared) or length 2 (treat, control).", call. = FALSE)
  }

  tail   <- match.arg(tail)
  dp_rep <- match.arg(dp_rep)

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.", call. = FALSE)
  }

  # ---- 1) Resolve A ----
  if (missing(A)) stop("argument 'A' is missing, with no default", call. = FALSE)

  if (is.character(A) && length(A) == 1L) {
    if (!A %in% names(data)) stop("Column '", A, "' not found in 'data'.", call. = FALSE)
    A_name <- A
    A_vec  <- data[[A]]
  } else {
    A_name <- deparse(substitute(A))
    A_vec  <- A
  }

  if (length(A_vec) != nrow(data)) {
    stop("'A' must have the same length as nrow(data).", call. = FALSE)
  }

  # allow logical / factor / numeric; coerce to 0/1
  if (is.factor(A_vec)) A_vec <- as.character(A_vec)
  if (is.logical(A_vec)) A_vec <- as.integer(A_vec)

  # if character like "0"/"1"
  suppressWarnings(A_num <- as.numeric(A_vec))
  if (anyNA(A_num) && !all(is.na(A_vec))) {
    stop("A must be binary coded as 0/1 (or coercible).", call. = FALSE)
  }
  A_vec <- A_num

  vals <- sort(unique(A_vec[!is.na(A_vec)]))
  if (!all(vals %in% c(0, 1))) {
    stop("A must be binary coded as 0/1 (or coercible).", call. = FALSE)
  }

  # ---- 2) Formula/data consistency check ----
  .check_formula_data(formula, data)

  # ---- 3) Build model frame, extract Y and X ----
  mf <- stats::model.frame(formula, data = data)
  Y  <- stats::model.response(mf)

  # terms object encodes intercept + covariates on RHS
  tt <- stats::terms(formula, data = mf)

  mm <- try(stats::model.matrix(tt, data = mf), silent = TRUE)
  if (inherits(mm, "try-error")) {
    stop("Could not build model matrix from formula. Check that RHS variables are in 'data'.",
         call. = FALSE)
  }

  # Drop the intercept column if present; we handle intercept explicitly via `intercept`
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

  # ---- 4) Drop incomplete cases jointly over (Y, A, X_use) ----
  keep <- stats::complete.cases(Y, A_vec)
  if (!is.null(X_use)) keep <- keep & stats::complete.cases(X_use)

  Y_keep <- Y[keep]
  A_keep <- A_vec[keep]
  X_keep <- if (is.null(X_use)) NULL else X_use[keep, , drop = FALSE]

  # ---- 5) Split by group ----
  id_trt <- which(A_keep == 1)
  id_con <- which(A_keep == 0)

  if (!length(id_trt)) stop("No treated observations (A==1) after filtering.", call. = FALSE)
  if (!length(id_con)) stop("No control observations (A==0) after filtering.", call. = FALSE)

  Y_trt <- Y_keep[id_trt]
  Y_con <- Y_keep[id_con]

  X_trt <- if (is.null(X_keep)) NULL else X_keep[id_trt, , drop = FALSE]
  X_con <- if (is.null(X_keep)) NULL else X_keep[id_con, , drop = FALSE]

  # ---- 6) Data-level checks (per group) ----
  .check_missing(Y_trt, X_trt)
  .check_missing(Y_con, X_con)

  .check_kernel_support(Y_trt, kernel_trt)
  .check_kernel_support(Y_con, kernel_con)

  .check_dp_ctrl(dp_ctrl, length(Y_trt))
  .check_dp_ctrl(dp_ctrl, length(Y_con))

  .check_mcmc(mcmc)

  # optional: store covariate ranges for extrapolation warnings
  x_range_trt <- NULL
  x_range_con <- NULL
  if (!is.null(X_trt)) {
    x_range_trt <- lapply(as.data.frame(X_trt), range, finite = TRUE)
  }
  if (!is.null(X_con)) {
    x_range_con <- lapply(as.data.frame(X_con), range, finite = TRUE)
  }

  # ---- 7) Build model specs ----
  spec_trt <- build_model_spec(
    Y       = Y_trt,
    X       = X_trt,
    kernel  = kernel_trt,
    tail    = tail,
    dp_rep  = dp_rep,
    dp_ctrl = dp_ctrl,
    priors  = priors,
    trans   = trans,
    alpha   = alpha
  )

  spec_con <- build_model_spec(
    Y       = Y_con,
    X       = X_con,
    kernel  = kernel_con,
    tail    = tail,
    dp_rep  = dp_rep,
    dp_ctrl = dp_ctrl,
    priors  = priors,
    trans   = trans,
    alpha   = alpha
  )

  # ---- 8) Run MCMC engine (per group) ----
  fit_trt <- run_mcmc_engine(spec_trt, mcmc)
  fit_con <- run_mcmc_engine(spec_con, mcmc)

  # ---- 9) Assemble result ----
  res <- list(
    call    = match.call(),
    formula = formula,
    A_name  = if (is.character(A) && length(A) == 1L) A else A_name,

    kernel  = c(trt = kernel_trt, con = kernel_con),
    tail    = tail,
    dp_rep  = dp_rep,
    dp_ctrl = dp_ctrl,
    priors  = priors,
    trans   = trans,
    alpha   = alpha,

    spec_trt = spec_trt,
    spec_con = spec_con,

    fit_trt  = list(
      mcmc_info  = fit_trt$mcmc_info,
      mcmc_draws = fit_trt$mcmc_draws
    ),
    fit_con  = list(
      mcmc_info  = fit_con$mcmc_info,
      mcmc_draws = fit_con$mcmc_draws
    ),

    N = c(trt = length(Y_trt), con = length(Y_con)),
    x_range = list(trt = x_range_trt, con = x_range_con),

    # store what we need later for conditional TE curves etc.
    .te_data = list(
      data      = data,
      keep      = keep,
      A         = A_keep,
      X_raw     = X_raw,
      intercept = intercept
    )
  )

  class(res) <- "mixgpd_te_fit"
  res
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
