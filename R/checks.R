#' Internal checks: kernel vs response support
#' @keywords internal
.check_kernel_support <- function(Y, kernel) {
  if (!is.vector(Y) || !is.numeric(Y)) {
    stop("Response Y must be a numeric vector.", call. = FALSE)
  }

  pos_kernels <- c("gamma", "lognormal", "inverse_gaussian", "amoroso", "pareto")

  if (kernel %in% pos_kernels) {
    if (any(Y <= 0, na.rm = TRUE)) {
      stop(
        "Kernel '", kernel, "' requires Y > 0. ",
        "Found non-positive values in the response.",
        call. = FALSE
      )
    }
  } else if (kernel %in% c("normal", "laplace")) {
    if (all(Y > 0, na.rm = TRUE)) {
      warning(
        "All response values are positive, but kernel '", kernel,
        "' has support on the whole real line. ",
        "A positive-support kernel (e.g., 'gamma', 'lognormal') might be more appropriate.",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

#' Internal checks: missing values in Y and X
#' @keywords internal
.check_missing <- function(Y, X) {
  if (any(!is.finite(Y))) {
    stop(
      "Response Y contains NA/NaN/Inf. ",
      "Please clean the response before calling the fitting function.",
      call. = FALSE
    )
  }
  if (!is.null(X)) {
    X_mat <- as.matrix(X)
    if (any(!is.finite(X_mat))) {
      warning(
        "Covariate matrix X contains NA/NaN/Inf. ",
        "Rows with missing covariates may cause sampler failure.",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

#' Internal checks: formula/data consistency
#' @keywords internal
.check_formula_data <- function(formula, data) {
  vars_in_formula <- all.vars(formula)
  missing_vars <- setdiff(vars_in_formula, names(data))
  if (length(missing_vars) > 0L) {
    stop(
      "Variables not found in 'data': ",
      paste(missing_vars, collapse = ", "),
      ". Check the formula and data frame names.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Internal checks: DP settings
#' @keywords internal
.check_dp_ctrl <- function(dp_ctrl, N) {
  if (is.null(dp_ctrl$K)) {
    stop("dp_ctrl$K (number of mixture components) must be specified.", call. = FALSE)
  }
  K <- as.integer(dp_ctrl$K)
  if (!is.finite(K) || K < 2L) {
    stop("dp_ctrl$K (number of mixture components) must be an integer >= 2.", call. = FALSE)
  }
  if (K > N) {
    warning(
      "dp_ctrl$K = ", K, " is larger than sample size N = ", N, ". ",
      "Many components may remain essentially empty.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Internal checks: MCMC control
#' @keywords internal
.check_mcmc <- function(mcmc) {
  n_iter  <- if (is.null(mcmc$n_iter)) 2000L else as.integer(mcmc$n_iter)
  burn_in <- if (is.null(mcmc$burn_in)) 1000L else as.integer(mcmc$burn_in)
  thin    <- if (is.null(mcmc$thin)) 1L else as.integer(mcmc$thin)
  chains  <- if (is.null(mcmc$chains)) 1L else as.integer(mcmc$chains)

  if (!is.finite(n_iter) || n_iter <= 0L) {
    stop("mcmc$n_iter must be a positive integer.", call. = FALSE)
  }
  if (!is.finite(burn_in) || burn_in < 0L) {
    stop("mcmc$burn_in must be a non-negative integer.", call. = FALSE)
  }
  if (n_iter <= burn_in) {
    stop(
      "mcmc$n_iter (", n_iter, ") must be greater than mcmc$burn_in (", burn_in, ").",
      call. = FALSE
    )
  }
  if (!is.finite(thin) || thin < 1L) {
    stop("mcmc$thin must be an integer >= 1.", call. = FALSE)
  }
  if (!is.finite(chains) || chains < 1L) {
    stop("mcmc$chains must be an integer >= 1.", call. = FALSE)
  }

  invisible(TRUE)
}
