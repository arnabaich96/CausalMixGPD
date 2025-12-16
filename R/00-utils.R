#' Internal constants
#'
#' Internal constants used by the package.
#'
#' @return See details.
#'
#' @examples
#' getFromNamespace(".valid_trans_labels", "DPmixGPD")
#'
#' @keywords internal
.valid_trans_labels <- c("identity", "log", "log1p", "sqrt")
# Utilities shared across the package (internal)

#' @noRd
.as_mcmc_matrix <- function(object) {
  draws <- object$mcmc_draws

  if (is.matrix(draws)) return(draws)

  if (inherits(draws, "mcmc")) return(as.matrix(draws))

  if (inherits(draws, "mcmc.list")) {
    return(do.call(rbind, lapply(draws, as.matrix)))
  }

  if (is.list(draws)) {
    mats <- lapply(draws, function(x) {
      if (inherits(x, "mcmc")) return(as.matrix(x))
      if (is.matrix(x)) return(x)
      stop("Cannot coerce element of 'mcmc_draws' to matrix.", call. = FALSE)
    })
    return(do.call(rbind, mats))
  }

  stop("'object$mcmc_draws' must be a matrix (iterations x parameters) or a coda object.",
       call. = FALSE)
}

#' @noRd
.is_uncond_gamma_draws <- function(draws) {
  cn <- colnames(draws)
  any(grepl("^w\\[", cn)) && any(grepl("^shape\\[", cn)) && any(grepl("^scale\\[", cn))
}

#' @noRd
.require_uncond_gamma <- function(draws, where = "this method") {
  if (!.is_uncond_gamma_draws(draws)) {
    stop(where, " currently supports only unconditional Gamma DP draws ",
         "with 'w[j]', 'shape[j]', and 'scale[j]' columns (i.e., y ~ 0).",
         call. = FALSE)
  }
  invisible(TRUE)
}

## -------------------------------------------------------------------------
## NIMBLE compatibility helper (no global assignments)
## -------------------------------------------------------------------------
##
## We keep a *namespace-local* getNimbleOption() wrapper because some nimble
## internals call getNimbleOption() unqualified. Creating functions in the
## global environment triggers R CMD check NOTES, so we avoid that.
##
## If nimble never calls this in your setup, it is harmless.
## If nimble does call it, and it searches the attached namespaces (or the
## evaluation environment where the model is built), this resolves the symbol.
##
## @keywords internal
getNimbleOption <- function(name) {
  if (!requireNamespace("nimble", quietly = TRUE)) {
    stop("Package 'nimble' is required but not installed.", call. = FALSE)
  }

  if (!is.character(name) || length(name) != 1L) {
    stop("'name' must be a single character string.", call. = FALSE)
  }

  # Prefer nimbleOptions() when available.
  if (exists("nimbleOptions", where = asNamespace("nimble"), inherits = FALSE)) {
    opts <- nimble::nimbleOptions()
    if (!name %in% names(opts)) {
      stop("Unknown nimble option: ", name, call. = FALSE)
    }
    return(opts[[name]])
  }

  # Fall back to any internal getter if present.
  if (exists("getNimbleOption", where = asNamespace("nimble"), inherits = FALSE)) {
    return(get("getNimbleOption", envir = asNamespace("nimble"))(name))
  }

  stop("Cannot resolve nimble option getter (no nimbleOptions()/getNimbleOption()).",
       call. = FALSE)
}

#' Ensure nimble is available
#'
#' Internal guard used before building/running nimble models.
#'
#' @keywords internal
.ensure_nimble_compat <- function() {
  if (!requireNamespace("nimble", quietly = TRUE)) {
    stop("Package 'nimble' is required but not installed.", call. = FALSE)
  }
  invisible(TRUE)
}
#' Check kernel support
#'
#' Check kernel support.
#'
#' @param Y Y.
#' @param kernel kernel.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".check_kernel_support", "DPmixGPD")
#' f
#'
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
#' Check missing
#'
#' Check missing.
#'
#' @param Y Y.
#' @param X X.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".check_missing", "DPmixGPD")
#' f
#'
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
#' Check formula data
#'
#' Check formula data.
#'
#' @param formula formula.
#' @param data data.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".check_formula_data", "DPmixGPD")
#' f
#'
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
#' Check dp ctrl
#'
#' Check dp ctrl.
#'
#' @param dp_ctrl dp_ctrl.
#' @param N N.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".check_dp_ctrl", "DPmixGPD")
#' f
#'
.check_dp_ctrl <- function(dp_ctrl, N, dp_rep = c("stick_breaking", "crp")) {
  dp_rep <- match.arg(dp_rep)

  if (is.null(dp_ctrl) || (is.list(dp_ctrl) && length(dp_ctrl) == 0L)) {
    dp_ctrl <- list()
  }
  if (!is.list(dp_ctrl)) {
    stop("dp_ctrl must be a list.", call. = FALSE)
  }

  if (identical(dp_rep, "crp")) {
    # CRP does not use a fixed truncation level K.
    if (!is.null(dp_ctrl$K)) {
      warning("dp_ctrl$K is ignored for dp_rep = 'crp' (CRP does not use truncation).", call. = FALSE)
    }
    if (!is.null(dp_ctrl$m_aux)) {
      m_aux <- as.integer(dp_ctrl$m_aux)
      if (!is.finite(m_aux) || m_aux < 1L) {
        stop("dp_ctrl$m_aux must be an integer >= 1 for dp_rep = 'crp'.", call. = FALSE)
      }
    }
    return(invisible(TRUE))
  }

  # stick-breaking / truncated representation requires K
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
#' Check mcmc
#'
#' Check mcmc.
#'
#' @param mcmc mcmc.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".check_mcmc", "DPmixGPD")
#' f
#'
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
#' DPmixGPD: Dirichlet process mixtures with optional GPD tail
#'
#' An experimental framework for Dirichlet process mixture models (including
#' regression and unconditional formulations) with flexible kernels and an
#' optional generalized Pareto (GPD) tail extension.
#'
#' @docType package
#' @name DPmixGPD-package
#' @import nimble
#' @import ggplot2
#' @importFrom stats pgamma predict rgamma uniroot
#' @importFrom utils modifyList
#' @importFrom methods is
#' @keywords internal
"_PACKAGE"
# ------------------------------------------------------------------
# Transform engine: resolve user transforms + generate nimble code
# ------------------------------------------------------------------

# Internal: validate and normalize column names for transforms
# scale_spec can be:
#   - TRUE  -> all columns of X
#   - FALSE / NULL / character(0) -> no columns
#   - character vector of names -> must be subset of colnames(X)
#   - numeric indices -> converted to names
#' Transform valid names
#'
#' Transform valid names.
#'
#' @param scale_spec scale_spec.
#' @param X X.
#' @param caller caller.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".transform_valid_names", "DPmixGPD")
#' f
#'
#' @keywords internal
.transform_valid_names <- function(scale_spec, X, caller = ".transform_resolve") {

  # No covariates at all
  if (is.null(X)) {
    return(character(0L))
  }

  cn <- colnames(X)
  if (is.null(cn)) {
    cn <- paste0("x", seq_len(ncol(X)))
    colnames(X) <- cn
  }

  # 1) Logical TRUE/FALSE
  if (is.logical(scale_spec) && length(scale_spec) == 1L) {
    if (isTRUE(scale_spec)) {
      # all columns
      return(cn)
    } else {
      # FALSE
      return(character(0L))
    }
  }

  # 2) NULL or empty
  if (is.null(scale_spec) || length(scale_spec) == 0L) {
    return(character(0L))
  }

  # 3) Numeric indices
  if (is.numeric(scale_spec)) {
    idx <- as.integer(scale_spec)
    if (any(is.na(idx)) || any(idx < 1L) || any(idx > ncol(X))) {
      stop(
        sprintf(
          "%s: numeric indices in 'scale' must be between 1 and %d.",
          caller, ncol(X)
        ),
        call. = FALSE
      )
    }
    return(cn[idx])
  }

  # 4) Character names
  if (is.character(scale_spec)) {
    bad <- setdiff(scale_spec, cn)
    if (length(bad) > 0L) {
      stop(
        sprintf(
          "%s: unknown variable(s) in 'scale': %s. Available: %s",
          caller,
          paste(bad, collapse = ", "),
          paste(cn, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    return(scale_spec)
  }

  # 5) Anything else is not supported
  stop(
    sprintf(
      "%s: 'scale' must be TRUE/FALSE, NULL, character names, or numeric indices.",
      caller
    ),
    call. = FALSE
  )
}

# (internal) transformation resolver docs live on .transform_resolve()
#' Transform generate code
#'
#' Transform generate code.
#'
#' @param lhs lhs.
#' @param lp lp.
#' @param transform transform.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".transform_generate_code", "DPmixGPD")
#' f
#'
#' @keywords internal
.transform_generate_code <- function(lhs, lp, transform) {
  transform <- tolower(trimws(transform))

  # "none" is treated as identity for safety
  if (identical(transform, "none")) {
    transform <- "identity"
  }

  expr <- switch(
    transform,
    "identity" = sprintf("%s <- %s", lhs, lp),

    "exp"      = sprintf("%s <- exp(%s)", lhs, lp),

    "log"      = sprintf("%s <- log(%s)", lhs, lp),

    "sq"       = sprintf("%s <- (%s)^2", lhs, lp),

    "sqrt"     = sprintf("%s <- sqrt(%s)", lhs, lp),

    stop(sprintf(
      "Unknown transform '%s' in .transform_generate_code().",
      transform
    ), call. = FALSE)
  )

  expr
}
## Resolve and apply simple transformations to X
##
## Right now we support:
##   trans = NULL                -> no change
##   trans = list(scale = TRUE)  -> center & scale all columns of X
##   trans = list(scale = c("x1","x2")) -> center & scale only those columns
##
#' Transform resolve
#'
#' Transform resolve.
#'
#' @param X X.
#' @param trans trans.
#' @param caller caller.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".transform_resolve", "DPmixGPD")
#' f
#'
#' @keywords internal
.transform_resolve <- function(X, trans, caller = "fit.dpm") {
  # no covariates or no transforms requested
  if (is.null(X) || is.null(trans) || length(trans) == 0L) {
    return(list(X = X,
                meta = list(
                  scale_cols  = integer(0),
                  scale_means = numeric(0),
                  scale_sds   = numeric(0)
                )))
  }

  if (!is.list(trans)) {
    stop(sprintf("%s: 'trans' must be a list or NULL.", caller), call. = FALSE)
  }

  ## ----- currently we only implement 'scale' -----
  scale_spec <- trans$scale

  if (is.null(scale_spec)) {
    # nothing to do
    return(list(X = X,
                meta = list(
                  scale_cols  = integer(0),
                  scale_means = numeric(0),
                  scale_sds   = numeric(0)
                )))
  }

  # decide which columns to scale
  if (isTRUE(scale_spec)) {
    cols <- seq_len(ncol(X))
  } else if (is.character(scale_spec)) {
    .transform_valid_names(scale_spec, X, caller = caller)
    cols <- match(scale_spec, colnames(X))
  } else if (is.numeric(scale_spec)) {
    cols <- as.integer(scale_spec)
    if (any(cols < 1L | cols > ncol(X))) {
      stop(sprintf("%s: 'trans$scale' column indices out of range.", caller),
           call. = FALSE)
    }
  } else {
    stop(sprintf("%s: 'trans$scale' must be TRUE, character, or numeric.",
                 caller),
         call. = FALSE)
  }

  cols <- unique(cols)
  cols <- cols[!is.na(cols)]

  if (length(cols) == 0L) {
    return(list(X = X,
                meta = list(
                  scale_cols  = integer(0),
                  scale_means = numeric(0),
                  scale_sds   = numeric(0)
                )))
  }

  means <- numeric(ncol(X))
  sds   <- numeric(ncol(X))

  for (j in cols) {
    xj <- X[, j]
    if (!is.numeric(xj)) {
      stop(sprintf(
        "%s: cannot scale non-numeric column '%s'.",
        caller, colnames(X)[j]
      ), call. = FALSE)
    }
    m  <- mean(xj, na.rm = TRUE)
    sdj <- stats::sd(xj, na.rm = TRUE)

    # guard against zero variance
    if (is.na(sdj) || sdj == 0) {
      sdj <- 1
    }

    X[, j] <- (xj - m) / sdj
    means[j] <- m
    sds[j]   <- sdj
  }

  meta <- list(
    scale_cols  = cols,
    scale_means = means[cols],
    scale_sds   = sds[cols]
  )

  list(X = X, meta = meta)
}
# Internal: ensure X is a proper matrix, optionally add intercept
#' Prepare design matrix
#'
#' Prepare design matrix.
#'
#' @param X X.
#' @param intercept intercept.
#'
#' @return See details.
#'
#' @keywords internal
.prepare_design_matrix <- function(X, intercept = TRUE) {

  # ---- Defensive: intercept must be a single TRUE/FALSE ----
  if (is.null(intercept)) intercept <- TRUE
  if (!is.logical(intercept) || length(intercept) != 1L || is.na(intercept)) {
    stop("'intercept' must be a single TRUE/FALSE.", call. = FALSE)
  }

  # No covariates case
  if (is.null(X)) return(NULL)

  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)

  # Give generic names if missing
  if (is.null(colnames(X))) {
    colnames(X) <- paste0("x", seq_len(p))
  }

  # If user does NOT want an intercept, just return matrix as-is
  if (!intercept) {
    return(X)
  }

  ## Intercept = TRUE:

  # 1) If it already looks like it has an intercept, keep it
  if (p >= 1L) {
    # named "(Intercept)" in first column
    if (!is.null(colnames(X)) && identical(colnames(X)[1L], "(Intercept)")) {
      return(X)
    }
    # or numerically all ones in first column
    if (all(abs(X[, 1L] - 1) < .Machine$double.eps^0.5)) {
      return(X)
    }
  }

  # 2) Otherwise, prepend an intercept column of ones
  intercept_col <- rep(1, n)
  X_new <- cbind("(Intercept)" = intercept_col, X)

  X_new
}


# ---- misc helpers ----
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x