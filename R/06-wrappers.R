# Lightweight user-facing wrappers

`%||%` <- function(a, b) if (!is.null(a)) a else b

.is_bundle <- function(x) {
  inherits(x, "causalmixgpd_bundle") || inherits(x, "causalmixgpd_causal_bundle")
}

.is_causal_bundle <- function(x) {
  inherits(x, "causalmixgpd_causal_bundle")
}

.coerce_treat <- function(treat) {
  if (is.null(treat)) return(NULL)

  if (is.factor(treat)) {
    if (nlevels(treat) != 2L) {
      stop("'treat' factor must have exactly 2 levels.", call. = FALSE)
    }
    treat <- as.integer(treat) - 1L
  } else if (is.logical(treat)) {
    treat <- as.integer(treat)
  } else if (is.character(treat)) {
    vals <- unique(treat)
    vals <- vals[!is.na(vals)]
    if (length(vals) != 2L) {
      stop("'treat' character input must be binary.", call. = FALSE)
    }
    treat <- as.integer(treat == vals[2L])
  } else {
    treat <- as.numeric(treat)
  }

  if (anyNA(treat)) stop("'treat' cannot contain NA.", call. = FALSE)
  if (!all(treat %in% c(0, 1))) stop("'treat' must be binary (0/1).", call. = FALSE)
  as.integer(treat)
}

.parse_formula_yX <- function(formula, data, na.action = stats::na.omit, drop_intercept = TRUE) {
  if (is.null(data)) stop("'data' is required when 'formula' is provided.", call. = FALSE)

  mf <- stats::model.frame(formula, data = data, na.action = na.action)
  y <- stats::model.response(mf)
  if (is.null(y)) stop("Could not extract response 'y' from formula.", call. = FALSE)
  y <- as.numeric(y)

  trm <- stats::terms(mf)
  term_labels <- attr(trm, "term.labels") %||% character(0)
  is_unconditional <- length(term_labels) == 0L

  X <- NULL
  X_cols <- character(0)
  ctr <- NULL
  if (!is_unconditional) {
    X <- stats::model.matrix(trm, data = mf)
    if (isTRUE(drop_intercept) && "(Intercept)" %in% colnames(X)) {
      X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
    }
    if (!ncol(X)) {
      X <- NULL
      is_unconditional <- TRUE
    } else {
      X_cols <- colnames(X) %||% character(0)
      ctr <- attr(X, "contrasts")
    }
  }

  list(
    y = y,
    X = X,
    mf = mf,
    terms = trm,
    xlevels = stats::.getXlevels(trm, mf),
    contrasts = ctr,
    X_cols = X_cols,
    is_unconditional = is_unconditional
  )
}

.extract_treat_from_data <- function(mf, data, treat, treat_expr = NULL) {
  n <- nrow(mf)
  vals <- NULL

  if (is.character(treat) && length(treat) == 1L) {
    if (!is.null(mf[[treat]])) {
      vals <- mf[[treat]]
    } else if (!is.null(data) && !is.null(data[[treat]])) {
      vals <- data[[treat]]
    } else {
      stop(sprintf("Could not find treatment column '%s'.", treat), call. = FALSE)
    }
  } else if (!is.null(treat_expr) &&
             is.symbol(treat_expr) &&
             as.character(treat_expr) != "NULL" &&
             as.character(treat_expr) %in% names(mf)) {
    vals <- mf[[as.character(treat_expr)]]
  } else {
    vals <- treat
  }

  if (length(vals) == n) return(.coerce_treat(vals))

  # Align vectors provided at full data length to model.frame rows after NA filtering.
  if (!is.null(data) && length(vals) == nrow(data)) {
    ridx <- suppressWarnings(as.integer(rownames(mf)))
    if (!anyNA(ridx) && length(ridx) == n) {
      return(.coerce_treat(vals[ridx]))
    }
  }

  stop("'treat' length does not match model rows after NA handling.", call. = FALSE)
}

.bundle_has_any_gpd <- function(b) {
  if (.is_causal_bundle(b)) {
    gpd <- b$meta$GPD %||% list()
    return(isTRUE(gpd$trt) || isTRUE(gpd$con))
  }
  isTRUE(b$spec$meta$GPD)
}

.bundle_all_gpd <- function(b) {
  if (.is_causal_bundle(b)) {
    gpd <- b$meta$GPD %||% list()
    return(isTRUE(gpd$trt) && isTRUE(gpd$con))
  }
  isTRUE(b$spec$meta$GPD)
}

.strip_gpd_single_bundle <- function(b) {
  stopifnot(inherits(b, "causalmixgpd_bundle"))

  b$spec$meta$GPD <- FALSE
  b$spec$plan$GPD <- FALSE
  b$spec$plan$gpd <- list()

  b$code <- .wrap_nimble_code(build_code_from_spec(b$spec))
  b$constants <- build_constants_from_spec(b$spec)
  b$dimensions <- build_dimensions_from_spec(b$spec)
  b$inits <- build_inits_from_spec(b$spec, y = b$data$y)
  b$monitors <- build_monitors_from_spec(b$spec)

  b
}

.bundle_strip_gpd <- function(b) {
  if (.is_causal_bundle(b)) {
    b$outcome$con <- .strip_gpd_single_bundle(b$outcome$con)
    b$outcome$trt <- .strip_gpd_single_bundle(b$outcome$trt)
    b$meta$GPD <- list(trt = FALSE, con = FALSE)
    return(b)
  }
  .strip_gpd_single_bundle(b)
}

.normalize_mcmc_inputs <- function(args) {
  if (!length(args)) return(list(overrides = list(), runner = list()))
  nm <- names(args)
  if (is.null(nm)) nm <- rep("", length(args))
  if (any(!nzchar(nm))) stop("All mcmc arguments must be named.", call. = FALSE)

  override_names <- c("niter", "nburn", "nburnin", "thin", "nchains", "seed", "waic")
  runner_names <- c("show_progress", "quiet")

  override_idx <- nm %in% override_names
  runner_idx <- nm %in% runner_names
  unknown <- nm[!(override_idx | runner_idx)]
  unknown <- unknown[nzchar(unknown)]
  if (length(unknown)) {
    stop(sprintf("Unknown mcmc argument(s): %s", paste(unique(unknown), collapse = ", ")), call. = FALSE)
  }

  overrides <- args[override_idx]
  if (!is.null(overrides$nburn) && is.null(overrides$nburnin)) {
    overrides$nburnin <- overrides$nburn
  }
  overrides$nburn <- NULL

  list(
    overrides = overrides,
    runner = args[runner_idx]
  )
}

.apply_mcmc_overrides <- function(b, overrides) {
  if (!length(overrides)) return(b)

  merge_one <- function(x) utils::modifyList(x %||% list(), overrides)

  if (.is_causal_bundle(b)) {
    b$outcome$con$mcmc <- merge_one(b$outcome$con$mcmc)
    b$outcome$trt$mcmc <- merge_one(b$outcome$trt$mcmc)
    if (inherits(b$design, "causalmixgpd_ps_bundle")) {
      b$design$mcmc <- merge_one(b$design$mcmc)
    }
    return(b)
  }

  b$mcmc <- merge_one(b$mcmc)
  b
}

.run_bundle_mcmc <- function(b, mcmc_args = list()) {
  if (!is.list(mcmc_args)) stop("'mcmc' must be a named list.", call. = FALSE)
  do.call(mcmc, c(list(b = b), mcmc_args))
}

#' Build a model bundle (short wrapper)
#'
#' @param x Either a response vector or an existing bundle.
#' @param data Optional data.frame used with \code{formula}.
#' @param X Optional design matrix/data.frame.
#' @param treat Optional binary treatment indicator.
#' @param formula Optional formula.
#' @param ... Additional arguments passed to \code{build_nimble_bundle()} or
#'   \code{build_causal_bundle()}.
#' @param GPD Logical; include GPD tail in build mode.
#' @return A \code{"causalmixgpd_bundle"} or \code{"causalmixgpd_causal_bundle"}.
#' @export
bundle <- function(x = NULL, data = NULL, X = NULL, treat = NULL, formula = NULL, ..., GPD = FALSE) {
  if (.is_bundle(x)) return(x)

  treat_expr <- substitute(treat)
  treat_supplied <- !missing(treat) && !identical(treat_expr, quote(NULL))

  y <- NULL
  x_mat <- X
  t_vec <- NULL
  formula_meta <- NULL

  if (!is.null(formula)) {
    parsed <- .parse_formula_yX(formula = formula, data = data)
    y <- parsed$y
    x_mat <- parsed$X
    if (treat_supplied) {
      if (is.symbol(treat_expr) && as.character(treat_expr) %in% names(parsed$mf)) {
        treat_in <- as.character(treat_expr)
      } else {
        treat_in <- treat
      }
      t_vec <- .extract_treat_from_data(parsed$mf, data = data, treat = treat_in, treat_expr = treat_expr)
    }

    formula_meta <- list(
      terms = parsed$terms,
      xlevels = parsed$xlevels,
      contrasts = parsed$contrasts,
      X_cols = parsed$X_cols,
      treat = if (is.symbol(treat_expr)) as.character(treat_expr) else NULL
    )
  } else {
    if (is.null(x)) stop("Provide either 'x' (response vector), 'formula', or a bundle.", call. = FALSE)
    y <- as.numeric(x)
    if (treat_supplied) t_vec <- .coerce_treat(treat)
  }

  if (!is.null(x_mat) && !is.matrix(x_mat)) x_mat <- as.matrix(x_mat)
  if (length(y) < 1L) stop("'y' must be a non-empty numeric vector.", call. = FALSE)
  if (!is.null(x_mat) && nrow(x_mat) != length(y)) stop("nrow(X) must match length(y).", call. = FALSE)
  if (!is.null(t_vec) && length(t_vec) != length(y)) stop("length(treat) must match length(y).", call. = FALSE)

  if (is.null(t_vec)) {
    b <- build_nimble_bundle(y = y, X = x_mat, GPD = GPD, ...)
  } else {
    b <- build_causal_bundle(y = y, X = x_mat, A = t_vec, GPD = GPD, ...)
  }

  if (!is.null(formula_meta)) {
    attr(b, "causalmixgpd_formula_meta") <- formula_meta
  }

  b
}

#' Run MCMC from a bundle (short wrapper)
#'
#' @param b A non-causal or causal bundle.
#' @param ... Optional MCMC overrides (\code{niter}, \code{nburnin}, \code{thin},
#'   \code{nchains}, \code{seed}, \code{waic}) and runner controls
#'   (\code{show_progress}, \code{quiet}).
#' @return A fitted object (\code{"mixgpd_fit"} or \code{"causalmixgpd_causal_fit"}).
#' @export
mcmc <- function(b, ...) {
  if (!.is_bundle(b)) stop("'b' must be a causalmixgpd bundle object.", call. = FALSE)

  parsed <- .normalize_mcmc_inputs(list(...))
  b <- .apply_mcmc_overrides(b, parsed$overrides)

  if (.is_causal_bundle(b)) {
    allowed <- c("show_progress")
    bad <- setdiff(names(parsed$runner), allowed)
    if (length(bad)) {
      stop(sprintf("Unsupported runner argument for causal bundles: %s", paste(bad, collapse = ", ")),
           call. = FALSE)
    }
    return(do.call(run_mcmc_causal, c(list(bundle = b), parsed$runner)))
  }

  do.call(run_mcmc_bundle_manual, c(list(bundle = b), parsed$runner))
}

#' Fit DP mixture model without GPD tail
#'
#' @param x Either a response vector or a bundle object.
#' @param data Optional data.frame used with \code{formula}.
#' @param X Optional design matrix/data.frame.
#' @param treat Optional binary treatment indicator.
#' @param formula Optional formula.
#' @param ... Additional build arguments in build mode.
#' @param mcmc Named list of run arguments passed to \code{mcmc()}.
#' @return A fitted object.
#' @export
dpmix <- function(x = NULL, data = NULL, X = NULL, treat = NULL, formula = NULL, ..., mcmc = list()) {
  b <- NULL

  if (.is_bundle(x)) {
    b <- if (.bundle_has_any_gpd(x)) .bundle_strip_gpd(x) else x
    return(.run_bundle_mcmc(b, mcmc_args = mcmc))
  }

  bundle_args <- c(
    list(x = x, data = data, X = X, formula = formula),
    list(...),
    list(GPD = FALSE)
  )
  if (!is.null(treat)) bundle_args$treat <- treat

  b <- do.call(bundle, bundle_args)
  .run_bundle_mcmc(b, mcmc_args = mcmc)
}

#' Fit DP mixture model with GPD tail
#'
#' @param x Either a response vector or a bundle object.
#' @param data Optional data.frame used with \code{formula}.
#' @param X Optional design matrix/data.frame.
#' @param treat Optional binary treatment indicator.
#' @param formula Optional formula.
#' @param ... Additional build arguments in build mode.
#' @param mcmc Named list of run arguments passed to \code{mcmc()}.
#' @return A fitted object.
#' @export
dpmgpd <- function(x = NULL, data = NULL, X = NULL, treat = NULL, formula = NULL, ..., mcmc = list()) {
  if (.is_bundle(x)) {
    if (!.bundle_all_gpd(x)) {
      stop(
        "dpmgpd() requires a bundle with GPD enabled for all modeled arms; use dpmix(bundle) for non-GPD runs.",
        call. = FALSE
      )
    }
    return(.run_bundle_mcmc(x, mcmc_args = mcmc))
  }

  bundle_args <- c(
    list(x = x, data = data, X = X, formula = formula),
    list(...),
    list(GPD = TRUE)
  )
  if (!is.null(treat)) bundle_args$treat <- treat

  b <- do.call(bundle, bundle_args)
  .run_bundle_mcmc(b, mcmc_args = mcmc)
}
