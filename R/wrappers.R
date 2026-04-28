# Lightweight user-facing wrappers


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

.treat_arg_supplied <- function(call_args, treat_expr) {
  ("treat" %in% names(call_args)) && !identical(treat_expr, quote(NULL))
}

.formula_design_matrix <- function(trm,
                                   mf,
                                   drop_intercept = TRUE,
                                   drop_terms = character(0),
                                   coerce_double = FALSE) {
  term_labels <- attr(trm, "term.labels") %||% character(0)
  if (!length(term_labels)) {
    return(list(X = NULL, contrasts = NULL, X_cols = character(0)))
  }

  mm <- stats::model.matrix(trm, data = mf)
  keep <- rep(TRUE, ncol(mm))

  if (isTRUE(drop_intercept) && ncol(mm)) {
    keep <- keep & ((colnames(mm) %||% rep("", ncol(mm))) != "(Intercept)")
  }

  drop_terms <- tolower(as.character(drop_terms %||% character(0)))
  if (length(drop_terms) && ncol(mm)) {
    assign_idx <- attr(mm, "assign") %||% integer(0)
    drop_idx <- which(tolower(term_labels) %in% drop_terms)
    if (length(drop_idx) && length(assign_idx) == ncol(mm)) {
      keep <- keep & !(assign_idx %in% drop_idx)
    }
  }

  if (!any(keep)) {
    return(list(
      X = NULL,
      contrasts = attr(mm, "contrasts"),
      X_cols = character(0)
    ))
  }

  X <- mm[, keep, drop = FALSE]
  if (isTRUE(coerce_double)) storage.mode(X) <- "double"

  list(
    X = X,
    contrasts = attr(mm, "contrasts"),
    X_cols = colnames(X) %||% character(0)
  )
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
    design <- .formula_design_matrix(
      trm = trm,
      mf = mf,
      drop_intercept = drop_intercept,
      drop_terms = "id"
    )
    X <- design$X
    if (is.null(X)) {
      X <- NULL
      is_unconditional <- TRUE
    } else {
      X_cols <- design$X_cols
      ctr <- design$contrasts
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
  b$inits <- build_inits_from_spec(b$spec, y = b$data$y, X = b$data$X %||% NULL)
  pol <- b$monitor_policy %||% list()
  b$monitors <- build_monitors_from_spec(
    b$spec,
    monitor_v = isTRUE(pol$monitor_v),
    monitor_latent = isTRUE(pol$monitor_latent)
  )

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
  if (!length(args)) {
    return(list(
      overrides = list(),
      runner = list(),
      outcome_overrides = list(),
      ps_overrides = list()
    ))
  }
  nm <- names(args)
  if (is.null(nm)) nm <- rep("", length(args))
  if (any(!nzchar(nm))) stop("All mcmc arguments must be named.", call. = FALSE)

  override_names <- c(
    "niter", "nburn", "nburnin", "thin", "nchains", "seed", "waic",
    "parallel_chains", "parallel_arms", "workers", "timing", "z_update_every"
  )
  runner_names <- c("show_progress", "quiet", "parallel_chains", "parallel_arms", "workers", "timing")

  normalize_override_list <- function(x, label) {
    if (is.null(x)) return(list())
    if (!is.list(x)) stop(sprintf("'%s' must be a named list.", label), call. = FALSE)
    x_names <- names(x)
    if (is.null(x_names)) x_names <- rep("", length(x))
    if (any(!nzchar(x_names))) stop(sprintf("All entries in '%s' must be named.", label), call. = FALSE)
    unknown_x <- x_names[!(x_names %in% override_names)]
    if (length(unknown_x)) {
      stop(sprintf("Unknown %s argument(s): %s", label, paste(unique(unknown_x), collapse = ", ")), call. = FALSE)
    }
    if (!is.null(x$nburn) && is.null(x$nburnin)) {
      x$nburnin <- x$nburn
    }
    x$nburn <- NULL
    x
  }

  outcome_overrides <- normalize_override_list(args$mcmc_outcome %||% NULL, "mcmc_outcome")
  ps_overrides <- normalize_override_list(args$mcmc_ps %||% NULL, "mcmc_ps")
  args$mcmc_outcome <- NULL
  args$mcmc_ps <- NULL
  nm <- names(args)

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
    runner = args[runner_idx],
    outcome_overrides = outcome_overrides,
    ps_overrides = ps_overrides
  )
}

.apply_mcmc_overrides <- function(b, overrides, outcome_overrides = list(), ps_overrides = list()) {
  if (!length(overrides) && !length(outcome_overrides) && !length(ps_overrides)) return(b)

  if (.is_causal_bundle(b)) {
    outcome_merged <- utils::modifyList(overrides %||% list(), outcome_overrides %||% list())
    if (length(overrides) || length(outcome_overrides)) {
      b$outcome$con$mcmc <- utils::modifyList(b$outcome$con$mcmc %||% list(), outcome_merged)
      b$outcome$trt$mcmc <- utils::modifyList(b$outcome$trt$mcmc %||% list(), outcome_merged)
    }
    if (length(ps_overrides) && inherits(b$design, "causalmixgpd_ps_bundle")) {
      b$design$mcmc <- utils::modifyList(b$design$mcmc %||% list(), ps_overrides)
    }
    return(b)
  }

  if (length(outcome_overrides) || length(ps_overrides)) {
    stop("mcmc_outcome/mcmc_ps overrides are available only for causal bundles.", call. = FALSE)
  }

  b$mcmc <- utils::modifyList(b$mcmc %||% list(), overrides %||% list())
  b
}

.run_bundle_mcmc <- function(b, mcmc_args = list()) {
  if (!is.list(mcmc_args)) stop("'mcmc' must be a named list.", call. = FALSE)
  do.call(mcmc, c(list(b = b), mcmc_args))
}

.wrapper_mcmc_arg_names <- function() {
  c(
    "niter", "nburn", "nburnin", "thin", "nchains", "seed", "waic",
    "parallel_chains", "parallel_arms", "workers", "timing", "z_update_every",
    "mcmc_outcome", "mcmc_ps",
    "show_progress", "quiet"
  )
}

.collect_inline_mcmc_from_dots <- function(dots_expr, mcmc, eval_env) {
  if (is.null(dots_expr) || !length(dots_expr)) {
    return(list(mcmc = mcmc, names = character(0)))
  }

  dots_list <- as.list(dots_expr)
  dot_names <- names(dots_list)
  if (is.null(dot_names)) dot_names <- rep("", length(dots_list))

  inline_idx <- nzchar(dot_names) & (dot_names %in% .wrapper_mcmc_arg_names())
  if (!any(inline_idx)) {
    return(list(mcmc = mcmc, names = character(0)))
  }

  inline_vals <- lapply(dots_list[inline_idx], eval, envir = eval_env)
  parsed <- .normalize_mcmc_inputs(inline_vals)
  merged <- utils::modifyList(mcmc %||% list(), c(parsed$overrides, parsed$runner))
  if (length(parsed$outcome_overrides)) {
    merged$mcmc_outcome <- utils::modifyList(merged$mcmc_outcome %||% list(), parsed$outcome_overrides)
  }
  if (length(parsed$ps_overrides)) {
    merged$mcmc_ps <- utils::modifyList(merged$mcmc_ps %||% list(), parsed$ps_overrides)
  }

  list(mcmc = merged, names = dot_names[inline_idx])
}

#' Build the workflow bundle used by the package fitters
#'
#' \code{bundle()} is the main workflow constructor. It converts raw inputs,
#' a formula/data pair, or an already prepared bundle into the canonical
#' object consumed by \code{\link{mcmc}}, \code{\link{dpmix}},
#' \code{\link{dpmgpd}}, \code{\link{dpmix.causal}}, and
#' \code{\link{dpmgpd.causal}}.
#'
#' For one-arm models the returned object represents a bulk Dirichlet process
#' mixture, optionally augmented with a spliced generalized Pareto tail. For
#' causal models the returned object contains two arm-specific outcome bundles
#' plus an optional propensity score block.
#'
#' @details
#' The workflow is:
#' \enumerate{
#'   \item prepare a bundle with \code{bundle()},
#'   \item run posterior sampling with \code{\link{mcmc}} or one of the
#'   \code{dpmix*}/\code{dpmgpd*} wrappers,
#'   \item inspect the fitted object with \code{\link{summary.mixgpd_fit}},
#'   \code{\link{params}}, \code{\link{predict.mixgpd_fit}}, or the causal
#'   estimand helpers.
#' }
#'
#' Setting \code{GPD = TRUE} requests the spliced bulk-tail model with
#' conditional distribution
#' \deqn{F(y \mid x) = F_{\mathrm{bulk}}(y \mid x)\mathbf{1}\{y \le u(x)\} +
#' \left[p_u(x) + \{1 - p_u(x)\}F_{\mathrm{GPD}}(y \mid x)\right]\mathbf{1}\{y > u(x)\},}
#' where \eqn{p_u(x)} is the bulk probability below the threshold \eqn{u(x)}.
#'
#' See the manuscript vignette for the DPM hierarchy, SB/CRP representations,
#' and the spliced bulk-tail construction used throughout the package.
#'
#' @param y Either a response vector or an existing bundle.
#' @param X Optional design matrix/data.frame.
#' @param treat Optional binary treatment indicator.
#' @param data Optional data.frame used with \code{formula}.
#' @param formula Optional formula.
#' @param GPD Logical; include GPD tail in build mode.
#' @param ... Additional arguments passed to \code{build_nimble_bundle()} or
#'   \code{build_causal_bundle()}.
#' @return A \code{"causalmixgpd_bundle"} for one-arm models or a
#'   \code{"causalmixgpd_causal_bundle"} for causal models. The bundle stores
#'   code-generation inputs, monitor policy, and default MCMC settings, but it
#'   does not run MCMC.
#' @seealso \code{\link{build_nimble_bundle}}, \code{\link{build_causal_bundle}},
#'   \code{\link{mcmc}}, \code{\link{dpmix}}, \code{\link{dpmgpd}}.
#' @export
bundle <- function(y = NULL, X = NULL, treat = NULL, data = NULL, formula = NULL, GPD = FALSE, ...) {
  if (.is_bundle(y)) return(y)

  treat_expr <- substitute(treat)
  call_args <- as.list(match.call(expand.dots = FALSE))
  treat_supplied <- .treat_arg_supplied(call_args, treat_expr)

  y_vec <- NULL
  x_mat <- X
  t_vec <- NULL
  formula_meta <- NULL

  if (!is.null(formula)) {
    parsed <- .parse_formula_yX(formula = formula, data = data)
    y_vec <- parsed$y
    x_mat <- parsed$X
    if (treat_supplied) {
      treat_name <- if (is.symbol(treat_expr)) as.character(treat_expr) else NULL
      if (!is.null(treat_name) &&
          treat_name != "NULL" &&
          (treat_name %in% names(parsed$mf) || (!is.null(data) && treat_name %in% names(data)))) {
        treat_in <- treat_name
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
    if (is.null(y)) stop("Provide either 'y' (response vector), 'formula', or a bundle.", call. = FALSE)
    y_vec <- as.numeric(y)
    if (treat_supplied) t_vec <- .coerce_treat(treat)
  }

  if (is.data.frame(x_mat) && any(tolower(names(x_mat)) == "id")) {
    keep <- tolower(names(x_mat)) != "id"
    x_mat <- x_mat[, keep, drop = FALSE]
  }

  if (!is.null(x_mat) && !is.matrix(x_mat)) x_mat <- as.matrix(x_mat)
  if (length(y_vec) < 1L) stop("'y' must be a non-empty numeric vector.", call. = FALSE)
  if (!is.null(x_mat) && nrow(x_mat) != length(y_vec)) stop("nrow(X) must match length(y).", call. = FALSE)
  if (!is.null(t_vec) && length(t_vec) != length(y_vec)) stop("length(treat) must match length(y).", call. = FALSE)

  if (is.null(t_vec)) {
    b <- build_nimble_bundle(y = y_vec, X = x_mat, GPD = GPD, ...)
  } else {
    b <- build_causal_bundle(y = y_vec, X = x_mat, A = t_vec, GPD = GPD, ...)
  }

  if (!is.null(formula_meta)) {
    attr(b, "causalmixgpd_formula_meta") <- formula_meta
  }

  b
}

#' Run posterior sampling from a prepared bundle
#'
#' \code{mcmc()} is the generic workflow runner. It dispatches to
#' \code{\link{run_mcmc_bundle_manual}} for one-arm bundles and to
#' \code{\link{run_mcmc_causal}} for causal bundles.
#'
#' @details
#' This wrapper is useful when you want a two-stage workflow:
#' build first, inspect or modify the bundle, then sample. Named MCMC arguments
#' supplied through \code{...} override the settings stored in the bundle before
#' execution.
#'
#' The returned fit represents posterior draws from the finite SB/CRP
#' approximation encoded in the bundle. Downstream summaries therefore target
#' posterior predictive quantities such as \eqn{f(y \mid x)},
#' \eqn{F(y \mid x)}, and derived treatment-effect functionals.
#'
#' @param b A non-causal or causal bundle.
#' @param ... Optional MCMC overrides (\code{niter}, \code{nburnin}, \code{thin},
#'   \code{nchains}, \code{seed}, \code{waic}) and runner controls
#'   (\code{show_progress}, \code{quiet}).
#' @return A fitted object of class \code{"mixgpd_fit"} or
#'   \code{"causalmixgpd_causal_fit"}.
#' @seealso \code{\link{bundle}}, \code{\link{run_mcmc_bundle_manual}},
#'   \code{\link{run_mcmc_causal}}, \code{\link{predict.mixgpd_fit}}.
#' @export
mcmc <- function(b, ...) {
  if (!.is_bundle(b)) stop("'b' must be a causalmixgpd bundle object.", call. = FALSE)

  parsed <- .normalize_mcmc_inputs(list(...))
  b <- .apply_mcmc_overrides(
    b,
    parsed$overrides,
    outcome_overrides = parsed$outcome_overrides,
    ps_overrides = parsed$ps_overrides
  )

  if (.is_causal_bundle(b)) {
    # For causal workflows, parallel_chains is an arm-level MCMC override
    # consumed by run_mcmc_bundle_manual() inside run_mcmc_causal().
    parsed$runner$parallel_chains <- NULL
    allowed <- c("show_progress", "quiet", "parallel_arms", "workers", "timing")
    bad <- setdiff(names(parsed$runner), allowed)
    if (length(bad)) {
      stop(sprintf("Unsupported runner argument for causal bundles: %s", paste(bad, collapse = ", ")),
           call. = FALSE)
    }
    return(do.call(run_mcmc_causal, c(list(bundle = b), parsed$runner)))
  }

  allowed <- c("show_progress", "quiet", "parallel_chains", "workers", "timing")
  bad <- setdiff(names(parsed$runner), allowed)
  if (length(bad)) {
    stop(sprintf("Unsupported runner argument for non-causal bundles: %s", paste(bad, collapse = ", ")),
         call. = FALSE)
  }
  do.call(run_mcmc_bundle_manual, c(list(bundle = b), parsed$runner))
}

#' Fit a one-arm Dirichlet process mixture without a GPD tail
#'
#' \code{dpmix()} is the one-step convenience wrapper for the bulk-only model.
#' It combines \code{\link{bundle}} and \code{\link{mcmc}} for one-arm data.
#'
#' @details
#' The fitted model targets the posterior predictive bulk distribution
#' \deqn{f(y \mid x) = \int f(y \mid x, \theta)\,d\Pi(\theta),}
#' without the spliced tail augmentation used by \code{\link{dpmgpd}}.
#'
#' Use this wrapper when the outcome support is adequately modeled by the bulk
#' kernel alone. If you need threshold exceedance modeling or extreme-quantile
#' extrapolation, use \code{\link{dpmgpd}} instead.
#'
#' @param y Either a response vector or a bundle object.
#' @param X Optional design matrix/data.frame.
#' @param treat Optional binary treatment indicator. If supplied, this wrapper
#'   errors; use \code{dpmix.causal()} for causal models.
#' @param data Optional data.frame used with \code{formula}.
#' @param mcmc Named list of run arguments passed to \code{mcmc()} (including
#'   optional performance controls such as \code{parallel_chains},
#'   \code{workers}, \code{timing}, and \code{z_update_every}).
#' @param formula Optional formula.
#' @param ... Additional build arguments passed to \code{\link{build_nimble_bundle}}.
#' @return A fitted object of class \code{"mixgpd_fit"}.
#' @seealso \code{\link{build_nimble_bundle}},
#'   \code{\link{bundle}}, \code{\link{dpmgpd}},
#'   \code{\link{predict.mixgpd_fit}}, \code{\link{summary.mixgpd_fit}}.
#' @export
dpmix <- function(y = NULL, X = NULL, treat = NULL, data = NULL, mcmc = list(), formula = NULL, ...) {
  treat_expr <- substitute(treat)
  call_match <- match.call(expand.dots = FALSE)
  call_args <- as.list(call_match)

  if (.is_causal_bundle(y) || .treat_arg_supplied(call_args, treat_expr)) {
    stop(
      "dpmix() is for one-arm models. Use dpmix.causal() for causal models.",
      call. = FALSE
    )
  }

  inline <- .collect_inline_mcmc_from_dots(call_args$..., mcmc = mcmc, eval_env = parent.frame())
  mcmc <- inline$mcmc

  b <- NULL

  if (.is_bundle(y)) {
    b <- if (.bundle_has_any_gpd(y)) .bundle_strip_gpd(y) else y
    return(.run_bundle_mcmc(b, mcmc_args = mcmc))
  }

  bundle_call <- match.call(expand.dots = TRUE)
  bundle_call[[1L]] <- quote(bundle)
  bundle_call$mcmc <- NULL
  bundle_call$GPD <- FALSE
  if (length(inline$names)) {
    for (nm in unique(inline$names)) bundle_call[[nm]] <- NULL
  }

  b <- eval.parent(bundle_call)
  .run_bundle_mcmc(b, mcmc_args = mcmc)
}

#' Fit a one-arm Dirichlet process mixture with a spliced GPD tail
#'
#' \code{dpmgpd()} is the one-step convenience wrapper for the spliced
#' bulk-tail model. It combines \code{\link{bundle}} and \code{\link{mcmc}} for
#' one-arm data.
#'
#' @details
#' This wrapper targets the posterior predictive distribution obtained by
#' combining a flexible bulk DPM with a generalized Pareto exceedance model
#' above the threshold \eqn{u(x)}. In the tail region the predictive density is
#' proportional to
#' \deqn{\{1 - p_u(x)\} f_{\mathrm{GPD}}(y \mid x), \qquad y > u(x),}
#' where \eqn{p_u(x)} is the posterior bulk mass below the threshold.
#'
#' Use this wrapper when upper-tail behavior matters for inference, prediction,
#' or extrapolation of extreme quantiles and survival probabilities.
#'
#' @param y Either a response vector or a bundle object.
#' @param X Optional design matrix/data.frame.
#' @param treat Optional binary treatment indicator. If supplied, this wrapper
#'   errors; use \code{dpmgpd.causal()} for causal models.
#' @param data Optional data.frame used with \code{formula}.
#' @param mcmc Named list of run arguments passed to \code{mcmc()} (including
#'   optional performance controls such as \code{parallel_chains},
#'   \code{workers}, \code{timing}, and \code{z_update_every}).
#' @param formula Optional formula.
#' @param ... Additional build arguments passed to \code{\link{build_nimble_bundle}}.
#' @return A fitted object of class \code{"mixgpd_fit"}.
#' @seealso \code{\link{build_nimble_bundle}},
#'   \code{\link{bundle}}, \code{\link{dpmix}},
#'   \code{\link{predict.mixgpd_fit}}, \code{\link{summary.mixgpd_fit}}.
#' @export
dpmgpd <- function(y = NULL, X = NULL, treat = NULL, data = NULL, mcmc = list(), formula = NULL, ...) {
  treat_expr <- substitute(treat)
  call_match <- match.call(expand.dots = FALSE)
  call_args <- as.list(call_match)

  if (.is_causal_bundle(y) || .treat_arg_supplied(call_args, treat_expr)) {
    stop(
      "dpmgpd() is for one-arm models. Use dpmgpd.causal() for causal models.",
      call. = FALSE
    )
  }

  inline <- .collect_inline_mcmc_from_dots(call_args$..., mcmc = mcmc, eval_env = parent.frame())
  mcmc <- inline$mcmc

  if (.is_bundle(y)) {
    if (!.bundle_all_gpd(y)) {
      stop(
        "dpmgpd() requires a bundle with GPD enabled for all modeled arms; use dpmix(bundle) for non-GPD runs.",
        call. = FALSE
      )
    }
    return(.run_bundle_mcmc(y, mcmc_args = mcmc))
  }

  bundle_call <- match.call(expand.dots = TRUE)
  bundle_call[[1L]] <- quote(bundle)
  bundle_call$mcmc <- NULL
  bundle_call$GPD <- TRUE
  if (length(inline$names)) {
    for (nm in unique(inline$names)) bundle_call[[nm]] <- NULL
  }

  b <- eval.parent(bundle_call)
  .run_bundle_mcmc(b, mcmc_args = mcmc)
}

#' Fit a causal two-arm Dirichlet process mixture without a GPD tail
#'
#' \code{dpmix.causal()} fits a causal model with separate treated and control
#' outcome mixtures and, when requested, a propensity score block. It is the
#' bulk-only companion to \code{\link{dpmgpd.causal}}.
#'
#' @details
#' The resulting fit supports conditional outcome prediction
#' \eqn{F_a(y \mid x)} for \eqn{a \in \{0,1\}}, followed by causal
#' functionals such as \code{\link{ate}}, \code{\link{qte}},
#' \code{\link{cate}}, and \code{\link{cqte}}.
#'
#' @param y Either a response vector or a causal bundle object.
#' @param X Optional design matrix/data.frame.
#' @param treat Binary treatment indicator.
#' @param data Optional data.frame used with \code{formula}.
#' @param mcmc Named list of run arguments passed to \code{mcmc()} (including
#'   optional performance controls such as \code{parallel_arms},
#'   \code{workers}, \code{timing}, and \code{z_update_every}).
#' @param formula Optional formula.
#' @param ... Additional build arguments passed to \code{\link{build_causal_bundle}}.
#' @return A fitted object of class \code{"causalmixgpd_causal_fit"}.
#' @seealso \code{\link{build_causal_bundle}},
#'   \code{\link{bundle}}, \code{\link{dpmgpd.causal}},
#'   \code{\link{predict.causalmixgpd_causal_fit}}, \code{\link{ate}},
#'   \code{\link{qte}}.
#' @export
dpmix.causal <- function(y = NULL, X = NULL, treat = NULL, data = NULL, mcmc = list(), formula = NULL, ...) {
  treat_expr <- substitute(treat)
  call_match <- match.call(expand.dots = FALSE)
  call_args <- as.list(call_match)

  inline <- .collect_inline_mcmc_from_dots(call_args$..., mcmc = mcmc, eval_env = parent.frame())
  mcmc <- inline$mcmc

  b <- NULL

  if (.is_bundle(y)) {
    if (!.is_causal_bundle(y)) {
      stop("dpmix.causal() requires a causal bundle; use dpmix() for one-arm models.", call. = FALSE)
    }
    b <- if (.bundle_has_any_gpd(y)) .bundle_strip_gpd(y) else y
    return(.run_bundle_mcmc(b, mcmc_args = mcmc))
  }

  if (!.treat_arg_supplied(call_args, treat_expr)) {
    stop("dpmix.causal() requires 'treat' when building from raw inputs.", call. = FALSE)
  }

  bundle_call <- match.call(expand.dots = TRUE)
  bundle_call[[1L]] <- quote(bundle)
  bundle_call$mcmc <- NULL
  bundle_call$GPD <- FALSE
  if (length(inline$names)) {
    for (nm in unique(inline$names)) bundle_call[[nm]] <- NULL
  }

  b <- eval.parent(bundle_call)
  .run_bundle_mcmc(b, mcmc_args = mcmc)
}

#' Fit a causal two-arm Dirichlet process mixture with a spliced GPD tail
#'
#' \code{dpmgpd.causal()} is the highest-level causal fitting wrapper. It builds
#' or accepts a causal bundle, runs posterior sampling for the treated and
#' control arms, and returns a single causal fit ready for prediction and effect
#' estimation.
#'
#' @details
#' The arm-specific predictive distributions
#' \eqn{F_1(y \mid x)} and \eqn{F_0(y \mid x)} inherit
#' the spliced bulk-tail structure. Downstream causal estimands are computed as
#' functionals of these two predictive laws, for example
#' \deqn{\mathrm{QTE}(\tau) = Q_1(\tau) - Q_0(\tau), \qquad
#' \mathrm{ATE} = E(Y_1) - E(Y_0).}
#'
#' @param y Either a response vector or a causal bundle object.
#' @param X Optional design matrix/data.frame.
#' @param treat Binary treatment indicator.
#' @param data Optional data.frame used with \code{formula}.
#' @param mcmc Named list of run arguments passed to \code{mcmc()} (including
#'   optional performance controls such as \code{parallel_arms},
#'   \code{workers}, \code{timing}, and \code{z_update_every}).
#' @param formula Optional formula.
#' @param ... Additional build arguments passed to \code{\link{build_causal_bundle}}.
#' @return A fitted object of class \code{"causalmixgpd_causal_fit"}.
#' @seealso \code{\link{build_causal_bundle}},
#'   \code{\link{bundle}}, \code{\link{dpmix.causal}},
#'   \code{\link{predict.causalmixgpd_causal_fit}}, \code{\link{ate}},
#'   \code{\link{qte}}, \code{\link{cate}}, \code{\link{cqte}}.
#' @examples
#' \donttest{
#' data("causal_pos500_p3_k2", package = "CausalMixGPD")
#' idx <- seq_len(30)
#' fit <- dpmgpd.causal(
#'   y = causal_pos500_p3_k2$y[idx],
#'   X = causal_pos500_p3_k2$X[idx, , drop = FALSE],
#'   treat = causal_pos500_p3_k2$A[idx],
#'   kernel = "gamma",
#'   backend = "sb",
#'   components = 3,
#'   mcmc = list(niter = 60, nburnin = 30, thin = 1, nchains = 1, seed = 1,
#'               show_progress = FALSE, quiet = TRUE)
#' )
#' ate(fit, show_progress = FALSE)
#' }
#' @export
dpmgpd.causal <- function(y = NULL, X = NULL, treat = NULL, data = NULL, mcmc = list(), formula = NULL, ...) {
  treat_expr <- substitute(treat)
  call_match <- match.call(expand.dots = FALSE)
  call_args <- as.list(call_match)

  inline <- .collect_inline_mcmc_from_dots(call_args$..., mcmc = mcmc, eval_env = parent.frame())
  mcmc <- inline$mcmc

  if (.is_bundle(y)) {
    if (!.is_causal_bundle(y)) {
      stop("dpmgpd.causal() requires a causal bundle; use dpmgpd() for one-arm models.", call. = FALSE)
    }
    if (!.bundle_all_gpd(y)) {
      stop(
        "dpmgpd.causal() requires a causal bundle with GPD enabled for all modeled arms; use dpmix.causal(bundle) for non-GPD runs.",
        call. = FALSE
      )
    }
    return(.run_bundle_mcmc(y, mcmc_args = mcmc))
  }

  if (!.treat_arg_supplied(call_args, treat_expr)) {
    stop("dpmgpd.causal() requires 'treat' when building from raw inputs.", call. = FALSE)
  }

  bundle_call <- match.call(expand.dots = TRUE)
  bundle_call[[1L]] <- quote(bundle)
  bundle_call$mcmc <- NULL
  bundle_call$GPD <- TRUE
  if (length(inline$names)) {
    for (nm in unique(inline$names)) bundle_call[[nm]] <- NULL
  }

  b <- eval.parent(bundle_call)
  .run_bundle_mcmc(b, mcmc_args = mcmc)
}
