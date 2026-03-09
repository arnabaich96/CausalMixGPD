`%||%` <- function(a, b) if (!is.null(a)) a else b

.cluster_or <- function(a, b) if (!is.null(a)) a else b

.cluster_parse_formula <- function(formula, data, na.action = stats::na.omit) {
  if (missing(formula) || is.null(formula)) {
    stop("'formula' is required.", call. = FALSE)
  }
  if (missing(data) || is.null(data)) {
    stop("'data' is required with 'formula'.", call. = FALSE)
  }

  mf <- stats::model.frame(formula, data = data, na.action = na.action)
  y <- stats::model.response(mf)
  if (is.null(y)) stop("Could not extract response from formula.", call. = FALSE)
  y <- as.numeric(y)

  trm <- stats::terms(mf)
  term_labels <- attr(trm, "term.labels") %||% character(0)
  X <- NULL
  ctr <- NULL
  X_cols <- character(0)

  if (length(term_labels) > 0L) {
    X <- stats::model.matrix(trm, data = mf)
    if ("(Intercept)" %in% colnames(X)) {
      X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
    }

    if (ncol(X)) {
      id_idx <- which(tolower(term_labels) == "id")
      assign_idx <- attr(X, "assign") %||% integer(0)
      if (length(id_idx) && length(assign_idx)) {
        keep <- !(assign_idx %in% id_idx)
        X <- X[, keep, drop = FALSE]
      }
    }

    if (!ncol(X)) {
      X <- NULL
    } else {
      storage.mode(X) <- "double"
      X_cols <- colnames(X) %||% character(0)
      ctr <- attr(X, "contrasts")
    }
  }

  resp_name <- tryCatch(as.character(formula[[2L]]), error = function(e) "y")

  list(
    y = y,
    X = X,
    terms = trm,
    xlevels = stats::.getXlevels(trm, mf),
    contrasts = ctr,
    X_cols = X_cols,
    response = resp_name,
    has_X = !is.null(X)
  )
}

.cluster_default_link <- function(kernel_info, param_name) {
  support <- as.character((kernel_info$bulk_support %||% list())[[param_name]] %||% "")
  if (support %in% c("positive_location", "positive_scale", "positive_shape", "positive_sd")) {
    return("exp")
  }
  "identity"
}

.cluster_link_bulk_specs <- function(kernel, beta_sd = 2) {
  kinfo <- get_kernel_registry()[[kernel]]
  if (is.null(kinfo)) stop(sprintf("Kernel '%s' not found.", kernel), call. = FALSE)

  defaults_X <- kinfo$defaults_X %||% list()
  out <- list()
  bulk_params <- kinfo$bulk_params %||% character(0)
  for (nm in bulk_params) {
    dx <- defaults_X[[nm]] %||% list()
    if (!identical(dx$mode %||% NA_character_, "link")) next
    out[[nm]] <- list(
      mode = "link",
      link = dx$link %||% .cluster_default_link(kinfo, nm),
      beta_prior = list(dist = "normal", args = list(mean = 0, sd = beta_sd))
    )
  }
  out
}

.cluster_validate_type <- function(type, default) {
  choices <- c("weights", "param", "both")
  default <- match.arg(default, choices = choices)
  if (missing(type) || is.null(type) || length(type) < 1L) type <- default
  match.arg(type, choices = choices)
}

.cluster_validate_type_requirements <- function(type, has_X, components_missing) {
  if (type %in% c("weights", "both") && !isTRUE(has_X)) {
    stop(sprintf("type='%s' requires covariates in the formula.", type), call. = FALSE)
  }
  if (type %in% c("weights", "both") && isTRUE(components_missing)) {
    stop(sprintf("type='%s' requires an explicit 'components' value.", type), call. = FALSE)
  }
}

.cluster_default_beta_prior <- function(param) {
  if (identical(param, "threshold")) return(list(dist = "normal", args = list(mean = 0, sd = 0.2)))
  if (identical(param, "tail_scale")) return(list(dist = "normal", args = list(mean = 0, sd = 0.5)))
  if (identical(param, "tail_shape")) return(list(dist = "normal", args = list(mean = 0, sd = 0.3)))
  list(dist = "normal", args = list(mean = 0, sd = 2))
}

.cluster_split_overrides <- function(x, bulk_names, gpd_names) {
  x <- x %||% list()
  if (!is.list(x)) stop("Overrides must be supplied as a list.", call. = FALSE)

  out <- list(bulk = list(), gpd = list(), concentration = NULL)
  if (!is.null(x$bulk) || !is.null(x$gpd) || !is.null(x$concentration) || !is.null(x$alpha)) {
    out$bulk <- x$bulk %||% list()
    out$gpd <- x$gpd %||% list()
    out$concentration <- x$concentration %||% x$alpha %||% NULL
    return(out)
  }

  nms <- names(x) %||% character(0)
  if (!length(nms) && length(x)) {
    stop("Unnamed override entries are not supported.", call. = FALSE)
  }
  if (!length(nms)) return(out)

  bad <- setdiff(nms, c(bulk_names, gpd_names, "concentration", "alpha"))
  if (length(bad)) {
    stop(sprintf("Unknown override names: %s", paste(bad, collapse = ", ")), call. = FALSE)
  }
  for (nm in nms) {
    if (nm %in% bulk_names) out$bulk[[nm]] <- x[[nm]]
    if (nm %in% gpd_names) out$gpd[[nm]] <- x[[nm]]
  }
  out$concentration <- x$concentration %||% x$alpha %||% NULL
  out
}

.cluster_normalize_link_entry <- function(entry) {
  if (is.character(entry) && length(entry) == 1L) {
    return(list(mode = "link", link = entry))
  }
  if (!is.list(entry)) {
    stop("Link overrides must be strings or lists.", call. = FALSE)
  }
  mode <- entry$mode %||% "link"
  if (!identical(mode, "link")) {
    stop("Link overrides must have mode='link'.", call. = FALSE)
  }
  out <- utils::modifyList(list(mode = "link"), entry)
  out
}

.cluster_apply_link_overrides <- function(spec, link, has_X) {
  if (is.null(link)) return(spec)
  if (!isTRUE(has_X)) {
    stop("`link` overrides require covariates in the formula.", call. = FALSE)
  }

  bulk_names <- names(spec$plan$bulk %||% list())
  gpd_names <- names(spec$plan$gpd %||% list())
  parsed <- .cluster_split_overrides(link, bulk_names = bulk_names, gpd_names = gpd_names)

  for (nm in names(parsed$bulk)) {
    ent <- .cluster_normalize_link_entry(parsed$bulk[[nm]])
    cur <- spec$plan$bulk[[nm]] %||% list()
    ent$beta_prior <- ent$beta_prior %||% cur$beta_prior %||% .cluster_default_beta_prior(nm)
    spec$plan$bulk[[nm]] <- utils::modifyList(cur, ent)
  }

  for (nm in names(parsed$gpd)) {
    ent <- .cluster_normalize_link_entry(parsed$gpd[[nm]])
    cur <- spec$plan$gpd[[nm]] %||% list()
    ent$beta_prior <- ent$beta_prior %||% cur$beta_prior %||% .cluster_default_beta_prior(nm)
    spec$plan$gpd[[nm]] <- utils::modifyList(cur, ent)
  }

  spec
}

.cluster_normalize_prior_entry <- function(entry, current_mode, param_name = NULL) {
  is_link_mode <- identical(current_mode, "link")

  if (is.numeric(entry) && length(entry) == 1L) {
    return(list(mode = "fixed", value = as.numeric(entry)))
  }
  if (is.character(entry) && length(entry) == 1L) {
    if (is_link_mode) {
      return(list(mode = "link", beta_prior = list(dist = entry, args = list())))
    }
    return(list(mode = "dist", dist = entry, args = list()))
  }
  if (!is.list(entry)) {
    stop("Prior overrides must be numeric, character, or list entries.", call. = FALSE)
  }

  if (!is.null(entry$mode)) {
    return(entry)
  }
  if (!is.null(entry$value)) {
    return(list(mode = "fixed", value = entry$value))
  }
  if (!is.null(entry$beta_prior)) {
    return(list(mode = "link", beta_prior = entry$beta_prior))
  }
  if (!is.null(entry$dist) || !is.null(entry$args)) {
    if (is_link_mode) {
      return(list(
        mode = "link",
        beta_prior = list(
          dist = entry$dist %||% "normal",
          args = entry$args %||% .cluster_default_beta_prior(param_name)$args
        )
      ))
    }
    return(list(
      mode = "dist",
      dist = entry$dist %||% "normal",
      args = entry$args %||% list(mean = 0, sd = 2)
    ))
  }

  stop("Could not interpret prior override entry.", call. = FALSE)
}

.cluster_apply_prior_overrides <- function(spec, priors) {
  if (is.null(priors)) return(spec)

  bulk_names <- names(spec$plan$bulk %||% list())
  gpd_names <- names(spec$plan$gpd %||% list())
  parsed <- .cluster_split_overrides(priors, bulk_names = bulk_names, gpd_names = gpd_names)

  for (nm in names(parsed$bulk)) {
    cur <- spec$plan$bulk[[nm]] %||% list()
    ent <- .cluster_normalize_prior_entry(parsed$bulk[[nm]], current_mode = cur$mode %||% NULL, param_name = nm)
    spec$plan$bulk[[nm]] <- utils::modifyList(cur, ent)
  }

  for (nm in names(parsed$gpd)) {
    cur <- spec$plan$gpd[[nm]] %||% list()
    ent <- .cluster_normalize_prior_entry(parsed$gpd[[nm]], current_mode = cur$mode %||% NULL, param_name = nm)
    spec$plan$gpd[[nm]] <- utils::modifyList(cur, ent)
  }

  if (!is.null(parsed$concentration)) {
    cc <- parsed$concentration
    if (is.numeric(cc) && length(cc) == 1L) {
      spec$plan$concentration <- list(mode = "fixed", value = as.numeric(cc))
    } else if (is.list(cc) && !is.null(cc$mode)) {
      spec$plan$concentration <- cc
    } else if (is.list(cc)) {
      if (!is.null(cc$value)) {
        spec$plan$concentration <- list(mode = "fixed", value = cc$value)
      } else {
        spec$plan$concentration <- list(
          mode = "dist",
          dist = cc$dist %||% "gamma",
          args = cc$args %||% list(shape = 1, rate = 1)
        )
      }
    } else if (is.character(cc) && length(cc) == 1L) {
      spec$plan$concentration <- list(mode = "dist", dist = cc, args = list())
    } else {
      stop("Invalid concentration prior override.", call. = FALSE)
    }
  }

  spec
}

codegen_cluster_model <- function(spec) {
  stopifnot(is.list(spec), !is.null(spec$meta))
  spec$cluster <- spec$cluster %||% list()
  type <- spec$cluster$type %||% "weights"
  spec$cluster$gating <- isTRUE(spec$cluster$gating %||% (type %in% c("weights", "both")))
  spec$cluster$param_link <- isTRUE(spec$cluster$param_link %||% (type %in% c("param", "both")))
  backend_target <- if (identical(type, "param")) "crp" else "sb"
  spec$meta$backend <- backend_target
  spec$plan$backend <- backend_target
  build_code_from_spec(spec)
}

build_cluster_bundle <- function(formula,
                                 data,
                                 kernel,
                                 GPD,
                                 type = c("weights", "param", "both"),
                                 default = "weights",
                                 link = NULL,
                                 priors = NULL,
                                 components = NULL,
                                 mcmc = list(niter = 2000, nburnin = 500, thin = 1, nchains = 1, seed = 1),
                                 param_specs = NULL,
                                 epsilon = 0.025,
                                 alpha_random = TRUE,
                                 monitor = c("core", "full"),
                                 monitor_v = FALSE,
                                 monitor_latent = TRUE,
                                 ...) {
  type <- .cluster_validate_type(type, default)
  parsed <- .cluster_parse_formula(formula = formula, data = data)
  components_missing <- missing(components) || is.null(components)
  .cluster_validate_type_requirements(
    type = type,
    has_X = parsed$has_X,
    components_missing = components_missing
  )

  if (components_missing) {
    components <- max(20L, min(50L, length(parsed$y)))
    warning(
      sprintf(
        "No 'components' provided for type='%s'; using default components=%d.",
        type,
        components
      ),
      call. = FALSE
    )
  }
  components <- as.integer(components)
  if (!is.finite(components) || components < 2L) {
    stop("'components' must be an integer >= 2.", call. = FALSE)
  }

  backend <- if (identical(type, "param")) "crp" else "sb"
  monitor <- match.arg(monitor)
  if (identical(monitor, "full")) {
    monitor_latent <- TRUE
    if (identical(backend, "sb")) monitor_v <- TRUE
  }
  monitor_latent <- TRUE

  user_param_specs <- param_specs %||% list()
  cluster_gating <- type %in% c("weights", "both")
  cluster_param_link <- (type %in% c("param", "both")) && isTRUE(parsed$has_X)
  if (cluster_param_link) {
    linked_bulk <- .cluster_link_bulk_specs(kernel = kernel)
    user_param_specs$bulk <- linked_bulk
  }

  spec <- compile_model_spec(
    y = parsed$y,
    X = parsed$X,
    backend = backend,
    kernel = kernel,
    GPD = isTRUE(GPD),
    components = components,
    param_specs = user_param_specs,
    alpha_random = alpha_random
  )
  spec <- .cluster_apply_link_overrides(spec, link = link, has_X = parsed$has_X)
  spec <- .cluster_apply_prior_overrides(spec, priors = priors)

  spec$cluster <- list(
    type = type,
    default = default,
    gating = cluster_gating,
    param_link = cluster_param_link,
    formula = formula,
    formula_meta = list(
      terms = parsed$terms,
      xlevels = parsed$xlevels,
      contrasts = parsed$contrasts,
      X_cols = parsed$X_cols,
      response = parsed$response
    )
  )

  code <- .wrap_nimble_code(codegen_cluster_model(spec))
  constants <- build_constants_from_spec(spec)
  dimensions <- build_dimensions_from_spec(spec)
  data_list <- build_data_from_inputs(y = parsed$y, X = parsed$X)
  inits <- build_inits_from_spec(spec, y = parsed$y)
  monitors <- build_monitors_from_spec(
    spec,
    monitor_v = isTRUE(monitor_v),
    monitor_latent = TRUE
  )

  out <- list(
    spec = spec,
    code = code,
    constants = constants,
    dimensions = dimensions,
    data = data_list,
    inits = inits,
    monitors = monitors,
    monitor_policy = list(
      monitor = monitor,
      monitor_latent = TRUE,
      monitor_v = isTRUE(monitor_v)
    ),
    mcmc = mcmc,
    epsilon = as.numeric(epsilon)[1],
    cluster = list(type = type),
    call = match.call()
  )
  class(out) <- c("dpmixgpd_cluster_bundle", "causalmixgpd_bundle", "list")
  out
}

#' Fit a clustering-only bulk model
#'
#' Build and fit a Dirichlet-process mixture for clustering without causal estimands or posterior
#' prediction for a response surface. This interface focuses on latent partition recovery from a
#' formula specification and returns a cluster-fit object that can be summarized, plotted, or
#' converted into labels and posterior similarity matrices with [predict.dpmixgpd_cluster_fit()].
#'
#' @param formula Model formula. The response must be present in `data`.
#' @param data Data frame containing the response and optional predictors.
#' @param ... Additional arguments passed to `build_cluster_bundle()`, including kernel settings,
#'   prior overrides, component counts, and monitoring controls.
#' @param type Clustering mode. `"weights"` links mixture weights to predictors, `"param"` links
#'   kernel parameters to predictors, and `"both"` does both.
#' @param default Default mode used when `type` is omitted.
#' @param mcmc MCMC control list passed into the cluster bundle.
#'
#' @return Object of class `dpmixgpd_cluster_fit`.
#'
#' @details
#' The fitted model targets a latent partition \eqn{z_1, \dots, z_n} with component-specific kernel
#' parameters. Depending on `type`, predictors can enter through the gating probabilities
#' \deqn{
#' \Pr(z_i = k \mid x_i) = \pi_k(x_i)
#' }
#' or through linked kernel parameters for each component. The returned fit stores posterior draws
#' of the latent allocations and associated parameters; the representative clustering is extracted
#' later by [predict.dpmixgpd_cluster_fit()] using Dahl's least-squares rule.
#'
#' Use `type = "weights"` or `type = "both"` only when the formula includes predictors and when an
#' explicit number of `components` is supplied. Otherwise the builder stops before fitting.
#'
#' @seealso [dpmgpd.cluster()], [predict.dpmixgpd_cluster_fit()],
#'   [summary.dpmixgpd_cluster_fit()], [plot.dpmixgpd_cluster_fit()],
#'   [build_nimble_bundle()], [dpmix()].
#' @family cluster workflow
#' @export
dpmix.cluster <- function(formula,
                          data,
                          ...,
                          type = c("weights", "param", "both"),
                          default = "weights",
                          mcmc = list()) {
  type <- .cluster_validate_type(type, default)
  bundle <- do.call(
    build_cluster_bundle,
    c(
      list(
        formula = formula,
        data = data,
        GPD = FALSE,
        type = type,
        default = default,
        mcmc = mcmc
      ),
      list(...)
    )
  )
  fit <- run_cluster_mcmc(bundle)
  fit$call <- match.call()
  fit
}

#' Fit a clustering-only bulk-tail model
#'
#' Variant of [dpmix.cluster()] that augments the cluster kernel with a generalized Pareto tail.
#' This is the clustering analogue of the spliced bulk-tail workflow used by [dpmgpd()].
#'
#' @inheritParams dpmix.cluster
#'
#' @return Object of class `dpmixgpd_cluster_fit`.
#'
#' @details
#' For observations above a component-specific threshold, the component density is spliced as
#' \deqn{
#' f(y) = (1 - F_{bulk}(u)) g_{GPD}(y \mid u, \sigma_u, \xi_u), \qquad y \ge u,
#' }
#' so cluster assignment can be informed by both central behavior and tail behavior.
#'
#' This interface is preferable when cluster separation is driven by upper-tail differences rather
#' than bulk-only shape or location differences.
#'
#' @seealso [dpmix.cluster()], [predict.dpmixgpd_cluster_fit()], [dpmgpd()],
#'   [sim_bulk_tail()].
#' @family cluster workflow
#' @export
dpmgpd.cluster <- function(formula,
                           data,
                           ...,
                           type = c("weights", "param", "both"),
                           default = "weights",
                           mcmc = list()) {
  type <- .cluster_validate_type(type, default)
  bundle <- do.call(
    build_cluster_bundle,
    c(
      list(
        formula = formula,
        data = data,
        GPD = TRUE,
        type = type,
        default = default,
        mcmc = mcmc
      ),
      list(...)
    )
  )
  fit <- run_cluster_mcmc(bundle)
  fit$call <- match.call()
  fit
}
