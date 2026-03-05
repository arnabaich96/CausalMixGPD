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

  if (!is.null(link) || !is.null(priors)) {
    warning("Arguments 'link' and 'priors' are currently placeholders and are not used directly.", call. = FALSE)
  }

  if (components_missing) {
    components <- max(20L, min(50L, length(parsed$y)))
    warning(
      sprintf(
        "No 'components' provided for type='param'; using default components=%d.",
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

#' Cluster-only DPM fit (bulk kernel)
#'
#' Fit a clustering-only model from a formula interface.
#'
#' @param formula Model formula. The response must be present in `data`.
#' @param data Data frame containing response and predictors.
#' @param ... Additional arguments passed to the internal cluster bundle builder.
#' @param type Cluster mode: `"weights"`, `"param"`, or `"both"`.
#' @param default Default mode when `type` is omitted.
#' @param mcmc MCMC control list.
#'
#' @return A `dpmixgpd_cluster_fit` object.
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

#' Cluster-only DPM fit (spliced kernel with GPD tail)
#'
#' Fit a clustering-only GPD-augmented model from a formula interface.
#'
#' @inheritParams dpmix.cluster
#' @return A `dpmixgpd_cluster_fit` object.
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
