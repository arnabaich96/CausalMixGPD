
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
    design <- .formula_design_matrix(
      trm = trm,
      mf = mf,
      drop_intercept = TRUE,
      drop_terms = "id",
      coerce_double = TRUE
    )
    X <- design$X

    if (is.null(X)) {
      X <- NULL
    } else {
      X_cols <- design$X_cols
      ctr <- design$contrasts
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

  nms <- names(x) %||% character(0)
  if (!length(nms) && length(x)) {
    stop("Unnamed override entries are not supported.", call. = FALSE)
  }
  if (!length(nms)) return(out)

  bad <- setdiff(nms, c("bulk", "gpd", "concentration", "alpha", bulk_names, gpd_names))
  if (length(bad)) {
    stop(sprintf("Unknown override names: %s", paste(bad, collapse = ", ")), call. = FALSE)
  }

  if ("bulk" %in% nms) {
    out$bulk <- x$bulk %||% list()
  }
  if ("gpd" %in% nms) {
    out$gpd <- x$gpd %||% list()
  }

  for (nm in intersect(nms, bulk_names)) {
    out$bulk[[nm]] <- x[[nm]]
  }
  for (nm in intersect(nms, gpd_names)) {
    out$gpd[[nm]] <- x[[nm]]
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
                                 components = 10L,
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
    components <- 10L
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
  inits <- build_inits_from_spec(spec, y = parsed$y, X = parsed$X)
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
#' @param type Clustering mode:
#'   \itemize{
#'     \item \code{"weights"}: links mixture weights to predictors
#'     \item \code{"param"}: links kernel parameters to predictors
#'     \item \code{"both"}: links both weights and kernel parameters to predictors
#'   }
#' @param default Default mode used when `type` is omitted.
#' @param mcmc MCMC control list passed into the cluster bundle.
#' @param ... Additional arguments passed to `build_cluster_bundle()`, including kernel settings,
#'   prior overrides, component counts, and monitoring controls.
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
#' of the latent cluster labels and associated parameters; the representative clustering is extracted
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
                          type = c("weights", "param", "both"),
                          default = "weights",
                          mcmc = list(),
                          ...) {
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
#' @seealso [dpmix.cluster()], [predict.dpmixgpd_cluster_fit()],
#'   [dpmgpd()], [sim_bulk_tail()].
#' @family cluster workflow
#' @export
dpmgpd.cluster <- function(formula,
                           data,
                           type = c("weights", "param", "both"),
                           default = "weights",
                           mcmc = list(),
                           ...) {
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






#' Print a cluster bundle
#'
#' Compact display for a `dpmixgpd_cluster_bundle` before MCMC is run.
#'
#' @details
#' A cluster bundle is the pre-sampling representation of the latent partition
#' model. It stores the formula-derived design, kernel choice, truncation level,
#' and the rule by which predictors enter the clustering model, but it does not
#' yet contain posterior draws of the latent labels \eqn{z_1, \dots, z_n}.
#'
#' `print()` is intentionally brief. It is meant to confirm that the bundle
#' matches the requested clustering structure before you run MCMC with
#' `run_cluster_mcmc()` or the higher-level wrappers [dpmix.cluster()] and
#' [dpmgpd.cluster()].
#'
#' @param x A cluster bundle.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#'
#' @seealso [dpmix.cluster()], [dpmgpd.cluster()], [summary.dpmixgpd_cluster_bundle()].
#' @family cluster workflow
#' @export
print.dpmixgpd_cluster_bundle <- function(x, ...) {
  stopifnot(inherits(x, "dpmixgpd_cluster_bundle"))
  meta <- x$spec$meta %||% list()
  cl <- x$spec$cluster %||% list()
  cat("Cluster bundle\n")
  cat("type      :", cl$type %||% "weights", "\n")
  cat("link mode :", if (isTRUE(cl$param_link)) "param|x" else "none", "\n")
  cat("kernel    :", meta$kernel %||% "?", "\n")
  cat("GPD       :", isTRUE(meta$GPD), "\n")
  cat("n         :", as.integer(meta$N %||% NA_integer_), "\n")
  cat("components:", as.integer(meta$components %||% NA_integer_), "\n")
  invisible(x)
}

#' Summarize a cluster bundle
#'
#' Report the modeling choices encoded in a cluster bundle before fitting.
#'
#' @details
#' This summary is a pre-flight check for the clustering workflow. It reports
#' the latent-partition design, the chosen kernel family, whether a GPD tail
#' will be spliced above the threshold, the effective sample and predictor
#' dimensions, and the monitor set that will be carried into MCMC.
#'
#' Because no posterior simulation has occurred yet, the summary describes only
#' the assumed model structure. Quantities such as representative labels,
#' pairwise co-clustering probabilities, and cluster-specific summaries become
#' available only after the fitted object has been created and post-processed.
#'
#' @param object A cluster bundle.
#' @param ... Unused.
#'
#' @return Summary list containing kernel choice, GPD flag, dimensions, component count, and
#'   monitor configuration.
#'
#' @seealso [print.dpmixgpd_cluster_bundle()], [plot.dpmixgpd_cluster_bundle()],
#'   [predict.dpmixgpd_cluster_fit()].
#' @family cluster workflow
#' @export
summary.dpmixgpd_cluster_bundle <- function(object, ...) {
  stopifnot(inherits(object, "dpmixgpd_cluster_bundle"))
  meta <- object$spec$meta %||% list()
  cl <- object$spec$cluster %||% list()
  out <- list(
    type = cl$type %||% "weights",
    link_mode = if (isTRUE(cl$param_link)) "param|x" else "none",
    kernel = meta$kernel %||% NA_character_,
    GPD = isTRUE(meta$GPD),
    N = as.integer(meta$N %||% NA_integer_),
    P = as.integer(meta$P %||% 0L),
    components = as.integer(meta$components %||% NA_integer_),
    monitors = object$monitors %||% character(0)
  )
  class(out) <- c("summary.dpmixgpd_cluster_bundle", "list")
  out
}

#' Plot a cluster bundle
#'
#' Produce a compact graphical summary of the cluster bundle metadata.
#'
#' @details
#' The bundle plot is a metadata display rather than an inferential graphic. It
#' mirrors the structural fields reported by `print()` and `summary()` in a
#' single panel so the pre-MCMC clustering specification can be reviewed in a
#' figure-oriented workflow or notebook.
#'
#' Because the object has not been sampled yet, no representative partition or
#' posterior uncertainty is shown here. Use [plot.dpmixgpd_cluster_fit()],
#' [plot.dpmixgpd_cluster_labels()], or [plot.dpmixgpd_cluster_psm()] after
#' fitting when you need substantive clustering output.
#'
#' @param x A cluster bundle.
#' @param plotly Logical; if `TRUE`, convert the `ggplot2` output to a `plotly` /
#'   `htmlwidget` representation via `.wrap_plotly()`. Defaults to
#'   `getOption("CausalMixGPD.plotly", FALSE)`.
#' @param ... Unused.
#'
#' @return A `ggplot2` object or a `plotly`/`htmlwidget` object when `plotly = TRUE`.
#'
#' @seealso [summary.dpmixgpd_cluster_bundle()], [dpmix.cluster()], [dpmgpd.cluster()].
#' @family cluster workflow
#' @export
plot.dpmixgpd_cluster_bundle <- function(x,
                                         plotly = getOption("CausalMixGPD.plotly", FALSE),
                                         ...) {
  stopifnot(inherits(x, "dpmixgpd_cluster_bundle"))
  sm <- summary(x)
  txt <- c(
    sprintf("Type: %s", sm$type),
    sprintf("Link mode: %s", sm$link_mode),
    sprintf("Kernel: %s", sm$kernel),
    sprintf("GPD: %s", sm$GPD),
    sprintf("N: %d  P: %d", sm$N, sm$P),
    sprintf("Components: %d", sm$components)
  )
  .cluster_require_ggplot()
  p <- ggplot2::ggplot(
    data.frame(x = 0, y = 1, label = paste(txt, collapse = "\n"), stringsAsFactors = FALSE),
    ggplot2::aes(x = x, y = y, label = label)
  ) +
    ggplot2::geom_text(hjust = 0, vjust = 1, family = "mono") +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    ggplot2::labs(title = "Cluster Bundle") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0),
      plot.margin = ggplot2::margin(15, 15, 15, 15)
    )
  .cluster_maybe_wrap_plotly(p, plotly = plotly)
}

#' Print a cluster fit
#'
#' Compact display for a fitted clustering object.
#'
#' @details
#' A fitted cluster object contains posterior draws for the latent labels and
#' associated component parameters. The printed header identifies the model
#' family that generated those draws, including whether the fit used a bulk-only
#' kernel or a spliced bulk-tail specification.
#'
#' The printed `components` value is the truncation size used by the sampler. It
#' is not the same thing as the number of occupied clusters in the Dahl
#' representative partition, which is computed later by
#' [predict.dpmixgpd_cluster_fit()] and summarized by
#' [summary.dpmixgpd_cluster_fit()].
#'
#' @param x A cluster fit.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#'
#' @seealso [summary.dpmixgpd_cluster_fit()], [predict.dpmixgpd_cluster_fit()],
#'   [plot.dpmixgpd_cluster_fit()].
#' @family cluster workflow
#' @export
print.dpmixgpd_cluster_fit <- function(x, ...) {
  stopifnot(inherits(x, "dpmixgpd_cluster_fit"))
  meta <- x$spec$meta %||% list()
  cl <- x$spec$cluster %||% list()
  cat("Cluster fit\n")
  cat("type      :", cl$type %||% "weights", "\n")
  cat("link mode :", if (isTRUE(cl$param_link)) "param|x" else "none", "\n")
  cat("kernel    :", meta$kernel %||% "?", "\n")
  cat("GPD       :", isTRUE(meta$GPD), "\n")
  cat("n         :", as.integer(meta$N %||% NA_integer_), "\n")
  cat("components:", as.integer(meta$components %||% NA_integer_), "\n")
  invisible(x)
}

#' Summarize a cluster fit
#'
#' Summarize the posterior clustering induced by the Dahl representative partition.
#'
#' @param object A cluster fit.
#' @param burnin Number of initial posterior draws to discard.
#' @param thin Keep every `thin`-th posterior draw.
#' @param top_n Number of populated clusters to profile when descriptive summaries are available.
#' @param order_by Ordering rule for descriptive cluster profiles:
#'   \itemize{
#'     \item \code{"size"}: decreasing cluster size
#'     \item \code{"label"}: ascending cluster label
#'   }
#' @param vars Optional character vector of numeric columns to summarize within each cluster.
#' @param ... Unused.
#'
#' @return Summary list with the number of retained clusters, cluster sizes, optional
#'   cluster-level descriptive summaries, and the burn-in/thinning settings used to construct the
#'   summary.
#'
#' @details
#' This summary is based on [predict.dpmixgpd_cluster_fit()] with `type = "label"`. The reported
#' cluster count \eqn{K^*} is the number of unique labels in the representative partition rather
#' than the number of components available in the truncated sampler.
#'
#' @seealso [predict.dpmixgpd_cluster_fit()], [plot.dpmixgpd_cluster_fit()],
#'   [summary.dpmixgpd_cluster_labels()].
#' @family cluster workflow
#' @export
summary.dpmixgpd_cluster_fit <- function(object,
                                        burnin = NULL,
                                        thin = NULL,
                                        top_n = 5L,
                                        order_by = c("size", "label"),
                                        vars = NULL,
                                        ...) {
  stopifnot(inherits(object, "dpmixgpd_cluster_fit"))
  lbl <- predict(object, type = "label", burnin = burnin, thin = thin, return_scores = TRUE)
  tab <- .cluster_size_table(lbl$labels, order_by = "size")
  lbl_sum <- summary(lbl, top_n = top_n, order_by = order_by, vars = vars)
  cluster_profiles <- lbl_sum$cluster_profiles
  if (is.data.frame(cluster_profiles) && nrow(cluster_profiles)) {
    num_cols <- vapply(cluster_profiles, is.numeric, logical(1))
    cluster_profiles[num_cols] <- lapply(cluster_profiles[num_cols], round, digits = 3L)
  }
  out <- list(
    K_star = length(tab),
    cluster_sizes = tab,
    cluster_profiles = cluster_profiles,
    certainty = lbl_sum$certainty,
    source = lbl$source,
    burnin = lbl$burnin,
    thin = lbl$thin
  )
  class(out) <- c("summary.dpmixgpd_cluster_fit", "list")
  out
}

#' Plot a cluster fit
#'
#' Visualize either the posterior similarity matrix, the posterior number of occupied clusters, the
#' size distribution of the representative clusters, or cluster-specific response summaries.
#'
#' @details
#' This plot method exposes the main posterior diagnostics for clustering. The
#' `which = "k"` view tracks the number of occupied clusters across retained
#' draws, `which = "psm"` visualizes pairwise co-clustering probabilities,
#' `which = "sizes"` displays the size profile of the representative partition,
#' and `which = "summary"` shows response summaries conditional on the selected
#' representative labels.
#'
#' The representative partition is obtained from
#' [predict.dpmixgpd_cluster_fit()] using Dahl's least-squares rule. As a
#' result, the `sizes` and `summary` views describe that representative
#' clustering rather than the full posterior distribution over partitions.
#'
#' @param x A cluster fit.
#' @param which Plot type:
#'   \itemize{
#'     \item \code{"psm"}: posterior similarity matrix heatmap
#'     \item \code{"k"}: posterior number of occupied clusters
#'     \item \code{"sizes"}: bar chart of representative cluster sizes
#'     \item \code{"summary"}: cluster-specific response summaries
#'   }
#' @param burnin Number of initial posterior draws to discard.
#' @param thin Keep every `thin`-th posterior draw.
#' @param psm_max_n Maximum training sample size allowed for PSM plotting.
#' @param top_n Number of populated representative clusters to display for
#'   \code{which = "sizes"} or \code{which = "summary"}. Use \code{NULL} to
#'   display all populated clusters.
#' @param order_by Ordering rule for cluster displays:
#'   \itemize{
#'     \item \code{"size"}: decreasing cluster size
#'     \item \code{"label"}: ascending cluster label
#'   }
#' @param plotly Logical; if `TRUE`, convert the `ggplot2` output to a `plotly` /
#'   `htmlwidget` representation via `.wrap_plotly()`. Defaults to
#'   `getOption("CausalMixGPD.plotly", FALSE)`.
#' @param ... Unused.
#'
#' @return A `ggplot2` object or a `plotly`/`htmlwidget` object when `plotly = TRUE`.
#'
#' @seealso [predict.dpmixgpd_cluster_fit()], [summary.dpmixgpd_cluster_fit()],
#'   [plot.dpmixgpd_cluster_psm()], [plot.dpmixgpd_cluster_labels()].
#' @family cluster workflow
#' @export
plot.dpmixgpd_cluster_fit <- function(x,
                                      which = c("psm", "k", "sizes", "summary"),
                                      burnin = NULL,
                                      thin = NULL,
                                      psm_max_n = 2000L,
                                      top_n = 5L,
                                      order_by = c("size", "label"),
                                      plotly = getOption("CausalMixGPD.plotly", FALSE),
                                      ...) {
  stopifnot(inherits(x, "dpmixgpd_cluster_fit"))
  which <- match.arg(which)
  .cluster_require_ggplot()

  if (identical(which, "psm")) {
    psm <- predict(x, type = "psm", burnin = burnin, thin = thin, psm_max_n = psm_max_n)
    return(plot(psm, psm_max_n = psm_max_n, plotly = plotly, ...))
  }

  if (identical(which, "k")) {
    z <- extract_z_draws(x$samples, burnin = burnin, thin = thin)
    k_draw <- apply(z, 1, function(v) length(unique(as.integer(v))))
    df <- data.frame(draw = seq_along(k_draw), K = as.numeric(k_draw))
    pal <- .plot_palette(2L)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = draw, y = K)) +
      ggplot2::geom_line(color = pal[1], linewidth = 0.8) +
      ggplot2::geom_point(color = pal[1], size = 1.8) +
      .plot_theme() +
      ggplot2::labs(x = "Draw", y = "K", title = "Clusters per draw")
    return(.cluster_maybe_wrap_plotly(p, plotly = plotly))
  }

  lbl <- predict(x, type = "label", burnin = burnin, thin = thin)
  if (identical(which, "summary")) {
    return(plot(
      lbl,
      type = "summary",
      top_n = top_n,
      order_by = order_by,
      plotly = plotly,
      ...
    ))
  }

  plot(
    lbl,
    type = "sizes",
    top_n = top_n,
    order_by = order_by,
    plotly = plotly,
    title = "Dahl cluster sizes",
    ...
  )
}

#' Print cluster labels
#'
#' Compact display for a representative clustering.
#'
#' @details
#' A `dpmixgpd_cluster_labels` object represents one partition, usually the Dahl
#' representative partition for the training data or the induced allocation of
#' `newdata` to those representative clusters. The printed output therefore
#' describes the selected labels and their sizes, not the full posterior
#' uncertainty over alternative partitions.
#'
#' When the object comes from `predict(..., return_scores = TRUE)`, richer
#' assignment information is carried alongside the labels and can be inspected
#' with [summary.dpmixgpd_cluster_labels()] or
#' [plot.dpmixgpd_cluster_labels()].
#'
#' @param x Cluster labels object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#'
#' @seealso [predict.dpmixgpd_cluster_fit()], [summary.dpmixgpd_cluster_labels()],
#'   [plot.dpmixgpd_cluster_labels()].
#' @family cluster workflow
#' @export
print.dpmixgpd_cluster_labels <- function(x, ...) {
  stopifnot(inherits(x, "dpmixgpd_cluster_labels"))
  tab <- .cluster_size_table(x$labels, order_by = "size")
  cat("Cluster labels (", x$source %||% "train", ")\n", sep = "")
  cat("n         :", length(x$labels), "\n")
  cat("components:", x$components %||% length(tab), "\n")
  cat("sizes     :", paste(sprintf("%s:%s", names(tab), as.integer(tab)), collapse = ", "), "\n")
  invisible(x)
}

#' Summarize cluster labels
#'
#' Summarize a representative clustering for training data or new observations.
#'
#' @param object Cluster labels object.
#' @param top_n Number of populated clusters to profile when attached data are available.
#' @param order_by Ordering rule for descriptive cluster profiles:
#'   \itemize{
#'     \item \code{"size"}: decreasing cluster size
#'     \item \code{"label"}: ascending cluster label
#'   }
#' @param vars Optional character vector of numeric columns to summarize within each cluster.
#' @param ... Unused.
#'
#' @return Summary list containing cluster sizes, optional cluster-level descriptive summaries, and,
#'   when available, assignment-certainty summaries.
#'
#' @details
#' If score or probability matrices are attached, certainty is summarized by the rowwise maxima
#' \eqn{\max_k p_{ik}}, which quantify how strongly each observation is assigned to its selected
#' cluster. When the labels object also carries attached training or prediction data, the summary
#' includes descriptive mean/sd profiles for the first populated clusters.
#'
#' @seealso [predict.dpmixgpd_cluster_fit()], [plot.dpmixgpd_cluster_labels()],
#'   [summary.dpmixgpd_cluster_fit()].
#' @family cluster workflow
#' @export
summary.dpmixgpd_cluster_labels <- function(object,
                                            top_n = 5L,
                                            order_by = c("size", "label"),
                                            vars = NULL,
                                            ...) {
  stopifnot(inherits(object, "dpmixgpd_cluster_labels"))
  tab <- .cluster_size_table(object$labels, order_by = "size")
  score_mat <- object$scores %||% object$probs
  max_prob <- if (is.matrix(score_mat)) apply(score_mat, 1, max) else NA_real_
  out <- list(
    source = object$source %||% "train",
    n = length(object$labels),
    components = object$components %||% length(tab),
    cluster_sizes = tab,
    cluster_profiles = .cluster_profile_table(
      data = object$data %||% NULL,
      labels = object$labels,
      score_mat = score_mat,
      top_n = top_n,
      order_by = order_by,
      vars = vars
    ),
    certainty = if (all(is.na(max_prob))) NULL else summary(max_prob)
  )
  class(out) <- c("summary.dpmixgpd_cluster_labels", "list")
  out
}

#' Plot cluster labels
#'
#' Visualize representative cluster sizes, assignment certainty, or cluster-specific response
#' summaries. For `type = "summary"`, the response view is shown as boxplots ordered by
#' cluster size or label. When `x` comes from `predict(..., newdata = ...)`, only clusters
#' represented in the new sample are displayed.
#'
#' @details
#' This method visualizes the representative partition stored in a
#' `dpmixgpd_cluster_labels` object. The `sizes` view emphasizes the empirical
#' distribution of the selected clusters, the `certainty` view summarizes the
#' assignment scores \eqn{\max_k p_{ik}}, and the `summary` view compares the
#' attached response data across representative clusters.
#'
#' For new-data prediction, the plots are always interpreted relative to the
#' representative training clusters. That is why only clusters observed in the
#' predicted sample are shown even though the training partition may contain
#' additional occupied groups.
#'
#' @param x Cluster labels object.
#' @param type Plot type:
#'   \itemize{
#'     \item \code{"sizes"}: bar chart of representative cluster sizes
#'     \item \code{"certainty"}: assignment certainty distribution
#'     \item \code{"summary"}: cluster-specific response boxplots
#'   }
#' @param top_n Number of populated representative clusters to display for
#'   \code{type = "sizes"} or \code{type = "summary"}. Use \code{NULL} to
#'   display all populated clusters.
#' @param order_by Ordering rule for cluster displays:
#'   \itemize{
#'     \item \code{"size"}: decreasing cluster size
#'     \item \code{"label"}: ascending cluster label
#'   }
#' @param plotly Logical; if `TRUE`, convert the `ggplot2` output to a `plotly` /
#'   `htmlwidget` representation via `.wrap_plotly()`. Defaults to
#'   `getOption("CausalMixGPD.plotly", FALSE)`.
#' @param ... Unused.
#'
#' @return A `ggplot2` object or a `plotly`/`htmlwidget` object when `plotly = TRUE`.
#'
#' @seealso [summary.dpmixgpd_cluster_labels()], [predict.dpmixgpd_cluster_fit()].
#' @family cluster workflow
#' @export
plot.dpmixgpd_cluster_labels <- function(x,
                                         type = c("sizes", "certainty", "summary"),
                                         top_n = 5L,
                                         order_by = c("size", "label"),
                                         plotly = getOption("CausalMixGPD.plotly", FALSE),
                                         ...) {
  stopifnot(inherits(x, "dpmixgpd_cluster_labels"))
  type <- match.arg(type)
  .cluster_require_ggplot()
  if (identical(type, "sizes")) {
    title <- list(...)$title %||% "Cluster sizes"
    ref_labels <- x$train_reference$labels %||% x$labels
    return(.cluster_plot_sizes(
      x$labels,
      title = title,
      top_n = top_n,
      order_by = order_by,
      reference_levels = .cluster_order_levels(ref_labels, order_by = order_by),
      plotly = plotly
    ))
  }
  if (identical(type, "summary")) {
    return(.cluster_plot_summary_labels(
      x,
      top_n = top_n,
      order_by = order_by,
      plotly = plotly
    ))
  }
  score_mat <- x$scores %||% x$probs
  if (!is.matrix(score_mat)) {
    warning("No score matrix available for certainty plot.", call. = FALSE)
    return(invisible(x))
  }
  max_prob <- apply(score_mat, 1, max)
  .cluster_plot_certainty(max_prob, plotly = plotly)
}

#' Print a cluster posterior similarity matrix
#'
#' Compact display for a posterior similarity matrix.
#'
#' @details
#' A posterior similarity matrix records pairwise co-clustering probabilities on
#' the training sample. Its \eqn{(i,j)} entry is the posterior probability that
#' observations \eqn{i} and \eqn{j} share the same latent cluster across the
#' retained MCMC draws.
#'
#' The printed header reports only matrix size and bookkeeping information. Use
#' [summary.dpmixgpd_cluster_psm()] for numerical summaries and
#' [plot.dpmixgpd_cluster_psm()] for a visual heatmap.
#'
#' @param x Cluster PSM object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#'
#' @seealso [predict.dpmixgpd_cluster_fit()], [summary.dpmixgpd_cluster_psm()],
#'   [plot.dpmixgpd_cluster_psm()].
#' @family cluster workflow
#' @export
print.dpmixgpd_cluster_psm <- function(x, ...) {
  stopifnot(inherits(x, "dpmixgpd_cluster_psm"))
  n <- nrow(x$psm %||% matrix(0, 0, 0))
  cat("Cluster PSM\n")
  cat("n         :", n, "\n")
  cat("components:", x$components %||% NA_integer_, "\n")
  cat("draw_index:", x$draw_index %||% NA_integer_, "\n")
  invisible(x)
}

#' Summarize a cluster posterior similarity matrix
#'
#' Summarize pairwise posterior co-clustering probabilities.
#'
#' @param object Cluster PSM object.
#' @param ... Unused.
#'
#' @return Summary list with matrix size and basic summaries of the similarity entries.
#'
#' @details
#' The diagonal of a posterior similarity matrix is always close to one, while off-diagonal values
#' near one indicate highly stable co-clustering across retained posterior draws.
#'
#' @seealso [predict.dpmixgpd_cluster_fit()], [plot.dpmixgpd_cluster_psm()],
#'   [summary.dpmixgpd_cluster_fit()].
#' @family cluster workflow
#' @export
summary.dpmixgpd_cluster_psm <- function(object, ...) {
  stopifnot(inherits(object, "dpmixgpd_cluster_psm"))
  p <- object$psm
  out <- list(
    n = nrow(p),
    components = object$components %||% NA_integer_,
    min = min(p, na.rm = TRUE),
    mean = mean(p, na.rm = TRUE),
    max = max(p, na.rm = TRUE),
    diagonal_mean = mean(diag(p), na.rm = TRUE)
  )
  class(out) <- c("summary.dpmixgpd_cluster_psm", "list")
  out
}

#' Plot a cluster posterior similarity matrix
#'
#' Heatmap of pairwise posterior co-clustering probabilities.
#'
#' @details
#' The heatmap visualizes the matrix
#' \deqn{
#' \mathrm{PSM}_{ij} \approx \frac{1}{S} \sum_{s=1}^S I(z_i^{(s)} = z_j^{(s)}),
#' }
#' so larger values indicate pairs of observations that are stably allocated to
#' the same cluster over the retained posterior draws.
#'
#' Because the PSM is an \eqn{n \times n} object, plotting and even storing it
#' becomes expensive for large `n`. The `psm_max_n` argument is therefore a
#' deliberate guard against accidental quadratic memory use.
#'
#' @param x Cluster PSM object.
#' @param psm_max_n Maximum allowed matrix size for plotting.
#' @param plotly Logical; if `TRUE`, convert the `ggplot2` output to a `plotly` /
#'   `htmlwidget` representation via `.wrap_plotly()`. Defaults to
#'   `getOption("CausalMixGPD.plotly", FALSE)`.
#' @param ... Unused.
#'
#' @return A `ggplot2` object or a `plotly`/`htmlwidget` object when `plotly = TRUE`.
#'
#' @seealso [predict.dpmixgpd_cluster_fit()], [summary.dpmixgpd_cluster_psm()],
#'   [plot.dpmixgpd_cluster_fit()].
#' @family cluster workflow
#' @export
plot.dpmixgpd_cluster_psm <- function(x,
                                      psm_max_n = x$psm_max_n %||% 2000L,
                                      plotly = getOption("CausalMixGPD.plotly", FALSE),
                                      ...) {
  stopifnot(inherits(x, "dpmixgpd_cluster_psm"))
  .cluster_require_ggplot()
  psm_max_n <- as.integer(psm_max_n)[1]
  if (!is.finite(psm_max_n) || psm_max_n < 1L) {
    stop("'psm_max_n' must be an integer >= 1.", call. = FALSE)
  }
  n <- nrow(x$psm %||% matrix(0, 0, 0))
  if (n > psm_max_n) {
    stop(
      sprintf(
        "PSM plot blocked: n=%d exceeds psm_max_n=%d. Increase 'psm_max_n' to plot.",
        n,
        psm_max_n
      ),
      call. = FALSE
    )
  }
  .cluster_plot_psm(x$psm, plotly = plotly)
}




.cluster_draw_indices <- function(n_draws, burnin = NULL, thin = NULL) {
  burnin <- as.integer(burnin %||% 0L)
  thin <- as.integer(thin %||% 1L)
  if (!is.finite(burnin) || burnin < 0L) stop("'burnin' must be >= 0.", call. = FALSE)
  if (!is.finite(thin) || thin < 1L) stop("'thin' must be >= 1.", call. = FALSE)
  if (burnin >= n_draws) stop("'burnin' is too large for available draws.", call. = FALSE)
  seq.int(from = burnin + 1L, to = n_draws, by = thin)
}

.cluster_samples_to_matrix <- function(samples) {
  if (!requireNamespace("coda", quietly = TRUE)) {
    stop("Package 'coda' is required.", call. = FALSE)
  }
  smp <- samples
  if (inherits(smp, "mcmc")) smp <- coda::mcmc.list(smp)
  if (!inherits(smp, "mcmc.list")) stop("Expected 'mcmc' or 'mcmc.list' samples.", call. = FALSE)
  do.call(rbind, lapply(smp, as.matrix))
}

.cluster_extract_z_from_matrix <- function(draw_mat) {
  z_cols <- grep("^z\\[[0-9]+\\]$", colnames(draw_mat))
  if (!length(z_cols)) stop("No z[i] columns found in samples.", call. = FALSE)
  z_names <- colnames(draw_mat)[z_cols]
  z_idx <- as.integer(sub("^z\\[([0-9]+)\\]$", "\\1", z_names))
  z_cols <- z_cols[order(z_idx)]
  z <- draw_mat[, z_cols, drop = FALSE]
  mode(z) <- "integer"
  z
}

extract_z_draws <- function(samples, burnin = NULL, thin = NULL) {
  draw_mat <- .cluster_samples_to_matrix(samples)
  idx <- .cluster_draw_indices(nrow(draw_mat), burnin = burnin, thin = thin)
  z <- .cluster_extract_z_from_matrix(draw_mat[idx, , drop = FALSE])
  z
}

compute_psm <- function(z_draws) {
  if (exists(".compute_psm", mode = "function")) return(.compute_psm(z_draws))
  n_iter <- nrow(z_draws)
  n_obs <- ncol(z_draws)
  psm <- matrix(0, n_obs, n_obs)
  for (s in seq_len(n_iter)) {
    z <- z_draws[s, ]
    psm <- psm + outer(z, z, "==") * 1.0
  }
  psm / n_iter
}

dahl_labels <- function(z_draws, psm) {
  if (exists(".dahl_representative", mode = "function")) return(.dahl_representative(z_draws, psm))

  n_iter <- nrow(z_draws)
  ssq <- numeric(n_iter)
  for (s in seq_len(n_iter)) {
    z <- z_draws[s, ]
    A <- outer(z, z, "==") * 1.0
    ssq[s] <- sum((A - psm)^2)
  }
  s_star <- which.min(ssq)
  z_hat <- as.integer(z_draws[s_star, ])
  labels <- match(z_hat, unique(z_hat))
  list(draw_index = s_star, labels = labels, K = length(unique(labels)))
}

.cluster_compute_scores <- function(z_draws, labels, psm) {
  if (exists(".compute_cluster_probs", mode = "function")) {
    return(.compute_cluster_probs(z_draws, labels, psm))
  }
  n_obs <- length(labels)
  K <- length(unique(labels))
  probs <- matrix(0, nrow = n_obs, ncol = K)
  for (k in seq_len(K)) {
    idx <- which(labels == k)
    for (i in seq_len(n_obs)) probs[i, k] <- mean(psm[i, idx])
  }
  rs <- rowSums(probs)
  rs[rs <= 0] <- 1
  probs / rs
}

.cluster_compute_probs <- .cluster_compute_scores

.cluster_order_levels <- function(labels, order_by = c("size", "label")) {
  order_by <- match.arg(order_by)
  tab <- table(as.character(labels))
  lev <- names(tab)
  lev_num <- suppressWarnings(as.numeric(lev))
  if (identical(order_by, "size")) {
    ord <- order(-as.integer(tab), ifelse(is.na(lev_num), Inf, lev_num), lev)
  } else {
    ord <- order(ifelse(is.na(lev_num), Inf, lev_num), lev)
  }
  lev[ord]
}

.cluster_validate_top_n <- function(top_n) {
  if (is.null(top_n)) return(NULL)
  top_n <- as.integer(top_n)[1]
  if (!is.finite(top_n) || top_n < 1L) {
    stop("'top_n' must be an integer >= 1.", call. = FALSE)
  }
  top_n
}

.cluster_select_levels <- function(labels,
                                   top_n = 5L,
                                   order_by = c("size", "label")) {
  keep <- .cluster_order_levels(labels, order_by = order_by)
  top_n <- .cluster_validate_top_n(top_n)
  if (!is.null(top_n)) keep <- head(keep, top_n)
  keep
}

.cluster_size_table <- function(labels, order_by = c("size", "label"), levels = NULL) {
  labels_chr <- as.character(labels)
  if (is.null(levels)) {
    levels <- .cluster_order_levels(labels_chr, order_by = order_by)
  } else {
    levels <- as.character(levels)
  }
  table(factor(labels_chr, levels = levels))
}

.cluster_data_frame_from_design <- function(design, formula_meta = list()) {
  if (is.null(design)) return(NULL)

  response_name <- formula_meta$response %||% "y"
  out <- data.frame(
    stats::setNames(list(as.numeric(design$y %||% numeric(0))), response_name),
    check.names = FALSE
  )

  X <- design$X %||% NULL
  X_cols <- formula_meta$X_cols %||% character(0)
  if (!is.null(X)) {
    X_df <- as.data.frame(X, check.names = FALSE, stringsAsFactors = FALSE)
    if (length(X_cols) == ncol(X_df)) names(X_df) <- X_cols
    out <- data.frame(out, X_df, check.names = FALSE)
  }

  out
}

.cluster_training_data_frame <- function(fit) {
  stopifnot(inherits(fit, "dpmixgpd_cluster_fit"))
  bundle <- fit$bundle %||% list()
  design <- bundle$data %||% NULL
  formula_meta <- ((fit$spec %||% list())$cluster %||% list())$formula_meta %||% list()
  .cluster_data_frame_from_design(design = design, formula_meta = formula_meta)
}

.cluster_training_reference <- function(fit, labels) {
  data <- .cluster_training_data_frame(fit)
  if (is.null(data) || !length(labels)) return(NULL)
  list(
    labels = as.integer(labels),
    data = data
  )
}

.cluster_response_name <- function(data) {
  data <- as.data.frame(data, check.names = FALSE, stringsAsFactors = FALSE)
  nm <- names(data)[1]
  if (is.null(nm) || !nzchar(nm)) "y" else as.character(nm)
}

.cluster_response_values <- function(data, response_name = NULL) {
  if (is.null(data)) return(numeric(0))
  data <- as.data.frame(data, check.names = FALSE, stringsAsFactors = FALSE)
  response_name <- as.character(response_name %||% .cluster_response_name(data))[1]
  if (!(response_name %in% names(data))) return(numeric(0))
  as.numeric(data[[response_name]])
}

.cluster_plot_ylim <- function(values) {
  vals <- as.numeric(values)
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return(c(0, 1))

  rng <- range(vals)
  pad <- if (rng[1] == rng[2]) {
    max(abs(rng[1]), 1) * 0.08
  } else {
    diff(rng) * 0.06
  }
  c(rng[1] - pad, rng[2] + pad)
}

.cluster_scale_values <- function(levels, reference_levels = NULL) {
  lev <- as.character(levels %||% character(0))
  if (!length(lev)) return(stats::setNames(character(0), character(0)))

  ref <- as.character(reference_levels %||% lev)
  ref <- unique(ref)
  if (!length(ref)) ref <- lev

  stats::setNames(.plot_palette(length(ref)), ref)[lev]
}

.cluster_hist_breaks <- function(values, bins = 18L) {
  vals <- as.numeric(values)
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return(c(0, 1))

  bins <- as.integer(bins %||% 18L)
  if (!is.finite(bins) || bins < 4L) bins <- 18L

  rng <- range(vals)
  if (rng[1] == rng[2]) {
    pad <- max(abs(rng[1]), 1) * 0.25
    return(seq(rng[1] - pad, rng[2] + pad, length.out = bins + 1L))
  }

  br <- pretty(rng, n = bins)
  br <- sort(unique(as.numeric(br[is.finite(br)])))
  if (length(br) < 2L) {
    br <- seq(rng[1], rng[2], length.out = bins + 1L)
  }
  if (min(br) > rng[1]) br <- c(rng[1], br)
  if (max(br) < rng[2]) br <- c(br, rng[2])
  br <- sort(unique(br))

  if (length(br) < 2L) {
    return(seq(rng[1], rng[2], length.out = bins + 1L))
  }
  br
}

.cluster_hist_max_counts <- function(df, levels, breaks) {
  lev <- as.character(levels %||% character(0))
  out <- stats::setNames(rep(0, length(lev)), lev)
  if (is.null(df) || !nrow(df) || length(breaks) < 2L) return(out)

  for (cl in lev) {
    vals <- as.numeric(df$response[as.character(df$cluster) == cl])
    vals <- vals[is.finite(vals)]
    if (!length(vals)) next
    hist_obj <- suppressWarnings(graphics::hist(vals, breaks = breaks, plot = FALSE))
    out[[cl]] <- max(c(0, hist_obj$counts), na.rm = TRUE)
  }

  out
}

.cluster_require_ggplot <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Install it first.", call. = FALSE)
  }
}

.cluster_maybe_wrap_plotly <- function(p, plotly = getOption("CausalMixGPD.plotly", FALSE)) {
  if (!isTRUE(plotly)) return(p)
  old_options <- options(CausalMixGPD.plotly = TRUE)
  on.exit(options(old_options), add = TRUE)
  .wrap_plotly(p)
}

.cluster_plot_sizes <- function(labels,
                                title = "Cluster sizes",
                                top_n = 5L,
                                order_by = c("size", "label"),
                                reference_levels = NULL,
                                plotly = getOption("CausalMixGPD.plotly", FALSE)) {
  .cluster_require_ggplot()
  keep <- .cluster_select_levels(labels, top_n = top_n, order_by = order_by)
  tab <- .cluster_size_table(labels, levels = keep)
  df <- data.frame(
    cluster = factor(names(tab), levels = names(tab)),
    size = as.integer(tab),
    stringsAsFactors = FALSE
  )
  cluster_values <- .cluster_scale_values(
    names(tab),
    reference_levels = reference_levels %||% keep
  )
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = cluster, y = size, fill = cluster, color = cluster)
  ) +
    ggplot2::geom_col(linewidth = 0.4, show.legend = FALSE) +
    .plot_theme() +
    ggplot2::scale_fill_manual(values = cluster_values, drop = FALSE) +
    ggplot2::scale_color_manual(values = cluster_values, drop = FALSE) +
    ggplot2::scale_x_discrete(labels = function(v) paste0("C", v)) +
    ggplot2::labs(x = "Cluster", y = "Size", title = title) +
    ggplot2::theme(legend.position = "none")
  .cluster_maybe_wrap_plotly(p, plotly = plotly)
}

.cluster_plot_certainty <- function(max_prob,
                                    plotly = getOption("CausalMixGPD.plotly", FALSE)) {
  .cluster_require_ggplot()
  df <- data.frame(max_prob = as.numeric(max_prob))
  df <- df[is.finite(df$max_prob), , drop = FALSE]
  pal <- .plot_palette(2L)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = max_prob)) +
    ggplot2::geom_histogram(bins = 20, fill = pal[1], color = pal[2], alpha = 0.75) +
    .plot_theme() +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      x = "Max assignment probability",
      y = "Count",
      title = "Label certainty"
    )
  .cluster_maybe_wrap_plotly(p, plotly = plotly)
}

.cluster_plot_psm <- function(psm,
                              plotly = getOption("CausalMixGPD.plotly", FALSE)) {
  .cluster_require_ggplot()
  n <- nrow(psm)
  df <- expand.grid(
    row = seq_len(n),
    col = seq_len(n),
    KEEP.OUT.ATTRS = FALSE
  )
  df$prob <- as.numeric(psm)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = col, y = row, fill = prob)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_viridis_c(limits = c(0, 1), option = "C") +
    ggplot2::scale_y_reverse(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::coord_fixed() +
    .plot_theme() +
    ggplot2::labs(
      x = "Observation index",
      y = "Observation index",
      fill = "PSM",
      title = "Posterior Similarity Matrix"
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  .cluster_maybe_wrap_plotly(p, plotly = plotly)
}

.cluster_summary_plot_data <- function(object,
                                      top_n = 5L,
                                      order_by = c("size", "label")) {
  stopifnot(inherits(object, "dpmixgpd_cluster_labels"))
  data <- object$data %||% NULL
  if (is.null(data) || !nrow(as.data.frame(data))) {
    warning("No attached data available for summary plot.", call. = FALSE)
    return(NULL)
  }

  data <- as.data.frame(data, check.names = FALSE, stringsAsFactors = FALSE)
  if (nrow(data) != length(object$labels)) {
    warning("Attached data and labels must have the same number of rows for summary plot.", call. = FALSE)
    return(NULL)
  }

  response_name <- .cluster_response_name(data)
  levels <- .cluster_select_levels(object$labels, top_n = top_n, order_by = order_by)

  main_df <- data.frame(
    cluster = factor(as.character(object$labels), levels = levels),
    response = .cluster_response_values(data, response_name = response_name),
    sample = if (identical(object$source %||% "", "newdata")) "newdata" else "train",
    stringsAsFactors = FALSE
  )
  main_df <- main_df[is.finite(main_df$response) & !is.na(main_df$cluster), , drop = FALSE]

  if (!nrow(main_df)) {
    warning("No finite response values available for summary plot.", call. = FALSE)
    return(NULL)
  }

  list(
    response_name = response_name,
    has_reference = FALSE,
    levels = levels,
    main_df = main_df,
    ref_df = NULL,
    ylim = .cluster_plot_ylim(main_df$response),
    title = if (identical(object$source %||% "", "newdata")) {
      "Cluster response summary: newdata"
    } else {
      "Cluster response summary"
    }
  )
}

.cluster_plot_summary_labels <- function(object,
                                         top_n = 5L,
                                         order_by = c("size", "label"),
                                         plotly = getOption("CausalMixGPD.plotly", FALSE)) {
  .cluster_require_ggplot()
  dat <- .cluster_summary_plot_data(object, top_n = top_n, order_by = order_by)
  if (is.null(dat)) return(invisible(object))
  ref_labels <- if (identical(object$source %||% "", "newdata")) {
    (object$train_reference %||% list())$labels %||% object$labels
  } else {
    object$labels
  }
  cluster_values <- .cluster_scale_values(
    dat$levels,
    reference_levels = .cluster_order_levels(ref_labels, order_by = order_by)
  )
  p <- ggplot2::ggplot(
    dat$main_df,
    ggplot2::aes(x = cluster, y = response, fill = cluster, color = cluster)
  ) +
    ggplot2::geom_boxplot(
      width = 0.72,
      alpha = 0.82,
      outlier.alpha = 0.45,
      linewidth = 0.45,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = cluster_values, drop = FALSE) +
    ggplot2::scale_color_manual(values = cluster_values, drop = FALSE) +
    ggplot2::scale_x_discrete(
      drop = FALSE,
      labels = function(v) paste0("C", v)
    ) +
    ggplot2::coord_cartesian(ylim = dat$ylim) +
    .plot_theme() +
    ggplot2::labs(
      x = "Cluster",
      y = dat$response_name,
      title = dat$title
    ) +
    ggplot2::theme(legend.position = "none")

  .cluster_maybe_wrap_plotly(p, plotly = plotly)
}

.cluster_profile_table <- function(data,
                                   labels,
                                   score_mat = NULL,
                                   top_n = 5L,
                                   order_by = c("size", "label"),
                                   vars = NULL) {
  if (is.null(data)) return(NULL)

  order_by <- match.arg(order_by)
  data <- as.data.frame(data, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(data) || !length(labels)) return(NULL)
  if (nrow(data) != length(labels)) {
    stop("Cluster labels and attached data must have the same number of rows.", call. = FALSE)
  }

  if (is.null(vars)) {
    vars <- names(data)[vapply(data, is.numeric, logical(1))]
  } else {
    vars <- as.character(vars)
    bad <- setdiff(vars, names(data))
    if (length(bad)) {
      stop(sprintf("Unknown profiling variables: %s", paste(bad, collapse = ", ")), call. = FALSE)
    }
    vars <- vars[vapply(data[vars], is.numeric, logical(1))]
  }

  keep <- .cluster_select_levels(labels, top_n = top_n, order_by = order_by)

  max_prob <- if (is.matrix(score_mat)) apply(score_mat, 1, max) else NULL
  rows <- lapply(keep, function(cl) {
    idx <- which(as.character(labels) == cl)
    row <- list(
      cluster = paste0("C", cl),
      n = length(idx)
    )

    if (length(vars)) {
      for (nm in vars) {
        x <- as.numeric(data[[nm]][idx])
        row[[paste0(nm, "_mean")]] <- mean(x, na.rm = TRUE)
        row[[paste0(nm, "_sd")]] <- if (sum(is.finite(x)) > 1L) stats::sd(x, na.rm = TRUE) else NA_real_
      }
    }

    if (!is.null(max_prob)) {
      p <- max_prob[idx]
      row$certainty_mean <- mean(p, na.rm = TRUE)
      row$certainty_sd <- if (sum(is.finite(p)) > 1L) stats::sd(p, na.rm = TRUE) else NA_real_
    }

    as.data.frame(row, check.names = FALSE)
  })

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

.cluster_check_new_factor_levels <- function(meta, newdata) {
  xl <- meta$xlevels %||% list()
  if (!length(xl)) return(invisible(NULL))
  for (nm in names(xl)) {
    if (!(nm %in% names(newdata))) next
    vals <- as.character(newdata[[nm]])
    vals <- unique(vals[!is.na(vals)])
    bad <- setdiff(vals, as.character(xl[[nm]]))
    if (length(bad)) {
      stop(
        sprintf(
          "newdata has unseen factor levels for '%s': %s",
          nm,
          paste(bad, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

.cluster_build_design <- function(meta, newdata) {
  if (!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
  response_name <- meta$response %||% "y"
  if (!(response_name %in% names(newdata))) {
    stop(sprintf("newdata must include response column '%s'.", response_name), call. = FALSE)
  }

  y_new <- as.numeric(newdata[[response_name]])
  if (anyNA(y_new)) stop("newdata response contains NA.", call. = FALSE)

  trm <- meta$terms
  has_X <- length(meta$X_cols %||% character(0)) > 0L
  if (!has_X) {
    return(list(y = y_new, X = NULL))
  }

  rhs <- stats::delete.response(trm)
  .cluster_check_new_factor_levels(meta = meta, newdata = newdata)
  mf_new <- tryCatch(
    stats::model.frame(
      rhs,
      data = newdata,
      xlev = meta$xlevels %||% NULL,
      na.action = stats::na.fail
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("new level", msg, fixed = TRUE)) {
        stop(sprintf("newdata has unseen factor levels: %s", msg), call. = FALSE)
      }
      stop(sprintf("Failed to build model frame for newdata: %s", conditionMessage(e)), call. = FALSE)
    }
  )
  mm <- stats::model.matrix(rhs, data = mf_new, contrasts.arg = meta$contrasts %||% NULL)
  if ("(Intercept)" %in% colnames(mm)) {
    mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  }
  X_cols <- meta$X_cols %||% character(0)
  miss <- setdiff(X_cols, colnames(mm))
  if (length(miss)) {
    stop(sprintf("newdata is missing required predictors: %s", paste(miss, collapse = ", ")), call. = FALSE)
  }
  mm <- mm[, X_cols, drop = FALSE]
  storage.mode(mm) <- "double"
  list(y = y_new, X = mm)
}

.cluster_link_apply <- function(eta, link = "identity", link_power = NULL) {
  link <- as.character(link %||% "identity")
  if (identical(link, "identity")) return(eta)
  if (identical(link, "exp")) return(exp(eta))
  if (identical(link, "log")) return(log(eta))
  if (identical(link, "softplus")) return(log1p(exp(eta)))
  if (identical(link, "power")) {
    pw <- as.numeric(link_power)
    if (!is.finite(pw) || length(pw) != 1L) stop("Invalid power link exponent.", call. = FALSE)
    return(eta ^ pw)
  }
  stop(sprintf("Unsupported link '%s'.", link), call. = FALSE)
}

.cluster_extract_beta_component <- function(draw_row, base, comp, P) {
  out <- rep(0, P)
  for (p in seq_len(P)) {
    nm <- sprintf("%s[%d,%d]", base, comp, p)
    if (!(nm %in% names(draw_row))) {
      nm <- sprintf("%s[%d, %d]", base, comp, p)
    }
    if (nm %in% names(draw_row)) out[p] <- as.numeric(draw_row[[nm]])
  }
  out
}

.cluster_extract_beta_global <- function(draw_row, base, P) {
  out <- rep(0, P)
  for (p in seq_len(P)) {
    nm <- sprintf("%s[%d]", base, p)
    if (nm %in% names(draw_row)) out[p] <- as.numeric(draw_row[[nm]])
  }
  out
}

.cluster_extract_beta_auto <- function(draw_row, base, comp, P) {
  if (P < 1L) return(numeric(0))
  nn <- names(draw_row)
  has_component <- any(grepl(sprintf("^%s\\[%d,\\s*[0-9]+\\]$", base, comp), nn))
  if (isTRUE(has_component)) {
    return(.cluster_extract_beta_component(draw_row, base = base, comp = comp, P = P))
  }
  .cluster_extract_beta_global(draw_row, base = base, P = P)
}

.cluster_softmax <- function(logits) {
  logits <- as.numeric(logits)
  if (!length(logits)) return(numeric(0))
  shift <- max(logits)
  ex <- exp(logits - shift)
  s <- sum(ex)
  if (!is.finite(s) || s <= 0) return(rep(1 / length(logits), length(logits)))
  ex / s
}

.cluster_extract_gating_draw <- function(draw_row, K, P) {
  K <- as.integer(K)
  P <- as.integer(P)
  if (K < 2L || P < 1L) return(NULL)
  eta <- rep(0, K - 1L)
  B <- matrix(0, nrow = K - 1L, ncol = P)
  nn <- names(draw_row)

  for (j in seq_len(K - 1L)) {
    nm_eta <- sprintf("eta[%d]", j)
    if (nm_eta %in% nn) eta[j] <- as.numeric(draw_row[[nm_eta]])
    for (p in seq_len(P)) {
      nm <- sprintf("B[%d,%d]", j, p)
      if (!(nm %in% nn)) nm <- sprintf("B[%d, %d]", j, p)
      if (nm %in% nn) B[j, p] <- as.numeric(draw_row[[nm]])
    }
  }

  if (!any(grepl("^eta\\[[0-9]+\\]$", nn)) || !any(grepl("^B\\[[0-9]+,\\s*[0-9]+\\]$", nn))) {
    return(NULL)
  }
  list(eta = eta, B = B)
}

.cluster_gating_weights <- function(gating_draw, x_row) {
  if (is.null(gating_draw)) return(NULL)
  eta <- gating_draw$eta
  B <- gating_draw$B
  K <- length(eta) + 1L
  x_row <- as.numeric(x_row)
  lin <- as.numeric(B %*% x_row)
  logits <- c(eta + lin, 0)
  out <- .cluster_softmax(logits)
  if (length(out) != K) return(NULL)
  out
}

.cluster_resolve_density_fun <- function(spec) {
  meta <- spec$meta %||% list()
  kernel <- meta$kernel
  GPD <- isTRUE(meta$GPD)
  kdef <- get_kernel_registry()[[kernel]]
  if (is.null(kdef)) stop(sprintf("Kernel '%s' not found.", kernel), call. = FALSE)
  d_name <- if (GPD) kdef$crp$d_gpd else kdef$crp$d_base
  if (is.null(d_name) || isTRUE(is.na(d_name))) stop("Could not resolve density function.", call. = FALSE)

  ns_pkg <- getNamespace("CausalMixGPD")
  ns_stats <- getNamespace("stats")
  ns_nimble <- getNamespace("nimble")

  if (exists(d_name, envir = ns_pkg, inherits = FALSE)) return(get(d_name, envir = ns_pkg))
  if (exists(d_name, envir = ns_stats, inherits = FALSE)) return(get(d_name, envir = ns_stats))
  if (exists(d_name, envir = ns_nimble, inherits = FALSE)) return(get(d_name, envir = ns_nimble))
  stop(sprintf("Density function '%s' is unavailable.", d_name), call. = FALSE)
}

.cluster_component_density <- function(spec, draw_row, k, x_row, y_val, density_fun) {
  meta <- spec$meta %||% list()
  plan <- spec$plan %||% list()
  bulk <- plan$bulk %||% list()
  arg_order <- if (isTRUE(meta$GPD)) spec$signatures$gpd$args else spec$signatures$bulk$args
  P <- as.integer(meta$P %||% 0L)

  args <- list(x = as.numeric(y_val))

  for (nm in arg_order) {
    if (nm %in% names(bulk)) {
      ent <- bulk[[nm]]
      mode <- ent$mode %||% "dist"
      if (identical(mode, "link")) {
        b <- .cluster_extract_beta_component(draw_row, paste0("beta_", nm), comp = k, P = P)
        eta <- if (P > 0L) sum(x_row * b) else 0
        args[[nm]] <- .cluster_link_apply(eta, ent$link, ent$link_power)
      } else {
        col_nm <- sprintf("%s[%d]", nm, k)
        if (!(col_nm %in% names(draw_row))) {
          args[[nm]] <- as.numeric(ent$value %||% NA_real_)
        } else {
          args[[nm]] <- as.numeric(draw_row[[col_nm]])
        }
      }
      next
    }

    if (nm %in% c("threshold", "tail_scale", "tail_shape")) {
      gpd <- plan$gpd %||% list()
      ent <- gpd[[nm]] %||% list(mode = "dist")
      if (identical(ent$mode, "link")) {
        b <- .cluster_extract_beta_auto(
          draw_row = draw_row,
          base = paste0("beta_", nm),
          comp = k,
          P = P
        )
        eta <- if (P > 0L) sum(x_row * b) else 0
        args[[nm]] <- .cluster_link_apply(eta, ent$link, ent$link_power)
      } else if (identical(ent$mode, "fixed")) {
        args[[nm]] <- as.numeric(ent$value)
      } else {
        # dist mode: prefer scalar draw, fallback to first indexed value
        if (nm %in% names(draw_row)) {
          args[[nm]] <- as.numeric(draw_row[[nm]])
        } else {
          col_nm <- sprintf("%s[%d]", nm, k)
          if (col_nm %in% names(draw_row)) {
            args[[nm]] <- as.numeric(draw_row[[col_nm]])
          } else {
            args[[nm]] <- NA_real_
          }
        }
      }
      next
    }
  }

  fm <- names(formals(density_fun))
  if ("log" %in% fm && is.null(args$log)) args$log <- FALSE

  val <- suppressWarnings(tryCatch(as.numeric(do.call(density_fun, args))[1], error = function(e) NA_real_))
  if (!is.finite(val) || val < 0) val <- 0
  val
}

predict_labels_newdata <- function(fit, newdata, burnin = NULL, thin = NULL) {
  stopifnot(inherits(fit, "dpmixgpd_cluster_fit"))
  spec <- fit$spec
  meta <- spec$meta %||% list()
  formula_meta <- (spec$cluster %||% list())$formula_meta %||% list()
  newdat <- .cluster_build_design(formula_meta, newdata = newdata)

  draw_mat <- .cluster_samples_to_matrix(fit$samples)
  idx <- .cluster_draw_indices(nrow(draw_mat), burnin = burnin, thin = thin)
  draw_sub <- draw_mat[idx, , drop = FALSE]
  z_draws <- .cluster_extract_z_from_matrix(draw_sub)

  cache_key <- paste("cache", length(idx), burnin %||% 0L, thin %||% 1L, sep = "_")
  cache_env <- fit$cache_env %||% new.env(parent = emptyenv())
  if (exists(cache_key, envir = cache_env, inherits = FALSE)) {
    tr <- get(cache_key, envir = cache_env, inherits = FALSE)
    if (is.null(tr$scores_train) && !is.null(tr$probs_train)) tr$scores_train <- tr$probs_train
  } else {
    psm <- compute_psm(z_draws)
    dahl <- dahl_labels(z_draws, psm)
    scores_train <- .cluster_compute_scores(z_draws, dahl$labels, psm)
    tr <- list(psm = psm, dahl = dahl, scores_train = scores_train)
    assign(cache_key, tr, envir = cache_env)
  }

  dahl_labels_train <- as.integer(tr$dahl$labels)
  Kd <- as.integer(tr$dahl$K)
  density_fun <- .cluster_resolve_density_fun(spec)
  n_new <- length(newdat$y)
  out_scores <- matrix(0, nrow = n_new, ncol = Kd)

  K <- as.integer(meta$components %||% max(z_draws, na.rm = TRUE))
  type <- (spec$cluster %||% list())$type %||% "weights"
  use_gating <- type %in% c("weights", "both")

  for (s in seq_len(nrow(draw_sub))) {
    draw_row <- draw_sub[s, ]
    z_s <- as.integer(z_draws[s, ])
    comp_sizes <- pmax(tabulate(z_s, nbins = K), 0)
    if (sum(comp_sizes) <= 0) comp_sizes <- rep(1, K)
    gating_draw <- if (isTRUE(use_gating) && !is.null(newdat$X)) {
      .cluster_extract_gating_draw(draw_row = draw_row, K = K, P = ncol(newdat$X))
    } else {
      NULL
    }
    w_nm <- paste0("w[", seq_len(K), "]")

    map_comp_to_dahl <- matrix(0, nrow = K, ncol = Kd)
    for (k in seq_len(K)) {
      idx_k <- which(z_s == k)
      if (!length(idx_k)) {
        map_comp_to_dahl[k, ] <- rep(1 / Kd, Kd)
      } else {
        map_comp_to_dahl[k, ] <- tabulate(dahl_labels_train[idx_k], nbins = Kd) / length(idx_k)
      }
    }

    for (i in seq_len(n_new)) {
      x_row <- if (is.null(newdat$X)) numeric(0) else as.numeric(newdat$X[i, ])
      weight_factor <- comp_sizes
      if (isTRUE(use_gating)) {
        w_new <- .cluster_gating_weights(gating_draw = gating_draw, x_row = x_row)
        if (!is.null(w_new)) {
          weight_factor <- pmax(w_new, 0)
        } else if (all(w_nm %in% names(draw_row))) {
          weight_factor <- pmax(as.numeric(draw_row[w_nm]), 0)
        }
      }
      if (sum(weight_factor) <= 0) weight_factor <- rep(1 / K, K)
      like_k <- numeric(K)
      for (k in seq_len(K)) {
        like_k[k] <- .cluster_component_density(
          spec = spec,
          draw_row = draw_row,
          k = k,
          x_row = x_row,
          y_val = newdat$y[i],
          density_fun = density_fun
        )
      }
      post_k <- weight_factor * pmax(like_k, 0)
      if (!any(is.finite(post_k)) || sum(post_k) <= 0) {
        post_k <- weight_factor
      } else {
        post_k <- post_k / sum(post_k)
      }
      out_scores[i, ] <- out_scores[i, ] + as.numeric(post_k %*% map_comp_to_dahl)
    }
  }

  out_scores <- out_scores / nrow(draw_sub)
  rs <- rowSums(out_scores)
  rs[rs <= 0] <- 1
  out_scores <- out_scores / rs
  labels <- max.col(out_scores, ties.method = "first")

  list(
    labels = as.integer(labels),
    scores = out_scores,
    data = .cluster_data_frame_from_design(design = newdat, formula_meta = formula_meta),
    K = Kd,
    cache = tr
  )
}

run_cluster_mcmc <- function(bundle, ...) {
  stopifnot(inherits(bundle, "dpmixgpd_cluster_bundle"))
  base_fit <- run_mcmc_bundle_manual(bundle, ...)

  out <- list(
    call = match.call(),
    spec = bundle$spec,
    bundle = bundle,
    base_fit = base_fit,
    samples = base_fit$samples %||% (base_fit$mcmc %||% list())$samples,
    mcmc = base_fit$mcmc %||% list(),
    timing = utils::modifyList(
      base_fit$timing %||% list(),
      list(total = (base_fit$timing %||% list())$total %||%
             sum(unlist((base_fit$timing %||% list())[c("build", "compile", "mcmc")]), na.rm = TRUE))
    ),
    cache_env = new.env(parent = emptyenv()),
    psm = NULL,
    dahl = NULL
  )
  class(out) <- c("dpmixgpd_cluster_fit", "list")
  out
}

## S3 -------------------------------------------------------------------------

#' Predict labels or similarity matrices from a cluster fit
#'
#' Convert posterior draws from a `dpmixgpd_cluster_fit` object into either a representative
#' clustering or a posterior similarity matrix (PSM). This is the main post-processing step for
#' the cluster workflow after [dpmix.cluster()] or [dpmgpd.cluster()].
#'
#' @param object A fitted cluster object.
#' @param newdata Optional new data containing the response and predictors required by the original
#'   formula. New-data prediction is available only for `type = "label"`.
#' @param type Prediction target:
#'   \itemize{
#'     \item \code{"label"}: representative partition via
#'       Dahl's least-squares rule
#'     \item \code{"psm"}: posterior similarity matrix on the
#'       training sample
#'   }
#' @param burnin Number of initial posterior draws to discard.
#' @param thin Keep every `thin`-th posterior draw.
#' @param return_scores Logical; if `TRUE` and `type = "label"`, include the matrix of Dahl-cluster
#'   assignment scores.
#' @param psm_max_n Maximum training sample size allowed for `type = "psm"`.
#' @param ... Unused.
#'
#' @return A `dpmixgpd_cluster_labels` object when `type = "label"` or a
#'   `dpmixgpd_cluster_psm` object when `type = "psm"`.
#'
#' @details
#' Let \eqn{z_i^{(s)}} denote the latent cluster label for observation \eqn{i} at posterior draw
#' \eqn{s}. The posterior similarity matrix is
#' \deqn{
#' \mathrm{PSM}_{ij} = \Pr(z_i = z_j \mid y) \approx \frac{1}{S} \sum_{s=1}^S I(z_i^{(s)} = z_j^{(s)}).
#' }
#' The returned label solution is the Dahl representative partition, obtained by choosing the draw
#' whose adjacency matrix is closest to the PSM in squared error.
#'
#' For `newdata`, the function combines draw-specific component weights and component densities to
#' produce posterior assignment scores relative to the representative training clusters. Returned
#' `newdata` label objects also carry the training labels and response data needed for comparative
#' `plot(..., type = "summary")` displays. A PSM is not defined for `newdata`, so `type = "psm"`
#' is restricted to the training sample.
#'
#' Computing the PSM is \eqn{O(n^2)} in the training sample size, so `psm_max_n` guards against
#' accidental large matrix allocations.
#'
#' @seealso [dpmix.cluster()], [dpmgpd.cluster()], [summary.dpmixgpd_cluster_fit()],
#'   [plot.dpmixgpd_cluster_fit()], [summary.dpmixgpd_cluster_labels()],
#'   [summary.dpmixgpd_cluster_psm()].
#' @family cluster workflow
#' @export
predict.dpmixgpd_cluster_fit <- function(object,
                                         newdata = NULL,
                                         type = c("label", "psm"),
                                         burnin = NULL,
                                         thin = NULL,
                                         return_scores = FALSE,
                                         psm_max_n = 2000L,
                                         ...) {
  stopifnot(inherits(object, "dpmixgpd_cluster_fit"))
  type <- match.arg(type)
  psm_max_n <- as.integer(psm_max_n)[1]
  if (!is.finite(psm_max_n) || psm_max_n < 1L) {
    stop("'psm_max_n' must be an integer >= 1.", call. = FALSE)
  }

  if (!is.null(newdata) && identical(type, "psm")) {
    stop("type='psm' is only available for training data (newdata=NULL).", call. = FALSE)
  }

  draw_mat <- .cluster_samples_to_matrix(object$samples)
  idx <- .cluster_draw_indices(nrow(draw_mat), burnin = burnin, thin = thin)
  draw_sub <- draw_mat[idx, , drop = FALSE]
  z_draws <- .cluster_extract_z_from_matrix(draw_sub)

  cache_key <- paste("cache", length(idx), burnin %||% 0L, thin %||% 1L, sep = "_")
  cache_env <- object$cache_env %||% new.env(parent = emptyenv())
  if (exists(cache_key, envir = cache_env, inherits = FALSE)) {
    tr <- get(cache_key, envir = cache_env, inherits = FALSE)
    if (is.null(tr$scores_train) && !is.null(tr$probs_train)) tr$scores_train <- tr$probs_train
  } else {
    n_train <- ncol(z_draws)
    if (identical(type, "psm") && n_train > psm_max_n) {
      stop(
        sprintf(
          "PSM is O(n^2): n=%d exceeds psm_max_n=%d. Increase 'psm_max_n' or use type='label'.",
          n_train,
          psm_max_n
        ),
        call. = FALSE
      )
    }
    psm <- compute_psm(z_draws)
    dahl <- dahl_labels(z_draws, psm)
    scores_train <- .cluster_compute_scores(z_draws, dahl$labels, psm)
    tr <- list(psm = psm, dahl = dahl, scores_train = scores_train)
    assign(cache_key, tr, envir = cache_env)
  }

  if (identical(type, "psm")) {
    n_train <- ncol(z_draws)
    if (n_train > psm_max_n) {
      stop(
        sprintf(
          "PSM is O(n^2): n=%d exceeds psm_max_n=%d. Increase 'psm_max_n' or use type='label'.",
          n_train,
          psm_max_n
        ),
        call. = FALSE
      )
    }
    out <- list(
      psm = tr$psm,
      labels = as.integer(tr$dahl$labels),
      components = as.integer(tr$dahl$K),
      draw_index = as.integer(tr$dahl$draw_index),
      burnin = as.integer(burnin %||% 0L),
      thin = as.integer(thin %||% 1L),
      psm_max_n = psm_max_n
    )
    class(out) <- c("dpmixgpd_cluster_psm", "list")
    return(out)
  }

  if (is.null(newdata)) {
    out <- list(
      labels = as.integer(tr$dahl$labels),
      components = as.integer(tr$dahl$K),
      data = .cluster_training_data_frame(object),
      source = "train",
      burnin = as.integer(burnin %||% 0L),
      thin = as.integer(thin %||% 1L)
    )
    if (isTRUE(return_scores)) out$scores <- tr$scores_train
    class(out) <- c("dpmixgpd_cluster_labels", "list")
    return(out)
  }

  pred <- predict_labels_newdata(fit = object, newdata = newdata, burnin = burnin, thin = thin)
  out <- list(
    labels = as.integer(pred$labels),
    components = as.integer(pred$K),
    data = pred$data,
    train_reference = .cluster_training_reference(object, labels = tr$dahl$labels),
    source = "newdata",
    burnin = as.integer(burnin %||% 0L),
    thin = as.integer(thin %||% 1L)
  )
  if (isTRUE(return_scores)) out$scores <- pred$scores
  class(out) <- c("dpmixgpd_cluster_labels", "list")
  out
}

