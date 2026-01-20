# =========================
# 02-build-and-run.R
# =========================

#' Build a NIMBLE bundle
#'
#' Creates a runnable "bundle" containing:
#' \itemize{
#'   \item compiled model \code{spec}
#'   \item \code{nimbleCode} model code
#'   \item \code{constants}, \code{data}, explicit \code{dimensions}
#'   \item initialization function \code{inits} (stored as a function)
#'   \item monitor specification
#'   \item MCMC settings list (stored but not used for code generation)
#' }
#'
#' This function intentionally stops at the "pre-run" stage (spec/code/constants/data/dimensions/inits/monitors).
#' Use \code{run_mcmc_bundle_manual()} to execute MCMC with the stored settings.
#'
#' @param y Numeric outcome vector.
#' @param X Optional design matrix/data.frame (N x p) for conditional variants.
#' @param ps Optional numeric vector (length N) of propensity scores. When provided,
#'   augments the design matrix for PS-adjusted outcome modeling.
#' @param backend Character; \code{"sb"} (stick-breaking) or \code{"crp"} (Chinese Restaurant Process).
#' @param kernel Character kernel name (must exist in \code{get_kernel_registry()}).
#' @param GPD Logical; whether a GPD tail is requested.
#' @param J Integer >= 2. Single user-facing truncation parameter:
#'   \itemize{
#'     \item SB: number of mixture components used in stick-breaking truncation
#'     \item CRP: maximum number of clusters represented in the finite NIMBLE model
#'   }
#' @param components Deprecated alias for \code{J}. Only one of \code{J} or \code{components}
#'   should be supplied.
#' @param param_specs Optional list with entries \code{bulk} and \code{tail} to override defaults.
#' @param mcmc Named list of MCMC settings (niter, nburnin, thin, nchains, seed). Stored in bundle.
#' @param epsilon Numeric in [0,1). For downstream summaries/plots/prediction we keep the
#'   smaller k defined by either (i) cumulative mass >= 1 - epsilon or (ii) per-component
#'   weights >= epsilon, then renormalize.
#' @param alpha_random Logical; whether concentration \code{alpha} is stochastic.
#' @return A named list (bundle) of class \code{"dpmixgpd_bundle"}.
#' @examples
#' \dontrun{
#' y <- abs(rnorm(60)) + 0.1
#' bundle <- build_nimble_bundle(
#'   y = y,
#'   backend = "sb",
#'   kernel = "normal",
#'   GPD = FALSE,
#'   components = 4,
#'   mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
#' )
#' bundle
#' }
#' @export
build_nimble_bundle <- function(
    y,
    X = NULL,
    ps = NULL,
    backend = c("sb", "crp"),
    kernel,
    GPD = FALSE,
    components = NULL,
    J = NULL,
    param_specs = NULL,
    mcmc = list(niter = 2000, nburnin = 500, thin = 1, nchains = 1, seed = 1),
    epsilon = 0.025,
    alpha_random = TRUE
) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  backend <- match.arg(backend, choices = c("sb", "crp"))

  y <- as.numeric(y)
  if (!length(y)) stop("y must be a non-empty numeric vector.", call. = FALSE)

  if (!is.null(X) && !is.matrix(X)) X <- as.matrix(X)
  if (!is.null(ps)) {
    ps <- as.numeric(ps)
    if (length(ps) != length(y)) stop("ps must have the same length as y.", call. = FALSE)
  }

  # Single truncation parameter for both backends
  if (!is.null(J) && !is.null(components)) {
    stop("Provide only one of 'J' or 'components'.", call. = FALSE)
  }
  if (!is.null(J)) components <- J
  if (is.null(components)) components <- length(y)
  components <- as.integer(components)
  if (!is.finite(components) || components < 2L) {
    stop("components must be an integer >= 2.", call. = FALSE)
  }

  # Basic epsilon validation (stored; used later by fit-level methods)
  if (!is.numeric(epsilon) || length(epsilon) != 1L || is.na(epsilon) || epsilon < 0 || epsilon >= 1) {
    stop("epsilon must be a single numeric value in [0, 1).", call. = FALSE)
  }

  # Compile spec (DO NOT pass mcmc here; spec/codegen is structural)
  spec <- compile_model_spec(
    y = y,
    X = X,
    ps = ps,
    backend = backend,
    kernel = kernel,
    GPD = GPD,
    components = components,
    param_specs = param_specs,
    alpha_random = alpha_random
  )

  code <- .wrap_nimble_code(build_code_from_spec(spec))

  bundle <- list(
    spec       = spec,
    code       = code,
    constants  = build_constants_from_spec(spec),
    dimensions = build_dimensions_from_spec(spec),
    data       = build_data_from_inputs(y = y, X = X, ps = ps),
    inits      = build_inits_from_spec(spec, y = y),
    monitors   = build_monitors_from_spec(spec),
    mcmc       = mcmc,
    epsilon    = epsilon
  )
  class(bundle) <- "dpmixgpd_bundle"
  bundle
}


# Internal helpers shared by SB and CRP code generators.
.codegen_prior_call <- function(dist, args, backend = "<codegen>") {
  dist <- as.character(dist)
  args <- args %||% list()
  backend <- as.character(backend %||% "<codegen>")

  if (dist == "normal") {
    m <- args$mean %||% 0
    s <- args$sd %||% 1
    return(sprintf("dnorm(%s, sd = %s)", deparse1(m), deparse1(s)))
  }
  if (dist == "gamma") {
    sh <- args$shape %||% 1
    rt <- args$rate %||% 1
    return(sprintf("dgamma(%s, %s)", deparse1(sh), deparse1(rt)))
  }
  if (dist == "invgamma") {
    sh <- args$shape %||% 1
    sc <- args$scale %||% 1
    return(sprintf("dinvgamma(%s, %s)", deparse1(sh), deparse1(sc)))
  }
  if (dist == "lognormal") {
    ml <- args$meanlog %||% 0
    sl <- args$sdlog %||% 1
    return(sprintf("dlnorm(meanlog = %s, sdlog = %s)", deparse1(ml), deparse1(sl)))
  }

  stop(sprintf("Unsupported prior dist '%s' in %s codegen.", dist, backend), call. = FALSE)
}

.codegen_link_expr <- function(eta, link, link_power = NULL) {
  link <- as.character(link %||% "identity")
  if (link == "identity") return(eta)
  if (link == "exp") return(sprintf("exp(%s)", eta))
  if (link == "log") return(sprintf("log(%s)", eta))
  if (link == "softplus") return(sprintf("log(1 + exp(%s))", eta))
  if (link == "power") {
    if (is.null(link_power) || length(link_power) != 1L || !is.finite(as.numeric(link_power))) {
      stop("power link requires numeric link_power.", call. = FALSE)
    }
    pw <- as.numeric(link_power)
    return(sprintf("pow(%s, %s)", eta, deparse1(pw)))
  }
  stop(sprintf("Unsupported link '%s'.", link), call. = FALSE)
}


#' Determine whether a compiled spec is conditional on covariates
#'
#' A spec is "conditional" if it uses covariates \code{X} (i.e., \code{has_X=TRUE})
#' and at least one parameter is specified in \code{link} mode.
#'
#' This helper is used in bundle validation and in deciding which builders to invoke.
#'
#' @param spec A compiled model specification produced by \code{compile_model_spec()}.
#' @return Logical; TRUE if the model uses covariate links, otherwise FALSE.
#' @keywords internal
#' @noRd
spec_requires_conditional <- function(spec) {
  stopifnot(is.list(spec), !is.null(spec$meta), !is.null(spec$plan))

  has_X <- isTRUE(spec$meta$has_X)
  plan <- spec$plan

  # No X means no conditional model (and link mode would be invalid).
  if (!has_X) {
    # defensive: ensure no link-mode sneaked in
    bulk <- plan$bulk %||% list()
    if (any(vapply(bulk, function(ent) identical(ent$mode, "link"), logical(1)))) {
      stop("Spec has link-mode bulk parameters but X is NULL.", call. = FALSE)
    }
    gpd <- plan$gpd %||% list()
    if (!is.null(gpd$threshold) && identical(gpd$threshold$mode, "link")) {
      stop("Spec has link-mode threshold but X is NULL.", call. = FALSE)
    }
    if (!is.null(gpd$tail_scale) && identical(gpd$tail_scale$mode, "link")) {
      stop("Spec has link-mode tail_scale but X is NULL.", call. = FALSE)
    }
    return(FALSE)
  }

  # With X present, conditional is TRUE iff any parameter uses link-mode.
  bulk <- plan$bulk %||% list()
  bulk_link <- any(vapply(bulk, function(ent) identical(ent$mode, "link"), logical(1)))

  gpd <- plan$gpd %||% list()
  thr_link <- !is.null(gpd$threshold) && identical(gpd$threshold$mode, "link")
  ts_link  <- !is.null(gpd$tail_scale) && identical(gpd$tail_scale$mode, "link")

  isTRUE(bulk_link || thr_link || ts_link)
}


#' Validate that code generation is supported for a compiled spec
#'
#' Checks that the kernel registry contains the required likelihood signatures
#' for the chosen backend and whether GPD is requested, and validates that any
#' link functions requested in \code{spec$plan} are supported by the code generator.
#'
#' @param spec A compiled model specification produced by \code{compile_model_spec()}.
#' @return Invisibly TRUE if all checks pass; otherwise errors.
#' @keywords internal
#' @noRd
assert_codegen_supported <- function(spec) {
  stopifnot(is.list(spec), !is.null(spec$meta), !is.null(spec$plan))

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  backend <- spec$meta$backend %||% NA_character_
  kernel  <- spec$meta$kernel  %||% NA_character_
  GPD     <- isTRUE(spec$meta$GPD)
  has_X   <- isTRUE(spec$meta$has_X)

  if (!backend %in% c("sb", "crp")) {
    stop(sprintf("Unsupported backend '%s'.", as.character(backend)), call. = FALSE)
  }

  krn <- get_kernel_registry()
  if (!kernel %in% names(krn)) {
    stop(sprintf("Unknown kernel '%s'.", as.character(kernel)), call. = FALSE)
  }

  kinfo <- krn[[kernel]]
  sigs <- kinfo$signatures %||% NULL
  if (is.null(sigs) || is.null(sigs[[backend]])) {
    stop(sprintf("Kernel '%s' does not provide signatures for backend '%s'.", kernel, backend), call. = FALSE)
  }

  if (GPD) {
    if (is.null(sigs[[backend]]$gpd) ||
        is.null(sigs[[backend]]$gpd$dist_name) ||
        is.null(sigs[[backend]]$gpd$args)) {
      stop(sprintf("Kernel '%s' is missing GPD signature for backend '%s'.", kernel, backend), call. = FALSE)
    }
  } else {
    if (is.null(sigs[[backend]]$bulk) ||
        is.null(sigs[[backend]]$bulk$dist_name) ||
        is.null(sigs[[backend]]$bulk$args)) {
      stop(sprintf("Kernel '%s' is missing bulk signature for backend '%s'.", kernel, backend), call. = FALSE)
    }
  }

  supported_links <- c("identity", "exp", "log", "softplus", "power")

  check_link <- function(where, ent) {
    if (!identical(ent$mode, "link")) return(invisible(TRUE))

    if (!has_X) stop(sprintf("%s uses link-mode but X is NULL.", where), call. = FALSE)

    lk <- ent$link %||% "identity"
    if (!lk %in% supported_links) {
      stop(sprintf("Unsupported link '%s' in %s. Supported: %s",
                   lk, where, paste(supported_links, collapse = ", ")),
           call. = FALSE)
    }
    if (lk == "power") {
      pw <- ent$link_power %||% NULL
      if (is.null(pw) || length(pw) != 1L || !is.finite(as.numeric(pw))) {
        stop(sprintf("power link in %s requires numeric link_power.", where), call. = FALSE)
      }
    }
    invisible(TRUE)
  }

  # bulk links
  bulk <- spec$plan$bulk %||% list()
  for (nm in names(bulk)) check_link(sprintf("bulk[%s]", nm), bulk[[nm]])

  # gpd links
  gpd <- spec$plan$gpd %||% list()
  if (!is.null(gpd$threshold))  check_link("gpd$threshold", gpd$threshold)
  if (!is.null(gpd$tail_scale)) check_link("gpd$tail_scale", gpd$tail_scale)

  invisible(TRUE)
}

#' Build default hypers used by internal priors
#'
#' @return Named list of hyperparameters.
#' @keywords internal
#' @noRd
default_hypers <- function() {
  list(
    alpha_shape = 1,
    alpha_rate  = 1,
    normal_mean = 0,
    normal_sd   = 10,
    gamma_shape = 2,
    gamma_rate  = 1
  )
}

#' Build NIMBLE data list from user inputs
#'
#' Converts user-provided outcome vector \code{y} and optional covariates \code{X}
#' into a NIMBLE-ready data list.
#'
#' Rules:
#' \itemize{
#'   \item \code{y} is always returned as a numeric vector.
#'   \item \code{X} is returned only if provided; it is coerced to a numeric matrix.
#'   \item No constants (N/P/components) are included here; those belong in \code{constants}.
#' }
#'
#' @param y Numeric outcome vector (length N).
#' @param X Optional covariate matrix/data.frame (N x P).
#' @param ps Optional numeric vector (length N) containing propensity scores.
#' @return Named list suitable to pass as \code{data} into \code{nimbleModel()}.
#' @keywords internal
#' @noRd
build_data_from_inputs <- function(y, X = NULL, ps = NULL) {
  y <- as.numeric(y)
  if (!length(y)) stop("y must be a non-empty numeric vector.", call. = FALSE)

  out <- list(y = y)

  if (!is.null(X)) {
    if (!is.matrix(X)) X <- as.matrix(X)
    # Coerce to numeric matrix (nimble expects numeric)
    storage.mode(X) <- "double"
    if (nrow(X) != length(y)) stop("X must have nrow(X) == length(y).", call. = FALSE)
    if (ncol(X) < 1L) stop("X must have at least one column.", call. = FALSE)
    out$X <- X
  }

  if (!is.null(ps)) {
    ps <- as.numeric(ps)
    if (length(ps) != length(y)) stop("ps must have the same length as y.", call. = FALSE)
    out$ps <- ps
  }

  out
}



#' Build default monitors from a compiled model spec
#'
#' Returns the character vector of node names to monitor in MCMC.
#' This is a pre-run builder used by \code{build_nimble_bundle()}.
#'
#' Monitoring follows these rules:
#' \itemize{
#'   \item Always monitor concentration \code{alpha} (whether fixed or stochastic).
#'   \item SB: monitor \code{w[1:components]} and optionally \code{v[1:(components-1)]}.
#'   \item CRP: monitor \code{z[1:N]}.
#'   \item Bulk parameters:
#'     \itemize{
#'       \item dist/fixed: monitor \code{<param>[1:components]}
#'       \item link: monitor \code{beta_<param>[1:components, 1:P]}
#'     }
#'   \item GPD (if enabled):
#'     \itemize{
#'       \item threshold: monitor scalar \code{threshold} when not link-mode; \code{threshold[1:N]} for link-mode
#'       \item if threshold is link-mode: monitor \code{beta_threshold[1:P]}
#'       \item if threshold uses LN link-dist default: monitor \code{sdlog_u}
#'       \item tail_scale: if link-mode, monitor \code{beta_tail_scale[1:P]}
#'       \item tail_shape: monitor scalar \code{tail_shape} (fixed or dist)
#'     }
#' }
#'
#' @param spec A compiled model specification produced by \code{compile_model_spec()}.
#' @param monitor_v Logical; for SB, whether to also monitor stick breaks \code{v}.
#' @return Character vector of node names to monitor.
#' @keywords internal
#' @noRd
build_monitors_from_spec <- function(spec, monitor_v = FALSE) {
  stopifnot(is.list(spec), !is.null(spec$meta), !is.null(spec$plan))

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  meta <- spec$meta
  plan <- spec$plan

  backend <- meta$backend
  N <- as.integer(meta$N)
  P <- as.integer(meta$P %||% 0L)
  # PS is optional: check if it's in the plan
  has_ps <- !is.null(plan$ps)
  K <- as.integer(meta$components)

  mons <- character()

  # Always monitor alpha (fixed alpha is still useful to carry in samples/prints)
  mons <- c(mons, "alpha")

  # BNP backbone
  if (identical(backend, "sb")) {
    mons <- c(mons, sprintf("w[1:%d]", K))
    mons <- c(mons, sprintf("z[1:%d]", N))
    if (isTRUE(monitor_v)) {
      mons <- c(mons, sprintf("v[1:%d]", K - 1L))
    }
  } else if (identical(backend, "crp")) {
    mons <- c(mons, sprintf("z[1:%d]", N))
  } else {
    stop("Unknown backend in spec$meta$backend.", call. = FALSE)
  }

  # Bulk parameters
  bulk <- plan$bulk %||% list()
  for (nm in names(bulk)) {
    ent <- bulk[[nm]]
    mode <- ent$mode %||% NA_character_

    if (mode %in% c("fixed", "dist")) {
      mons <- c(mons, sprintf("%s[1:%d]", nm, K))
    } else if (identical(mode, "link")) {
      if (P < 1L) stop(sprintf("bulk[%s] is link-mode but P=0.", nm), call. = FALSE)
      mons <- c(mons, sprintf("beta_%s[1:%d,1:%d]", nm, K, P))
      if (has_ps) {
        mons <- c(mons, sprintf("beta_ps_%s[1:%d]", nm, K))
      }
    } else {
      stop(sprintf("Invalid bulk plan mode for '%s'.", nm), call. = FALSE)
    }
  }

  # GPD
  if (isTRUE(meta$GPD)) {
    gpd <- plan$gpd %||% list()

    if (!is.null(gpd$threshold)) {
      thr_mode <- gpd$threshold$mode %||% NA_character_
      if (identical(thr_mode, "link")) {
        if (P < 1L) stop("GPD threshold is link-mode but P=0.", call. = FALSE)
        mons <- c(mons, sprintf("threshold[1:%d]", N))
        mons <- c(mons, sprintf("beta_threshold[1:%d]", P))

        # LN around-link default: monitor sdlog_u if present in plan
        if (!is.null(gpd$threshold$link_dist) &&
            identical(gpd$threshold$link_dist$dist, "lognormal")) {
          mons <- c(mons, "sdlog_u")
        }
      } else if (thr_mode %in% c("fixed", "dist")) {
        mons <- c(mons, "threshold")
      } else {
        stop("Invalid gpd$threshold mode.", call. = FALSE)
      }
    }

    # tail_scale
    if (!is.null(gpd$tail_scale)) {
      ts_mode <- gpd$tail_scale$mode %||% NA_character_
      if (identical(ts_mode, "link")) {
        if (P < 1L) stop("GPD tail_scale is link-mode but P=0.", call. = FALSE)
        mons <- c(mons, sprintf("beta_tail_scale[1:%d]", P))
      } else if (ts_mode %in% c("fixed", "dist")) {
        mons <- c(mons, "tail_scale")
      } else {
        stop("Invalid gpd$tail_scale mode.", call. = FALSE)
      }
    }

    # tail_shape: scalar (fixed or dist). Monitor for inference.
    if (!is.null(gpd$tail_shape)) {
      mons <- c(mons, "tail_shape")
    }
  }

  unique(mons)
}

#' Build initial values from a compiled model spec
#'
#' Produces a list of initial values suitable for passing to \code{nimbleModel}.
#' The initial values are derived from \code{spec$plan} and are intended to be
#' stable and support-respecting (e.g., positive parameters start positive).
#'
#' Notes:
#' \itemize{
#'   \item Uses only \code{components} as the model size parameter.
#'   \item SB: initializes stick breaks \code{v}; weights \code{w} are deterministic.
#'   \item CRP: initializes memberships \code{z} in \code{1:components}.
#'   \item Link-mode parameters initialize regression coefficients \code{beta_<param>}
#'         with shape \code{components x P}.
#'   \item Default GPD threshold under X is stochastic lognormal:
#'         initializes \code{threshold[1:N]} and scalar \code{sdlog_u} (non-link thresholds are scalar).
#' }
#'
#' @param spec A compiled model specification produced by \code{compile_model_spec()}.
#' @param seed Optional seed (single integer or vector). If provided, the first element is used.
#' @param y Optional numeric vector of observed outcomes used for heuristic initializations.
#' @return Named list of initial values.
#' @keywords internal
#' @noRd
build_inits_from_spec <- function(spec, seed = NULL, y = NULL) {
  stopifnot(is.list(spec), !is.null(spec$meta), !is.null(spec$plan))

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  if (!is.null(seed)) {
    seed <- as.integer(seed)
    if (length(seed) >= 1L && is.finite(seed[1L])) {
      set.seed(seed[1L])
    }
  }

  meta <- spec$meta
  plan <- spec$plan

  backend <- meta$backend
  N <- as.integer(meta$N)
  P <- as.integer(meta$P %||% 0L)
  has_ps <- !is.null(plan$ps)
  K <- as.integer(meta$components)
  y_obs <- if (!is.null(y)) as.numeric(y) else numeric()

  inits <- list()

  # ---- concentration alpha ----
  conc <- plan$concentration %||% list()
  if (identical(conc$mode, "dist")) {
    # alpha is stochastic -> needs init
    inits$alpha <- 1
  } else if (identical(conc$mode, "fixed")) {
    # alpha deterministic in code: alpha <- value
    # do not set inits$alpha to avoid conflicts
  } else {
    stop("Invalid plan$concentration$mode.", call. = FALSE)
  }

  # ---- BNP backbone ----
  if (identical(backend, "sb")) {
    # v[j] ~ dbeta(1, alpha); initialize in (0,1)
    if (K <= 2L) {
      inits$v <- runif(1L, 0.2, 0.8)
    } else {
      inits$v <- runif(K - 1L, 0.2, 0.8)
    }
    # w is deterministic from stick_breaking(v); do not init w
    K_init <- max(2L, min(K, 5L))
    inits$z <- sample.int(K_init, size = N, replace = TRUE)
  } else if (identical(backend, "crp")) {
    # z[1:N] ~ dCRP(...); init in 1:K, avoid all unique labels for stability
    K_init <- max(2L, min(K, 5L))
    inits$z <- sample.int(K_init, size = N, replace = TRUE)
  } else {
    stop("Unknown backend in spec$meta$backend.", call. = FALSE)
  }

  # ---- bulk parameters ----
  bulk <- plan$bulk %||% list()
  kinfo <- spec$kernel_info %||% list()
  ptypes <- kinfo$param_types %||% list()
  psupport <- kinfo$bulk_support %||% list()

  init_by_type <- function(type, support = NULL) {
    type <- as.character(type %||% "location")
    support <- as.character(support %||% "")
    if (support %in% c("positive_location", "positive_scale", "positive_shape", "positive_sd")) return(1)
    if (type == "location") return(0)
    if (type %in% c("scale", "shape", "sd")) return(1)
    0
  }

  for (nm in names(bulk)) {
    ent <- bulk[[nm]]
    mode <- ent$mode %||% NA_character_

    if (identical(mode, "fixed")) {
      # deterministic; no init
      next
    }

    if (identical(mode, "dist")) {
      val <- init_by_type(ptypes[[nm]], psupport[[nm]])
      inits[[nm]] <- rep(val, K)
      next
    }

    if (identical(mode, "link")) {
      if (P < 1L) stop(sprintf("bulk[%s] is link-mode but P=0.", nm), call. = FALSE)
      # beta_<nm>[1:K,1:P]
      inits[[paste0("beta_", nm)]] <- matrix(0, nrow = K, ncol = P)
      if (has_ps) {
        inits[[paste0("beta_ps_", nm)]] <- rep(0, K)
      }
      next
    }

    stop(sprintf("Invalid bulk plan mode for '%s'.", nm), call. = FALSE)
  }

  # ---- GPD parameters ----
  if (isTRUE(meta$GPD)) {
    gpd <- plan$gpd %||% list()

    # tail_shape (stochastic only)
    if (!is.null(gpd$tail_shape) && identical(gpd$tail_shape$mode, "dist")) {
      inits$tail_shape <- 0
    }

    # threshold
    if (!is.null(gpd$threshold)) {
      thr_mode <- gpd$threshold$mode %||% NA_character_

      if (thr_mode %in% c("fixed", "dist")) {
        inits$threshold <- 1
      } else if (identical(thr_mode, "link")) {
        if (P < 1L) stop("GPD threshold is link-mode but P=0.", call. = FALSE)
        inits$beta_threshold <- rep(0, P)

        # if LN around-link: threshold[i] is stochastic lognormal and sdlog_u exists
        if (!is.null(gpd$threshold$link_dist) &&
            identical(gpd$threshold$link_dist$dist, "lognormal")) {
          # positive threshold init; 0.8-quantile is usually safe and data-informed
          q <- if (length(y_obs)) {
            suppressWarnings(stats::quantile(y_obs, probs = 0.8, na.rm = TRUE, names = FALSE))
          } else {
            NA_real_
          }
          if (!is.finite(q) || length(q) != 1L) q <- 1
          q <- max(as.numeric(q), .Machine$double.eps)
          inits$threshold <- rep(q, N)
          inits$sdlog_u <- 0.2
        } else {
          # link-mode without link_dist: treat threshold deterministic from link later;
          # but our code currently represents threshold[i] as stochastic only in LN default.
          # Still initialize threshold to be safe if node exists.
          inits$threshold <- rep(1, N)
        }
      } else {
        stop("Invalid gpd$threshold mode.", call. = FALSE)
      }
    }

    # tail_scale
    if (!is.null(gpd$tail_scale)) {
      ts_mode <- gpd$tail_scale$mode %||% NA_character_
      if (identical(ts_mode, "link")) {
        if (P < 1L) stop("GPD tail_scale is link-mode but P=0.", call. = FALSE)
        inits$beta_tail_scale <- rep(0, P)
        # tail_scale[i] deterministic from beta_tail_scale; do not init tail_scale
      } else if (identical(ts_mode, "dist")) {
        # scalar stochastic tail_scale; init positive if node exists in code
        inits$tail_scale <- 1
      } else if (identical(ts_mode, "fixed")) {
        # deterministic; no init
      } else {
        stop("Invalid gpd$tail_scale mode.", call. = FALSE)
      }
    }
  }

  inits
}


#' Build constants list from a compiled model spec
#'
#' Produces a named list of constants to pass into \code{nimbleModel}.
#' Constants include core sizes (\code{N}, \code{P}, \code{components}) and
#' hyperparameters for priors implied by \code{spec$plan}.
#'
#' This function is pre-run only; it does not compile or execute NIMBLE.
#'
#' @param spec A compiled model specification produced by \code{compile_model_spec()}.
#' @return Named list of constants.
#' @keywords internal
#' @noRd
build_constants_from_spec <- function(spec) {
  stopifnot(is.list(spec), !is.null(spec$meta), !is.null(spec$plan))

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  meta <- spec$meta
  plan <- spec$plan
  has_ps <- !is.null(plan$ps)

  N <- as.integer(meta$N)
  P <- as.integer(meta$P %||% 0L)
  K <- as.integer(meta$components)
  default_ps_prior <- list(dist = "normal", args = list(mean = 0, sd = 2))
  ps_prior <- (plan$ps %||% list(prior = default_ps_prior))$prior

  const <- list(
    N = N,
    P = P,
    components = K
  )

  # ---- helper: register prior hypers in a uniform way ----
  # Supported priors here (as constants):
  # normal: mean, sd
  # gamma: shape, rate
  # invgamma: shape, scale
  # lognormal: meanlog, sdlog  (rare as prior, but allowed)
  add_prior_constants <- function(prefix, dist, args) {
    dist <- as.character(dist)
    args <- args %||% list()

    if (dist == "normal") {
      const[[paste0(prefix, "_mean")]] <- as.numeric(args$mean %||% 0)
      const[[paste0(prefix, "_sd")]]   <- as.numeric(args$sd %||% 1)
    } else if (dist == "gamma") {
      const[[paste0(prefix, "_shape")]] <- as.numeric(args$shape %||% 1)
      const[[paste0(prefix, "_rate")]]  <- as.numeric(args$rate %||% 1)
    } else if (dist == "invgamma") {
      const[[paste0(prefix, "_shape")]] <- as.numeric(args$shape %||% 1)
      const[[paste0(prefix, "_scale")]] <- as.numeric(args$scale %||% 1)
    } else if (dist == "lognormal") {
      const[[paste0(prefix, "_meanlog")]] <- as.numeric(args$meanlog %||% 0)
      const[[paste0(prefix, "_sdlog")]]   <- as.numeric(args$sdlog %||% 1)
    } else {
      stop(sprintf("Unsupported prior distribution '%s' for constants.", dist), call. = FALSE)
    }

    invisible(NULL)
  }

  # ---- concentration alpha ----
  conc <- plan$concentration %||% list()
  if (identical(conc$mode, "dist")) {
    add_prior_constants("alpha", conc$dist %||% "gamma", conc$args %||% list(shape = 1, rate = 1))
  } else if (identical(conc$mode, "fixed")) {
    # fixed alpha: no hypers; the code will set alpha <- value
  } else {
    stop("Invalid plan$concentration$mode.", call. = FALSE)
  }

  # ---- bulk priors ----
  bulk <- plan$bulk %||% list()
  for (nm in names(bulk)) {
    ent <- bulk[[nm]]
    mode <- ent$mode %||% NA_character_

    if (identical(mode, "dist")) {
      add_prior_constants(paste0("bulk_", nm), ent$dist, ent$args)
    } else if (identical(mode, "link")) {
      bp <- ent$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 2))
      add_prior_constants(paste0("beta_", nm), bp$dist %||% "normal", bp$args %||% list(mean = 0, sd = 2))
      if (has_ps) {
        add_prior_constants(paste0("beta_ps_", nm), ps_prior$dist %||% "normal",
                            ps_prior$args %||% list(mean = 0, sd = 2))
      }
    } else if (identical(mode, "fixed")) {
      # no hypers
    } else {
      stop(sprintf("Invalid bulk plan mode for '%s'.", nm), call. = FALSE)
    }
  }

  # ---- GPD priors ----
  if (isTRUE(meta$GPD)) {
    gpd <- plan$gpd %||% list()

    # threshold
    thr <- gpd$threshold %||% NULL
    if (!is.null(thr)) {
      thr_mode <- thr$mode %||% NA_character_

      if (identical(thr_mode, "dist")) {
        add_prior_constants("gpd_threshold", thr$dist, thr$args)
      } else if (identical(thr_mode, "link")) {
        # beta_threshold prior
        bp <- thr$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 0.2))
        add_prior_constants("beta_threshold", bp$dist %||% "normal", bp$args %||% list(mean = 0, sd = 0.2))

        # link_dist: if LN around-link default, sdlog_u prior exists in plan as gpd$sdlog_u
        if (!is.null(thr$link_dist) && identical(thr$link_dist$dist, "lognormal")) {
          sdlog_u <- gpd$sdlog_u %||% list(mode = "dist", dist = "invgamma", args = list(shape = 2, scale = 1))
          if (!identical(sdlog_u$mode, "dist")) {
            stop("sdlog_u must be dist-mode when using lognormal threshold link_dist.", call. = FALSE)
          }
          add_prior_constants("sdlog_u", sdlog_u$dist %||% "invgamma", sdlog_u$args %||% list(shape = 2, scale = 1))
        }
      } else if (identical(thr_mode, "fixed")) {
        # no hypers
      } else {
        stop("Invalid gpd$threshold mode.", call. = FALSE)
      }
    }

    # tail_scale
    ts <- gpd$tail_scale %||% NULL
    if (!is.null(ts)) {
      ts_mode <- ts$mode %||% NA_character_
      if (identical(ts_mode, "dist")) {
        add_prior_constants("gpd_tail_scale", ts$dist, ts$args)
      } else if (identical(ts_mode, "link")) {
        bp <- ts$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 0.5))
        add_prior_constants("beta_tail_scale", bp$dist %||% "normal", bp$args %||% list(mean = 0, sd = 0.5))
      } else if (identical(ts_mode, "fixed")) {
        # no hypers
      } else {
        stop("Invalid gpd$tail_scale mode.", call. = FALSE)
      }
    }

    # tail_shape
    tsh <- gpd$tail_shape %||% NULL
    if (!is.null(tsh)) {
      tsh_mode <- tsh$mode %||% NA_character_
      if (identical(tsh_mode, "dist")) {
        add_prior_constants("gpd_tail_shape", tsh$dist, tsh$args)
      } else if (identical(tsh_mode, "fixed")) {
        # no hypers
      } else {
        stop("Invalid gpd$tail_shape mode.", call. = FALSE)
      }
    }
  }

  const
}

#' Build dimension declarations from a compiled model spec
#'
#' Returns a named list of array dimensions used by downstream builders
#' (inits/monitors/code generation). Dimensions are derived solely from
#' \code{spec$meta} and \code{spec$plan}. This function does not inspect data.
#'
#' The model size is controlled by a single parameter: \code{components}.
#' For SB this is the truncation level of the stick-breaking mixture.
#' For CRP this is the maximum number of clusters represented in the finite model.
#'
#' @param spec A compiled model specification produced by \code{compile_model_spec()}.
#' @return Named list of dimensions (integer vectors). Scalars are omitted.
#' @keywords internal
#' @noRd
build_dimensions_from_spec <- function(spec) {
  stopifnot(is.list(spec), !is.null(spec$meta), !is.null(spec$plan))

  meta <- spec$meta
  plan <- spec$plan

  backend <- meta$backend
  N <- as.integer(meta$N)
  P <- as.integer(meta$P %||% 0L)
  has_ps <- !is.null(plan$ps)
  K <- as.integer(meta$components)

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  dims <- list()

  # --- BNP backbone dims ---
  if (identical(backend, "sb")) {
    # SB breaks v[1:(K-1)] and weights w[1:K]
    dims$v <- c(K - 1L)
    dims$w <- c(K)
    dims$z <- c(N)
  } else if (identical(backend, "crp")) {
    # CRP memberships z[1:N]
    dims$z <- c(N)
  } else {
    stop("Unknown backend in spec$meta$backend.", call. = FALSE)
  }

  # --- Bulk parameter dims ---
  bulk_plan <- plan$bulk %||% list()
  bulk_names <- names(bulk_plan)

  for (nm in bulk_names) {
    entry <- bulk_plan[[nm]]
    mode <- entry$mode %||% NA_character_

    # Component-level parameter vectors exist for fixed/dist modes.
    # For link mode, the component-level parameter is represented via beta_<nm>,
    # and per-(i,component) derived arrays are deterministic and not dimension-declared here.
    if (mode %in% c("fixed", "dist")) {
      dims[[nm]] <- c(K)
    }

    if (identical(mode, "link")) {
      # regression coefficients: beta_<param>[1:K, 1:P]
      if (P < 1L) stop(sprintf("Parameter '%s' is link-mode but P=0.", nm), call. = FALSE)
      dims[[paste0("beta_", nm)]] <- c(K, P)
      if (has_ps) {
        dims[[paste0("beta_ps_", nm)]] <- c(K)
      }
    }
  }

  # --- GPD dims (if enabled) ---
  if (isTRUE(meta$GPD)) {
    gpd_plan <- plan$gpd %||% list()

    # threshold
    thr <- gpd_plan$threshold %||% NULL
    if (!is.null(thr)) {
      thr_mode <- thr$mode %||% NA_character_

      if (thr_mode %in% c("fixed", "dist")) {
        # scalar threshold
      } else if (identical(thr_mode, "link")) {
        # threshold[i] stochastic LN around X beta
        dims$threshold <- c(N)
        if (P < 1L) stop("GPD threshold is link-mode but P=0.", call. = FALSE)
        if (P > 1L) dims$beta_threshold <- c(P)

        # if link_dist exists and uses sdlog_u, include its scalar node (no dims entry)
        # but if user chooses to model sdlog_u as a vector later, this would change.
        # For now: sdlog_u is scalar -> omitted from dims.
      } else {
        stop("Invalid gpd$threshold mode in plan.", call. = FALSE)
      }
    }

    # tail_scale
    ts <- gpd_plan$tail_scale %||% NULL
    if (!is.null(ts)) {
      ts_mode <- ts$mode %||% NA_character_

      if (ts_mode %in% c("fixed", "dist")) {
        # scalar tail_scale when not linked (most common non-X default)
        # (no dims entry for scalar)
      } else if (identical(ts_mode, "link")) {
        # tail_scale[i] is deterministic from X beta
        if (P < 1L) stop("GPD tail_scale is link-mode but P=0.", call. = FALSE)
        if (P > 1L) dims$beta_tail_scale <- c(P)
        # tail_scale[i] deterministic -> not dimension-declared
      } else {
        stop("Invalid gpd$tail_scale mode in plan.", call. = FALSE)
      }
    }

    # tail_shape
    tsh <- gpd_plan$tail_shape %||% NULL
    if (!is.null(tsh)) {
      tsh_mode <- tsh$mode %||% NA_character_
      if (!tsh_mode %in% c("fixed", "dist")) stop("Invalid gpd$tail_shape mode in plan.", call. = FALSE)
      # tail_shape scalar -> no dims entry
    }

    # sdlog_u (scalar by design; omitted from dims)
    # If you later allow sdlog_u[i], you'd add dims here.
  }

  dims
}

#' Build NIMBLE model code from a compiled model spec
#'
#' Dispatches to the backend-specific code generators:
#' \itemize{
#'   \item \code{build_code_sb_from_spec()} for stick-breaking (\code{"sb"})
#'   \item \code{build_code_crp_from_spec()} for CRP (\code{"crp"})
#' }
#'
#' The model size is controlled by \code{spec$meta$components} only.
#'
#' @param spec A compiled model specification produced by \code{compile_model_spec()}.
#' @return A \code{nimbleCode} object.
#' @keywords internal
#' @noRd
build_code_from_spec <- function(spec) {
  stopifnot(is.list(spec), !is.null(spec$meta), !is.null(spec$meta$backend))

  backend <- spec$meta$backend
  if (identical(backend, "sb")) {
    return(build_code_sb_from_spec(spec))
  }
  if (identical(backend, "crp")) {
    return(build_code_crp_from_spec(spec))
  }

  stop(sprintf("Unknown backend '%s' in spec$meta$backend.", as.character(backend)), call. = FALSE)
}


#' Build NIMBLE code for SB backend from a compiled spec
#'
#' Generates \code{nimbleCode} for the stick-breaking (SB) backend using native
#' NIMBLE BNP utilities:
#' \itemize{
#'   \item stick breaks \code{v[j] ~ dbeta(1, alpha)}
#'   \item weights computed via \code{v} stick multiplications (no call to \code{stick_breaking})
#' }
#'
#' Likelihood calls are emitted using positional arguments only (no names).
#'
#' @param spec A compiled model specification from \code{compile_model_spec()}.
#' @return A \code{nimbleCode} object.
#' @keywords internal
#' @noRd
# nocov start
build_code_sb_from_spec <- function(spec) {
  stopifnot(is.list(spec), !is.null(spec$meta), !is.null(spec$plan))

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  meta <- spec$meta
  plan <- spec$plan
  kinfo <- spec$kernel_info %||% list()
  sigs <- spec$signatures %||% list()

  if (!identical(meta$backend, "sb")) stop("spec backend is not 'sb'.", call. = FALSE)

  N <- as.integer(meta$N)
  P <- as.integer(meta$P %||% 0L)
  K <- as.integer(meta$components)
  has_X <- isTRUE(meta$has_X)
  has_ps <- !is.null(plan$ps)
  default_ps_prior <- list(dist = "normal", args = list(mean = 0, sd = 2))
  ps_prior <- (plan$ps %||% list(prior = default_ps_prior))$prior

  # ---- resolve single-component likelihood signature (uses latent z) ----
  dist_name <- NULL
  arg_order <- NULL
  if (isTRUE(meta$GPD)) {
    dist_name <- kinfo$crp$d_gpd %||% NULL
    arg_order <- kinfo$crp$args_gpd %||% NULL
    if (is.null(dist_name) || isTRUE(is.na(dist_name))) {
      dist_name <- kinfo$sb$d_gpd %||% NULL
      if (!is.null(dist_name)) dist_name <- sub("Mix", "", dist_name, fixed = TRUE)
    }
    if (is.null(arg_order) || anyNA(arg_order)) {
      arg_order <- kinfo$sb$args_gpd %||% NULL
    }
  } else {
    dist_name <- kinfo$crp$d_base %||% NULL
    arg_order <- kinfo$bulk_params %||% NULL
  }
  if (is.null(dist_name) || isTRUE(is.na(dist_name))) {
    stop("Missing SB single-component likelihood in kernel registry.", call. = FALSE)
  }
  if (is.null(arg_order) || !length(arg_order)) {
    stop("Missing SB likelihood argument order.", call. = FALSE)
  }
  if (any(arg_order == "w")) arg_order <- setdiff(arg_order, "w")

  bulk_plan <- plan$bulk %||% list()
  bulk_params <- kinfo$bulk_params %||% names(bulk_plan)
  bulk_link <- any(vapply(bulk_params, function(nm) {
    ent <- bulk_plan[[nm]]
    identical(ent$mode %||% NA_character_, "link")
  }, logical(1)))
  default_ps_prior <- list(dist = "normal", args = list(mean = 0, sd = 2))
  ps_prior <- (plan$ps %||% list(prior = default_ps_prior))$prior

  # ---- build nimbleCode ----
  nimble::nimbleCode({

    # --- concentration ---
    CONC_PLACEHOLDER()

    # --- stick-breaking ---
    for (j in 1:(components - 1)) {
      v[j] ~ dbeta(1, alpha)
    }
    stick_mass[1] <- 1
    for (j in 2:components) {
      stick_mass[j] <- stick_mass[j - 1] * (1 - v[j - 1])
    }
    for (j in 1:(components - 1)) {
      w[j] <- v[j] * stick_mass[j]
    }
    w[components] <- stick_mass[components]

    # --- bulk parameters (component-level + betas) ---
    for (j in 1:components) {

      # dist/fixed bulk params at component level
      # link-mode params are handled via beta blocks below
      # (we still loop j here to keep parameter declarations aligned)
      BULK_DECL_PLACEHOLDER()
    }

    # --- beta blocks for link-mode bulk params ---
    HASX_BETA_BLOCK()

    # --- build per-(i,j) linked bulk params as deterministic arrays ---
    HASX_DET_BLOCK()

    # --- GPD tail nodes (if requested) ---
    GPD_BLOCK()

    # --- likelihood ---
    LIKELIHOOD_BLOCK()
  }) -> code

  # ---- Now patch the code body programmatically (no placeholders left) ----
  # Convert nimbleCode to text, inject lines, then re-parse into nimbleCode.
  txt <- paste(deparse(code), collapse = "\n")

  inject <- function(pattern, replacement) {
    txt <<- sub(pattern, replacement, txt)
  }

  # (0) concentration
  conc <- plan$concentration %||% list()
  conc_line <- if (identical(conc$mode, "fixed")) {
    sprintf("alpha <- %s", deparse1(conc$value))
  } else if (identical(conc$mode, "dist")) {
    sprintf("alpha ~ %s", .codegen_prior_call(conc$dist, conc$args, backend = "SB"))
  } else {
    stop("Invalid plan$concentration$mode.", call. = FALSE)
  }
  inject("CONC_PLACEHOLDER\\(\\)", paste0(conc_line, "\n"))

  # (1) Bulk param declarations inside for (j in 1:components)
  bulk_decl_lines <- character()
  for (nm in bulk_params) {
    ent <- bulk_plan[[nm]]
    mode <- ent$mode %||% NA_character_
    if (mode == "fixed") {
      bulk_decl_lines <- c(bulk_decl_lines, sprintf("%s[j] <- %s", nm, deparse1(ent$value)))
    } else if (mode == "dist") {
      bulk_decl_lines <- c(bulk_decl_lines, sprintf("%s[j] ~ %s", nm,
                                                    .codegen_prior_call(ent$dist, ent$args, backend = "SB")))
    } else if (mode == "link") {
      bulk_decl_lines <- c(bulk_decl_lines, sprintf("# %s is link-mode (via beta_%s)", nm, nm))
    } else {
      stop(sprintf("Invalid bulk mode for '%s'.", nm), call. = FALSE)
    }
  }
  inject("BULK_DECL_PLACEHOLDER\\(\\)",
         paste0(paste(bulk_decl_lines, collapse = "\n      "), "\n"))

  # (2) Beta priors for link-mode bulk params
  beta_lines <- character()
  if (has_X) {
    for (nm in bulk_params) {
      ent <- bulk_plan[[nm]]
      if (identical(ent$mode, "link")) {
        bp <- ent$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 2))
        if (bp$dist != "normal") stop("Only normal beta priors are supported by default for link-mode betas.", call. = FALSE)
        m <- bp$args$mean %||% 0
        s <- bp$args$sd %||% 2
        beta_lines <- c(beta_lines, sprintf("for (p in 1:P) beta_%s[j, p] ~ dnorm(%s, sd = %s)", nm, deparse1(m), deparse1(s)))
        if (has_ps) {
          if (!identical(ps_prior$dist, "normal")) {
            stop("beta_ps priors must be normal for SB codegen.", call. = FALSE)
          }
          m_ps <- ps_prior$args$mean %||% 0
          s_ps <- ps_prior$args$sd %||% 2
          beta_lines <- c(beta_lines, sprintf("beta_ps_%s[j] ~ dnorm(%s, sd = %s)", nm, deparse1(m_ps), deparse1(s_ps)))
        }
      }
    }
  }
  beta_block <- if (has_X && length(beta_lines)) {
    paste0("for (j in 1:components) {\n",
           "      ", paste(beta_lines, collapse = "\n      "), "\n",
           "    }\n")
  } else {
    ""
  }
  inject("HASX_BETA_BLOCK\\(\\)", beta_block)

  # (3) Deterministic linked bulk param_ij
  det_lines <- character()
  if (has_X) {
    for (nm in bulk_params) {
      ent <- bulk_plan[[nm]]
      if (identical(ent$mode, "link")) {
        eta_terms <- character()
        if (P == 1L) {
          eta_terms <- c(eta_terms, sprintf("X[i, 1] * beta_%s[j, 1]", nm))
        } else {
          eta_terms <- c(eta_terms, sprintf("inprod(X[i, 1:P], beta_%s[j, 1:P])", nm))
        }
        if (has_ps) {
          eta_terms <- c(eta_terms, sprintf("ps[i] * beta_ps_%s[j]", nm))
        }
        if (!length(eta_terms)) stop(sprintf("Unable to build eta for '%s'.", nm), call. = FALSE)
        eta <- paste(eta_terms, collapse = " + ")
        expr <- .codegen_link_expr(eta, ent$link, ent$link_power)
        det_lines <- c(det_lines, sprintf("%s_ij[i, j] <- %s", nm, expr))
      }
    }
  }
  det_block <- if (has_X && length(det_lines)) {
    paste0("for (i in 1:N) {\n",
           "      for (j in 1:components) {\n",
           "        ", paste(det_lines, collapse = "\n        "), "\n",
           "      }\n",
           "    }\n")
  } else {
    ""
  }
  inject("HASX_DET_BLOCK\\(\\)", det_block)

  # (4) GPD blocks
  gpd_lines <- character()
  if (isTRUE(meta$GPD)) {
    gpd <- plan$gpd %||% list()

    # threshold
    thr <- gpd$threshold %||% NULL
    if (!is.null(thr)) {
      thr_scalar <- thr$mode %in% c("fixed", "dist")
      if (thr$mode == "fixed") {
        if (thr_scalar) {
          gpd_lines <- c(gpd_lines, sprintf("threshold <- %s", deparse1(thr$value)))
        } else {
          gpd_lines <- c(gpd_lines, sprintf("for (i in 1:N) threshold[i] <- %s", deparse1(thr$value)))
        }
      } else if (thr$mode == "dist") {
        if (thr_scalar) {
          gpd_lines <- c(gpd_lines, sprintf("threshold ~ %s",
                                            .codegen_prior_call(thr$dist, thr$args, backend = "SB")))
        } else {
          gpd_lines <- c(gpd_lines, sprintf("for (i in 1:N) threshold[i] ~ %s",
                                            .codegen_prior_call(thr$dist, thr$args, backend = "SB")))
        }
      } else if (thr$mode == "link") {
        if (!has_X) stop("threshold link-mode requires X.", call. = FALSE)
        bp <- thr$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 0.2))
        m <- bp$args$mean %||% 0
        s <- bp$args$sd %||% 0.2
        gpd_lines <- c(gpd_lines, sprintf("for (p in 1:P) beta_threshold[p] ~ dnorm(%s, sd = %s)", deparse1(m), deparse1(s)))

        if (!is.null(thr$link_dist) && identical(thr$link_dist$dist, "lognormal")) {
          sdlog_u <- gpd$sdlog_u %||% list(mode = "dist", dist = "invgamma", args = list(shape = 2, scale = 1))
          if (!identical(sdlog_u$mode, "dist")) stop("sdlog_u must be dist-mode under lognormal threshold.", call. = FALSE)
          gpd_lines <- c(gpd_lines, sprintf("sdlog_u ~ %s",
                                            .codegen_prior_call(sdlog_u$dist, sdlog_u$args, backend = "SB")))
          eta_u_line <- if (P == 1L) "  eta_u[i] <- X[i, 1] * beta_threshold[1]" else
            "  eta_u[i] <- inprod(X[i, 1:P], beta_threshold[1:P])"
          gpd_lines <- c(gpd_lines, "for (i in 1:N) {",
                         eta_u_line,
                         "  threshold[i] ~ dlnorm(meanlog = eta_u[i], sdlog = sdlog_u)",
                         "}")
        } else {
          eta_u_line <- if (P == 1L) "  eta_u[i] <- X[i, 1] * beta_threshold[1]" else
            "  eta_u[i] <- inprod(X[i, 1:P], beta_threshold[1:P])"
          gpd_lines <- c(gpd_lines, "for (i in 1:N) {",
                         eta_u_line,
                         sprintf("  threshold[i] <- %s",
                                 .codegen_link_expr("eta_u[i]", thr$link, thr$link_power)),
                         "}")
        }
      } else {
        stop("Invalid gpd threshold mode.", call. = FALSE)
      }
    }

    # tail_scale
    ts <- gpd$tail_scale %||% NULL
    if (!is.null(ts)) {
      if (ts$mode == "fixed") {
        gpd_lines <- c(gpd_lines, sprintf("tail_scale <- %s", deparse1(ts$value)))
      } else if (ts$mode == "dist") {
        gpd_lines <- c(gpd_lines, sprintf("tail_scale ~ %s",
                                          .codegen_prior_call(ts$dist, ts$args, backend = "SB")))
      } else if (ts$mode == "link") {
        if (!has_X) stop("tail_scale link-mode requires X.", call. = FALSE)
        bp <- ts$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 0.5))
        m <- bp$args$mean %||% 0
        s <- bp$args$sd %||% 0.5
        gpd_lines <- c(gpd_lines, sprintf("for (p in 1:P) beta_tail_scale[p] ~ dnorm(%s, sd = %s)", deparse1(m), deparse1(s)))
        eta_ts_line <- if (P == 1L) "  eta_ts[i] <- X[i, 1] * beta_tail_scale[1]" else
          "  eta_ts[i] <- inprod(X[i, 1:P], beta_tail_scale[1:P])"
        gpd_lines <- c(gpd_lines, "for (i in 1:N) {",
                       eta_ts_line,
                       "  tail_scale[i] <- exp(eta_ts[i])",
                       "}")
      } else {
        stop("Invalid gpd tail_scale mode.", call. = FALSE)
      }
    }

    # tail_shape
    tsh <- gpd$tail_shape %||% NULL
    if (!is.null(tsh)) {
      if (tsh$mode == "fixed") {
        gpd_lines <- c(gpd_lines, sprintf("tail_shape <- %s", deparse1(tsh$value)))
      } else if (tsh$mode == "dist") {
        gpd_lines <- c(gpd_lines, sprintf("tail_shape ~ %s",
                                          .codegen_prior_call(tsh$dist, tsh$args, backend = "SB")))
      } else {
        stop("Invalid gpd tail_shape mode.", call. = FALSE)
      }
    }
  }

  inject("GPD_BLOCK\\(\\)",
         if (length(gpd_lines)) paste0(paste(gpd_lines, collapse = "\n    "), "\n") else "")

  # (5) Likelihood call
  like_lines <- character()
  gpd_for_args <- plan$gpd %||% list()
  thr_for_args <- gpd_for_args$threshold %||% NULL
  thr_scalar <- !is.null(thr_for_args) && thr_for_args$mode %in% c("fixed", "dist")
  args_expr <- character()
  for (a in arg_order) {
    if (a %in% bulk_params) {
      ent <- bulk_plan[[a]]
      if (identical(ent$mode, "link")) {
        args_expr <- c(args_expr, sprintf("%s_ij[i, z[i]]", a))
      } else {
        args_expr <- c(args_expr, sprintf("%s[z[i]]", a))
      }
    } else if (a == "threshold") {
      args_expr <- c(args_expr, if (thr_scalar) "threshold" else "threshold[i]")
    } else if (a == "tail_scale") {
      ts <- plan$gpd$tail_scale %||% NULL
      if (!is.null(ts) && identical(ts$mode, "link")) args_expr <- c(args_expr, "tail_scale[i]") else args_expr <- c(args_expr, "tail_scale")
    } else if (a == "tail_shape") {
      args_expr <- c(args_expr, "tail_shape")
    } else {
      stop(sprintf("Unknown argument '%s' in SB signature for kernel '%s'.", a, meta$kernel), call. = FALSE)
    }
  }

  like_lines <- c(
    "z[i] ~ dcat(prob = w[1:components])",
    sprintf("y[i] ~ %s(%s)", dist_name, paste(args_expr, collapse = ", "))
  )
  like_block <- paste0("for (i in 1:N) {\n",
                       "      ", paste(like_lines, collapse = "\n      "), "\n",
                       "    }\n")
  inject("LIKELIHOOD_BLOCK\\(\\)", like_block)

  # Rebuild nimbleCode from modified text without evaluating model symbols.
  expr <- parse(text = txt)[[1]]

  # IMPORTANT: nimbleCode uses NSE; use do.call to pass evaluated expression, not a captured call.
  do.call(nimble::nimbleCode, list(expr))

}


#' Build NIMBLE code for CRP backend from a compiled spec
#'
#' Generates \code{nimbleCode} for the Chinese Restaurant Process (CRP) backend
#' using native NIMBLE BNP distribution:
#' \itemize{
#'   \item memberships: \code{z[1:N] ~ dCRP(conc = alpha, size = N)}
#' }
#'
#' The finite represented number of clusters is controlled by a single parameter
#' \code{components}. Component-specific parameters are declared for
#' \code{k = 1:components}.
#'
#' Bulk parameters follow the tri-mode plan:
#' \itemize{
#'   \item fixed: \code{param[k] <- value}
#'   \item dist:  \code{param[k] ~ d<prior>(...)}
#'   \item link:  \code{beta_param[k,p] ~ dnorm(...)} and
#'         \code{param_ik[i,k] <- g(inprod(X[i,], beta_param[k,]))}
#' }
#'
#' If \code{GPD=TRUE}, the default tail under \code{X} is:
#' \itemize{
#'   \item \code{threshold[i] ~ dlnorm(meanlog = inprod(X[i,], beta_threshold), sdlog = sdlog_u)}
#'   \item \code{tail_scale[i] <- exp(inprod(X[i,], beta_tail_scale))}
#'   \item \code{tail_shape ~ dnorm(0, sd = 0.2)} (unless fixed)
#' }
#'
#' Likelihood calls are emitted using positional arguments only (no names).
#'
#' @param spec A compiled model specification from \code{compile_model_spec()}.
#' @return A \code{nimbleCode} object.
#' @keywords internal
#' @noRd
build_code_crp_from_spec <- function(spec) {
  stopifnot(is.list(spec), !is.null(spec$meta), !is.null(spec$plan))

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  meta <- spec$meta
  plan <- spec$plan
  kinfo <- spec$kernel_info %||% list()
  sigs <- spec$signatures %||% list()

  if (!identical(meta$backend, "crp")) stop("spec backend is not 'crp'.", call. = FALSE)

  N <- as.integer(meta$N)
  P <- as.integer(meta$P %||% 0L)
  K <- as.integer(meta$components)
  has_X <- isTRUE(meta$has_X)
  has_ps <- !is.null(plan$ps)
  default_ps_prior <- list(dist = "normal", args = list(mean = 0, sd = 2))
  ps_prior <- (plan$ps %||% list(prior = default_ps_prior))$prior

  # ---- resolve likelihood signature ----
  dist_name <- NULL
  arg_order <- NULL
  if (isTRUE(meta$GPD)) {
    dist_name <- sigs$gpd$dist_name %||% NULL
    arg_order <- sigs$gpd$args %||% NULL
  } else {
    dist_name <- sigs$bulk$dist_name %||% NULL
    arg_order <- sigs$bulk$args %||% NULL
  }
  if (is.null(dist_name) || is.null(arg_order) || !length(arg_order)) {
    stop("Missing CRP likelihood signature in spec$signatures.", call. = FALSE)
  }

  bulk_plan <- plan$bulk %||% list()
  bulk_params <- kinfo$bulk_params %||% names(bulk_plan)
  bulk_link <- any(vapply(bulk_params, function(nm) {
    ent <- bulk_plan[[nm]]
    identical(ent$mode %||% NA_character_, "link")
  }, logical(1)))

  # ---- assemble model code as text ----
  lines <- character()
  add <- function(...) lines <<- c(lines, ...)

  # concentration
  conc <- plan$concentration %||% list()
  if (identical(conc$mode, "fixed")) {
    add(sprintf("  alpha <- %s", deparse1(conc$value)))
  } else if (identical(conc$mode, "dist")) {
    add(sprintf("  alpha ~ %s", .codegen_prior_call(conc$dist, conc$args, backend = "CRP")))
  } else {
    stop("Invalid plan$concentration$mode.", call. = FALSE)
  }

  # CRP memberships
  add("  z[1:N] ~ dCRP(conc = alpha, size = N)")

  # bulk component-level declarations
  add("  for (k in 1:components) {")
  for (nm in bulk_params) {
    ent <- bulk_plan[[nm]]
    mode <- ent$mode %||% NA_character_

    if (mode == "fixed") {
      add(sprintf("    %s[k] <- %s", nm, deparse1(ent$value)))
    } else if (mode == "dist") {
      add(sprintf("    %s[k] ~ %s", nm,
                  .codegen_prior_call(ent$dist, ent$args, backend = "CRP")))
    } else if (mode == "link") {
      add(sprintf("    # %s is link-mode (via beta_%s)", nm, nm))
    } else {
      stop(sprintf("Invalid bulk mode for '%s'.", nm), call. = FALSE)
    }
  }
  add("  }")

  # beta priors for link-mode bulk params
  if (has_X) {
    for (nm in bulk_params) {
      ent <- bulk_plan[[nm]]
      if (identical(ent$mode, "link")) {
        bp <- ent$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 2))
        if (!identical(bp$dist, "normal")) {
          stop("Only normal beta priors are supported by default for link-mode betas.", call. = FALSE)
        }
        m <- bp$args$mean %||% 0
        s <- bp$args$sd %||% 2
        add("  for (k in 1:components) {")
        add(sprintf("    for (p in 1:P) beta_%s[k, p] ~ dnorm(%s, sd = %s)", nm, deparse1(m), deparse1(s)))
        if (has_ps) {
          if (!identical(ps_prior$dist, "normal")) {
            stop("beta_ps priors must be normal for CRP codegen.", call. = FALSE)
          }
          m_ps <- ps_prior$args$mean %||% 0
          s_ps <- ps_prior$args$sd %||% 2
          add(sprintf("    beta_ps_%s[k] ~ dnorm(%s, sd = %s)", nm, deparse1(m_ps), deparse1(s_ps)))
        }
        add("  }")
      }
    }

    # deterministic param_ik for link-mode bulk params
    if (bulk_link) {
      add("  for (i in 1:N) {")
      add("    for (k in 1:components) {")
      for (nm in bulk_params) {
        ent <- bulk_plan[[nm]]
        if (identical(ent$mode, "link")) {
          eta_terms <- character()
          if (P == 1L) {
            eta_terms <- c(eta_terms, sprintf("X[i, 1] * beta_%s[k, 1]", nm))
          } else {
            eta_terms <- c(eta_terms, sprintf("inprod(X[i, 1:P], beta_%s[k, 1:P])", nm))
          }
          if (has_ps) {
            eta_terms <- c(eta_terms, sprintf("ps[i] * beta_ps_%s[k]", nm))
          }
          if (!length(eta_terms)) {
            stop(sprintf("Unable to build eta for '%s'.", nm), call. = FALSE)
          }
          eta <- paste(eta_terms, collapse = " + ")
          expr <- .codegen_link_expr(eta, ent$link, ent$link_power)
          add(sprintf("      %s_ik[i, k] <- %s", nm, expr))
        }
      }
      add("    }")
      add("  }")
    }
  }

  # ---- GPD tail blocks ----
  if (isTRUE(meta$GPD)) {
    gpd <- plan$gpd %||% list()

    # threshold
    thr <- gpd$threshold %||% NULL
    if (!is.null(thr)) {
      thr_scalar <- thr$mode %in% c("fixed", "dist")
      if (thr$mode == "fixed") {
        if (thr_scalar) {
          add(sprintf("  threshold <- %s", deparse1(thr$value)))
        } else {
          add(sprintf("  for (i in 1:N) threshold[i] <- %s", deparse1(thr$value)))
        }
      } else if (thr$mode == "dist") {
        if (thr_scalar) {
          add(sprintf("  threshold ~ %s",
                      .codegen_prior_call(thr$dist, thr$args, backend = "CRP")))
        } else {
          add(sprintf("  for (i in 1:N) threshold[i] ~ %s",
                      .codegen_prior_call(thr$dist, thr$args, backend = "CRP")))
        }
      } else if (thr$mode == "link") {
        if (!has_X) stop("threshold link-mode requires X.", call. = FALSE)
        bp <- thr$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 0.2))
        if (!identical(bp$dist, "normal")) stop("beta_threshold prior must be normal.", call. = FALSE)
        m <- bp$args$mean %||% 0
        s <- bp$args$sd %||% 0.2
        add(sprintf("  for (p in 1:P) beta_threshold[p] ~ dnorm(%s, sd = %s)", deparse1(m), deparse1(s)))

        if (!is.null(thr$link_dist) && identical(thr$link_dist$dist, "lognormal")) {
          sdlog_u <- gpd$sdlog_u %||% list(mode = "dist", dist = "invgamma", args = list(shape = 2, scale = 1))
          if (!identical(sdlog_u$mode, "dist")) stop("sdlog_u must be dist-mode under lognormal threshold.", call. = FALSE)
          add(sprintf("  sdlog_u ~ %s",
                      .codegen_prior_call(sdlog_u$dist, sdlog_u$args, backend = "CRP")))
          add("  for (i in 1:N) {")
          if (P == 1L) {
            add("    eta_u[i] <- X[i, 1] * beta_threshold[1]")
          } else {
            add("    eta_u[i] <- inprod(X[i, 1:P], beta_threshold[1:P])")
          }
          add("    threshold[i] ~ dlnorm(meanlog = eta_u[i], sdlog = sdlog_u)")
          add("  }")
        } else {
          add("  for (i in 1:N) {")
          if (P == 1L) {
            add("    eta_u[i] <- X[i, 1] * beta_threshold[1]")
          } else {
            add("    eta_u[i] <- inprod(X[i, 1:P], beta_threshold[1:P])")
          }
          add(sprintf("    threshold[i] <- %s", .codegen_link_expr("eta_u[i]", thr$link, thr$link_power)))
          add("  }")
        }
      } else {
        stop("Invalid gpd threshold mode.", call. = FALSE)
      }
    }

    # tail_scale
    ts <- gpd$tail_scale %||% NULL
    if (!is.null(ts)) {
      if (ts$mode == "fixed") {
        add(sprintf("  tail_scale <- %s", deparse1(ts$value)))
      } else if (ts$mode == "dist") {
        add(sprintf("  tail_scale ~ %s",
                    .codegen_prior_call(ts$dist, ts$args, backend = "CRP")))
      } else if (ts$mode == "link") {
        if (!has_X) stop("tail_scale link-mode requires X.", call. = FALSE)
        bp <- ts$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 0.5))
        if (!identical(bp$dist, "normal")) stop("beta_tail_scale prior must be normal.", call. = FALSE)
        m <- bp$args$mean %||% 0
        s <- bp$args$sd %||% 0.5
        add(sprintf("  for (p in 1:P) beta_tail_scale[p] ~ dnorm(%s, sd = %s)", deparse1(m), deparse1(s)))
        add("  for (i in 1:N) {")
        if (P == 1L) {
          add("    eta_ts[i] <- X[i, 1] * beta_tail_scale[1]")
        } else {
          add("    eta_ts[i] <- inprod(X[i, 1:P], beta_tail_scale[1:P])")
        }
        add("    tail_scale[i] <- exp(eta_ts[i])")
        add("  }")
      } else {
        stop("Invalid gpd tail_scale mode.", call. = FALSE)
      }
    }

    # tail_shape
    tsh <- gpd$tail_shape %||% NULL
    if (!is.null(tsh)) {
      if (tsh$mode == "fixed") {
        add(sprintf("  tail_shape <- %s", deparse1(tsh$value)))
      } else if (tsh$mode == "dist") {
        add(sprintf("  tail_shape ~ %s",
                    .codegen_prior_call(tsh$dist, tsh$args, backend = "CRP")))
      } else {
        stop("Invalid gpd tail_shape mode.", call. = FALSE)
      }
    }
  }

  # ---- likelihood ----
  add("  for (i in 1:N) {")

  # build arg expressions in signature order
  gpd_for_args <- plan$gpd %||% list()
  thr_for_args <- gpd_for_args$threshold %||% NULL
  thr_scalar <- !is.null(thr_for_args) && thr_for_args$mode %in% c("fixed", "dist")
  args_expr <- character()
  for (a in arg_order) {
    if (a %in% bulk_params) {
      ent <- bulk_plan[[a]]
      if (identical(ent$mode, "link")) {
        args_expr <- c(args_expr, sprintf("%s_ik[i, z[i]]", a))
      } else {
        args_expr <- c(args_expr, sprintf("%s[z[i]]", a))
      }
    } else if (a == "threshold") {
      args_expr <- c(args_expr, if (thr_scalar) "threshold" else "threshold[i]")
    } else if (a == "tail_scale") {
      ts <- plan$gpd$tail_scale %||% NULL
      if (!is.null(ts) && identical(ts$mode, "link")) {
        args_expr <- c(args_expr, "tail_scale[i]")
      } else {
        args_expr <- c(args_expr, "tail_scale")
      }
    } else if (a == "tail_shape") {
      args_expr <- c(args_expr, "tail_shape")
    } else {
      stop(sprintf("Unknown argument '%s' in CRP signature for kernel '%s'.", a, meta$kernel), call. = FALSE)
    }
  }

  add(sprintf("    y[i] ~ %s(%s)", dist_name, paste(args_expr, collapse = ", ")))
  add("  }")

  # parse into nimbleCode
  code_str <- paste(lines, collapse = "\n")
  expr <- parse(text = paste0("{\n", code_str, "\n}"))[[1]]
  nimble::nimbleCode(expr)
}
# nocov end

#' Validate a dpmixgpd bundle
#'
#' Performs internal consistency checks on a bundle created by \code{build_nimble_bundle()}.
#' This is a pre-run validation step (it does not compile or run NIMBLE).
#'
#' @param bundle A bundle object of class \code{"dpmixgpd_bundle"}.
#' @return Invisibly TRUE if valid; otherwise errors.
#' @keywords internal
#' @noRd
check_dpmixgpd_bundle <- function(bundle) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  if (!is.list(bundle) || !inherits(bundle, "dpmixgpd_bundle")) {
    stop("bundle must be an object of class 'dpmixgpd_bundle'.", call. = FALSE)
  }

  req <- c("spec", "code", "constants", "dimensions", "data", "inits", "monitors")
  miss <- setdiff(req, names(bundle))
  if (length(miss)) stop(sprintf("bundle is missing fields: %s", paste(miss, collapse = ", ")), call. = FALSE)

  spec <- bundle$spec
  if (is.null(spec$meta)) spec$meta <- list()
  spec$meta$epsilon <- bundle$epsilon %||% spec$meta$epsilon
  if (is.null(spec$meta) || is.null(spec$plan)) stop("bundle$spec is missing meta/plan.", call. = FALSE)

  meta <- spec$meta
  plan <- spec$plan

  backend <- meta$backend %||% NA_character_
  if (!backend %in% c("sb", "crp")) stop("spec$meta$backend must be 'sb' or 'crp'.", call. = FALSE)

  N <- as.integer(meta$N %||% NA_integer_)
  P <- as.integer(meta$P %||% 0L)
  K <- as.integer(meta$components %||% NA_integer_)
  if (!is.finite(N) || N < 1L) stop("spec$meta$N is invalid.", call. = FALSE)
  if (!is.finite(K) || K < 2L) stop("spec$meta$components must be >= 2.", call. = FALSE)

  # constants agreement
  if (is.null(bundle$constants$components) || as.integer(bundle$constants$components) != K) {
    stop("constants$components does not match spec$meta$components.", call. = FALSE)
  }
  if (is.null(bundle$constants$N) || as.integer(bundle$constants$N) != N) {
    stop("constants$N does not match spec$meta$N.", call. = FALSE)
  }
  if (P > 0L) {
    if (is.null(bundle$constants$P) || as.integer(bundle$constants$P) != P) {
      stop("constants$P does not match spec$meta$P.", call. = FALSE)
    }
  }

  dims <- bundle$dimensions %||% list()
  mons <- bundle$monitors %||% character()

  # block legacy weight name
  if (any(grepl("^weights\\[", mons))) {
    stop("Monitors still contain legacy 'weights[...]'. Use 'w[...]' for SB.", call. = FALSE)
  }

  # backend-specific dims
  if (backend == "sb") {
    if (is.null(dims$w) || !identical(as.integer(dims$w), c(K))) stop("SB requires dims$w = components.", call. = FALSE)
    if (is.null(dims$v) || !identical(as.integer(dims$v), c(K - 1L))) stop("SB requires dims$v = components-1.", call. = FALSE)
    if (is.null(dims$z) || !identical(as.integer(dims$z), c(N))) stop("SB requires dims$z = N.", call. = FALSE)
    if (!any(grepl("^w\\[", mons))) stop("SB monitors should include w[...].", call. = FALSE)
    if (!any(grepl("^z\\[", mons))) stop("SB monitors should include z[...].", call. = FALSE)
  } else {
    if (is.null(dims$z) || !identical(as.integer(dims$z), c(N))) stop("CRP requires dims$z = N.", call. = FALSE)
    if (!any(grepl("^z\\[", mons))) stop("CRP monitors should include z[...].", call. = FALSE)
  }

  # X consistency
  has_X <- isTRUE(meta$has_X)
  if (has_X) {
    if (is.null(bundle$data$X)) stop("spec indicates has_X=TRUE but bundle$data$X is NULL.", call. = FALSE)
    X <- bundle$data$X
    if (!is.matrix(X)) stop("bundle$data$X must be a matrix.", call. = FALSE)
    if (nrow(X) != N) stop("bundle$data$X has wrong number of rows.", call. = FALSE)
    if (ncol(X) != P) stop("bundle$data$X has wrong number of columns.", call. = FALSE)
  } else {
    if (!is.null(bundle$data$X)) stop("spec indicates has_X=FALSE but bundle$data$X is provided.", call. = FALSE)
  }

  # link-mode implies beta dims exist and match (K,P)
  bulk <- plan$bulk %||% list()
  for (nm in names(bulk)) {
    ent <- bulk[[nm]]
    if (identical(ent$mode, "link")) {
      if (!has_X) stop(sprintf("bulk[%s] link-mode but has_X=FALSE.", nm), call. = FALSE)
      bnm <- paste0("beta_", nm)
      if (is.null(dims[[bnm]]) || !identical(as.integer(dims[[bnm]]), c(K, P))) {
        stop(sprintf("dims$%s must be c(components, P).", bnm), call. = FALSE)
      }
    }
  }

  # GPD checks
  if (isTRUE(meta$GPD)) {
    gpd <- plan$gpd %||% list()

    if (!is.null(gpd$threshold)) {
      thr_mode <- gpd$threshold$mode %||% NA_character_
      if (thr_mode %in% c("fixed", "dist")) {
        if (!is.null(dims$threshold)) stop("GPD threshold should be scalar when not link-mode.", call. = FALSE)
        if (!any(mons == "threshold")) stop("Monitors should include threshold.", call. = FALSE)
      } else if (identical(thr_mode, "link")) {
        if (!has_X) stop("threshold link-mode but has_X=FALSE.", call. = FALSE)
        if (is.null(dims$threshold) || !identical(as.integer(dims$threshold), c(N))) {
          stop("GPD threshold requires dims$threshold = N under link-mode.", call. = FALSE)
        }
        if (!any(grepl("^threshold\\[", mons))) stop("Monitors should include threshold[1:N].", call. = FALSE)
        if (P > 1L) {
          if (is.null(dims$beta_threshold) || !identical(as.integer(dims$beta_threshold), c(P))) {
            stop("threshold link-mode requires dims$beta_threshold = P.", call. = FALSE)
          }
        }
        if (!any(grepl("^beta_threshold\\[", mons))) stop("Monitors should include beta_threshold[1:P].", call. = FALSE)

        if (!is.null(gpd$threshold$link_dist) && identical(gpd$threshold$link_dist$dist, "lognormal")) {
          if (!any(mons == "sdlog_u")) stop("LN threshold requires monitoring sdlog_u.", call. = FALSE)
        }
      } else {
        stop("Invalid gpd$threshold mode.", call. = FALSE)
      }
    }

    if (!is.null(gpd$tail_scale) && identical(gpd$tail_scale$mode, "link")) {
      if (P > 1L) {
        if (is.null(dims$beta_tail_scale) || !identical(as.integer(dims$beta_tail_scale), c(P))) {
          stop("tail_scale link-mode requires dims$beta_tail_scale = P.", call. = FALSE)
        }
      }
      if (!any(grepl("^beta_tail_scale\\[", mons))) stop("Monitors should include beta_tail_scale[1:P].", call. = FALSE)
    }

    if (!any(mons == "tail_shape")) stop("GPD should monitor tail_shape.", call. = FALSE)
  }

  # Ensure codegen is supported for the selected kernel/backend/GPD/signatures
  assert_codegen_supported(spec)

  invisible(TRUE)
}


#' Build a prior/parameter specification table from a compiled model spec
#'
#' Creates a human-readable table describing how each parameter is modeled:
#' fixed value, prior distribution (no regression), or regression/link (with beta prior),
#' including the special case where a linked parameter is stochastic around the link
#' (e.g., `threshold[i] ~ Lognormal(meanlog = X %*% beta, sdlog = sdlog_u)`).
#'
#' This is purely descriptive and is used by bundle-level summaries.
#'
#' @param spec A compiled model specification produced by \code{compile_model_spec()}.
#' @return A data.frame with columns describing each parameter block.
#' @keywords internal
#' @noRd
build_prior_table_from_spec <- function(spec) {
  stopifnot(is.list(spec), !is.null(spec$meta), !is.null(spec$plan))

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  meta <- spec$meta
  plan <- spec$plan

  backend <- meta$backend
  kernel  <- meta$kernel
  GPD     <- isTRUE(meta$GPD)
  has_X   <- isTRUE(meta$has_X)
  N       <- as.integer(meta$N)
  P       <- as.integer(meta$P %||% 0L)
  K       <- as.integer(meta$components)

  fmt_args <- function(args) {
    if (is.null(args) || !length(args)) return("")
    paste(sprintf("%s=%s", names(args), vapply(args, deparse1, character(1))), collapse = ", ")
  }

  add_row <- function(block, param, mode, level, prior, link, notes) {
    data.frame(
      block = block,
      parameter = param,
      mode = mode,
      level = level,
      prior = prior,
      link = link,
      notes = notes,
      stringsAsFactors = FALSE
    )
  }

  rows <- list()

  # --- meta header-ish rows (not priors, but useful) ---
  rows[[length(rows) + 1L]] <- add_row("meta", "backend", "info", "model", backend, "", "")
  rows[[length(rows) + 1L]] <- add_row("meta", "kernel", "info", "model", kernel, "", "")
  rows[[length(rows) + 1L]] <- add_row("meta", "components", "info", "model", as.character(K), "", "")
  rows[[length(rows) + 1L]] <- add_row("meta", "N", "info", "model", as.character(N), "", "")
  rows[[length(rows) + 1L]] <- add_row("meta", "P", "info", "model", as.character(P), "", "")

  # --- concentration ---
  conc <- plan$concentration %||% list()
  if (identical(conc$mode, "fixed")) {
    rows[[length(rows) + 1L]] <- add_row("concentration", "alpha", "fixed", "scalar",
                                         prior = deparse1(conc$value), link = "", notes = "fixed concentration")
  } else if (identical(conc$mode, "dist")) {
    rows[[length(rows) + 1L]] <- add_row("concentration", "alpha", "dist", "scalar",
                                         prior = sprintf("%s(%s)", conc$dist, fmt_args(conc$args)),
                                         link = "", notes = "stochastic concentration")
  } else {
    stop("Invalid plan$concentration$mode.", call. = FALSE)
  }

  # --- bulk ---
  bulk <- plan$bulk %||% list()
  for (nm in names(bulk)) {
    ent <- bulk[[nm]]
    mode <- ent$mode %||% NA_character_

    if (mode == "fixed") {
      rows[[length(rows) + 1L]] <- add_row("bulk", nm, "fixed", sprintf("component (1:%d)", K),
                                           prior = deparse1(ent$value), link = "", notes = "")
    } else if (mode == "dist") {
      rows[[length(rows) + 1L]] <- add_row("bulk", nm, "dist", sprintf("component (1:%d)", K),
                                           prior = sprintf("%s(%s)", ent$dist, fmt_args(ent$args)),
                                           link = "", notes = "iid across components")
    } else if (mode == "link") {
      bp <- ent$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 2))
      lk <- ent$link %||% "identity"
      note <- sprintf("beta_%s is %d x %d (components x P)", nm, K, P)
      if (lk == "power") note <- paste0(note, sprintf("; power=%s", deparse1(ent$link_power)))
      rows[[length(rows) + 1L]] <- add_row("bulk", nm, "link", "regression",
                                           prior = sprintf("beta_%s ~ %s(%s)", nm, bp$dist, fmt_args(bp$args)),
                                           link = lk, notes = note)
    } else {
      stop(sprintf("Invalid bulk mode for '%s'.", nm), call. = FALSE)
    }
  }

  # --- GPD ---
  if (GPD) {
    gpd <- plan$gpd %||% list()

    # threshold
    thr <- gpd$threshold %||% NULL
    if (!is.null(thr)) {
      if (thr$mode == "fixed") {
        rows[[length(rows) + 1L]] <- add_row("gpd", "threshold", "fixed", "scalar",
                                             prior = deparse1(thr$value),
                                             link = "", notes = "scalar threshold")
      } else if (thr$mode == "dist") {
        rows[[length(rows) + 1L]] <- add_row("gpd", "threshold", "dist", "scalar",
                                             prior = sprintf("%s(%s)", thr$dist, fmt_args(thr$args)),
                                             link = "", notes = "scalar threshold")
      } else if (thr$mode == "link") {
        bp <- thr$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 0.2))
        lk <- thr$link %||% "identity"
        note <- sprintf("beta_threshold is length P=%d", P)

        # link_dist case (LN around link)
        if (!is.null(thr$link_dist) && identical(thr$link_dist$dist, "lognormal")) {
          sdlog_u <- gpd$sdlog_u %||% list(mode = "dist", dist = "invgamma", args = list(shape = 2, scale = 1))
          note <- paste0(note, "; threshold[i] ~ Lognormal(meanlog = X beta, sdlog = sdlog_u)")
          rows[[length(rows) + 1L]] <- add_row("gpd", "threshold", "link+dist", "observation (1:N)",
                                               prior = sprintf("beta_threshold ~ %s(%s); sdlog_u ~ %s(%s)",
                                                               bp$dist, fmt_args(bp$args),
                                                               sdlog_u$dist, fmt_args(sdlog_u$args)),
                                               link = lk, notes = note)
        } else {
          if (lk == "power") note <- paste0(note, sprintf("; power=%s", deparse1(thr$link_power)))
          rows[[length(rows) + 1L]] <- add_row("gpd", "threshold", "link", "observation (1:N)",
                                               prior = sprintf("beta_threshold ~ %s(%s)", bp$dist, fmt_args(bp$args)),
                                               link = lk, notes = note)
        }
      } else {
        stop("Invalid gpd$threshold mode.", call. = FALSE)
      }
    }

    # tail_scale
    ts <- gpd$tail_scale %||% NULL
    if (!is.null(ts)) {
      if (ts$mode == "fixed") {
        rows[[length(rows) + 1L]] <- add_row("gpd", "tail_scale", "fixed", "scalar",
                                             prior = deparse1(ts$value), link = "", notes = "")
      } else if (ts$mode == "dist") {
        rows[[length(rows) + 1L]] <- add_row("gpd", "tail_scale", "dist", "scalar",
                                             prior = sprintf("%s(%s)", ts$dist, fmt_args(ts$args)),
                                             link = "", notes = "")
      } else if (ts$mode == "link") {
        bp <- ts$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 0.5))
        lk <- ts$link %||% "exp"
        note <- sprintf("beta_tail_scale is length P=%d; tail_scale[i] deterministic", P)
        if (lk == "power") note <- paste0(note, sprintf("; power=%s", deparse1(ts$link_power)))
        rows[[length(rows) + 1L]] <- add_row("gpd", "tail_scale", "link", "observation (1:N)",
                                             prior = sprintf("beta_tail_scale ~ %s(%s)", bp$dist, fmt_args(bp$args)),
                                             link = lk, notes = note)
      } else {
        stop("Invalid gpd$tail_scale mode.", call. = FALSE)
      }
    }

    # tail_shape
    tsh <- gpd$tail_shape %||% NULL
    if (!is.null(tsh)) {
      if (tsh$mode == "fixed") {
        rows[[length(rows) + 1L]] <- add_row("gpd", "tail_shape", "fixed", "scalar",
                                             prior = deparse1(tsh$value), link = "", notes = "")
      } else if (tsh$mode == "dist") {
        rows[[length(rows) + 1L]] <- add_row("gpd", "tail_shape", "dist", "scalar",
                                             prior = sprintf("%s(%s)", tsh$dist, fmt_args(tsh$args)),
                                             link = "", notes = "")
      } else {
        stop("Invalid gpd$tail_shape mode.", call. = FALSE)
      }
    }
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


#' Run MCMC for a prepared bundle
#'
#' @param bundle A \code{dpmixgpd_bundle} from \code{build_nimble_bundle()}.
#' @param show_progress Logical; passed to nimble.
#' @return A fitted object of class \code{"mixgpd_fit"}.
#' @examples
#' \dontrun{
#' library(nimble)
#' y <- abs(rnorm(40)) + 0.1
#' bundle <- build_nimble_bundle(
#'   y = y,
#'   backend = "sb",
#'   kernel = "normal",
#'   GPD = FALSE,
#'   components = 3,
#'   mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
#' )
#' fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
#' fit
#' }
#' @export
run_mcmc_bundle_manual <- function(bundle, show_progress = TRUE) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  stopifnot(inherits(bundle, "dpmixgpd_bundle"))

  spec <- bundle$spec
  meta <- spec$meta
  m <- bundle$mcmc %||% list()
  waic_enabled <- !isFALSE(m$waic)

  code <- .extract_nimble_code(bundle$code)
  constants <- bundle$constants %||% list()
  data <- bundle$data %||% list()

  inits_obj <- bundle$inits_fun %||% bundle$inits %||% function() list()
  inits_fun <- if (is.function(inits_obj)) {
    inits_obj
  } else if (is.list(inits_obj)) {
    function() inits_obj
  } else {
    function() list()
  }

  dims <- bundle$dimensions %||% list()
  monitors <- bundle$monitors %||% character(0)

  cat("[MCMC] Creating NIMBLE model...\n")
  Rmodel <- tryCatch({
    nimble::nimbleModel(
      code = code,
      data = data,
      constants = constants,
      inits = inits_fun(),
      dimensions = dims,
      check = TRUE,
      calculate = FALSE
    )
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("keywords:", msg, ignore.case = TRUE) &&
        grepl("Please use a different name", msg, fixed = TRUE)) {
      cat("[WARNING] NIMBLE name check failed; retrying with check=FALSE.\n")
      return(tryCatch({
        nimble::nimbleModel(
          code = code,
          data = data,
          constants = constants,
          inits = inits_fun(),
          dimensions = dims,
          check = FALSE,
          calculate = FALSE
        )
      }, error = function(e2) {
        cat("[ERROR] Failed to create NIMBLE model after retry:\n")
        stop(e2)
      }))
    }
    cat("[ERROR] Failed to create NIMBLE model:\n")
    stop(e)
  })
  cat("[MCMC] NIMBLE model created successfully.\n")

  cat("[MCMC] Configuring MCMC...\n")
  conf <- tryCatch({
    nimble::configureMCMC(
      Rmodel,
      monitors = monitors,
      enableWAIC = waic_enabled
    )
  }, error = function(e) {
    cat("[ERROR] Failed to configure MCMC:\n")
    stop(e)
  })

  # If any samplerConf is missing checkConjugacy, set it to FALSE.
  if (!is.null(conf$samplerConfs) && length(conf$samplerConfs) > 0) {
    for (i in seq_along(conf$samplerConfs)) {
      ctl <- conf$samplerConfs[[i]]$control
      if (is.null(ctl)) ctl <- list()
      if (is.null(ctl$checkConjugacy)) ctl$checkConjugacy <- FALSE
      conf$samplerConfs[[i]]$control <- ctl
    }
  }
  cat("[MCMC] MCMC configured.\n")

  cat("[MCMC] Building MCMC object...\n")
  Rmcmc <- tryCatch({
    nimble::buildMCMC(conf)
  }, error = function(e) {
    cat("[ERROR] Failed to build MCMC:\n")
    stop(e)
  })
  cat("[MCMC] MCMC object built.\n")

  compiled <- TRUE
  Cmodel <- NULL
  Cmcmc  <- NULL
  Rmcmc_inst <- NULL

  # Attempt to compile; on failure, fall back to running uncompiled MCMC
  cat("[MCMC] Attempting NIMBLE compilation (this may take a minute)...\n")
  compile_err <- tryCatch({
    cat("[MCMC] Compiling model...\n")
    Cmodel <- nimble::compileNimble(Rmodel, showCompilerOutput = FALSE)
    cat("[MCMC] Compiling MCMC sampler...\n")
    Cmcmc  <- nimble::compileNimble(Rmcmc, project = Rmodel, showCompilerOutput = FALSE)
    cat("[MCMC] Compilation successful.\n")
    NULL
  }, error = function(e) {
    cat("[WARNING] Compilation failed, falling back to uncompiled MCMC.\n")
    e
  })

  if (inherits(compile_err, "error")) {
    compiled <- FALSE
    cat("[MCMC] Creating uncompiled MCMC instance...\n")
    Rmcmc_inst <- tryCatch({
      Rmcmc()
    }, error = function(e) {
      cat("[ERROR] Failed to create uncompiled MCMC instance:\n")
      stop(e)
    })
    warning(
      paste0(
        "nimble model compilation failed; running uncompiled MCMC for portability: ",
        conditionMessage(compile_err)
      ),
      call. = FALSE
    )
  }

  niter   <- as.integer(m$niter   %||% 2000)
  nburnin <- as.integer(m$nburnin %||% 500)
  thin    <- as.integer(m$thin    %||% 1)
  nchains <- as.integer(m$nchains %||% 1)

  # Seeds: if NULL or FALSE, generate a random seed; otherwise use provided seed
  # Used for init generation only, NOT passed to runMCMC
  seed <- m$seed %||% NULL
  if (is.null(seed) || identical(seed, FALSE)) {
    # Generate a random seed using system time combined with process ID
    seed <- as.integer(Sys.time()) + Sys.getpid()
  }
  seed <- as.integer(seed)
  if (length(seed) == 1L && nchains > 1L) seed <- seed + seq_len(nchains) - 1L
  if (length(seed) != nchains) stop("mcmc$seed must be length 1 or length nchains.", call. = FALSE)

  # Inits: list-of-lists for multiple chains
  # Use seed to generate reproducible initial values, but don't pass seed to runMCMC
  if (nchains > 1L) {
    inits_list <- vector("list", nchains)
    for (ch in seq_len(nchains)) {
      set.seed(seed[ch])
      inits_list[[ch]] <- inits_fun()
    }
  } else {
    set.seed(seed[1])
    inits_list <- inits_fun()
  }

  engine_mcmc <- if (compiled) Cmcmc else Rmcmc_inst

  # Run MCMC (try WAIC; fall back if nimble version doesn’t support it)
  res <- tryCatch(
    nimble::runMCMC(
      engine_mcmc,
      niter = niter,
      nburnin = nburnin,
      thin = thin,
      nchains = nchains,
      inits = inits_list,
      progressBar = isTRUE(show_progress),
      samplesAsCodaMCMC = TRUE
    ),
    error = function(e) e
  )

  if (inherits(res, "error")) {
    samples <- nimble::runMCMC(
      engine_mcmc,
      niter = niter,
      nburnin = nburnin,
      thin = thin,
      nchains = nchains,
      inits = inits_list,
      progressBar = isTRUE(show_progress),
      samplesAsCodaMCMC = TRUE
    )
    waic_obj <- NULL
  } else {
    # Extract WAIC if available
    if (is.list(res) && !is.null(res$WAIC)) {
      waic_obj <- res$WAIC
      samples <- res$samples
    } else {
      samples <- res
      waic_obj <- NULL
    }
  }

  cat("[MCMC] MCMC execution complete. Processing results...\n")

  # Attempt to calculate WAIC if enabled
  if (is.null(waic_obj) && waic_enabled) {
    waic_obj <- tryCatch({
      if (compiled) {
        nimble::calculateWAIC(Cmcmc)
      } else {
        nimble::calculateWAIC(Rmcmc_inst)
      }
    }, error = function(e) {
      cat("[Note] WAIC calculation not available or failed.\n")
      NULL
    })
  }

  fit <- list(
    call = match.call(),
    spec = spec,
    data = data,
    model = if (compiled) Cmodel else Rmodel,
    mcmc_conf = conf,
    mcmc = list(
      engine = if (compiled) "compiled" else "uncompiled",
      niter = niter,
      nburnin = nburnin,
      thin = thin,
      nchains = nchains,
      seed = seed,
      samples = samples,
      waic = waic_obj
    ),
    code = code,
    constants = constants,
    dimensions = dims,
    monitors = monitors,
    cache = list(),
    epsilon = bundle$epsilon %||% NULL
  )

  fit$samples <- samples
  fit$waic <- waic_obj

  class(fit) <- unique(c("mixgpd_fit", "list"))
  fit
}
