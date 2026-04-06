# =========================
# 02-build-and-run.R
# =========================

#' Build the explicit one-arm NIMBLE bundle
#'
#' \code{build_nimble_bundle()} is the detailed constructor behind
#' \code{\link{bundle}} for one-arm models. It compiles the modeling plan into a
#' self-contained object holding code-generation inputs, initialization rules,
#' monitor policy, and stored MCMC defaults.
#'
#' @details
#' The returned bundle encodes a finite approximation to a Dirichlet process
#' mixture using either a stick-breaking (\code{"sb"}) or Chinese restaurant
#' process / spliced (\code{"crp"} / \code{"spliced"}) representation.
#'
#' For the bulk-only model, the target likelihood is the DPM predictive law
#' \deqn{f(y \mid x) = \sum_{k=1}^{K} w_k(x) f_k(y \mid x, \theta_k).}
#' When \code{GPD = TRUE}, the bundle augments the bulk model with a threshold
#' \eqn{u(x)} and generalized Pareto tail above that threshold, producing the
#' spliced predictive distribution described in the manuscript vignette.
#'
#' This function intentionally stops before model compilation and sampling.
#' Use \code{\link{run_mcmc_bundle_manual}} or \code{\link{mcmc}} to execute the
#' stored model definition.
#'
#' The object contains:
#' \itemize{
#'   \item compiled model \code{spec}
#'   \item \code{nimbleCode} model code
#'   \item \code{constants}, \code{data}, explicit \code{dimensions}
#'   \item initialization function \code{inits} (stored as a function)
#'   \item monitor specification
#'   \item MCMC settings list (stored but not used for code generation)
#' }
#'
#' @param y Numeric outcome vector.
#' @param X Optional design matrix/data.frame (N x p) for conditional variants.
#' @param ps Optional numeric vector (length N) of propensity scores. When provided,
#'   augments the design matrix for PS-adjusted outcome modeling.
#' @param backend Character; the Dirichlet process representation:
#'   \itemize{
#'     \item \code{"sb"}: stick-breaking truncation
#'     \item \code{"crp"}: Chinese Restaurant Process
#'   }
#' @param kernel Character kernel name (must exist in \code{get_kernel_registry()}).
#' @param GPD Logical; whether a GPD tail is requested.
#' @param components Integer >= 2. Single user-facing truncation parameter:
#'   \itemize{
#'     \item SB: number of mixture components used in stick-breaking truncation
#'     \item CRP: maximum number of clusters represented in the finite NIMBLE model
#'   }
#' @param param_specs Optional list with entries \code{bulk} and \code{tail} to override defaults.
#' @param mcmc Named list of MCMC settings (niter, nburnin, thin, nchains, seed). Stored in bundle.
#' @param epsilon Numeric in [0,1). For downstream summaries/plots/prediction we keep the
#'   smaller k defined by either (i) cumulative mass >= 1 - epsilon or (ii) per-component
#'   weights >= epsilon, then renormalize.
#' @param alpha_random Logical; whether the DP concentration parameter \eqn{\kappa} is stochastic.
#' @param monitor Character monitor profile:
#'   \itemize{
#'     \item \code{"core"} (default): monitors only the essential model parameters
#'     \item \code{"full"}: monitors all model nodes
#'   }
#' @param monitor_latent Logical; if TRUE, include latent cluster labels (\code{z}) in monitors.
#' @param monitor_v Logical; if TRUE and backend is SB, include stick breaks (\code{v}) in monitors.
#' @return A named list of class \code{"causalmixgpd_bundle"}. Its primary
#'   components are \code{spec}, \code{code}, \code{constants},
#'   \code{dimensions}, \code{data}, \code{inits}, \code{monitors}, and stored
#'   \code{mcmc} settings.
#' @seealso \code{\link{bundle}}, \code{\link{run_mcmc_bundle_manual}},
#'   \code{\link{predict.mixgpd_fit}}, \code{\link{kernel_support_table}},
#'   \code{\link{get_kernel_registry}}.
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
    backend = c("sb", "crp", "spliced"),
    kernel,
    GPD = FALSE,
    components = NULL,
    param_specs = NULL,
    mcmc = list(niter = 2000, nburnin = 500, thin = 1, nchains = 1, seed = 1),
    epsilon = 0.025,
    alpha_random = TRUE,
    monitor = c("core", "full"),
    monitor_latent = FALSE,
    monitor_v = FALSE
) {

  requested_backend <- match.arg(backend, choices = allowed_backends)
  backend <- if (identical(requested_backend, "spliced") && !isTRUE(GPD)) "crp" else requested_backend
  monitor <- match.arg(monitor)

  if (identical(monitor, "full")) {
    monitor_latent <- TRUE
    if (identical(backend, "sb")) monitor_v <- TRUE
  }
  if (backend %in% c("crp", "spliced") && !isTRUE(monitor_latent)) {
    monitor_latent <- TRUE
  }

  y <- as.numeric(y)
  if (!length(y)) stop("y must be a non-empty numeric vector.", call. = FALSE)

  if (!is.null(X) && !is.matrix(X)) X <- as.matrix(X)
  if (!is.null(ps)) {
    ps <- as.numeric(ps)
    if (length(ps) != length(y)) stop("ps must have the same length as y.", call. = FALSE)
  }

  # Single truncation parameter for both backends
  if (is.null(components)) components <- length(y)
  if (length(components) != 1L) {
    stop("components must be a single integer >= 2.", call. = FALSE)
  }
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
  if (identical(requested_backend, "spliced")) {
    spec$meta$backend <- "spliced"
    spec$dispatch$backend <- "spliced"
  }

  code <- .wrap_nimble_code(build_code_from_spec(spec))

  bundle <- list(
    spec       = spec,
    code       = code,
    constants  = build_constants_from_spec(spec),
    dimensions = build_dimensions_from_spec(spec),
    data       = build_data_from_inputs(y = y, X = X, ps = ps),
    inits      = build_inits_from_spec(spec, y = y, X = X),
    monitors   = build_monitors_from_spec(spec, monitor_v = monitor_v, monitor_latent = monitor_latent),
    monitor_policy = list(
      monitor = monitor,
      monitor_latent = isTRUE(monitor_latent),
      monitor_v = isTRUE(monitor_v)
    ),
    mcmc       = mcmc,
    epsilon    = epsilon
  )
  class(bundle) <- "causalmixgpd_bundle"
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
#' Validate that code generation is supported for a compiled spec
#'
#' Checks that the kernel registry contains the required likelihood signatures
#' for the chosen backend and whether GPD is requested, and validates that any
#' link functions requested in \code{spec$plan} are supported by the code generator.
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
#'   \item Always monitor concentration \code{kappa} (whether fixed or stochastic).
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
#' @param monitor_latent Logical; whether to monitor latent cluster labels \code{z}.
#' @return Character vector of node names to monitor.
#' @keywords internal
#' @noRd
build_monitors_from_spec <- function(spec, monitor_v = FALSE, monitor_latent = FALSE) {
  stopifnot(is.list(spec), !is.null(spec$meta), !is.null(spec$plan))


  meta <- spec$meta
  plan <- spec$plan

  backend <- meta$backend
  N <- as.integer(meta$N)
  P <- as.integer(meta$P %||% 0L)
  # PS is optional: check if it's in the plan
  has_ps <- !is.null(plan$ps)
  K <- as.integer(meta$components)
  cluster_gating <- isTRUE((spec$cluster %||% list())$gating)

  mons <- character()

  # Always monitor alpha (fixed alpha is still useful to carry in samples/prints)
  mons <- c(mons, "alpha")

  # BNP backbone
  if (identical(backend, "sb")) {
    if (isTRUE(cluster_gating)) {
      if (P < 1L) stop("Cluster gating requires P > 0.", call. = FALSE)
      mons <- c(mons, sprintf("eta[1:%d]", K - 1L))
      mons <- c(mons, sprintf("B[1:%d,1:%d]", K - 1L, P))
      if (isTRUE(monitor_latent)) mons <- c(mons, sprintf("z[1:%d]", N))
    } else {
      mons <- c(mons, sprintf("w[1:%d]", K))
      if (isTRUE(monitor_latent)) mons <- c(mons, sprintf("z[1:%d]", N))
      if (isTRUE(monitor_v)) {
        mons <- c(mons, sprintf("v[1:%d]", K - 1L))
      }
    }
  } else if (backend %in% c("crp", "spliced")) {
    if (isTRUE(monitor_latent)) mons <- c(mons, sprintf("z[1:%d]", N))
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
    is_spliced <- identical(backend, "spliced")

    if (is_spliced) {
      # ======== SPLICED BACKEND: Component-level GPD parameterization ========
      
      # threshold
      if (!is.null(gpd$threshold)) {
        thr_mode <- gpd$threshold$mode %||% NA_character_
        if (identical(thr_mode, "link")) {
          if (P < 1L) stop("GPD threshold is link-mode but P=0.", call. = FALSE)
          mons <- c(mons, sprintf("beta_threshold[1:%d,1:%d]", K, P))
          if (!is.null(gpd$threshold$link_dist) &&
              identical(gpd$threshold$link_dist$dist, "lognormal")) {
            mons <- c(mons, sprintf("threshold_i[1:%d,1:%d]", N, K))
            mons <- c(mons, "sdlog_u")
          }
          # Do NOT monitor deterministic threshold_i[i] by default.
        } else if (thr_mode %in% c("fixed", "dist")) {
          mons <- c(mons, sprintf("threshold[1:%d]", K))
        } else {
          stop("Invalid gpd$threshold mode.", call. = FALSE)
        }
      }

      # tail_scale
      if (!is.null(gpd$tail_scale)) {
        ts_mode <- gpd$tail_scale$mode %||% NA_character_
        if (identical(ts_mode, "link")) {
          if (P < 1L) stop("GPD tail_scale is link-mode but P=0.", call. = FALSE)
          mons <- c(mons, sprintf("beta_tail_scale[1:%d,1:%d]", K, P))
        } else if (ts_mode %in% c("fixed", "dist")) {
          mons <- c(mons, sprintf("tail_scale[1:%d]", K))
        } else {
          stop("Invalid gpd$tail_scale mode.", call. = FALSE)
        }
      }

      # tail_shape
      if (!is.null(gpd$tail_shape)) {
        tsh_mode <- gpd$tail_shape$mode %||% NA_character_
        if (identical(tsh_mode, "link")) {
          if (P < 1L) stop("GPD tail_shape is link-mode but P=0.", call. = FALSE)
          mons <- c(mons, sprintf("beta_tail_shape[1:%d,1:%d]", K, P))
        } else if (tsh_mode %in% c("fixed", "dist")) {
          mons <- c(mons, sprintf("tail_shape[1:%d]", K))
        } else {
          stop("Invalid gpd$tail_shape mode.", call. = FALSE)
        }
      }

    } else {
      # ======== STANDARD CRP/SB BACKEND: Original behavior ========
      
      # threshold
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

      # tail_shape
      if (!is.null(gpd$tail_shape)) {
        tsh_mode <- gpd$tail_shape$mode %||% NA_character_
        if (identical(tsh_mode, "link")) {
          if (P < 1L) stop("GPD tail_shape is link-mode but P=0.", call. = FALSE)
          mons <- c(mons, sprintf("beta_tail_shape[1:%d]", P))
        } else {
          # fixed or dist: monitor scalar
          mons <- c(mons, "tail_shape")
        }
      }
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
build_inits_from_spec <- function(spec, seed = NULL, y = NULL, X = NULL) {
  stopifnot(is.list(spec), !is.null(spec$meta), !is.null(spec$plan))


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
  X_obs <- if (!is.null(X)) {
    if (!is.matrix(X)) X <- as.matrix(X)
    storage.mode(X) <- "double"
    X
  } else {
    NULL
  }
  cluster_gating <- isTRUE((spec$cluster %||% list())$gating)

  inits <- list()

  threshold_seed_value <- function() {
    q <- if (length(y_obs)) {
      suppressWarnings(stats::quantile(y_obs, probs = 0.8, na.rm = TRUE, names = FALSE))
    } else {
      NA_real_
    }
    if (!is.finite(q) || length(q) != 1L) q <- 1
    max(as.numeric(q), .Machine$double.eps)
  }

  threshold_link_seed <- function() {
    q <- threshold_seed_value()
    if (P < 1L) {
      return(numeric())
    }
    target <- rep(log(q), N)
    beta <- tryCatch(
      as.numeric(stats::coef(stats::lm.fit(x = X_obs, y = target))),
      error = function(e) rep(NA_real_, P)
    )
    if (length(beta) != P || anyNA(beta) || any(!is.finite(beta))) {
      beta <- rep(0, P)
      if (!is.null(X_obs) && is.matrix(X_obs) && ncol(X_obs) >= 1L &&
          all(is.finite(X_obs[, 1L])) && stats::var(X_obs[, 1L]) < .Machine$double.eps) {
        beta[1L] <- log(q)
      }
    }
    beta
  }

  latent_label_seed <- function(K_init) {
    if (length(y_obs) == N && N >= 1L && sum(is.finite(y_obs)) == N) {
      ord <- order(y_obs)
      z_init <- integer(N)
      grp <- floor(((seq_len(N) - 1L) * K_init) / N) + 1L
      grp_labels <- sample.int(K_init, K_init)
      z_init[ord] <- grp_labels[pmin.int(K_init, grp)]
      return(z_init)
    }
    sample.int(K_init, size = N, replace = TRUE)
  }

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
    if (isTRUE(cluster_gating)) {
      if (P < 1L) stop("Cluster gating requires P > 0.", call. = FALSE)
      inits$eta <- stats::rnorm(K - 1L, mean = 0, sd = 0.1)
      inits$B <- matrix(
        stats::rnorm((K - 1L) * P, mean = 0, sd = 0.1),
        nrow = K - 1L,
        ncol = P
      )
    } else {
      # v[j] ~ dbeta(1, alpha); initialize in (0,1)
      if (K <= 2L) {
        inits$v <- runif(1L, 0.2, 0.8)
      } else {
        inits$v <- runif(K - 1L, 0.2, 0.8)
      }
    }
    # w/w_x are deterministic from latent gating/stick blocks; do not init
    K_init <- max(2L, min(K, 5L))
    inits$z <- latent_label_seed(K_init)
  } else if (backend %in% c("crp", "spliced")) {
    # z[1:N] ~ dCRP(...); init in 1:K, avoid all unique labels for stability
    K_init <- max(2L, min(K, 5L))
    inits$z <- latent_label_seed(K_init)
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
    is_spliced <- identical(backend, "spliced")

    if (is_spliced) {
      # ======== SPLICED BACKEND: Component-level GPD parameterization ========

      # threshold
      if (!is.null(gpd$threshold)) {
        thr_mode <- gpd$threshold$mode %||% NA_character_

        if (identical(thr_mode, "dist")) {
          # Stochastic component-level vector
          q <- if (length(y_obs)) {
            suppressWarnings(stats::quantile(y_obs, probs = 0.8, na.rm = TRUE, names = FALSE))
          } else {
            NA_real_
          }
          if (!is.finite(q) || length(q) != 1L) q <- 1
          q <- max(as.numeric(q), .Machine$double.eps)
          inits$threshold <- rep(q, K)
        } else if (identical(thr_mode, "link")) {
          # Component-specific beta coefficients
          if (P < 1L) stop("GPD threshold is link-mode but P=0.", call. = FALSE)
          inits$beta_threshold <- matrix(0, nrow = K, ncol = P)
          if (!is.null(gpd$threshold$link_dist) &&
              identical(gpd$threshold$link_dist$dist, "lognormal")) {
            inits$threshold_i <- matrix(threshold_seed_value(), nrow = N, ncol = K)
            inits$sdlog_u <- 0.2
          }
        } else if (identical(thr_mode, "fixed")) {
          # Deterministic; no init
        } else {
          stop("Invalid gpd$threshold mode.", call. = FALSE)
        }
      }

      # tail_scale
      if (!is.null(gpd$tail_scale)) {
        ts_mode <- gpd$tail_scale$mode %||% NA_character_

        if (identical(ts_mode, "dist")) {
          # Stochastic component-level vector
          inits$tail_scale <- rep(1, K)
        } else if (identical(ts_mode, "link")) {
          # Component-specific beta coefficients
          if (P < 1L) stop("GPD tail_scale is link-mode but P=0.", call. = FALSE)
          inits$beta_tail_scale <- matrix(0, nrow = K, ncol = P)
          # tail_scale_i[i] is deterministic; do not init
        } else if (identical(ts_mode, "fixed")) {
          # Deterministic; no init
        } else {
          stop("Invalid gpd$tail_scale mode.", call. = FALSE)
        }
      }

      # tail_shape
      if (!is.null(gpd$tail_shape)) {
        tsh_mode <- gpd$tail_shape$mode %||% NA_character_

        if (identical(tsh_mode, "dist")) {
          # Stochastic component-level vector
          inits$tail_shape <- rep(0, K)
        } else if (identical(tsh_mode, "link")) {
          # Component-specific beta coefficients
          if (P < 1L) stop("GPD tail_shape is link-mode but P=0.", call. = FALSE)
          inits$beta_tail_shape <- matrix(0, nrow = K, ncol = P)
          # tail_shape_i[i] is deterministic; do not init
        } else if (identical(tsh_mode, "fixed")) {
          # Deterministic; no init
        } else {
          stop("Invalid gpd$tail_shape mode.", call. = FALSE)
        }
      }

    } else {
      # ======== STANDARD CRP/SB BACKEND: Original behavior ========

      # tail_shape (stochastic only)
      if (!is.null(gpd$tail_shape) && identical(gpd$tail_shape$mode, "dist")) {
        inits$tail_shape <- 0
      } else if (!is.null(gpd$tail_shape) && identical(gpd$tail_shape$mode, "link")) {
        # Standard CRP with link mode for tail_shape (newly supported)
        if (P < 1L) stop("GPD tail_shape is link-mode but P=0.", call. = FALSE)
        inits$beta_tail_shape <- rep(0, P)
        # tail_shape[i] deterministic; do not init
      }

      # threshold
      if (!is.null(gpd$threshold)) {
        thr_mode <- gpd$threshold$mode %||% NA_character_

        if (thr_mode %in% c("fixed", "dist")) {
          inits$threshold <- threshold_seed_value()
        } else if (identical(thr_mode, "link")) {
          if (P < 1L) stop("GPD threshold is link-mode but P=0.", call. = FALSE)
          inits$beta_threshold <- threshold_link_seed()

          # if LN around-link: threshold[i] is stochastic lognormal and sdlog_u exists
          if (!is.null(gpd$threshold$link_dist) &&
              identical(gpd$threshold$link_dist$dist, "lognormal")) {
            # positive threshold init; 0.8-quantile is usually safe and data-informed
            q <- threshold_seed_value()
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
      } else if (identical(tsh_mode, "link")) {
        bp <- tsh$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 0.3))
        add_prior_constants("beta_tail_shape", bp$dist %||% "normal", bp$args %||% list(mean = 0, sd = 0.3))
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

  cluster_gating <- isTRUE((spec$cluster %||% list())$gating)

  dims <- list()

  # --- BNP backbone dims ---
  if (identical(backend, "sb")) {
    if (isTRUE(cluster_gating)) {
      if (P < 1L) stop("Cluster gating requires P > 0.", call. = FALSE)
      dims$eta <- c(K - 1L)
      dims$B <- c(K - 1L, P)
      dims$logit_ij <- c(N, K)
      dims$logit_max <- c(N)
      dims$logit_denom <- c(N)
      dims$exp_logit_ij <- c(N, K)
      dims$w_x <- c(N, K)
    } else {
      # SB breaks v[1:(K-1)] and weights w[1:K]
      dims$v <- c(K - 1L)
      dims$w <- c(K)
    }
    dims$z <- c(N)
  } else if (backend %in% c("crp", "spliced")) {
    # CRP/spliced memberships z[1:N]
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
    is_spliced <- identical(backend, "spliced")

    if (is_spliced) {
      # ======== SPLICED BACKEND: Component-level GPD parameterization ========
      
      # threshold
      thr <- gpd_plan$threshold %||% NULL
      if (!is.null(thr)) {
        thr_mode <- thr$mode %||% NA_character_

        if (thr_mode %in% c("fixed", "dist")) {
          dims$threshold <- c(K)  # component-level vector
        } else if (identical(thr_mode, "link")) {
          if (P < 1L) stop("GPD threshold is link-mode but P=0.", call. = FALSE)
          dims$beta_threshold <- c(K, P)      # component-specific coefficients
          if (!is.null(thr$link_dist) && identical(thr$link_dist$dist, "lognormal")) {
            dims$eta_threshold <- c(N, K)
            dims$threshold_i <- c(N, K)
          } else {
            dims$eta_threshold <- c(N)        # linear predictor (optional, but harmless)
            dims$threshold_i <- c(N)          # transformed parameter
          }
        } else {
          stop("Invalid gpd$threshold mode in plan.", call. = FALSE)
        }
      }

      # tail_scale
      ts <- gpd_plan$tail_scale %||% NULL
      if (!is.null(ts)) {
        ts_mode <- ts$mode %||% NA_character_

        if (ts_mode %in% c("fixed", "dist")) {
          dims$tail_scale <- c(K)  # component-level vector
        } else if (identical(ts_mode, "link")) {
          if (P < 1L) stop("GPD tail_scale is link-mode but P=0.", call. = FALSE)
          dims$beta_tail_scale <- c(K, P)
          dims$eta_tail_scale <- c(N)
          dims$tail_scale_i <- c(N)
        } else {
          stop("Invalid gpd$tail_scale mode in plan.", call. = FALSE)
        }
      }

      # tail_shape
      tsh <- gpd_plan$tail_shape %||% NULL
      if (!is.null(tsh)) {
        tsh_mode <- tsh$mode %||% NA_character_

        if (tsh_mode %in% c("fixed", "dist")) {
          dims$tail_shape <- c(K)  # component-level vector
        } else if (identical(tsh_mode, "link")) {
          if (P < 1L) stop("GPD tail_shape is link-mode but P=0.", call. = FALSE)
          dims$beta_tail_shape <- c(K, P)
          dims$eta_tail_shape <- c(N)
          dims$tail_shape_i <- c(N)
        } else {
          stop("Invalid gpd$tail_shape mode in plan.", call. = FALSE)
        }
      }

    } else {
      # ======== STANDARD CRP/SB BACKEND: Original behavior ========
      
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
        if (!tsh_mode %in% c("fixed", "dist", "link")) stop("Invalid gpd$tail_shape mode in plan.", call. = FALSE)
        # For standard CRP: tail_shape scalar for fixed/dist, or observation-level for link
        if (identical(tsh_mode, "link")) {
          if (P < 1L) stop("GPD tail_shape is link-mode but P=0.", call. = FALSE)
          if (P > 1L) dims$beta_tail_shape <- c(P)
          # tail_shape[i] deterministic -> not dimension-declared
        }
        # tail_shape scalar (fixed/dist) -> no dims entry
      }

      # sdlog_u (scalar by design; omitted from dims)
      # If you later allow sdlog_u[i], you'd add dims here.
    }
  }

  dims
}

#' Build NIMBLE model code from a compiled model spec
#'
#' Dispatches to the backend-specific code generators:
#' \itemize{
#'   \item \code{build_code_sb_from_spec()} for stick-breaking (\code{"sb"})
#'   \item \code{build_code_crp_from_spec()} for CRP and spliced (\code{"crp"}, \code{"spliced"})
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
  if (backend %in% c("crp", "spliced")) {
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
.strip_covr_counts <- function(txt) {
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  if (!length(lines)) return("")

  # Remove standalone and inline covr counter calls inserted by instrumentation.
  lines <- gsub("\\bcovr:::count\\([^)]*\\)\\s*;?", "", lines, perl = TRUE)

  # Drop covr-only conditional wrappers that can leave bare `if` tokens in parsed code.
  covr_if <- grepl("^\\s*if\\s*\\([^)]*covr:::[^)]*\\)\\s*\\{?\\s*$", lines)
  lines[covr_if] <- ""

  # Remove braces that only closed an instrumentation-only `if` block.
  orphan_closing <- logical(length(lines))
  for (i in seq_along(lines)) {
    if (grepl("^\\s*}\\s*$", lines[i])) {
      prev_nonempty <- i - 1L
      while (prev_nonempty >= 1L && !nzchar(trimws(lines[prev_nonempty]))) {
        prev_nonempty <- prev_nonempty - 1L
      }
      if (prev_nonempty >= 1L && grepl("^\\s*if\\s*\\([^)]*covr:::[^)]*\\)\\s*\\{?\\s*$", lines[prev_nonempty])) {
        orphan_closing[i] <- TRUE
      }
    }
  }
  lines[orphan_closing] <- ""

  lines <- trimws(lines, which = "right")
  lines <- lines[nzchar(trimws(lines))]
  paste(lines, collapse = "\n")
}

.deparse_without_covr <- function(expr) {
  .strip_covr_counts(paste(deparse(expr), collapse = "\n"))
}

build_code_sb_from_spec <- function(spec) {
  stopifnot(is.list(spec), !is.null(spec$meta), !is.null(spec$plan))


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
  cluster_gating <- isTRUE((spec$cluster %||% list())$gating)
  default_ps_prior <- list(dist = "normal", args = list(mean = 0, sd = 2))
  ps_prior <- (plan$ps %||% list(prior = default_ps_prior))$prior
  if (isTRUE(cluster_gating) && !isTRUE(has_X)) {
    stop("Cluster gating for SB requires X covariates.", call. = FALSE)
  }
  if (isTRUE(cluster_gating) && P < 1L) {
    stop("Cluster gating for SB requires P > 0.", call. = FALSE)
  }

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

    # --- mixing weights ---
    MIXING_BLOCK()

    # --- bulk parameters (component-level + betas) ---
    BULK_BLOCK()

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
  txt <- .deparse_without_covr(code)
  txt <- .strip_covr_counts(txt)

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

  # (0b) SB global weights vs cluster-gating weights
  mixing_block <- if (isTRUE(cluster_gating)) {
    gating_eta <- if (P == 1L) {
      "eta[j] + X[i, 1] * B[j, 1]"
    } else {
      "eta[j] + inprod(X[i, 1:P], B[j, 1:P])"
    }
    paste0(
      "for (j in 1:(components - 1)) {\n",
      "      eta[j] ~ dnorm(0, sd = 2)\n",
      if (P == 1L) {
        "      B[j, 1] ~ dnorm(0, sd = 1)\n"
      } else {
        "      for (p in 1:P) B[j, p] ~ dnorm(0, sd = 1)\n"
      },
      "    }\n",
      "    for (i in 1:N) {\n",
      "      for (j in 1:(components - 1)) {\n",
      "        logit_ij[i, j] <- ", gating_eta, "\n",
      "      }\n",
      "      logit_ij[i, components] <- 0\n",
      "      logit_max[i] <- max(logit_ij[i, 1:components])\n",
      "      for (j in 1:components) {\n",
      "        exp_logit_ij[i, j] <- exp(logit_ij[i, j] - logit_max[i])\n",
      "      }\n",
      "      logit_denom[i] <- sum(exp_logit_ij[i, 1:components])\n",
      "      for (j in 1:components) {\n",
      "        w_x[i, j] <- exp_logit_ij[i, j] / logit_denom[i]\n",
      "      }\n",
      "    }\n"
    )
  } else {
    paste0(
      "for (j in 1:(components - 1)) {\n",
      "      v[j] ~ dbeta(1, alpha)\n",
      "    }\n",
      "    stick_mass[1] <- 1\n",
      "    for (j in 2:components) {\n",
      "      stick_mass[j] <- stick_mass[j - 1] * (1 - v[j - 1])\n",
      "    }\n",
      "    for (j in 1:(components - 1)) {\n",
      "      w[j] <- v[j] * stick_mass[j]\n",
      "    }\n",
      "    w[components] <- stick_mass[components]\n"
    )
  }
  inject("MIXING_BLOCK\\(\\)", mixing_block)

  # (1) Bulk param declarations (component-level)
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
      # link-mode params are represented via beta_<param>; no component prior node.
    } else {
      stop(sprintf("Invalid bulk mode for '%s'.", nm), call. = FALSE)
    }
  }
  bulk_block <- if (length(bulk_decl_lines)) {
    paste0(
      "for (j in 1:components) {\n",
      "      ", paste(bulk_decl_lines, collapse = "\n      "), "\n",
      "    }\n"
    )
  } else {
    ""
  }
  inject("BULK_BLOCK\\(\\)", bulk_block)

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
      } else if (tsh$mode == "link") {
        if (!has_X) stop("tail_shape link-mode requires X.", call. = FALSE)
        bp <- tsh$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 0.3))
        m <- bp$args$mean %||% 0
        s <- bp$args$sd %||% 0.3
        gpd_lines <- c(gpd_lines, sprintf("for (p in 1:P) beta_tail_shape[p] ~ dnorm(%s, sd = %s)", deparse1(m), deparse1(s)))
        eta_tsh_line <- if (P == 1L) "  eta_tsh[i] <- X[i, 1] * beta_tail_shape[1]" else
          "  eta_tsh[i] <- inprod(X[i, 1:P], beta_tail_shape[1:P])"
        link_expr <- .codegen_link_expr("eta_tsh[i]", tsh$link %||% "identity", tsh$link_power)
        gpd_lines <- c(gpd_lines, "for (i in 1:N) {",
                       eta_tsh_line,
                       sprintf("  tail_shape[i] <- %s", link_expr),
                       "}")
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
      tsh <- plan$gpd$tail_shape %||% NULL
      if (!is.null(tsh) && identical(tsh$mode, "link")) args_expr <- c(args_expr, "tail_shape[i]") else args_expr <- c(args_expr, "tail_shape")
    } else {
      stop(sprintf("Unknown argument '%s' in SB signature for kernel '%s'.", a, meta$kernel), call. = FALSE)
    }
  }

  alloc_prob <- if (isTRUE(cluster_gating)) "w_x[i, 1:components]" else "w[1:components]"
  like_lines <- c(
    sprintf("z[i] ~ dcat(prob = %s)", alloc_prob),
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


  meta <- spec$meta
  plan <- spec$plan
  kinfo <- spec$kernel_info %||% list()
  sigs <- spec$signatures %||% list()

  if (!meta$backend %in% c("crp", "spliced")) {
    stop("spec backend must be 'crp' or 'spliced'.", call. = FALSE)
  }
  is_spliced <- identical(meta$backend, "spliced")

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
  bulk_decl_lines <- character()
  for (nm in bulk_params) {
    ent <- bulk_plan[[nm]]
    mode <- ent$mode %||% NA_character_

    if (mode == "fixed") {
      bulk_decl_lines <- c(bulk_decl_lines, sprintf("    %s[k] <- %s", nm, deparse1(ent$value)))
    } else if (mode == "dist") {
      bulk_decl_lines <- c(
        bulk_decl_lines,
        sprintf("    %s[k] ~ %s", nm, .codegen_prior_call(ent$dist, ent$args, backend = "CRP"))
      )
    } else if (mode == "link") {
      # link-mode params are represented via beta_<param>; no component prior node.
    } else {
      stop(sprintf("Invalid bulk mode for '%s'.", nm), call. = FALSE)
    }
  }
  if (length(bulk_decl_lines)) {
    add("  for (k in 1:components) {")
    add(bulk_decl_lines)
    add("  }")
  }

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

    # CRP link-mode parameters are applied directly in the likelihood expression.
  }

  # ---- GPD tail blocks ----
  if (isTRUE(meta$GPD)) {
    gpd <- plan$gpd %||% list()

    if (is_spliced) {
      # ======== SPLICED BACKEND: Component-level GPD parameterization ========
      # Generate component-level nodes for fixed/dist modes, or component-specific
      # beta coefficients + observation-level deterministic nodes for link mode.

      # threshold
      thr <- gpd$threshold %||% NULL
      if (!is.null(thr)) {
        if (thr$mode == "fixed") {
          add("  for (k in 1:components) {")
          add(sprintf("    threshold[k] <- %s", deparse1(thr$value)))
          add("  }")
        } else if (thr$mode == "dist") {
          add("  for (k in 1:components) {")
          add(sprintf("    threshold[k] ~ %s",
                      .codegen_prior_call(thr$dist, thr$args, backend = "CRP")))
          add("  }")
        } else if (thr$mode == "link") {
          if (!has_X) stop("threshold link-mode requires X.", call. = FALSE)
          bp <- thr$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 0.2))
          if (!identical(bp$dist, "normal")) stop("beta_threshold prior must be normal.", call. = FALSE)
          m <- bp$args$mean %||% 0
          s <- bp$args$sd %||% 0.2
          add("  for (k in 1:components) {")
          add(sprintf("    for (p in 1:P) beta_threshold[k, p] ~ dnorm(%s, sd = %s)", deparse1(m), deparse1(s)))
          add("  }")
          if (!is.null(thr$link_dist) && identical(thr$link_dist$dist, "lognormal")) {
            sdlog_u <- gpd$sdlog_u %||% list(mode = "dist", dist = "invgamma", args = list(shape = 2, scale = 1))
            if (!identical(sdlog_u$mode, "dist")) stop("sdlog_u must be dist-mode under lognormal threshold.", call. = FALSE)
            add(sprintf("  sdlog_u ~ %s",
                        .codegen_prior_call(sdlog_u$dist, sdlog_u$args, backend = "CRP")))
            add("  for (i in 1:N) {")
            add("    for (k in 1:components) {")
            if (P == 1L) {
              add("      eta_threshold[i, k] <- X[i, 1] * beta_threshold[k, 1]")
            } else {
              add("      eta_threshold[i, k] <- inprod(X[i, 1:P], beta_threshold[k, 1:P])")
            }
            add("      threshold_i[i, k] ~ dlnorm(meanlog = eta_threshold[i, k], sdlog = sdlog_u)")
            add("    }")
            add("  }")
          } else {
            add("  for (i in 1:N) {")
            if (P == 1L) {
              add("    eta_threshold[i] <- X[i, 1] * beta_threshold[z[i], 1]")
            } else {
              add("    eta_threshold[i] <- inprod(X[i, 1:P], beta_threshold[z[i], 1:P])")
            }
            add(sprintf("    threshold_i[i] <- %s", .codegen_link_expr("eta_threshold[i]", thr$link, thr$link_power)))
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
          add("  for (k in 1:components) {")
          add(sprintf("    tail_scale[k] <- %s", deparse1(ts$value)))
          add("  }")
        } else if (ts$mode == "dist") {
          add("  for (k in 1:components) {")
          add(sprintf("    tail_scale[k] ~ %s",
                      .codegen_prior_call(ts$dist, ts$args, backend = "CRP")))
          add("  }")
        } else if (ts$mode == "link") {
          if (!has_X) stop("tail_scale link-mode requires X.", call. = FALSE)
          bp <- ts$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 0.5))
          if (!identical(bp$dist, "normal")) stop("beta_tail_scale prior must be normal.", call. = FALSE)
          m <- bp$args$mean %||% 0
          s <- bp$args$sd %||% 0.5
          add("  for (k in 1:components) {")
          add(sprintf("    for (p in 1:P) beta_tail_scale[k, p] ~ dnorm(%s, sd = %s)", deparse1(m), deparse1(s)))
          add("  }")
          add("  for (i in 1:N) {")
          if (P == 1L) {
            add("    eta_tail_scale[i] <- X[i, 1] * beta_tail_scale[z[i], 1]")
          } else {
            add("    eta_tail_scale[i] <- inprod(X[i, 1:P], beta_tail_scale[z[i], 1:P])")
          }
          add(sprintf("    tail_scale_i[i] <- %s", .codegen_link_expr("eta_tail_scale[i]", ts$link, ts$link_power)))
          add("  }")
        } else {
          stop("Invalid gpd tail_scale mode.", call. = FALSE)
        }
      }

      # tail_shape
      tsh <- gpd$tail_shape %||% NULL
      if (!is.null(tsh)) {
        if (tsh$mode == "fixed") {
          add("  for (k in 1:components) {")
          add(sprintf("    tail_shape[k] <- %s", deparse1(tsh$value)))
          add("  }")
        } else if (tsh$mode == "dist") {
          add("  for (k in 1:components) {")
          add(sprintf("    tail_shape[k] ~ %s",
                      .codegen_prior_call(tsh$dist, tsh$args, backend = "CRP")))
          add("  }")
        } else if (tsh$mode == "link") {
          if (!has_X) stop("tail_shape link-mode requires X.", call. = FALSE)
          bp <- tsh$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 0.2))
          if (!identical(bp$dist, "normal")) stop("beta_tail_shape prior must be normal.", call. = FALSE)
          m <- bp$args$mean %||% 0
          s <- bp$args$sd %||% 0.2
          add("  for (k in 1:components) {")
          add(sprintf("    for (p in 1:P) beta_tail_shape[k, p] ~ dnorm(%s, sd = %s)", deparse1(m), deparse1(s)))
          add("  }")
          add("  for (i in 1:N) {")
          if (P == 1L) {
            add("    eta_tail_shape[i] <- X[i, 1] * beta_tail_shape[z[i], 1]")
          } else {
            add("    eta_tail_shape[i] <- inprod(X[i, 1:P], beta_tail_shape[z[i], 1:P])")
          }
          add(sprintf("    tail_shape_i[i] <- %s", .codegen_link_expr("eta_tail_shape[i]", tsh$link, tsh$link_power)))
          add("  }")
        } else {
          stop("Invalid gpd tail_shape mode.", call. = FALSE)
        }
      }

    } else {
      # ======== STANDARD CRP BACKEND: Observation-level GPD parameterization ========
      # (Original behavior preserved for backward compatibility)

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
        } else if (tsh$mode == "link") {
          if (!has_X) stop("tail_shape link-mode requires X.", call. = FALSE)
          bp <- tsh$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 0.2))
          if (!identical(bp$dist, "normal")) stop("beta_tail_shape prior must be normal.", call. = FALSE)
          m <- bp$args$mean %||% 0
          s <- bp$args$sd %||% 0.2
          add(sprintf("  for (p in 1:P) beta_tail_shape[p] ~ dnorm(%s, sd = %s)", deparse1(m), deparse1(s)))
          add("  for (i in 1:N) {")
          if (P == 1L) {
            add("    eta_ts[i] <- X[i, 1] * beta_tail_shape[1]")
          } else {
            add("    eta_ts[i] <- inprod(X[i, 1:P], beta_tail_shape[1:P])")
          }
          add(sprintf("    tail_shape[i] <- %s", .codegen_link_expr("eta_ts[i]", tsh$link, tsh$link_power)))
          add("  }")
        } else {
          stop("Invalid gpd tail_shape mode.", call. = FALSE)
        }
      }
    }
  }

  # ---- likelihood ----
  add("  for (i in 1:N) {")

  # build arg expressions in signature order
  gpd_for_args <- plan$gpd %||% list()
  thr_for_args <- gpd_for_args$threshold %||% NULL
  ts_for_args <- gpd_for_args$tail_scale %||% NULL
  tsh_for_args <- gpd_for_args$tail_shape %||% NULL
  
  args_expr <- character()
  for (a in arg_order) {
    if (a %in% bulk_params) {
      ent <- bulk_plan[[a]]
      if (identical(ent$mode, "link")) {
        eta_terms <- character()
        if (P == 1L) {
          eta_terms <- c(eta_terms, sprintf("X[i, 1] * beta_%s[z[i], 1]", a))
        } else {
          eta_terms <- c(eta_terms, sprintf("inprod(X[i, 1:P], beta_%s[z[i], 1:P])", a))
        }
        if (has_ps) {
          eta_terms <- c(eta_terms, sprintf("ps[i] * beta_ps_%s[z[i]]", a))
        }
        if (!length(eta_terms)) {
          stop(sprintf("Unable to build eta for '%s'.", a), call. = FALSE)
        }
        eta <- paste(eta_terms, collapse = " + ")
        args_expr <- c(args_expr, .codegen_link_expr(eta, ent$link, ent$link_power))
      } else {
        args_expr <- c(args_expr, sprintf("%s[z[i]]", a))
      }
    } else if (a == "threshold") {
      if (is_spliced) {
        # Spliced backend: link mode uses threshold_i[i], others use threshold[z[i]]
        if (!is.null(thr_for_args) && identical(thr_for_args$mode, "link")) {
          if (!is.null(thr_for_args$link_dist) &&
              identical(thr_for_args$link_dist$dist, "lognormal")) {
            args_expr <- c(args_expr, "threshold_i[i, z[i]]")
          } else {
            args_expr <- c(args_expr, "threshold_i[i]")
          }
        } else {
          args_expr <- c(args_expr, "threshold[z[i]]")
        }
      } else {
        # Standard CRP backend: scalar or observation-level
        thr_scalar <- !is.null(thr_for_args) && thr_for_args$mode %in% c("fixed", "dist")
        args_expr <- c(args_expr, if (thr_scalar) "threshold" else "threshold[i]")
      }
    } else if (a == "tail_scale") {
      if (is_spliced) {
        # Spliced backend: link mode uses tail_scale_i[i], others use tail_scale[z[i]]
        if (!is.null(ts_for_args) && identical(ts_for_args$mode, "link")) {
          args_expr <- c(args_expr, "tail_scale_i[i]")
        } else {
          args_expr <- c(args_expr, "tail_scale[z[i]]")
        }
      } else {
        # Standard CRP backend: scalar or observation-level
        if (!is.null(ts_for_args) && identical(ts_for_args$mode, "link")) {
          args_expr <- c(args_expr, "tail_scale[i]")
        } else {
          args_expr <- c(args_expr, "tail_scale")
        }
      }
    } else if (a == "tail_shape") {
      if (is_spliced) {
        # Spliced backend: link mode uses tail_shape_i[i], others use tail_shape[z[i]]
        if (!is.null(tsh_for_args) && identical(tsh_for_args$mode, "link")) {
          args_expr <- c(args_expr, "tail_shape_i[i]")
        } else {
          args_expr <- c(args_expr, "tail_shape[z[i]]")
        }
      } else {
        # Standard CRP backend: scalar only (no link mode support in original)
        args_expr <- c(args_expr, "tail_shape")
      }
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
      } else if (tsh$mode == "link") {
        bp <- tsh$beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 0.3))
        lk <- tsh$link %||% "identity"
        note <- sprintf("beta_tail_shape is length P=%d; tail_shape[i] deterministic", P)
        if (lk == "power") note <- paste0(note, sprintf("; power=%s", deparse1(tsh$link_power)))
        rows[[length(rows) + 1L]] <- add_row("gpd", "tail_shape", "link", "observation (1:N)",
                                            prior = sprintf("beta_tail_shape ~ %s(%s)", bp$dist, fmt_args(bp$args)),
                                             link = lk, notes = note)
      } else {
        stop("Invalid gpd$tail_shape mode.", call. = FALSE)
      }
    }
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.mcmc_compile_cache_env <- local({
  env <- new.env(parent = emptyenv())
  env$entries <- new.env(parent = emptyenv())
  env
})

.mcmc_cache_key <- function(code, constants, data, dims, monitors, waic_enabled) {
  parts <- list(
    code = .deparse_without_covr(code),
    constants = constants,
    data = data,
    dims = dims,
    monitors = sort(unique(as.character(monitors %||% character(0)))),
    waic_enabled = isTRUE(waic_enabled)
  )
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(parts, tf)
  unname(tools::md5sum(tf))
}

.mcmc_cache_get <- function(key) {
  if (exists(key, envir = .mcmc_compile_cache_env$entries, inherits = FALSE)) {
    return(get(key, envir = .mcmc_compile_cache_env$entries, inherits = FALSE))
  }
  NULL
}

.mcmc_cache_set <- function(key, value) {
  assign(key, value, envir = .mcmc_compile_cache_env$entries)
  invisible(value)
}

.configure_samplers <- function(conf, spec, data_info = list(), z_update_every = 1L) {
  if (is.null(conf$samplerConfs) || !length(conf$samplerConfs)) return(conf)

  model <- tryCatch(conf$getModel(), error = function(e) NULL)
  if (is.null(model)) {
    model <- tryCatch(conf$model, error = function(e) NULL)
  }
  stoch_nodes <- tryCatch(model$getNodeNames(stochOnly = TRUE, includeData = FALSE), error = function(e) character(0))
  z_nodes <- stoch_nodes[grepl("^z\\[", stoch_nodes)]

  # Normalize existing sampler controls.
  for (i in seq_along(conf$samplerConfs)) {
    ctl <- conf$samplerConfs[[i]]$control %||% list()
    if (is.null(ctl$checkConjugacy)) ctl$checkConjugacy <- FALSE

    # Reduce adaptation overhead for RW-family samplers.
    nm <- conf$samplerConfs[[i]]$name %||% ""
    if (nm %in% c("RW", "RW_block")) {
      if (is.null(ctl$adaptInterval)) ctl$adaptInterval <- 200L
      if (is.null(ctl$adaptScaleOnly)) ctl$adaptScaleOnly <- TRUE
    }

    # Keep any CRP-related cluster nodes restricted to stochastic nodes.
    if (!is.null(ctl$clusterVarInfo) && length(stoch_nodes) > 0) {
      cvi <- ctl$clusterVarInfo
      cn <- cvi$clusterNodes
      if (is.character(cn)) cn <- list(cn)
      if (is.list(cn) && length(cn) > 0) {
        cn_filtered <- lapply(cn, function(nodes) nodes[nodes %in% stoch_nodes])
        if (any(lengths(cn_filtered) != lengths(cn))) {
          cvi$clusterNodes <- cn_filtered
          if (!is.null(cvi$numNodesPerCluster)) cvi$numNodesPerCluster <- lengths(cn_filtered)
          ctl$clusterVarInfo <- cvi
        }
      }
    }

    conf$samplerConfs[[i]]$control <- ctl
  }

  # Only re-block beta nodes that already use RW-family samplers.
  # Leave conjugate and CRP-cluster wrapper samplers intact; removing them can
  # strand nodes like beta_threshold without any sampler at all.
  beta_block_nodes <- unique(unlist(lapply(conf$samplerConfs, function(sc) {
    nm <- sc$name %||% ""
    tgt <- sc$target %||% character(0)
    if (!nm %in% c("RW", "RW_block") || !length(tgt) || !all(grepl("^beta", tgt))) {
      return(character(0))
    }
    tgt
  }), use.names = FALSE))
  if (length(beta_block_nodes) >= 2L) {
    if (all(vapply(beta_block_nodes, function(nn) {
      any(vapply(conf$getSamplers(), function(ss) nn %in% ss$target, logical(1)))
    }, logical(1)))) {
      suppressWarnings(try(conf$removeSamplers(beta_block_nodes), silent = TRUE))
      add_beta_block <- try(
        conf$addSampler(
          target = beta_block_nodes,
          type = "RW_block",
          control = list(adaptInterval = 200L, adaptive = TRUE)
        ),
        silent = TRUE
      )
      if (inherits(add_beta_block, "try-error")) {
        for (nn in beta_block_nodes) {
          suppressWarnings(try(conf$addSampler(
            target = nn,
            type = "RW",
            control = list(adaptInterval = 200L, adaptive = TRUE)
          ), silent = TRUE))
        }
      }
    }
  }

  # Prefer slice for strictly positive scale/rate style parameters.
  pos_nodes <- stoch_nodes[grepl("(sigma|scale|rate|sdlog|tail_scale)", stoch_nodes)]
  if (length(pos_nodes)) {
    for (nn in unique(pos_nodes)) {
      suppressWarnings(try(conf$removeSamplers(nn), silent = TRUE))
      suppressWarnings(try(conf$addSampler(target = nn, type = "slice"), silent = TRUE))
    }
  }

  # z_update_every > 1 retains exact target but lowers frequency of z updates.
  # NIMBLE lacks built-in periodic execution per-sampler here; we tune only adaptation
  # and retain z samplers each iteration for correctness.
  if (length(z_nodes) && isTRUE(z_update_every > 1L)) {
    for (i in seq_along(conf$samplerConfs)) {
      tgt <- conf$samplerConfs[[i]]$target %||% character(0)
      if (!length(tgt) || !all(grepl("^z\\[", tgt))) next
      ctl <- conf$samplerConfs[[i]]$control %||% list()
      if (is.null(ctl$adaptInterval)) ctl$adaptInterval <- max(200L, as.integer(200L * z_update_every))
      conf$samplerConfs[[i]]$control <- ctl
    }
  }

  conf
}


#' Run posterior sampling for a prepared one-arm bundle
#'
#' \code{run_mcmc_bundle_manual()} is the explicit runner for objects created by
#' \code{\link{build_nimble_bundle}}. It compiles the stored NIMBLE code,
#' executes MCMC, and returns a \code{"mixgpd_fit"} object.
#'
#' @details
#' The resulting fit supports posterior summaries of the model parameters as
#' well as posterior predictive functionals such as
#' \eqn{f(y \mid x)}, \eqn{S(y \mid x)},
#' \eqn{Q(\tau \mid x)}, and restricted means.
#'
#' If \code{parallel_chains = TRUE}, chains are run concurrently when the stored
#' MCMC configuration uses more than one chain. If the bundle was built with
#' latent cluster labels monitored, the \code{z_update_every} argument controls how
#' frequently those latent indicators are refreshed during sampling.
#'
#' @param bundle A \code{causalmixgpd_bundle} from \code{build_nimble_bundle()}.
#' @param show_progress Logical; if TRUE, print step messages and render progress where supported.
#' @param quiet Logical; if TRUE, suppress console status messages.
#'   Set to FALSE to see progress messages during MCMC setup and execution.
#' @param parallel_chains Logical; run chains concurrently when \code{nchains > 1}.
#' @param workers Optional integer number of workers for parallel execution.
#' @param timing Logical; if TRUE, include stage timings (\code{build}, \code{compile}, \code{mcmc})
#'   in \code{fit$timing}.
#' @param z_update_every Integer >= 1 controlling latent cluster-label update cadence.
#' @return A fitted object of class \code{"mixgpd_fit"} containing posterior
#'   draws, model metadata, and cached objects used by downstream S3 methods.
#' @seealso \code{\link{build_nimble_bundle}}, \code{\link{mcmc}},
#'   \code{\link{summary.mixgpd_fit}}, \code{\link{predict.mixgpd_fit}}.
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
run_mcmc_bundle_manual <- function(bundle, show_progress = TRUE, quiet = FALSE,
                                   parallel_chains = FALSE, workers = NULL, timing = FALSE,
                                   z_update_every = NULL) {
  suppressPackageStartupMessages(base::require("nimble", quietly = TRUE, warn.conflicts = FALSE))
  stopifnot(inherits(bundle, "causalmixgpd_bundle"))
  progress_label <- if (inherits(bundle, "dpmixgpd_cluster_bundle")) "cluster" else "mixgpd"

  progress_ctx <- .cmgpd_progress_start(
    total_steps = 8L,
    enabled = isTRUE(show_progress),
    quiet = isTRUE(quiet),
    label = progress_label
  )
  on.exit(.cmgpd_progress_done(progress_ctx, final_label = NULL), add = TRUE)
  .cmgpd_progress_step(progress_ctx, "Validating configuration")

  spec <- bundle$spec
  m <- bundle$mcmc %||% list()
  waic_enabled <- !isFALSE(m$waic)

  code <- .extract_nimble_code(bundle$code)
  constants <- bundle$constants %||% list()
  data <- bundle$data %||% list()
  dims <- bundle$dimensions %||% list()
  monitors <- bundle$monitors %||% character(0)

  inits_obj <- bundle$inits_fun %||% bundle$inits %||% function() list()
  inits_fun <- if (is.function(inits_obj)) inits_obj else if (is.list(inits_obj)) function() inits_obj else function() list()

  niter <- as.integer(m$niter %||% 2000)
  nburnin <- as.integer(m$nburnin %||% 500)
  thin <- as.integer(m$thin %||% 1)
  nchains <- as.integer(m$nchains %||% 1)
  timing <- isTRUE(timing %||% m$timing %||% FALSE)
  parallel_chains <- isTRUE(parallel_chains %||% m$parallel_chains %||% FALSE)
  nimble_quiet <- isTRUE(show_progress) || isTRUE(quiet)
  z_update_every <- as.integer(z_update_every %||% m$z_update_every %||% 1L)
  if (!is.finite(z_update_every) || z_update_every < 1L) {
    stop("'z_update_every' must be an integer >= 1.", call. = FALSE)
  }
  if (z_update_every > 1L && !isTRUE(quiet) && !isTRUE(show_progress)) {
    .cmgpd_message(sprintf("Using z_update_every = %d; this may reduce compute but can slow mixing.", z_update_every))
  }
  workers <- as.integer(workers %||% m$workers %||% max(1L, min(nchains, parallel::detectCores(logical = FALSE))))
  if (!is.finite(workers) || workers < 1L) workers <- 1L

  seed <- m$seed %||% NULL
  if (is.null(seed) || identical(seed, FALSE)) seed <- as.integer(Sys.time()) + Sys.getpid()
  seed <- as.integer(seed)
  if (length(seed) == 1L && nchains > 1L) seed <- seed + seq_len(nchains) - 1L
  if (length(seed) != nchains) stop("mcmc$seed must be length 1 or length nchains.", call. = FALSE)

  tic <- function() proc.time()[["elapsed"]]
  timing_info <- list(build = 0, compile = 0, mcmc = 0, cache_hit = FALSE)

  if (parallel_chains && nchains > 1L &&
      requireNamespace("future", quietly = TRUE) &&
      requireNamespace("future.apply", quietly = TRUE)) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = min(workers, nchains))
    t0 <- tic()
    chain_fits <- future.apply::future_lapply(seq_len(nchains), function(ch) {
      b <- bundle
      b$mcmc <- utils::modifyList(
        b$mcmc %||% list(),
        list(nchains = 1L, seed = seed[ch], parallel_chains = FALSE, z_update_every = z_update_every)
      )
      run_mcmc_bundle_manual(
        b,
        show_progress = FALSE,
        quiet = TRUE,
        parallel_chains = FALSE,
        workers = 1L,
        timing = timing,
        z_update_every = z_update_every
      )
    }, future.seed = TRUE)
    timing_info$mcmc <- tic() - t0

    sample_list <- lapply(chain_fits, function(fit_i) {
      smp <- fit_i$samples
      if (inherits(smp, "mcmc.list")) smp[[1L]] else smp
    })
    samples <- if (requireNamespace("coda", quietly = TRUE)) coda::mcmc.list(sample_list) else sample_list

    fit <- chain_fits[[1L]]
    fit$mcmc$nchains <- nchains
    fit$mcmc$seed <- seed
    fit$mcmc$samples <- samples
    fit$mcmc$parallel_chains <- TRUE
    fit$samples <- samples
    fit$timing <- timing_info
    class(fit) <- unique(c("mixgpd_fit", "list"))
    return(fit)
  }

  if (parallel_chains && nchains > 1L && !isTRUE(quiet) && !isTRUE(show_progress)) {
    warning("parallel_chains=TRUE requested but 'future'/'future.apply' are unavailable; running sequentially.",
            call. = FALSE)
  }

  .cmgpd_progress_step(progress_ctx, "Checking build/compile cache")
  cache_key <- .mcmc_cache_key(code = code, constants = constants, data = data, dims = dims, monitors = monitors, waic_enabled = waic_enabled)
  cache_entry <- .mcmc_cache_get(cache_key)
  if (!is.null(cache_entry)) {
    timing_info$cache_hit <- TRUE
    if (!isTRUE(quiet) && !isTRUE(show_progress)) .cmgpd_message("[MCMC] Reusing cached build/compile.")
  }

  if (is.null(cache_entry)) {
    .cmgpd_progress_step(progress_ctx, "Building model and MCMC configuration")
    t0_build <- tic()
    # Generated models are validated upstream; NIMBLE's full check path can be
    # disproportionately expensive for manuscript-scale fits.
    Rmodel <- tryCatch(
      .cmgpd_capture_nimble(
        nimble::nimbleModel(
          code = code, data = data, constants = constants,
          inits = inits_fun(), dimensions = dims, check = FALSE, calculate = FALSE
        ),
        suppress = nimble_quiet
      ),
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("keywords:", msg, ignore.case = TRUE) && grepl("Please use a different name", msg, fixed = TRUE)) {
          return(
            .cmgpd_capture_nimble(
              nimble::nimbleModel(
                code = code, data = data, constants = constants,
                inits = inits_fun(), dimensions = dims, check = FALSE, calculate = FALSE
              ),
              suppress = nimble_quiet
            )
          )
        }
        stop(e)
      }
    )

    conf <- .cmgpd_capture_nimble(
      nimble::configureMCMC(Rmodel, monitors = monitors, enableWAIC = waic_enabled),
      suppress = nimble_quiet
    )
    conf <- .cmgpd_capture_nimble(
      .configure_samplers(
        conf,
        spec = spec,
        data_info = list(constants = constants, dimensions = dims),
        z_update_every = z_update_every
      ),
      suppress = nimble_quiet
    )

    Rmcmc <- .cmgpd_capture_nimble(nimble::buildMCMC(conf), suppress = nimble_quiet)
    timing_info$build <- tic() - t0_build

    .cmgpd_progress_step(progress_ctx, "Compiling NIMBLE model")
    compiled <- TRUE
    Cmodel <- NULL
    Cmcmc <- NULL
    t0_compile <- tic()
    compile_err <- tryCatch({
      Cmodel <- .cmgpd_capture_nimble(
        nimble::compileNimble(Rmodel, showCompilerOutput = FALSE),
        suppress = nimble_quiet
      )
      Cmcmc <- .cmgpd_capture_nimble(
        nimble::compileNimble(Rmcmc, project = Rmodel, showCompilerOutput = FALSE),
        suppress = nimble_quiet
      )
      NULL
    }, error = function(e) e)
    timing_info$compile <- tic() - t0_compile
    if (inherits(compile_err, "error")) {
      compiled <- FALSE
      if (!isTRUE(quiet) && !isTRUE(show_progress)) {
        warning(paste0("nimble model compilation failed; running uncompiled MCMC for portability: ", conditionMessage(compile_err)), call. = FALSE)
      }
    }

    cache_entry <- list(
      compiled = compiled,
      Rmodel = Rmodel,
      Rmcmc = Rmcmc,
      Cmodel = if (compiled) Cmodel else NULL,
      Cmcmc = if (compiled) Cmcmc else NULL,
      conf = conf
    )
    .mcmc_cache_set(cache_key, cache_entry)
  } else {
    .cmgpd_progress_step(progress_ctx, "Building model and MCMC configuration (cached)")
    .cmgpd_progress_step(progress_ctx, "Compiling NIMBLE model (cached)")
  }

  compiled <- isTRUE(cache_entry$compiled)
  Rmodel <- cache_entry$Rmodel
  Rmcmc <- cache_entry$Rmcmc
  Cmodel <- cache_entry$Cmodel
  Cmcmc <- cache_entry$Cmcmc
  conf <- cache_entry$conf
  engine_mcmc <- if (compiled) Cmcmc else Rmcmc

  .cmgpd_progress_step(progress_ctx, "Initializing chains")
  inits_list <- if (nchains > 1L) {
    out <- vector("list", nchains)
    for (ch in seq_len(nchains)) {
      set.seed(seed[ch])
      out[[ch]] <- inits_fun()
    }
    out
  } else {
    set.seed(seed[1L])
    inits_fun()
  }

  .cmgpd_progress_step(progress_ctx, "Running MCMC")
  t0_mcmc <- tic()
  run_mcmc_once <- function(current_inits) {
    tryCatch(
      .cmgpd_capture_nimble(
        nimble::runMCMC(
          engine_mcmc,
          niter = niter,
          nburnin = nburnin,
          thin = thin,
          nchains = nchains,
          inits = current_inits,
          progressBar = FALSE,
          samplesAsCodaMCMC = TRUE
        ),
        suppress = nimble_quiet
      ),
      error = function(e) e
    )
  }
  res <- run_mcmc_once(inits_list)
  is_crp_init_error <- inherits(res, "error") &&
    grepl(
      "CRP_sampler: sampler encountered case where the log probability density values corresponding to all potential cluster memberships are negative infinity",
      conditionMessage(res),
      fixed = TRUE
    )
  if (is_crp_init_error && identical(spec$meta$backend, "crp")) {
    max_crp_retries <- 5L
    for (attempt in seq_len(max_crp_retries)) {
      retry_seed <- seed + attempt
      retry_inits <- if (nchains > 1L) {
        out <- vector("list", nchains)
        for (ch in seq_len(nchains)) {
          out[[ch]] <- build_inits_from_spec(
            spec,
            seed = retry_seed[ch],
            y = data$y,
            X = data$X %||% NULL
          )
        }
        out
      } else {
        build_inits_from_spec(
          spec,
          seed = retry_seed[1L],
          y = data$y,
          X = data$X %||% NULL
        )
      }
      res <- run_mcmc_once(retry_inits)
      if (!inherits(res, "error")) {
        inits_list <- retry_inits
        break
      }
    }
  }
  timing_info$mcmc <- tic() - t0_mcmc

  if (inherits(res, "error")) {
    stop(res)
  } else if (is.list(res) && !is.null(res$WAIC)) {
    waic_obj <- res$WAIC
    samples <- res$samples
  } else {
    waic_obj <- NULL
    samples <- res
  }

  .cmgpd_progress_step(progress_ctx, "Finalizing WAIC and diagnostics")
  if (is.null(waic_obj) && waic_enabled) {
    waic_obj <- tryCatch(
      .cmgpd_capture_nimble(
        if (compiled) nimble::calculateWAIC(Cmcmc) else nimble::calculateWAIC(Rmcmc),
        suppress = nimble_quiet
      ),
      error = function(e) NULL
    )
  }

  .cmgpd_progress_step(progress_ctx, "Assembling fit object")
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
      z_update_every = z_update_every,
      samples = samples,
      waic = waic_obj
    ),
    code = code,
    constants = constants,
    dimensions = dims,
    monitors = monitors,
    cache = list(predict_env = new.env(parent = emptyenv())),
    epsilon = bundle$epsilon %||% NULL,
    timing = timing_info
  )

  fit$samples <- samples
  fit$waic <- waic_obj

  class(fit) <- unique(c("mixgpd_fit", "list"))
  fit
}
