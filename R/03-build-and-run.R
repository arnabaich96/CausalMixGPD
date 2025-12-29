# =========================
# 02-build-and-run.R
# =========================

#' Build a NIMBLE bundle (internal)
#'
#' Creates a runnable "bundle" containing:
#' \itemize{
#'   \item compiled model \code{spec}
#'   \item \code{nimbleCode} model code
#'   \item \code{constants}, \code{data}, explicit \code{dimensions}
#'   \item initialization function \code{inits_fun}
#'   \item monitor specification
#' }
#'
#' The bundle is consumed by \code{run_mcmc_bundle_manual()} / internal runners.
#'
#' Notes:
#' \itemize{
#'   \item We do NOT touch kernel files (single-number scripts).
#'   \item Conditional codegen is enabled when \code{X != NULL} AND at least one
#'         node in \code{spec$node_plan} has \code{type == "link"}.
#'   \item If \code{X != NULL} but no \code{"link"} nodes exist, we allow build and
#'         simply ignore \code{X} in the model code (but keep it in \code{data}).
#' }
#'
#' @param y Numeric outcome vector.
#' @param X Optional design matrix (N x p) for conditional variants.
#' @param backend Character; \code{"sb"} or \code{"crp"}.
#' @param kernel Character kernel name.
#' @param GPD Logical; whether a tail model is requested. (Tail wiring is handled
#'        in other files; this builder focuses on bulk dispatch + core scaffolding.)
#' @param components Number of mixture components \code{J} when backend="sb".
#' @param Kmax Maximum clusters when backend="crp".
#' @param mcmc Named list of MCMC settings (niter, nburnin, thin, nchains, seed).
#'
#' @return A named list (bundle) used by MCMC runner.
#' @keywords internal
#' @noRd
build_nimble_bundle <- function(
    y,
    X = NULL,
    backend = c("sb", "crp"),
    kernel,
    GPD = FALSE,
    Kmax = NULL,
    components = NULL,
    param_specs = NULL,
    mcmc = list(niter = 2000, nburnin = 500, thin = 1, nchains = 1, seed = 1),
    alpha_random = TRUE
) {
  backend <- match.arg(backend)

  if (!is.null(X) && !is.matrix(X)) X <- as.matrix(X)

  if (is.null(Kmax)) Kmax <- length(y)
  Kmax <- as.integer(Kmax)
  if (Kmax < 2) stop("Kmax must be >= 2.", call. = FALSE)

  spec <- compile_model_spec(
    y = y,
    X = X,
    backend = backend,
    kernel = kernel,
    GPD = GPD,
    Kmax = Kmax,
    J = if (identical(backend, "sb")) components else NULL,
    components = if (identical(backend, "sb")) components else NULL,
    param_specs = param_specs,
    mcmc = mcmc,
    alpha_random = alpha_random
  )

  code <- build_code_from_spec(spec)

  bundle <- list(
    spec       = spec,
    code       = code,
    constants  = build_constants_from_spec(spec),
    dimensions = build_dimensions_from_spec(spec),
    data       = build_data_from_inputs(y = y, X = X),

    # IMPORTANT: pass seed, not y
    inits      = build_inits_from_spec(spec, seed = mcmc$seed %||% NULL),

    monitors   = build_monitors_from_spec(spec),
    mcmc       = mcmc
  )
  class(bundle) <- "dpmixgpd_bundle"
  bundle
}




#' Detect whether conditional code generation is required (X + at least one link node)
#'
#' @param spec A compiled \code{dpmixgpd_spec}.
#' @return Logical.
#' @keywords internal
#' @noRd
spec_requires_conditional <- function(spec) {
  meta <- spec$meta
  if (!isTRUE(meta$has_X)) return(FALSE)

  # bulk nodes
  if (!is.null(spec$node_plan$bulk) && length(spec$node_plan$bulk)) {
    for (nm in names(spec$node_plan$bulk)) {
      if (identical(spec$node_plan$bulk[[nm]]$type, "link")) return(TRUE)
    }
  }

  # tail nodes (if present)
  if (isTRUE(meta$GPD) && !is.null(spec$node_plan$tail) && length(spec$node_plan$tail)) {
    for (nm in names(spec$node_plan$tail)) {
      if (identical(spec$node_plan$tail[[nm]]$type, "link")) return(TRUE)
    }
  }

  FALSE
}

#' Assert code generation is supported for the given spec (internal)
#'
#' This is the replacement for the old hard-stop \code{X != NULL} check.
#'
#' Rules:
#' \itemize{
#'   \item If \code{X == NULL}: always allowed.
#'   \item If \code{X != NULL} but no \code{"link"} nodes: allowed (unconditional model; X ignored).
#'   \item If conditional is required: allowed IF the required dispatch hooks exist.
#' }
#'
#' @param spec A compiled \code{dpmixgpd_spec}.
#' @keywords internal
#' @noRd
assert_codegen_supported <- function(spec) {
  meta <- spec$meta

  if (!isTRUE(meta$has_X)) return(invisible(TRUE))

  needs_cond <- spec_requires_conditional(spec)
  if (!needs_cond) return(invisible(TRUE))

  # Conditional: require appropriate dispatch hooks so failures are crisp.
  if (identical(meta$backend, "sb")) {
    if (is.null(spec$dispatch$density_x) || length(spec$dispatch$density_x) != 1) {
      stop("Conditional SB codegen requested but spec$dispatch$density_x is missing/invalid.", call. = FALSE)
    }
  } else if (identical(meta$backend, "crp")) {
    # CRP conditional uses the scalar kernel density (same as CRP unconditional),
    # but will additionally require X to exist in data/constants.
    if (is.null(spec$dispatch$density) || length(spec$dispatch$density) != 1) {
      stop("Conditional CRP codegen requested but spec$dispatch$density is missing/invalid.", call. = FALSE)
    }
  }

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

#' Build NIMBLE data list from user inputs (internal)
#'
#' @param y outcome vector
#' @param X optional design matrix
#' @return Named list suitable for \code{nimbleModel(data=...)}.
#' @keywords internal
#' @noRd
build_data_from_inputs <- function(y, X = NULL) {
  data <- list(y = as.numeric(y))
  if (!is.null(X)) {
    data$X <- as.matrix(X)
  }
  data
}

#' Build monitors requested from spec (internal)
#'
#' @param spec A compiled \code{dpmixgpd_spec}.
#' @return Character vector.
#' @keywords internal
#' @noRd
build_monitors_from_spec <- function(spec) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # If spec already has monitors, keep them
  if (!is.null(spec$monitors) && length(spec$monitors)) {
    return(unique(spec$monitors))
  }

  # Otherwise build a sensible default from node_plan
  np <- spec$node_plan %||% NULL
  if (is.null(np)) return(character(0))

  out <- character(0)

  # DP nodes
  if (!is.null(np$dp$nodes$stochastic)) out <- c(out, np$dp$nodes$stochastic)
  if (!is.null(np$dp$nodes$deterministic)) out <- c(out, np$dp$nodes$deterministic)

  # Bulk params
  if (!is.null(np$bulk)) {
    for (pname in names(np$bulk)) {
      d <- np$bulk[[pname]]
      if (is.null(d) || identical(d$type, "fixed")) next

      if (identical(d$type, "link")) {
        out <- c(out, paste0("beta_", pname))
      } else {
        # monitor the parameter name (even if invgamma uses inv_ internally)
        out <- c(out, pname)
      }
    }
  }

  # Tail params (if any)
  if (!is.null(np$tail)) {
    for (pname in names(np$tail)) {
      d <- np$tail[[pname]]
      if (is.null(d) || identical(d$type, "fixed")) next

      if (identical(d$type, "link")) {
        out <- c(out, paste0("beta_", pname))
      } else {
        out <- c(out, pname)
      }
    }
  }

  unique(out)
}


#' Build an initialization function (internal)
#'
#' @param spec A compiled \code{dpmixgpd_spec}.
#' @param y Outcome vector (may be used for sensible inits).
#' @return A function() that returns a list of initial values.
#' @keywords internal
#' @noRd
build_inits_from_spec <- function(spec, seed = NULL) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  meta <- spec$meta %||% list()
  kdef <- spec$kernel %||% get_kernel_registry()[[meta$kernel]]

  backend <- meta$backend
  GPD     <- isTRUE(meta$GPD)
  has_X   <- isTRUE(meta$has_X)

  N <- as.integer(meta$N %||% spec$N %||% length(spec$data$y))
  Kmax <- as.integer(meta$Kmax %||% spec$Kmax %||% N)
  J <- meta$J %||% spec$J
  if (!is.null(J)) J <- as.integer(J)

  X <- spec$data$X
  P <- meta$P %||% spec$P %||% meta$p %||% spec$p
  if (has_X) {
    if (is.null(P)) P <- ncol(as.matrix(X))
    P <- as.integer(P)
  } else {
    P <- 0L
  }

  bulk_plan   <- spec$node_plan$bulk %||% list()
  bulk_params <- kdef$bulk_params %||% names(bulk_plan) %||% character(0)

  y <- spec$data$y
  y_med <- stats::median(y)
  y_sd  <- stats::sd(y)

  function() {
    if (!is.null(seed)) set.seed(as.integer(seed)[1])

    inits <- list()

    # -----------------------------
    # DP prior blocks
    # -----------------------------
    inits$alpha <- 1.0

    if (identical(backend, "crp")) {
      # z[1:N] is stochastic
      K <- as.integer(Kmax)
      if (!is.finite(K) || K < 2L) K <- max(2L, min(N, 10L))
      inits$z <- sample.int(K, N, replace = TRUE)

    } else if (identical(backend, "sb")) {
      # v[1:(J-1)] is stochastic; weights is deterministic
      JJ <- as.integer(J %||% max(2L, min(N, 10L)))
      inits$v <- stats::rbeta(JJ - 1L, 1, 1)  # in (0,1)
    }

    # -----------------------------
    # Bulk component parameters
    # Initialize only those that exist in the model (per node_plan)
    # -----------------------------
    if (identical(backend, "crp")) {
      K <- as.integer(Kmax)
      for (pname in bulk_params) {
        d <- bulk_plan[[pname]]
        if (is.null(d) || identical(d$type, "fixed")) next

        sup <- d$support %||% "real"

        if (sup %in% c("positive_sd", "positive_scale", "positive_shape", "positive_mean")) {
          # positive inits
          inits[[pname]] <- rep(1.0, K)
        } else {
          # real inits
          inits[[pname]] <- rnorm(K, mean = 0, sd = 0.5)
        }

        # If you ever turn on covariate-linked bulk params later (type="link"),
        # initialize beta_<param> ONLY when X exists
        if (identical(d$type, "link") && has_X) {
          inits[[paste0("beta_", pname)]] <- matrix(0, nrow = P, ncol = K)
        }
      }
    } else {
      JJ <- as.integer(J %||% max(2L, min(N, 10L)))
      for (pname in bulk_params) {
        d <- bulk_plan[[pname]]
        if (is.null(d) || identical(d$type, "fixed")) next

        sup <- d$support %||% "real"

        if (sup %in% c("positive_sd", "positive_scale", "positive_shape", "positive_mean")) {
          inits[[pname]] <- rep(1.0, JJ)
        } else {
          inits[[pname]] <- rnorm(JJ, mean = 0, sd = 0.5)
        }

        if (identical(d$type, "link") && has_X) {
          inits[[paste0("beta_", pname)]] <- matrix(0, nrow = P, ncol = JJ)
        }
      }
    }

    # -----------------------------
    # Tail / threshold (only when GPD=TRUE)
    # Initialize only stochastic nodes that appear in code
    # -----------------------------
    if (GPD) {
      if (has_X) {
        # threshold[i] is deterministic in your codegen; only beta_threshold is stochastic
        inits$beta_threshold <- rep(0, P)
      } else {
        # threshold is scalar stochastic
        inits$threshold <- if (is.finite(y_med)) y_med else 1.0
      }

      # tail_scale, tail_shape are scalar stochastic
      inits$tail_scale <- 1.0
      inits$tail_shape <- 0.1
    }

    # -----------------------------
    # Tiny safety: avoid NA
    # -----------------------------
    inits <- inits[!vapply(inits, function(x) any(is.na(x)), logical(1))]

    inits
  }
}



#' Build constants from spec (internal)
#'
#' @param spec A compiled \code{dpmixgpd_spec}.
#' @return Named list.
#' @keywords internal
#' @noRd
build_constants_from_spec <- function(spec) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  meta <- spec$meta %||% list()
  pri  <- spec$priors %||% list()
  bulk_plan <- spec$node_plan$bulk %||% list()

  backend <- meta$backend
  GPD     <- isTRUE(meta$GPD)
  has_X   <- isTRUE(meta$has_X)

  N <- as.integer(meta$N %||% spec$N %||% length(spec$data$y))
  P <- as.integer(meta$P %||% spec$P %||% if (has_X) ncol(as.matrix(spec$data$X)) else 0L)
  Kmax <- as.integer(meta$Kmax %||% spec$Kmax %||% N)
  J <- meta$J %||% spec$J
  if (!is.null(J)) J <- as.integer(J)

  # ------------------------------------------------------------------
  # Base structural constants (only those actually referenced in code)
  # ------------------------------------------------------------------
  constants <- list(N = N)

  if (has_X) constants$P <- P

  if (identical(backend, "crp")) {
    constants$Kmax <- Kmax
  } else {
    # SB
    constants$J <- as.integer(J %||% max(2L, min(N, 10L)))
  }

  # ------------------------------------------------------------------
  # Decide which prior hyperparameters are needed
  # based on node_plan prior families + tail usage
  # ------------------------------------------------------------------

  # alpha prior always exists in both backends
  # (only include if your code uses alpha_shape/alpha_rate; if code uses dgamma(1,1),
  # keep these out to avoid "ignored".)
  uses_alpha_hypers <- TRUE
  if (uses_alpha_hypers) {
    constants$alpha_shape <- pri$alpha_shape %||% 1
    constants$alpha_rate  <- pri$alpha_rate  %||% 1
  }

  # Bulk priors: include normal_* and/or gamma_* only if at least one bulk param uses them
  bulk_prior_families <- vapply(
    names(bulk_plan),
    function(pn) {
      d <- bulk_plan[[pn]]
      if (is.null(d) || identical(d$type, "fixed")) return(NA_character_)
      tolower(d$prior_family %||% "normal")
    },
    character(1)
  )
  bulk_prior_families <- bulk_prior_families[!is.na(bulk_prior_families)]

  if (any(bulk_prior_families %in% c("normal"))) {
    constants$normal_mean <- pri$normal_mean %||% 0
    constants$normal_sd   <- pri$normal_sd   %||% 10
  }
  if (any(bulk_prior_families %in% c("gamma"))) {
    constants$gamma_shape <- pri$gamma_shape %||% 2
    constants$gamma_rate  <- pri$gamma_rate  %||% 1
  }

  # ------------------------------------------------------------------
  # Tail / threshold priors (only if GPD)
  # ------------------------------------------------------------------
  if (GPD) {
    # Threshold: if no X, threshold is stochastic and uses threshold_meanlog/sdlog
    if (!has_X) {
      constants$threshold_meanlog <- pri$threshold_meanlog %||% 0
      constants$threshold_sdlog   <- pri$threshold_sdlog   %||% 1
    } else {
      # Regression: beta_threshold exists only if X exists
      constants$beta_threshold_mean <- pri$beta_threshold_mean %||% 0
      constants$beta_threshold_sd   <- pri$beta_threshold_sd   %||% 10
    }

    # Tail scalars always present when GPD=TRUE
    constants$tail_scale_sdlog <- pri$tail_scale_sdlog %||% 1
    constants$tail_shape_sd    <- pri$tail_shape_sd    %||% 1
  }

  constants
}


#' Build explicit dimensions to avoid NIMBLE size inference issues (internal)
#'
#' @param spec A compiled \code{dpmixgpd_spec}.
#' @return Named list of dimensions.
#' @keywords internal
#' @noRd
build_dimensions_from_spec <- function(spec) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  meta <- spec$meta
  dims <- list()

  if (identical(meta$backend, "sb")) {
    J <- as.integer(meta$J)

    dims$v       <- as.integer(J - 1L)
    dims$weights <- as.integer(J)

    # component-level bulk params
    for (pname in names(spec$node_plan$bulk %||% list())) {
      d <- spec$node_plan$bulk[[pname]]
      if (is.null(d) || identical(d$type, "fixed")) next
      dims[[pname]] <- as.integer(J)
    }

    if (isTRUE(meta$GPD)) {
      # Only include dimensions for genuinely vector-valued nodes.
      # Scalars (threshold, tail_scale, tail_shape) MUST be omitted to avoid
      # scalar-vs-length1-vector conflicts in NIMBLE.
      if (isTRUE(meta$has_X)) {
        # If your conditional build actually creates threshold[i] (i=1..N),
        # then it is a vector and we must declare it.
        dims$beta_threshold <- as.integer(meta$P)
        dims$threshold      <- as.integer(meta$N)
      }
      # else: threshold is scalar -> omit
      # tail_scale, tail_shape are scalar -> omit
    }

    return(dims)
  }

  # ---- CRP branch ----
  dims$z <- as.integer(meta$N)
  Kmax <- as.integer(meta$Kmax)

  for (pname in names(spec$node_plan$bulk %||% list())) {
    d <- spec$node_plan$bulk[[pname]]
    if (is.null(d) || identical(d$type, "fixed")) next
    dims[[pname]] <- as.integer(Kmax)
  }

  if (isTRUE(meta$GPD)) {
    if (isTRUE(meta$has_X)) {
      dims$beta_threshold <- as.integer(meta$P)
      dims$threshold      <- as.integer(meta$N)
    }
    # else: threshold is scalar -> omit
    # tail_scale, tail_shape are scalar -> omit
  }

  dims
}


#' Build nimbleCode from a compiled spec (dispatcher)
#'
#' This is a thin dispatcher around backend-specific code builders:
#' \code{build_code_sb_from_spec()} and \code{build_code_crp_from_spec()}.
#'
#' @param spec A compiled \code{dpmixgpd_spec}.
#' @return nimbleCode object.
#' @keywords internal
#' @noRd
build_code_from_spec <- function(spec) {
  backend <- spec$meta$backend
  if (identical(backend, "sb")) {
    build_code_sb_from_spec(spec)
  } else if (identical(backend, "crp")) {
    build_code_crp_from_spec(spec)
  } else {
    stop("Unknown backend in spec$meta$backend.", call. = FALSE)
  }
}

#' SB backend: build nimbleCode from spec
#'
#' SB supports unconditional by default. If conditional is required, SB expects
#' \code{spec$dispatch$density_x} to be present, and will build per-(i,j) parameter
#' arrays for all bulk nodes with \code{type=="link"}.
#'
#' IMPORTANT: likelihood call uses NAMED vector arguments to avoid NIMBLE
#' dimension confusion for \code{w}.
#'
#' @param spec A compiled \code{dpmixgpd_spec}.
#' @return nimbleCode object.
#' @keywords internal
#' @noRd
build_code_sb_from_spec <- function(spec) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  meta <- spec$meta
  kdef <- get_kernel_registry()[[meta$kernel]]

  dens_name <- spec$dispatch$density  # <- comes from compile_model_spec()

  N_sym <- as.name("N")
  J_sym <- as.name("J")
  P_sym <- as.name("P")

  bulk_params <- kdef$bulk_params %||% character(0)

  # ---- priors for bulk component params ----
  bulk_exprs <- list()
  for (pname in bulk_params) {
    d <- (spec$node_plan$bulk %||% list())[[pname]]
    if (is.null(d) || identical(d$type, "fixed")) next

    fam <- tolower(d$prior_family %||% "normal")
    if (identical(fam, "gamma")) {
      bulk_exprs[[length(bulk_exprs) + 1]] <-
        substitute(PNAME[j] ~ dgamma(gamma_shape, gamma_rate),
                   list(PNAME = as.name(pname)))
    } else {
      bulk_exprs[[length(bulk_exprs) + 1]] <-
        substitute(PNAME[j] ~ dnorm(normal_mean, sd = normal_sd),
                   list(PNAME = as.name(pname)))
    }
  }
  bulk_block <- substitute(for (j in 1:J) BODY,
                           list(J = J_sym, BODY = as.call(c(as.name("{"), bulk_exprs))))

  # ---- optional tail blocks (SB + GPD) ----
  threshold_block <- NULL
  tail_block <- NULL
  if (isTRUE(meta$GPD)) {
    if (isTRUE(meta$has_X)) {
      threshold_block <- substitute({
        for (k in 1:P) beta_threshold[k] ~ dnorm(beta_threshold_mean, sd = beta_threshold_sd)
        for (i in 1:N) threshold[i] <- exp(inprod(beta_threshold[1:P], X[i, 1:P]))
      }, list(N = N_sym, P = P_sym))
    } else {
      threshold_block <- substitute(threshold ~ dlnorm(threshold_meanlog, threshold_sdlog))
    }

    tail_block <- substitute({
      tail_scale ~ dlnorm(0, sdlog = tail_scale_sdlog)
      tail_shape ~ dnorm(0, sd = tail_shape_sd)
    })
  }

  # ---- model pieces ----
  pieces <- list(
    as.name("{"),
    substitute(alpha ~ dgamma(alpha_shape, alpha_rate)),
    substitute(for (j in 1:(J-1)) v[j] ~ dbeta(1, alpha), list(J = J_sym)),
    substitute(weights[1:J] <- stick_breaking(v[1:(J-1)]), list(J = J_sym)),
    bulk_block
  )

  if (!is.null(threshold_block)) pieces <- c(pieces, list(threshold_block))
  if (!is.null(tail_block))      pieces <- c(pieces, list(tail_block))

  # ---- likelihood: SB ALWAYS uses MIX density ----
  # Mix densities in your package are defined like: dLognormalMix(x, w = w, meanlog = ..., sdlog = ..., log = 0)
  like_args <- list()

  # Named weights argument (this is the key fix)
  like_args[["w"]] <- substitute(weights[1:J], list(J = J_sym))

  # Bulk params passed by NAME
  for (pname in bulk_params) {
    like_args[[pname]] <- substitute(PNAME[1:J], list(PNAME = as.name(pname), J = J_sym))
  }

  # Tail params if GPD
  if (isTRUE(meta$GPD)) {
    like_args[["threshold"]]  <- if (isTRUE(meta$has_X)) substitute(threshold[i]) else as.name("threshold")
    like_args[["tail_scale"]] <- as.name("tail_scale")
    like_args[["tail_shape"]] <- as.name("tail_shape")
  }

  like_call <- as.call(c(as.name(dens_name), like_args))
  pieces <- c(pieces, list(
    substitute(for (i in 1:N) y[i] ~ DENS, list(N = N_sym, DENS = like_call))
  ))

  do.call(nimble::nimbleCode, list(as.call(pieces)))
}




#' CRP backend: build nimbleCode from spec
#'
#' CRP unconditional uses \code{z} cluster labels and scalar kernel density dispatch.
#' If conditional is required, it builds per-(i,k) parameter arrays for all bulk nodes
#' with \code{type=="link"}, and then indexes them by \code{z[i]} in the likelihood.
#'
#' @param spec A compiled \code{dpmixgpd_spec}.
#' @return nimbleCode object.
#' @keywords internal
#' @noRd
build_code_crp_from_spec <- function(spec) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  .stopf <- function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)

  meta <- spec$meta %||% list()
  kdef <- get_kernel_registry()[[meta$kernel]]
  if (is.null(kdef)) .stopf("Kernel '%s' not found in registry.", meta$kernel)

  N_sym <- as.name("N")
  K_sym <- as.name("Kmax")
  P_sym <- as.name("P")

  bulk_params <- kdef$bulk_params %||% character(0)

  # CRP uses non-mix densities always (base or *Gpd)
  dens_name <- if (isTRUE(meta$GPD)) kdef$crp$d_gpd else kdef$crp$d_base
  if (is.null(dens_name) || length(dens_name) != 1L || !nzchar(dens_name)) {
    .stopf("CRP density dispatch missing for kernel '%s'.", meta$kernel)
  }

  # ---- priors/constants ----
  # (These are read from constants by NIMBLE; keep generic names consistent with your constants builder)
  alpha_line <- substitute(alpha ~ dgamma(alpha_shape, alpha_rate))

  # ✅ FIX: size MUST equal N (length of z[1:N])
  z_line <- substitute(z[1:N] ~ dCRP(alpha, size = N), list(N = N_sym))

  # ---- component parameter priors over k=1..Kmax ----
  bulk_prior_exprs <- list()
  for (pname in bulk_params) {
    d <- (spec$node_plan$bulk %||% list())[[pname]]
    if (is.null(d) || identical(d$type, "fixed")) next

    fam <- tolower(d$prior_family %||% "normal")
    if (identical(fam, "gamma")) {
      bulk_prior_exprs[[length(bulk_prior_exprs) + 1]] <-
        substitute(PNAME[k] ~ dgamma(gamma_shape, gamma_rate),
                   list(PNAME = as.name(pname)))
    } else {
      bulk_prior_exprs[[length(bulk_prior_exprs) + 1]] <-
        substitute(PNAME[k] ~ dnorm(normal_mean, sd = normal_sd),
                   list(PNAME = as.name(pname)))
    }
  }
  bulk_block <- substitute(for (k in 1:Kmax) BODY,
                           list(Kmax = K_sym,
                                BODY = as.call(c(as.name("{"), bulk_prior_exprs))))

  # ---- threshold regression + tail priors (only if GPD) ----
  threshold_block <- NULL
  tail_block <- NULL

  if (isTRUE(meta$GPD)) {
    if (isTRUE(meta$has_X)) {
      threshold_block <- substitute({
        for (k in 1:P) beta_threshold[k] ~ dnorm(beta_threshold_mean, sd = beta_threshold_sd)
        for (i in 1:N) threshold[i] <- exp(inprod(beta_threshold[1:P], X[i, 1:P]))
      }, list(N = N_sym, P = P_sym))
    } else {
      threshold_block <- substitute(threshold ~ dlnorm(threshold_meanlog, threshold_sdlog))
    }

    tail_block <- substitute({
      tail_scale ~ dlnorm(0, sdlog = tail_scale_sdlog)
      tail_shape ~ dnorm(0, sd = tail_shape_sd)
    })
  }

  # ---- likelihood: y[i] ~ d<Kernel>( param = param[z[i]] , ... ) ----
  like_args <- list()
  for (pname in bulk_params) {
    like_args[[pname]] <- substitute(PNAME[z[i]], list(PNAME = as.name(pname)))
  }
  if (isTRUE(meta$GPD)) {
    like_args[["threshold"]]  <- if (isTRUE(meta$has_X)) substitute(threshold[i]) else as.name("threshold")
    like_args[["tail_scale"]] <- as.name("tail_scale")
    like_args[["tail_shape"]] <- as.name("tail_shape")
  }
  like_call <- as.call(c(as.name(dens_name), like_args))

  obs_line <- substitute(for (i in 1:N) y[i] ~ DENS,
                         list(N = N_sym, DENS = like_call))

  # ---- assemble nimbleCode ----
  pieces <- list(as.name("{"), alpha_line, z_line, bulk_block)
  if (!is.null(threshold_block)) pieces <- c(pieces, list(threshold_block))
  if (!is.null(tail_block))      pieces <- c(pieces, list(tail_block))
  pieces <- c(pieces, list(obs_line))

  do.call(nimble::nimbleCode, list(as.call(pieces)))
}




#' Validate a dpmixgpd_bundle before running MCMC
#'
#' @param bundle A \code{dpmixgpd_bundle}.
#' @param strict Logical; if TRUE, stop() on errors.
#' @return A list with $ok, $errors, $warnings, and some key derived info.
#' @keywords internal
#' @export
check_dpmixgpd_bundle <- function(bundle, strict = FALSE) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  errors <- character(0)
  warns  <- character(0)

  if (!inherits(bundle, "dpmixgpd_bundle")) {
    errors <- c(errors, "Object is not a dpmixgpd_bundle.")
    out <- list(ok = FALSE, errors = errors, warnings = warns)
    if (isTRUE(strict)) stop(paste(errors, collapse = "\n"), call. = FALSE)
    return(out)
  }

  spec <- bundle$spec %||% list()
  meta <- spec$meta %||% list()

  # ---- data checks ----
  y <- bundle$data$y %||% spec$data$y %||% NULL
  if (is.null(y)) errors <- c(errors, "Missing y in bundle$data$y (and spec$data$y).")
  if (!is.null(y) && !is.numeric(y)) errors <- c(errors, "y must be numeric.")
  if (!is.null(y) && any(!is.finite(y))) warns <- c(warns, "y contains non-finite values (NA/Inf).")

  X <- bundle$data$X %||% spec$data$X %||% NULL
  has_X <- !is.null(X)
  if (has_X) {
    Xm <- as.matrix(X)
    if (nrow(Xm) != length(y)) errors <- c(errors, "X rows must equal length(y).")
  }

  # ---- meta checks ----
  backend <- meta$backend %||% spec$dispatch$backend %||% NULL
  kernel  <- meta$kernel  %||% spec$kernel$key %||% NULL
  if (is.null(backend) || !backend %in% c("sb","crp")) errors <- c(errors, "meta$backend must be 'sb' or 'crp'.")
  if (is.null(kernel) || !is.character(kernel) || length(kernel) != 1) errors <- c(errors, "meta$kernel must be a single string.")

  # ---- dimensions / constants sanity ----
  N <- meta$N %||% spec$N %||% if (!is.null(y)) length(y) else NA_integer_
  if (!is.na(N) && !is.null(y) && N != length(y)) warns <- c(warns, sprintf("meta/spec N=%s but length(y)=%s; using length(y) at runtime.", N, length(y)))

  # SB needs J, CRP needs Kmax
  if (identical(backend, "sb")) {
    J <- meta$J %||% spec$J %||% NA_integer_
    if (is.na(J) || J < 2) errors <- c(errors, "SB backend requires J >= 2.")
  }
  if (identical(backend, "crp")) {
    Kmax <- meta$Kmax %||% spec$Kmax %||% NA_integer_
    if (is.na(Kmax) || Kmax < 2) errors <- c(errors, "CRP backend requires Kmax >= 2.")
  }

  # ---- monitors checks ----
  mons <- bundle$monitors %||% character(0)
  if (!length(mons)) warns <- c(warns, "No monitors specified; runMCMC will return empty samples.")

  # common “I forgot tail nodes” check
  GPD <- isTRUE(meta$GPD %||% FALSE)
  if (GPD) {
    need <- c("threshold","tail_scale","tail_shape")
    missing_tail <- setdiff(need, mons)
    if (length(missing_tail)) warns <- c(warns, paste0("GPD=TRUE but monitors missing tail nodes: ", paste(missing_tail, collapse=", ")))
  }

  # ---- inits check: function should run ----
  inits_obj <- bundle$inits_fun %||% bundle$inits %||% NULL
  if (is.null(inits_obj)) {
    warns <- c(warns, "No inits_fun/inits provided; nimble will initialize where possible.")
  } else {
    inits_val <- tryCatch(if (is.function(inits_obj)) inits_obj() else inits_obj,
                          error = function(e) e)
    if (inherits(inits_val, "error")) {
      errors <- c(errors, paste0("inits function failed: ", inits_val$message))
    } else if (!is.list(inits_val)) {
      errors <- c(errors, "inits must return a list.")
    }
  }

  # ---- seed / chains check (runner can recycle length-1 seed, but warn) ----
  mcmc <- bundle$mcmc %||% list()
  nchains <- as.integer(mcmc$nchains %||% 1)
  seed <- mcmc$seed %||% NULL
  if (!is.null(seed) && length(seed) == 1L && nchains > 1L) {
    warns <- c(warns, "mcmc$seed is length 1 but nchains > 1; runner will recycle seed across chains unless you provide length nchains.")
  }
  if (!is.null(seed) && length(seed) > 1L && length(seed) != nchains) {
    errors <- c(errors, "mcmc$seed must be length 1 or length nchains.")
  }

  ok <- length(errors) == 0L
  out <- list(
    ok = ok,
    errors = errors,
    warnings = warns,
    backend = backend,
    kernel = kernel,
    GPD = GPD,
    N = if (!is.null(y)) length(y) else N,
    P = if (has_X) ncol(as.matrix(X)) else 0L
  )
  if (isTRUE(strict) && !ok) stop(paste(errors, collapse = "\n"), call. = FALSE)
  out
}





#' Build a prior table from a spec (internal)
#'
#' @param spec A compiled spec.
#' @return data.frame describing parameter support and prior.
#' @keywords internal
#' @noRd
#' @export
build_prior_table_from_spec <- function(spec) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  kdef <- get_kernel_registry()[[spec$meta$kernel]]
  bulk_params <- kdef$bulk_params

  rows <- list()

  # bulk
  for (p in bulk_params) {
    d <- spec$node_plan$bulk[[p]]
    if (is.null(d)) next
    if (identical(d$type, "fixed")) {
      pr <- paste0("fixed = ", d$fixed_value %||% "<fixed>")
    } else {
      fam <- tolower(d$prior_family %||% "normal")
      if (fam == "normal") pr <- sprintf("normal(mean=%s, sd=%s)", d$mean %||% "normal_mean", d$sd %||% "normal_sd")
      if (fam == "gamma")  pr <- sprintf("gamma(shape=%s, rate=%s)", d$shape %||% "gamma_shape", d$rate %||% "gamma_rate")
      if (fam == "invgamma") pr <- sprintf("invgamma(shape=%s, rate=%s)", d$shape %||% "gamma_shape", d$rate %||% "gamma_rate")
    }

    support <- d$support %||% if (p %in% c("sd","scale","shape1","shape2")) "positive" else "real"
    rows[[length(rows) + 1]] <- data.frame(
      parameter = p,
      support = support,
      prior = pr,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}




#' Run MCMC for a prepared bundle (manual runner; internal)
#'
#' @param bundle A \code{dpmixgpd_bundle} from \code{build_nimble_bundle()}.
#' @param show_progress Logical; passed to nimble.
#' @param compile Logical; whether to compile model and MCMC.
#' @return A fitted object of class \code{"mixgpd_fit"}.
#' @keywords internal
#' @noRd
run_mcmc_bundle_manual <- function(bundle, show_progress = TRUE, compile = TRUE) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  stopifnot(inherits(bundle, "dpmixgpd_bundle"))

  spec <- bundle$spec
  meta <- spec$meta
  m <- bundle$mcmc %||% list()

  code <- bundle$code
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

  # ---------------------------
  # Build model
  # ---------------------------
  Rmodel <- nimble::nimbleModel(
    code = code,
    data = data,
    constants = constants,
    inits = inits_fun(),
    dimensions = dims,
    check = TRUE,
    calculate = FALSE
  )

  # ---------------------------
  # Configure MCMC
  # ---------------------------
  conf <- nimble::configureMCMC(
    Rmodel,
    monitors = monitors,
    enableWAIC = TRUE
  )

  # ---------------------------
  # CRP sampler: ensure checkConjugacy exists (NIMBLE expects it)
  # ---------------------------
  if (identical(meta$backend, "crp")) {
    if ("z" %in% conf$getSamplers()) conf$removeSamplers("z")

    stoch_nodes <- Rmodel$getNodeNames(stochOnly = TRUE, includeData = FALSE)
    cluster_nodes <- setdiff(stoch_nodes, "z")

    conf$addSampler(
      target = "z",
      type = "CRP",
      control = list(
        clusterNodes = cluster_nodes,
        checkConjugacy = FALSE
      )
    )
  }

  # ---------------------------
  # Bulletproof patch: if any samplerConf is missing checkConjugacy, set it
  # ---------------------------
  if (!is.null(conf$samplerConfs) && length(conf$samplerConfs) > 0) {
    for (i in seq_along(conf$samplerConfs)) {
      ctl <- conf$samplerConfs[[i]]$control
      if (is.null(ctl)) ctl <- list()
      if (is.null(ctl$checkConjugacy)) ctl$checkConjugacy <- FALSE
      conf$samplerConfs[[i]]$control <- ctl
    }
  }

  # ---------------------------
  # Build MCMC object
  # ---------------------------
  Rmcmc <- nimble::buildMCMC(conf)

  # ---------------------------
  # Compile if requested
  # ---------------------------
  if (isTRUE(compile)) {
    Cmodel <- nimble::compileNimble(Rmodel, showCompilerOutput = FALSE)
    Cmcmc  <- nimble::compileNimble(Rmcmc, project = Rmodel, showCompilerOutput = FALSE)
  } else {
    Cmodel <- NULL
    Cmcmc  <- NULL
  }

  niter   <- as.integer(m$niter   %||% 2000)
  nburnin <- as.integer(m$nburnin %||% 500)
  thin    <- as.integer(m$thin    %||% 1)
  nchains <- as.integer(m$nchains %||% 1)

  # Seeds: must be length == nchains when provided
  seed <- m$seed %||% NULL
  if (!is.null(seed)) {
    seed <- as.integer(seed)
    if (length(seed) == 1L && nchains > 1L) seed <- seed + seq_len(nchains) - 1L
    if (length(seed) != nchains) stop("mcmc$seed must be length 1 or length nchains.", call. = FALSE)
  }

  # Inits: list-of-lists for multiple chains
  if (nchains > 1L) {
    inits_list <- vector("list", nchains)
    for (ch in seq_len(nchains)) {
      if (!is.null(seed)) set.seed(seed[ch])
      inits_list[[ch]] <- inits_fun()
    }
  } else {
    if (!is.null(seed)) set.seed(seed[1])
    inits_list <- inits_fun()
  }

  engine_mcmc <- if (isTRUE(compile)) Cmcmc else Rmcmc

  # Run MCMC (try WAIC; fall back if nimble version doesn’t support it)
  res <- tryCatch(
    nimble::runMCMC(
      engine_mcmc,
      niter = niter,
      nburnin = nburnin,
      thin = thin,
      nchains = nchains,
      inits = inits_list,
      setSeed = seed,
      progressBar = isTRUE(show_progress),
      samplesAsCodaMCMC = TRUE,
      WAIC = TRUE
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
      setSeed = seed,
      progressBar = isTRUE(show_progress),
      samplesAsCodaMCMC = TRUE
    )
    waic_obj <- NULL
  } else {
    if (is.list(res) && !is.null(res$samples)) {
      samples <- res$samples
      waic_obj <- res$WAIC %||% res$waic %||% NULL
    } else {
      samples <- res
      waic_obj <- attr(res, "WAIC") %||% NULL
    }
  }

  fit <- list(
    call = match.call(),
    spec = spec,
    data = data,
    model = if (isTRUE(compile)) Cmodel else Rmodel,
    mcmc_conf = conf,
    mcmc = list(
      engine = if (isTRUE(compile)) "compiled" else "interpreted",
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
    cache = list()
  )

  fit$samples <- samples
  fit$waic <- waic_obj

  class(fit) <- unique(c("mixgpd_fit", "list"))
  fit
}
