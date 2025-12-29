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

  function() {
    if (!is.null(seed)) set.seed(as.integer(seed)[1])

    # N
    N <- as.integer(meta$N %||% spec$N %||% length(spec$data$y))

    # X / P
    X <- spec$data$X
    has_X <- !is.null(X)
    P <- meta$P %||% spec$P %||% meta$p %||% spec$p
    if (has_X) {
      if (is.null(P) || length(P) != 1L || !is.finite(P)) {
        P <- ncol(as.matrix(X))
      }
      P <- as.integer(P)
      if (P < 1L) stop("Invalid predictor dimension P for X.", call. = FALSE)
    } else {
      P <- 0L
    }

    inits <- list()

    # DP inits
    if (identical(meta$backend, "crp")) {
      Kmax <- as.integer(meta$Kmax %||% spec$Kmax %||% 10L)
      if (Kmax < 2L) stop("Invalid Kmax for CRP inits.", call. = FALSE)
      inits$alpha <- 1.0
      inits$z <- sample.int(Kmax, N, replace = TRUE)

    } else if (identical(meta$backend, "sb")) {
      J <- as.integer(meta$J %||% spec$J)
      if (J < 2L) stop("Invalid J for SB inits.", call. = FALSE)
      inits$alpha <- 1.0
      inits$v <- rep(0.5, J - 1L)
      # weights are deterministic via stick_breaking(); don't init
    } else {
      stop("Unknown backend in spec: ", meta$backend, call. = FALSE)
    }

    # Bulk parameter inits
    bulk_plan <- spec$node_plan$bulk %||% list()

    if (identical(meta$backend, "crp")) {
      Kmax <- as.integer(meta$Kmax %||% spec$Kmax)
      for (pname in names(bulk_plan)) {
        d <- bulk_plan[[pname]]
        if (is.null(d) || identical(d$type, "fixed")) next

        if (!is.null(d$support) && identical(d$support, "real")) {
          inits[[pname]] <- rnorm(Kmax, 0, 0.5)
        } else if (!is.null(d$support) && grepl("positive", d$support)) {
          inits[[pname]] <- rep(1.0, Kmax)
        } else {
          inits[[pname]] <- rep(0.1, Kmax)
        }
      }

    } else if (identical(meta$backend, "sb")) {
      J <- as.integer(meta$J %||% spec$J)
      for (pname in names(bulk_plan)) {
        d <- bulk_plan[[pname]]
        if (is.null(d) || identical(d$type, "fixed")) next

        if (!is.null(d$support) && identical(d$support, "real")) {
          inits[[pname]] <- rnorm(J, 0, 0.5)
        } else if (!is.null(d$support) && grepl("positive", d$support)) {
          inits[[pname]] <- rep(1.0, J)
        } else {
          inits[[pname]] <- rep(0.1, J)
        }
      }
    }

    # Tail / threshold inits
    if (isTRUE(meta$GPD)) {
      if (has_X) {
        inits$beta_threshold <- rep(0, P)
      } else {
        inits$threshold <- median(spec$data$y)
      }
      inits$tail_scale <- 1.0
      inits$tail_shape <- 0.1
    }

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

  meta <- spec$meta
  pri  <- spec$priors %||% list()

  const <- list(
    N = as.integer(meta$N)
  )

  if (isTRUE(meta$has_X)) {
    const$P <- as.integer(meta$P)
    const$p <- as.integer(meta$P)  # backward-compatible alias
  }

  if (identical(meta$backend, "sb")) {
    const$J <- as.integer(meta$J)
  } else {
    const$Kmax <- as.integer(meta$Kmax)
  }

  # symbols referenced in code
  const$alpha_shape <- as.numeric(pri$alpha_shape %||% 1)
  const$alpha_rate  <- as.numeric(pri$alpha_rate  %||% 1)

  const$normal_mean <- as.numeric(pri$normal_mean %||% 0)
  const$normal_sd   <- as.numeric(pri$normal_sd   %||% 10)

  const$gamma_shape <- as.numeric(pri$gamma_shape %||% 2)
  const$gamma_rate  <- as.numeric(pri$gamma_rate  %||% 1)

  if (isTRUE(meta$GPD)) {
    const$threshold_meanlog <- as.numeric(pri$threshold_meanlog %||% 0)
    const$threshold_sdlog   <- as.numeric(pri$threshold_sdlog   %||% 1)

    const$beta_threshold_mean <- as.numeric(pri$beta_threshold_mean %||% 0)
    const$beta_threshold_sd   <- as.numeric(pri$beta_threshold_sd   %||% 10)

    const$tail_scale_sdlog <- as.numeric(pri$tail_scale_sdlog %||% 1)
    const$tail_shape_sd    <- as.numeric(pri$tail_shape_sd    %||% 1)
  }

  const
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



