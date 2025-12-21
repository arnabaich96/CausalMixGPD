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
    inits      = build_inits_function(spec, y),
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
build_inits_function <- function(spec, y) {
  meta <- spec$meta
  backend <- meta$backend

  function() {
    inits <- list()

    # Concentration
    inits$alpha <- 1

    if (identical(backend, "sb")) {
      J <- as.integer(meta$J)
      inits$v <- rep(0.5, J - 1L)

      # Bulk params
      for (pname in names(spec$node_plan$bulk)) {
        d <- spec$node_plan$bulk[[pname]]
        if (identical(d$type, "fixed")) next
        inits[[pname]] <- rep(1, J)
        if (!is.null(d$prior_family) && tolower(d$prior_family) == "invgamma") {
          inits[[paste0("inv_", pname)]] <- rep(1, J)
        }
      }

      # Conditional regression coefs if needed
      if (spec_requires_conditional(spec)) {
        p <- as.integer(meta$p)
        for (pname in names(spec$node_plan$bulk)) {
          d <- spec$node_plan$bulk[[pname]]
          if (identical(d$type, "link")) {
            inits[[paste0("beta_", pname)]] <- matrix(0, nrow = p, ncol = J)
          }
        }
      }

    } else {

      N <- as.integer(meta$N)
      Kmax <- as.integer(meta$Kmax)
      inits$z <- sample.int(Kmax, N, replace = TRUE)

      # Bulk params by cluster
      for (pname in names(spec$node_plan$bulk)) {
        d <- spec$node_plan$bulk[[pname]]
        if (identical(d$type, "fixed")) next
        inits[[pname]] <- rep(1, Kmax)
        if (!is.null(d$prior_family) && tolower(d$prior_family) == "invgamma") {
          inits[[paste0("inv_", pname)]] <- rep(1, Kmax)
        }
      }

      # Conditional regression coefs if needed
      if (spec_requires_conditional(spec)) {
        p <- as.integer(meta$p)
        for (pname in names(spec$node_plan$bulk)) {
          d <- spec$node_plan$bulk[[pname]]
          if (identical(d$type, "link")) {
            inits[[paste0("beta_", pname)]] <- matrix(0, nrow = p, ncol = Kmax)
          }
        }
      }
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
  meta <- spec$meta
  const <- list(N = as.integer(meta$N))

  if (isTRUE(meta$has_X)) const$p <- as.integer(meta$p)

  const$Kmax <- as.integer(meta$Kmax)
  const
}


#' Build explicit dimensions to avoid NIMBLE size inference issues (internal)
#'
#' @param spec A compiled \code{dpmixgpd_spec}.
#' @return Named list of dimensions.
#' @keywords internal
#' @noRd
build_dimensions_from_spec <- function(spec) {
  meta <- spec$meta
  backend <- meta$backend
  dims <- list()

  if (identical(backend, "sb")) {
    J <- as.integer(meta$J)
    dims$v <- as.integer(J - 1L)
    dims$w <- as.integer(J)
    dims$prod1 <- as.integer(J - 1L)

    for (pname in names(spec$node_plan$bulk)) {
      d <- spec$node_plan$bulk[[pname]]
      if (identical(d$type, "fixed")) next
      dims[[pname]] <- as.integer(J)
      if (!is.null(d$prior_family) && tolower(d$prior_family) == "invgamma") {
        dims[[paste0("inv_", pname)]] <- as.integer(J)
      }
      if (identical(d$type, "link")) {
        dims[[paste0("beta_", pname)]] <- c(as.integer(meta$p), as.integer(J))
        dims[[paste0("lp_", pname)]] <- c(as.integer(meta$N), as.integer(J))
      }
    }

    if (isTRUE(meta$GPD)) {
      for (tp in names(spec$node_plan$tail)) {
        td <- spec$node_plan$tail[[tp]]
        if (identical(td$type, "fixed")) next

        if (identical(td$type, "link")) {
          dims[[paste0("beta_", tp)]] <- as.integer(meta$p)
          dims[[paste0("lp_", tp)]] <- as.integer(meta$N)
          dims[[tp]] <- as.integer(meta$N)
        } else {
          dims[[tp]] <- as.integer(1L)
          if (!is.null(td$prior_family) && tolower(td$prior_family) == "invgamma") {
            dims[[paste0("inv_", tp)]] <- as.integer(1L)
          }
        }
      }
    }

  } else {
    dims$z <- as.integer(meta$N)
    Kmax <- as.integer(meta$Kmax)

    for (pname in names(spec$node_plan$bulk)) {
      d <- spec$node_plan$bulk[[pname]]
      if (identical(d$type, "fixed")) next
      dims[[pname]] <- as.integer(Kmax)
      if (!is.null(d$prior_family) && tolower(d$prior_family) == "invgamma") {
        dims[[paste0("inv_", pname)]] <- as.integer(Kmax)
      }
      if (identical(d$type, "link")) {
        dims[[paste0("beta_", pname)]] <- c(as.integer(meta$p), as.integer(Kmax))
        dims[[paste0("lp_", pname)]] <- as.integer(meta$N)
        dims[[paste0(pname, "_i")]] <- as.integer(meta$N)
      }
    }

    if (isTRUE(meta$GPD)) {
      for (tp in names(spec$node_plan$tail)) {
        td <- spec$node_plan$tail[[tp]]
        if (identical(td$type, "fixed")) next

        if (identical(td$type, "link")) {
          dims[[paste0("beta_", tp)]] <- as.integer(meta$p)
          dims[[paste0("lp_", tp)]] <- as.integer(meta$N)
          dims[[tp]] <- as.integer(meta$N)
        } else {
          dims[[tp]] <- as.integer(1L)
          if (!is.null(td$prior_family) && tolower(td$prior_family) == "invgamma") {
            dims[[paste0("inv_", tp)]] <- as.integer(1L)
          }
        }
      }
    }
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

  N_sym <- as.name("N")
  J_sym <- as.name("J")
  bulk_params <- kdef$bulk_params

  # --- priors for component parameters (indexed by component id) ---
  bulk_exprs <- list()
  for (pname in bulk_params) {
    d <- spec$node_plan$bulk[[pname]]
    if (is.null(d) || identical(d$type, "fixed")) next
    fam <- tolower(d$prior_family %||% "normal")
    if (identical(fam, "gamma")) {
      bulk_exprs[[length(bulk_exprs) + 1]] <- substitute(PNAME[j] ~ dgamma(gamma_shape, gamma_rate),
                                                         list(PNAME = as.name(pname)))
    } else {
      bulk_exprs[[length(bulk_exprs) + 1]] <- substitute(PNAME[j] ~ dnorm(normal_mean, sd = normal_sd),
                                                         list(PNAME = as.name(pname)))
    }
  }
  bulk_block <- substitute(for (j in 1:J) BODY,
                           list(J = J_sym, BODY = as.call(c(as.name("{"), bulk_exprs))))

  # --- optional threshold regression for GPD ---
  threshold_block <- NULL
  if (isTRUE(meta$GPD)) {
    if (isTRUE(meta$has_X)) {
      threshold_block <- as.call(c(
        as.name("{"),
        substitute(for (k in 1:p) beta_threshold[k] ~ dnorm(0, sd = 10)),
        substitute(for (i in 1:N) threshold[i] <- exp(inprod(beta_threshold[1:p], X[i, 1:p])),
                   list(N = N_sym))
      ))
    } else {
      threshold_block <- substitute(threshold ~ dlnorm(threshold_meanlog, threshold_sdlog))
    }
  }

  # --- stick-breaking weights via latent allocation z[i] ---
  # This avoids illegal dynamic indexing like w[1:J] where J is not constant in compiled code.
  pieces <- list(
    as.name("{"),
    substitute(alpha ~ dgamma(alpha_shape, alpha_rate)),
    substitute(for (j in 1:(J-1)) v[j] ~ dbeta(1, alpha), list(J = J_sym)),
    substitute(w[1] <- v[1]),
    substitute(prod1[1] <- (1 - v[1])),
    substitute(for (m in 2:(J-1)) prod1[m] <- prod1[m-1] * (1 - v[m]), list(J = J_sym)),
    substitute(for (j in 2:(J-1)) w[j] <- v[j] * prod1[j-1], list(J = J_sym)),
    substitute(w[J] <- prod1[J-1], list(J = J_sym)),
    bulk_block,
    substitute(for (i in 1:N) z[i] ~ dcat(w[1:J]), list(N = N_sym, J = J_sym))
  )

  if (!is.null(threshold_block)) pieces <- c(pieces, list(threshold_block))

  # likelihood: y[i] depends on component-specific parameters via scalar z[i]
  like_args <- list()
  for (pname in bulk_params) {
    like_args[[length(like_args) + 1]] <- substitute(PNAME[z[i]], list(PNAME = as.name(pname)))
  }
  if (isTRUE(meta$GPD)) {
    # NOTE: no named args; users want plain positional args.
    like_args[[length(like_args) + 1]] <- if (isTRUE(meta$has_X)) substitute(threshold[i]) else as.name("threshold")
    like_args[[length(like_args) + 1]] <- as.name("tail_scale")
    like_args[[length(like_args) + 1]] <- as.name("tail_shape")
    dens_name <- kdef$dens_gpd
  } else {
    dens_name <- kdef$dens
  }
  like_call <- as.call(c(as.name(dens_name), like_args))
  pieces <- c(pieces, list(substitute(for (i in 1:N) y[i] ~ DENS, list(N = N_sym, DENS = like_call))))

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

  meta  <- spec$meta
  N     <- spec$N
  Kmax  <- as.integer(meta$Kmax %||% spec$Kmax %||% 10L)
  kdef  <- get_kernel_registry()[[meta$kernel]]

  # ---- priors (defaults, but overridden if spec$priors provides them) ----
  pri <- spec$priors %||% list()
  alpha_shape <- pri$alpha_shape %||% 1
  alpha_rate  <- pri$alpha_rate  %||% 1

  # Component parameter priors (simple, generic defaults)
  # mean-like params: N(0, 10); positive params: half-normal via trunc by exp on a log scale
  bulk_params <- kdef$bulk_params %||% character(0)

  bulk_prior_block <- function() {
    exprs <- list()
    for (pname in bulk_params) {
      # allow user-specified prior per param: pri$bulk[[pname]] = list(family=..., ...)
      pp <- pri$bulk[[pname]] %||% list()
      fam <- tolower(pp$family %||% "normal")
      if (identical(fam, "gamma")) {
        sh <- pp$shape %||% 2
        rt <- pp$rate  %||% 1
        exprs[[length(exprs) + 1]] <- bquote(.(as.name(pname))[k] ~ dgamma(.(sh), .(rt)))
      } else {
        mu <- pp$mean %||% 0
        sd <- pp$sd   %||% 10
        exprs[[length(exprs) + 1]] <- bquote(.(as.name(pname))[k] ~ dnorm(.(mu), sd = .(sd)))
      }
    }
    if (!length(exprs)) return(NULL)
    bquote(for (k in 1:.(Kmax)) { .(as.call(c(quote(`{`), exprs))) })
  }

  # ---- tail / threshold blocks ----
  threshold_block <- NULL
  tail_block <- NULL
  threshold_expr <- NULL

  if (isTRUE(meta$GPD)) {
    # threshold regression (X) or scalar threshold
    if (isTRUE(meta$has_X)) {
      p <- spec$p
      bt_mean <- pri$beta_threshold_mean %||% 0
      bt_sd   <- pri$beta_threshold_sd   %||% 10
      threshold_block <- bquote({
        for (k in 1:.(p)) beta_threshold[k] ~ dnorm(.(bt_mean), sd = .(bt_sd))
        for (i in 1:.(N)) threshold[i] <- exp(inprod(beta_threshold[1:.(p)], X[i, 1:.(p)]))
      })
      threshold_expr <- quote(threshold[i])
    } else {
      th_mean <- pri$threshold_mean %||% 0
      th_sd   <- pri$threshold_sd   %||% 1
      threshold_block <- bquote({
        threshold ~ dlnorm(.(th_mean), sdlog = .(th_sd))
      })
      threshold_expr <- quote(threshold)
    }

    # tail parameters (generic priors)
    ts_sd <- pri$tail_scale_sd %||% 1
    xi_sd <- pri$tail_shape_sd %||% 1
    tail_block <- bquote({
      tail_scale ~ dlnorm(0, sdlog = .(ts_sd))
      tail_shape ~ dnorm(0, sd = .(xi_sd))
    })
  }

  # ---- likelihood ----
  dens_name <- spec$dispatch$density
  if (is.null(dens_name) || length(dens_name) != 1) {
    stop("CRP: spec$dispatch$density is missing/invalid.", call. = FALSE)
  }

  like_args <- list()
  for (pname in bulk_params) {
    like_args[[length(like_args) + 1]] <- bquote(.(as.name(pname))[z[i]])
  }
  if (isTRUE(meta$GPD)) {
    like_args[[length(like_args) + 1]] <- threshold_expr
    like_args[[length(like_args) + 1]] <- quote(tail_scale)
    like_args[[length(like_args) + 1]] <- quote(tail_shape)
  }
  like_call <- as.call(c(as.name(dens_name), like_args))

  code_exprs <- list(
    bquote(alpha ~ dgamma(.(alpha_shape), .(alpha_rate))),
    bquote(for (i in 1:.(N)) z[i] ~ dCRP(alpha, size = .(Kmax)))
  )

  bb <- bulk_prior_block()
  if (!is.null(bb)) code_exprs <- c(code_exprs, list(bb))
  if (!is.null(threshold_block)) code_exprs <- c(code_exprs, list(threshold_block))
  if (!is.null(tail_block)) code_exprs <- c(code_exprs, list(tail_block))

  code_exprs <- c(code_exprs, list(bquote(for (i in 1:.(N)) y[i] ~ .(like_call))))

  nimble::nimbleCode(as.call(c(quote(`{`), code_exprs)))
}


