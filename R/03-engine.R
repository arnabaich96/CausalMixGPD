#' Build nimble model gamma uncond
#'
#' Build nimble model gamma uncond.
#'
#' @param spec spec.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("build_nimble_model_gamma_uncond", "DPmixGPD")
#' f
#'
#' @keywords internal
build_nimble_model_gamma_uncond <- function(spec) {
  if (!requireNamespace("nimble", quietly = TRUE)) {
    stop("Package 'nimble' is required for the DP mixture engine.", call. = FALSE)
  }

  y <- spec$Y
  N <- length(y)

  K <- spec$dp_ctrl$K
  if (is.null(K) || K < 2L) {
    stop("dp_ctrl$K (number of mixture components) must be >= 2 for the Nimble engine.")
  }

  # ---- hyperparameters (Gamma / Inv-Gamma) ----
  priors <- spec$priors %||% list()

  # shape_j > 0  ~ Gamma(a_shape, b_shape)
  a_shape <- priors$a_shape %||% 2
  b_shape <- priors$b_shape %||% 1

  # scale_j > 0  ~ Gamma(a_scale, b_scale)
  # (if you prefer Inv-Gamma, we can switch to dinvgamma later)
  a_scale <- priors$a_scale %||% 2
  b_scale <- priors$b_scale %||% 1

  # DP concentration alpha > 0 ~ Gamma(a_alpha, b_alpha)
  a_alpha <- priors$a_alpha %||% 1
  b_alpha <- priors$b_alpha %||% 1

  code <- nimble::nimbleCode({
    # DP concentration parameter
    alpha ~ dgamma(a_alpha, b_alpha)

    # Stick-breaking variables
    for (j in 1:K) {
      v[j] ~ dbeta(1, alpha)
    }

    # mixture weights from stick-breaking
    w[1] <- v[1]
    for (j in 2:K) {
      w[j] <- v[j] * (1 - sum(w[1:(j - 1)]))
    }

    # Component parameters (all > 0 with Gamma priors)
    for (j in 1:K) {
      shape[j] ~ dgamma(a_shape, b_shape)
      scale[j] ~ dgamma(a_scale, b_scale)
    }

    # Allocations + likelihood
    for (i in 1:N) {
      z[i] ~ dcat(w[1:K])
      # Nimble's dgamma uses shape, rate; we parameterize by scale
      y[i] ~ dgamma(shape[z[i]], rate = 1 / scale[z[i]])
    }
  })

  constants <- list(
    N       = N,
    K       = K,
    a_shape = a_shape,
    b_shape = b_shape,
    a_scale = a_scale,
    b_scale = b_scale,
    a_alpha = a_alpha,
    b_alpha = b_alpha
  )

  data <- list(
    y = y
  )

  # crude but safe inits
  inits <- list(
    alpha = 1,
    v     = rep(0.5, K),
    shape = rep(2, K),
    scale = rep(stats::sd(y), K),
    z     = sample.int(K, N, replace = TRUE)
  )

  list(
    code      = code,
    constants = constants,
    data      = data,
    inits     = inits
  )
}

# small helper if you don't already have it

#' Internal: run Nimble MCMC for unconditional Gamma SB mixture
#' @keywords internal
#' Build nimble model gamma reg
#'
#' Build nimble model gamma reg.
#'
#' @param spec spec.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("build_nimble_model_gamma_reg", "DPmixGPD")
#' f
#'
#' @keywords internal
build_nimble_model_gamma_reg <- function(spec) {
  if (!requireNamespace("nimble", quietly = TRUE)) {
    stop("Package 'nimble' is required for the DP mixture engine.", call. = FALSE)
  }

  y <- spec$Y
  X <- spec$X
  N <- length(y)

  if (is.null(X)) {
    stop("build_nimble_model_gamma_reg() called with X = NULL.")
  }
  X <- as.matrix(X)
  p <- ncol(X)

  K <- spec$dp_ctrl$K
  if (is.null(K) || K < 2L) {
    stop("dp_ctrl$K (number of mixture components) must be >= 2 for the Nimble engine.")
  }

  priors <- spec$priors %||% list()

  # shape_j > 0  ~ Gamma(a_shape, b_shape)
  a_shape <- priors$a_shape %||% 2
  b_shape <- priors$b_shape %||% 1

  # regression coefficients for scale: beta_scale[j,1:p] ~ N(0, tau_beta)
  tau_beta <- priors$tau_beta %||% 0.01 # precision = 1 / variance

  # DP concentration alpha > 0 ~ Gamma(a_alpha, b_alpha)
  a_alpha <- priors$a_alpha %||% 1
  b_alpha <- priors$b_alpha %||% 1

  code <- nimble::nimbleCode({
    ## --- DP concentration ---
    alpha ~ dgamma(a_alpha, b_alpha)

    ## --- stick-breaking for v and weights w ---
    for (j in 1:K) {
      v[j] ~ dbeta(1, alpha)
    }
    w[1] <- v[1]
    for (j in 2:K) {
      w[j] <- v[j] * (1 - sum(w[1:(j - 1)]))
    }

    ## --- component parameters ---
    for (j in 1:K) {
      # positive shape per component
      shape[j] ~ dgamma(a_shape, b_shape)

      # regression coefficients for (log) scale
      for (q in 1:p) {
        beta_scale[j, q] ~ dnorm(0, tau_beta)
      }
    }

    ## --- linear predictors for each (i, j) pair ---
    for (i in 1:N) {
      for (j in 1:K) {
        eta_scale_ij[i, j] <- inprod(X[i, 1:p], beta_scale[j, 1:p])
      }
      eta_scale[i] <- eta_scale_ij[i, z[i]]
    }
    ## --- allocations + likelihood ---
    for (i in 1:N) {
      z[i] ~ dcat(w[1:K])
      # transform to positive scale
      scale[i] <- exp(eta_scale[i])

      # Nimble's dgamma uses shape, rate; we parameterize by scale
      y[i] ~ dgamma(shape[z[i]], 1.0 / scale[i])
    }
  })

  constants <- list(
    N        = N,
    K        = K,
    p        = p,
    a_shape  = a_shape,
    b_shape  = b_shape,
    tau_beta = tau_beta,
    a_alpha  = a_alpha,
    b_alpha  = b_alpha
  )

  data <- list(
    y = y,
    X = as.matrix(X)
  )

  # crude but stable initials
  y_mean <- mean(y)
  if (!is.finite(y_mean) || y_mean <= 0) y_mean <- 1

  inits <- list(
    alpha      = 1,
    v          = rep(0.5, K),
    shape      = stats::rgamma(K, a_shape, b_shape),
    beta_scale = matrix(0, nrow = K, ncol = p),
    z          = sample.int(K, N, replace = TRUE)
  )

  list(
    code      = code,
    constants = constants,
    data      = data,
    inits     = inits
  )
}
#' Run mcmc nimble gamma
#'
#' Run mcmc nimble gamma.
#'
#' @param spec spec.
#' @param mcmc mcmc.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("run_mcmc_nimble_gamma", "DPmixGPD")
#' f
#'
#' @keywords internal
run_mcmc_nimble_gamma <- function(spec, mcmc) {
  if (!requireNamespace("nimble", quietly = TRUE)) {
    stop("Package 'nimble' is required for the DP mixture engine.", call. = FALSE)
  }

  # ---- MCMC controls ----
  n_iter <- mcmc$n_iter %||% 2000L
  burn_in <- mcmc$burn_in %||% 1000L
  thin <- mcmc$thin %||% 1L
  chains <- mcmc$chains %||% 1L

  if (n_iter <= burn_in) {
    stop("mcmc$n_iter must be greater than mcmc$burn_in.")
  }
  if (chains < 1L) {
    stop("mcmc$chains must be >= 1.")
  }

  # ---- Choose model builder based on spec$mode ----
  if (identical(spec$mode, "response_only")) {
    mm <- build_nimble_model_gamma_uncond(spec)
  } else if (identical(spec$mode, "regression")) {
    # Transforms for the scale are now handled inside build_nimble_model_gamma_reg().
    # We only forbid shape regression for now.
    if (!is.null(spec$trans$shape)) {
      stop("Shape regression is not yet implemented in the Nimble gamma engine.")
    }

    mm <- build_nimble_model_gamma_reg(spec)
  } else {
    stop("Unknown spec$mode: ", spec$mode)
  }

  # ---- Build and compile Nimble model ----
  .ensure_nimble_compat()

  model <- nimble::nimbleModel(
    code      = mm$code,
    constants = mm$constants,
    data      = mm$data,
    inits     = mm$inits
  )

  cmodel <- nimble::compileNimble(model)

  # ---- Monitors: what to save from the chain ----
  if (identical(spec$mode, "response_only")) {
    monitors <- c("alpha", "v", "w", "shape", "scale")
  } else {
    # regression mode: save regression coefficients instead of per-i scale
    monitors <- c("alpha", "v", "w", "shape", "beta_scale")
  }

  conf <- nimble::configureMCMC(cmodel, monitors = monitors)
  mcmc_o <- nimble::buildMCMC(conf)
  cmcmc <- nimble::compileNimble(mcmc_o, project = cmodel)

  # ---- Run MCMC ----
  samples <- nimble::runMCMC(
    cmcmc,
    niter             = n_iter,
    nburnin           = burn_in,
    thin              = thin,
    nchains           = chains,
    samplesAsCodaMCMC = TRUE
  )

  # ---- Return in your standard structure ----
  list(
    mcmc_draws = samples,
    mcmc_info = list(
      n_iter  = n_iter,
      burn_in = burn_in,
      thin    = thin,
      chains  = chains
    )
  )
}

# Internal: extract Gamma DP component parameters from an MCMC draw matrix
#' Extract gamma dp params uncond
#'
#' Extract gamma dp params uncond.
#'
#' @param object object.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".extract_gamma_dp_params_uncond", "DPmixGPD")
#' f
#'
#' @keywords internal
.extract_gamma_dp_params_uncond_engine <- function(object) {
  draws <- .as_mcmc_matrix(object)
  cn <- colnames(draws)

  # ---- component counts ----
  shape_cols <- grep("^shape\\[", cn, value = TRUE)
  if (length(shape_cols) == 0L) {
    stop("No 'shape[j]' columns found in MCMC draws for Gamma DP.", call. = FALSE)
  }
  K <- length(shape_cols)

  # Re-order shape columns by component index
  shape_j <- as.integer(sub("^shape\\[([0-9]+)\\]$", "\\1", shape_cols))
  shape_cols <- shape_cols[order(shape_j)]
  shape_j <- sort(shape_j)

  # Scale/mean terms:
  #  - unconditional Gamma DP uses scale[j]
  #  - (older/alternate) intercept-only regression uses beta_scale[1,j] (log-mean)
  scale_cols <- grep("^scale\\[", cn, value = TRUE)
  beta_cols <- grep("^beta_scale\\[", cn, value = TRUE)
  beta_int_cols <- grep("^beta_scale\\[1,", beta_cols, value = TRUE)

  has_scale <- length(scale_cols) > 0L
  has_beta0 <- length(beta_int_cols) > 0L

  if (!has_scale && !has_beta0) {
    stop(
      "Cannot find Gamma scale parameters in MCMC draws.\n",
      "Expected either:\n",
      "  - 'scale[j]' (unconditional Gamma DP), or\n",
      "  - 'beta_scale[1, j]' (intercept-only scale/mean terms).",
      call. = FALSE
    )
  }

  if (has_scale) {
    # Order scale[j]
    scale_j <- as.integer(sub("^scale\\[([0-9]+)\\]$", "\\1", scale_cols))
    scale_cols <- scale_cols[order(scale_j)]
    scale_j <- sort(scale_j)
    if (!identical(shape_j, scale_j)) {
      stop("Mismatch between 'shape[j]' and 'scale[j]' component indices.",
        call. = FALSE
      )
    }
  } else {
    # Order beta_scale[1,j]
    beta_j <- as.integer(sub("^beta_scale\\[1,([0-9]+)\\]$", "\\1", beta_int_cols))
    beta_int_cols <- beta_int_cols[order(beta_j)]
    beta_j <- sort(beta_j)
    if (!identical(shape_j, beta_j)) {
      stop("Mismatch between 'shape[j]' and 'beta_scale[1,j]' component indices.",
        call. = FALSE
      )
    }
  }

  # Weights: prefer direct 'w[j]' if available, otherwise stick-breaking from v[j]
  w_cols <- grep("^w\\[", cn, value = TRUE)
  if (length(w_cols) > 0L) {
    w_j <- as.integer(sub("^w\\[([0-9]+)\\]$", "\\1", w_cols))
    w_cols <- w_cols[order(w_j)]
    w_j <- sort(w_j)
    if (!identical(w_j, shape_j)) {
      stop("Mismatch between 'w[j]' and 'shape[j]' indices.", call. = FALSE)
    }

    W <- draws[, w_cols, drop = FALSE]

    # ---- IMPORTANT: normalize mixture weights per iteration ----
    # NIMBLE sometimes yields w[j] that don't sum exactly to 1, which breaks
    # mixture CDF/quantiles because F(x) plateaus at sum(w) < 1.
    W <- pmax(W, 0)
    rs <- rowSums(W)
    bad <- !is.finite(rs) | rs <= 0
    if (any(bad)) {
      stop("Invalid mixture weights in some MCMC iterations (sum <= 0 or non-finite).",
        call. = FALSE
      )
    }
    W <- W / rs
  } else {
    # stick-breaking from v[1:(K-1)]
    v_cols <- grep("^v\\[", cn, value = TRUE)
    if (length(v_cols) == 0L) {
      stop("No 'w[j]' or 'v[j]' columns found in MCMC draws.", call. = FALSE)
    }
    v_j <- as.integer(sub("^v\\[([0-9]+)\\]$", "\\1", v_cols))
    v_cols <- v_cols[order(v_j)]
    v_j <- sort(v_j)

    if (length(v_cols) != K - 1L) {
      stop("For K components, expected K-1 'v[j]' stick-breaking terms.",
        call. = FALSE
      )
    }

    V <- draws[, v_cols, drop = FALSE] # M x (K-1)
    M <- nrow(V)
    W <- matrix(NA_real_, nrow = M, ncol = K)
    colnames(W) <- paste0("w[", 1:K, "]")

    # stick-breaking weights per iteration
    for (m in seq_len(M)) {
      v_m <- V[m, ]
      w_m <- numeric(K)
      prod_term <- 1
      for (j in seq_len(K - 1L)) {
        w_m[j] <- v_m[j] * prod_term
        prod_term <- prod_term * (1 - v_m[j])
      }
      w_m[K] <- prod_term

      # numerical cleanup + renormalize
      w_m <- pmax(w_m, 0)
      s <- sum(w_m)
      if (!is.finite(s) || s <= 0) {
        stop("Invalid stick-breaking weights in some MCMC iterations.", call. = FALSE)
      }
      w_m <- w_m / s

      W[m, ] <- w_m
    }
  }

  Shape <- draws[, shape_cols, drop = FALSE]

  if (has_scale) {
    Scale <- draws[, scale_cols, drop = FALSE]
    Mu <- Shape * Scale # mean = shape * scale
  } else {
    Beta0 <- draws[, beta_int_cols, drop = FALSE]
    Mu <- exp(Beta0) # your convention in the earlier predictive code
    Scale <- Mu / Shape # implied scale
  }

  list(
    W      = W, # M x K
    Shape  = Shape, # M x K
    Mu     = Mu, # M x K
    Scale  = Scale, # M x K (always returned, for convenience)
    n_iter = nrow(draws),
    K      = K
  )
}

# internal: posterior predictive CDF for Gamma DP mixture at scalar x
#' Pp cdf gamma uncond
#'
#' Pp cdf gamma uncond.
#'
#' @param params params.
#' @param x x.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".pp_cdf_gamma_uncond", "DPmixGPD")
#' f
#'
#' @keywords internal
.pp_cdf_gamma_uncond <- function(params, x) {
  # params from .extract_gamma_dp_params_uncond_engine(object)
  W <- params$W
  Shape <- params$Shape
  Mu <- params$Mu
  Rate <- Shape / Mu

  # F_m(x) = sum_j w_mj * stats::pgamma(x; shape, rate)
  Fm <- rowSums(W * stats::pgamma(x, shape = Shape, rate = Rate))
  mean(Fm)
}

# internal: quantile for Gamma DP mixture via uniroot
# Simple MCMC engine -----------------------------------------------------------
# Currently a lightweight stochastic sampler; can be replaced later
# by a full DP + GPD implementation without changing the interface.
#' Simulate chain
#'
#' Simulate chain.
#'
#' @param spec spec.
#' @param n_iter n_iter.
#' @param burn_in burn_in.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("simulate_chain", "DPmixGPD")
#' f
#'
#' @keywords internal
simulate_chain <- function(spec, n_iter, burn_in) {
  Y <- spec$Y
  N <- spec$N

  y_mean <- mean(Y)
  y_sd   <- stats::sd(Y)
  if (!is.finite(y_sd) || y_sd <= 0) y_sd <- 1

  n_keep <- max(1L, n_iter - burn_in)
  mu        <- numeric(n_keep)
  log_sigma <- numeric(n_keep)

  for (t in seq_len(n_keep)) {
    mu[t]        <- stats::rnorm(1L, mean = y_mean, sd = y_sd / sqrt(N))
    log_sigma[t] <- stats::rnorm(1L, mean = log(y_sd), sd = 0.1)
  }

  draws <- cbind(mu = mu, log_sigma = log_sigma)
  coda::mcmc(draws)
}

# Main MCMC dispatcher ---------------------------------------------------------
# spec is a "mixgpd_spec" from build_model_spec_xy()
#' Run mcmc engine
#'
#' Run mcmc engine.
#'
#' @param spec spec.
#' @param mcmc mcmc.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("run_mcmc_engine", "DPmixGPD")
#' f
#'
#' @keywords internal
run_mcmc_engine <- function(spec, mcmc) {

  # Special case: Gamma kernel, no tail, stick-breaking DP
  if (identical(spec$kernel, "gamma") &&
      (is.null(spec$tail) || spec$tail == "none") &&
      identical(spec$dp_rep, "stick_breaking")) {

    return(run_mcmc_nimble_gamma(spec, mcmc))
  }

  # Fallback: simple generic engine (no regression yet)


  # Generic engine logic (unchanged for now)
  n_iter   <- if (is.null(mcmc$n_iter)) 2000L else as.integer(mcmc$n_iter)
  burn_in  <- if (is.null(mcmc$burn_in)) 1000L else as.integer(mcmc$burn_in)
  thin     <- if (is.null(mcmc$thin)) 1L else as.integer(mcmc$thin)
  chains   <- if (is.null(mcmc$chains)) 1L else as.integer(mcmc$chains)
  parallel <- isTRUE(mcmc$parallel)

  if (n_iter <= burn_in) {
    stop("mcmc$n_iter must be greater than mcmc$burn_in.")
  }
  if (chains < 1L) stop("mcmc$chains must be >= 1.")

  if (chains == 1L || !parallel) {
    chain_list <- vector("list", chains)
    for (ch in seq_len(chains)) {
      chain_list[[ch]] <- simulate_chain(spec, n_iter, burn_in)
    }
  } else {
    cl <- parallel::makeCluster(chains)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterExport(
      cl,
      varlist = c("spec", "n_iter", "burn_in", "simulate_chain"),
      envir   = environment()
    )
    parallel::clusterEvalQ(cl, {
      library(coda)
      NULL
    })

    chain_list <- parallel::parLapply(
      cl,
      X   = seq_len(chains),
      fun = function(ch) simulate_chain(spec, n_iter, burn_in)
    )
  }

  if (thin > 1L) {
    chain_list <- lapply(chain_list, function(m) m[seq(1, nrow(m), by = thin), ])
  }

  mcmc_obj <- coda::mcmc.list(chain_list)

  list(
    mcmc_draws = mcmc_obj,
    mcmc_info  = list(
      n_iter   = n_iter,
      burn_in  = burn_in,
      thin     = thin,
      chains   = chains,
      parallel = parallel
    )
  )
}
