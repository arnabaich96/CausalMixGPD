#' Internal: build Nimble model for unconditional Gamma SB mixture
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
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Internal: run Nimble MCMC for unconditional Gamma SB mixture
#' @keywords internal
run_mcmc_nimble_gamma_uncond <- function(spec, mcmc) {
  if (!requireNamespace("nimble", quietly = TRUE)) {
    stop("Package 'nimble' is required for the DP mixture engine.", call. = FALSE)
  }

  mm <- build_nimble_model_gamma_uncond(spec)

  model <- nimble::nimbleModel(
    code      = mm$code,
    constants = mm$constants,
    data      = mm$data,
    inits     = mm$inits
  )

  cmodel <- nimble::compileNimble(model)

  # monitors: DP structure + component params
  monitors <- c("alpha", "v", "w", "shape", "scale")

  conf <- nimble::configureMCMC(cmodel, monitors = monitors)

  mcmc_obj <- nimble::buildMCMC(conf)
  cmcmc    <- nimble::compileNimble(mcmc_obj, project = cmodel)

  n_iter  <- if (!is.null(mcmc$n_iter))  mcmc$n_iter  else 2000L
  burn_in <- if (!is.null(mcmc$burn_in)) mcmc$burn_in else 1000L
  thin    <- if (!is.null(mcmc$thin))    mcmc$thin    else 1L
  chains  <- if (!is.null(mcmc$chains))  mcmc$chains  else 1L

  samples <- nimble::runMCMC(
    cmcmc,
    niter             = n_iter,
    nburnin           = burn_in,
    thin              = thin,
    nchains           = chains,
    samplesAsCodaMCMC = TRUE
  )

  list(
    mcmc_draws = samples,
    mcmc_info  = list(
      n_iter  = n_iter,
      burn_in = burn_in,
      thin    = thin,
      chains  = chains
    )
  )
}


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
  tau_beta <- priors$tau_beta %||% 0.01  # precision = 1 / variance

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
    shape      = rgamma(K, a_shape, b_shape),
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

#' Internal: run Nimble MCMC for Gamma kernel (uncond or regression)
#' @keywords internal
run_mcmc_nimble_gamma <- function(spec, mcmc) {
  if (!requireNamespace("nimble", quietly = TRUE)) {
    stop("Package 'nimble' is required for the DP mixture engine.", call. = FALSE)
  }

  # ---- MCMC controls ----
  n_iter  <- mcmc$n_iter  %||% 2000L
  burn_in <- mcmc$burn_in %||% 1000L
  thin    <- mcmc$thin    %||% 1L
  chains  <- mcmc$chains  %||% 1L

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

  conf   <- nimble::configureMCMC(cmodel, monitors = monitors)
  mcmc_o <- nimble::buildMCMC(conf)
  cmcmc  <- nimble::compileNimble(mcmc_o, project = cmodel)

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
    mcmc_info  = list(
      n_iter  = n_iter,
      burn_in = burn_in,
      thin    = thin,
      chains  = chains
    )
  )
}

# Internal: extract Gamma DP component parameters from an MCMC draw matrix
#' @keywords internal
.extract_gamma_params <- function(draws) {
  if (is.null(colnames(draws))) {
    stop("MCMC draws must have column names.", call. = FALSE)
  }

  # weights
  w_idx <- grep("^w\\[", colnames(draws))
  if (!length(w_idx)) {
    stop("Cannot find mixture weights 'w[j]' in MCMC draws.", call. = FALSE)
  }

  # shapes
  sh_idx <- grep("^shape\\[", colnames(draws))
  if (!length(sh_idx)) {
    stop("Cannot find 'shape[j]' in MCMC draws.", call. = FALSE)
  }

  # scales: unconditional model uses scale[j]
  sc_idx <- grep("^scale\\[", colnames(draws))

  # regression model might only have beta_scale[1,j] as intercept terms
  if (!length(sc_idx)) {
    sc_idx <- grep("^beta_scale\\[1,\\s*", colnames(draws))
  }

  if (!length(sc_idx)) {
    stop(
      "Cannot find Gamma scale parameters in MCMC draws.\n",
      "Expected either:\n",
      "  - 'scale[j]' (unconditional Gamma DP), or\n",
      "  - 'beta_scale[1, j]' (regression intercept-only scale terms).",
      call. = FALSE
    )
  }

  list(
    w     = draws[, w_idx,  drop = FALSE],
    shape = draws[, sh_idx, drop = FALSE],
    scale = draws[, sc_idx, drop = FALSE]
  )
}
