# ============================================================================
# Helpers to standardize MCMC draws
# ============================================================================

.as_mcmc_matrix <- function(object) {
  draws <- object$mcmc_draws

  # Case 1: already a matrix
  if (is.matrix(draws)) {
    return(draws)
  }

  # Case 2: coda::mcmc or mcmc.list
  if (inherits(draws, "mcmc")) {
    return(as.matrix(draws))
  }
  if (inherits(draws, "mcmc.list")) {
    mats <- lapply(draws, as.matrix)
    return(do.call(rbind, mats))
  }

  # Case 3: list of matrices
  if (is.list(draws)) {
    mats <- lapply(draws, function(x) {
      if (inherits(x, "mcmc")) {
        as.matrix(x)
      } else if (is.matrix(x)) {
        x
      } else {
        stop("Cannot coerce element of 'mcmc_draws' to matrix.", call. = FALSE)
      }
    })
    return(do.call(rbind, mats))
  }

  stop("'object$mcmc_draws' must be a matrix (iterations x parameters) or ",
       "a coda::mcmc/mcmc.list/list of matrices.", call. = FALSE)
}

# ============================================================================
# Detect unconditional Gamma DP mixture (no tail, no covariates)
# ============================================================================

.is_gamma_uncond_dp <- function(object) {
  spec <- object$spec
  if (is.null(spec)) return(FALSE)

  # We only care about:
  #   - kernel = "gamma"
  #   - no GPD tail
  #   - no covariates (unconditional: y ~ 0)
  kernel_ok <- identical(spec$kernel, "gamma")
  tail_ok   <- is.null(spec$tail) || identical(spec$tail, "none")

  # X is either NULL or a 0-column matrix
  x_ok <- is.null(spec$X) ||
    (is.matrix(spec$X) && ncol(spec$X) == 0L)

  kernel_ok && tail_ok && x_ok
}


# ============================================================================
# Extract mixture parameters from Gamma DP (unconditional)
# ============================================================================

.extract_gamma_dp_params_uncond <- function(object) {
  draws <- .as_mcmc_matrix(object)
  cn    <- colnames(draws)

  # ---- component counts ----
  shape_cols <- grep("^shape\\[", cn, value = TRUE)
  if (length(shape_cols) == 0L) {
    stop("No 'shape[j]' columns found in MCMC draws for Gamma DP.", call. = FALSE)
  }
  K <- length(shape_cols)

  # Re-order shape columns by component index
  shape_j <- as.integer(sub("^shape\\[([0-9]+)\\]$", "\\1", shape_cols))
  shape_cols <- shape_cols[order(shape_j)]
  shape_j    <- sort(shape_j)

  # ---- scales for unconditional model: expect scale[j] ----
  scale_cols <- grep("^scale\\[", cn, value = TRUE)
  if (length(scale_cols) == 0L) {
    stop("Cannot find 'scale[j]' terms in MCMC draws for unconditional Gamma DP.",
         call. = FALSE)
  }

  scale_j <- as.integer(sub("^scale\\[([0-9]+)\\]$", "\\1", scale_cols))
  scale_cols <- scale_cols[order(scale_j)]
  scale_j    <- sort(scale_j)

  if (!identical(shape_j, scale_j)) {
    stop("Mismatch between 'shape[j]' and 'scale[j]' component indices.",
         call. = FALSE)
  }

  # ---- weights: prefer w[j], otherwise compute from v[j] ----
  w_cols <- grep("^w\\[", cn, value = TRUE)
  if (length(w_cols) > 0L) {
    w_j <- as.integer(sub("^w\\[([0-9]+)\\]$", "\\1", w_cols))
    w_cols <- w_cols[order(w_j)]
    w_j    <- sort(w_j)
    if (!identical(w_j, shape_j)) {
      stop("Mismatch between 'w[j]' and 'shape[j]' indices.", call. = FALSE)
    }
    W <- draws[, w_cols, drop = FALSE]
  } else {
    v_cols <- grep("^v\\[", cn, value = TRUE)
    if (length(v_cols) == 0L) {
      stop("No 'w[j]' or 'v[j]' columns found in MCMC draws.", call. = FALSE)
    }
    v_j <- as.integer(sub("^v\\[([0-9]+)\\]$", "\\1", v_cols))
    v_cols <- v_cols[order(v_j)]
    v_j    <- sort(v_j)

    if (length(v_cols) != K - 1L) {
      stop("For K components, expected K-1 'v[j]' stick-breaking terms.",
           call. = FALSE)
    }

    V <- draws[, v_cols, drop = FALSE]  # M x (K-1)
    M <- nrow(V)
    W <- matrix(NA_real_, nrow = M, ncol = K)
    colnames(W) <- paste0("w[", 1:K, "]")

    for (m in seq_len(M)) {
      v_m <- V[m, ]
      w_m <- numeric(K)
      prod_term <- 1
      for (j in seq_len(K - 1L)) {
        w_m[j] <- v_m[j] * prod_term
        prod_term <- prod_term * (1 - v_m[j])
      }
      w_m[K] <- prod_term
      W[m, ] <- w_m
    }
  }

  # ---- extract shape & scale ----
  Shape <- draws[, shape_cols, drop = FALSE]
  Scale <- draws[, scale_cols, drop = FALSE]

  # If you still want to work with mean/rate:
  # In R's Gamma: mean = shape * scale; rate = 1/scale
  Mu <- Shape * Scale

  list(
    W      = W,
    Shape  = Shape,
    Scale  = Scale,
    Mu     = Mu,
    n_iter = nrow(draws),
    K      = K
  )
}


# ============================================================================
# Predictive for unconditional Gamma DP mixture
# ============================================================================

.predict_gamma_uncond <- function(object,
                                  newdata,
                                  type      = c("density", "cdf", "sample", "quantile"),
                                  probs     = c(0.1, 0.5, 0.9),
                                  n_samples = 1000L) {

  type <- match.arg(type)

  if (missing(newdata) && type %in% c("density", "cdf")) {
    stop("'newdata' must be supplied for type = 'density' or 'cdf'.",
         call. = FALSE)
  }

  params <- .extract_gamma_dp_params_uncond(object)
  W     <- params$W
  Shape <- params$Shape
  Scale <- params$Scale
  M     <- params$n_iter
  K     <- params$K

  Rate <- 1 / Scale


  if (type %in% c("density", "cdf")) {
    x <- as.numeric(newdata)
    nx <- length(x)

    out <- numeric(nx)

    for (i in seq_len(nx)) {
      z <- x[i]

      # mixture density / cdf for each draw m, then average over draws
      if (type == "density") {
        # f_m(z) = sum_j w_mj * dgamma(z; shape, rate)
        fm <- rowSums(W * dgamma(z,
                                 shape = Shape,
                                 rate  = Rate))
        out[i] <- mean(fm)
      } else {
        # F_m(z) = sum_j w_mj * pgamma(z; shape, rate)
        Fm <- rowSums(W * pgamma(z,
                                 shape = Shape,
                                 rate  = Rate))
        out[i] <- mean(Fm)
      }
    }

    return(out)
  }

  if (type == "sample") {
    # Posterior predictive sampling
    n_samples <- as.integer(n_samples)
    if (n_samples <= 0L) {
      stop("'n_samples' must be positive for type = 'sample'.", call. = FALSE)
    }

    y_rep <- numeric(n_samples)

    for (s in seq_len(n_samples)) {
      # pick an MCMC draw at random
      m <- sample.int(M, 1L)
      w_m     <- W[m, ]
      shape_m <- Shape[m, ]
      rate_m  <- Rate[m, ]

      # choose component and sample from Gamma
      j <- sample.int(K, size = 1L, prob = w_m)
      y_rep[s] <- rgamma(1L, shape = shape_m[j], rate = rate_m[j])
    }

    return(y_rep)
  }

  if (type == "quantile") {
    probs <- sort(as.numeric(probs))
    if (any(probs < 0 | probs > 1)) {
      stop("'probs' must be in [0, 1].", call. = FALSE)
    }

    # Approximate quantiles from posterior predictive samples
    # (simpler than solving mixture CDF equations)
    n_samples <- max(5e4L, length(probs) * 2000L)
    y_rep <- .predict_gamma_uncond(object,
                                   newdata   = NULL,
                                   type      = "sample",
                                   n_samples = n_samples)

    return(stats::quantile(y_rep, probs = probs, names = TRUE))
  }

  stop("Unknown prediction 'type'. This should not happen.", call. = FALSE)
}

# ============================================================================
# Fallback Normal predictive (toy engine)
# ============================================================================

.predict_normal_fallback <- function(object,
                                     newdata,
                                     type      = c("density", "cdf", "sample", "quantile"),
                                     probs     = c(0.1, 0.5, 0.9),
                                     n_samples = 1000L) {

  type <- match.arg(type)
  draws <- .as_mcmc_matrix(object)
  cn    <- colnames(draws)

  if (!all(c("mu", "sigma") %in% cn)) {
    stop("Normal fallback requires 'mu' and 'sigma' columns in mcmc_draws.",
         call. = FALSE)
  }

  mu    <- draws[, "mu"]
  sigma <- draws[, "sigma"]
  M     <- length(mu)

  if (type %in% c("density", "cdf")) {
    x <- as.numeric(newdata)
    nx <- length(x)
    out <- numeric(nx)

    for (i in seq_len(nx)) {
      z <- x[i]
      if (type == "density") {
        out[i] <- mean(dnorm(z, mean = mu, sd = sigma))
      } else {
        out[i] <- mean(pnorm(z, mean = mu, sd = sigma))
      }
    }
    return(out)
  }

  if (type == "sample") {
    n_samples <- as.integer(n_samples)
    if (n_samples <= 0L) {
      stop("'n_samples' must be positive for type = 'sample'.", call. = FALSE)
    }
    idx <- sample.int(M, n_samples, replace = TRUE)
    return(rnorm(n_samples, mean = mu[idx], sd = sigma[idx]))
  }

  if (type == "quantile") {
    probs <- sort(as.numeric(probs))
    if (any(probs < 0 | probs > 1)) {
      stop("'probs' must be in [0, 1].", call. = FALSE)
    }
    n_samples <- max(5e4L, length(probs) * 2000L)
    y_rep <- .predict_normal_fallback(object,
                                      newdata   = NULL,
                                      type      = "sample",
                                      n_samples = n_samples)
    return(stats::quantile(y_rep, probs = probs, names = TRUE))
  }

  stop("Unknown prediction 'type' in normal fallback.", call. = FALSE)
}

# ============================================================================
# S3 method: predict.mixgpd_fit
# ============================================================================

#' @export
predict.mixgpd_fit <- function(object,
                               newdata,
                               type = c("density", "cdf", "sample", "quantile"),
                               probs = c(0.1, 0.5, 0.9),
                               n_samples = 1000L,
                               ...) {

  type <- match.arg(type)

  # 1) Gamma DP response-only: full mixture predictive
  if (.is_gamma_uncond_dp(object)) {
    return(
      .predict_gamma_uncond(
        object    = object,
        newdata   = newdata,
        type      = type,
        probs     = probs,
        n_samples = n_samples
      )
    )
  }

  # 2) Fallback: toy Normal engine (mu, sigma)
  draws <- try(.as_mcmc_matrix(object), silent = TRUE)
  if (!inherits(draws, "try-error") &&
      all(c("mu", "sigma") %in% colnames(draws))) {

    warning("Using Normal fallback predictive (toy engine). ",
            "Gamma/DP tail structure is not yet implemented for prediction.",
            call. = FALSE)

    return(
      .predict_normal_fallback(
        object    = object,
        newdata   = newdata,
        type      = type,
        probs     = probs,
        n_samples = n_samples
      )
    )
  }

  stop("Prediction not yet implemented for this model configuration.\n",
       "Currently supported:\n",
       "  - Unconditional Gamma DP mixtures (kernel = 'gamma', tail = 'none', y ~ 0).\n",
       "  - Toy Normal engine with 'mu' and 'sigma' in mcmc_draws.\n",
       "Other kernels / tails / regression will be added next.",
       call. = FALSE)
}
