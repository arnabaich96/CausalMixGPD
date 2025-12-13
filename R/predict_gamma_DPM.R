# ============================================================================
# Gamma DP mixture predictive helpers (package-native; no qTotal/rTotal names)
# ============================================================================

.as_mcmc_matrix <- function(object) {
  draws <- object$mcmc_draws

  if (is.matrix(draws)) {
    return(draws)
  }

  if (inherits(draws, "mcmc")) {
    return(as.matrix(draws))
  }
  if (inherits(draws, "mcmc.list")) {
    return(do.call(rbind, lapply(draws, as.matrix)))
  }

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

  stop("'object$mcmc_draws' must be a matrix (iterations x parameters) or coda object.",
    call. = FALSE
  )
}

.is_gamma_uncond_dp <- function(object) {
  spec <- object$spec
  if (is.null(spec)) {
    return(FALSE)
  }

  kernel_ok <- identical(spec$kernel, "gamma")
  tail_ok <- is.null(spec$tail) || identical(spec$tail, "none")

  x_ok <- is.null(spec$X) || (is.matrix(spec$X) && ncol(spec$X) == 0L)

  kernel_ok && tail_ok && x_ok
}

# ---- extract parameters for unconditional Gamma DP mixture ----
# Accepts either:
#   - scale[j] (your current unconditional engine), OR
#   - beta_scale[1,j] (older intercept-style engine)
.extract_gamma_dp_params_uncond <- function(object) {
  draws <- .as_mcmc_matrix(object)
  cn    <- colnames(draws)

  # ---- component counts ----
  shape_cols <- grep("^shape\\[", cn, value = TRUE)
  if (length(shape_cols) == 0L) {
    stop("No 'shape[j]' columns found in MCMC draws for Gamma DP.", call. = FALSE)
  }
  K <- length(shape_cols)

  # order by component index
  shape_j <- as.integer(sub("^shape\\[([0-9]+)\\]$", "\\1", shape_cols))
  shape_cols <- shape_cols[order(shape_j)]
  shape_j    <- sort(shape_j)

  # unconditional scale[j] is expected
  scale_cols <- grep("^scale\\[", cn, value = TRUE)
  if (length(scale_cols) == 0L) {
    stop("No 'scale[j]' columns found in MCMC draws for unconditional Gamma DP.", call. = FALSE)
  }
  scale_j <- as.integer(sub("^scale\\[([0-9]+)\\]$", "\\1", scale_cols))
  scale_cols <- scale_cols[order(scale_j)]
  scale_j    <- sort(scale_j)

  if (!identical(shape_j, scale_j)) {
    stop("Mismatch between 'shape[j]' and 'scale[j]' component indices.", call. = FALSE)
  }

  # weights: prefer w[j] if present, else build from v[j]
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
    V <- draws[, v_cols, drop = FALSE]

    # we expect K-1 stick breaks; if you monitored K by mistake, drop extras
    if (ncol(V) >= K) {
      V <- V[, 1:(K - 1L), drop = FALSE]
    } else if (ncol(V) != K - 1L) {
      stop("For K components, expected K-1 'v[j]' stick-breaking terms.", call. = FALSE)
    }

    M <- nrow(V)
    W <- matrix(NA_real_, nrow = M, ncol = K)
    colnames(W) <- paste0("w[", 1:K, "]")

    for (m in seq_len(M)) {
      v_m <- as.numeric(V[m, ])
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

  # ---- pull shape/scale ----
  Shape <- draws[, shape_cols, drop = FALSE]
  Scale <- draws[, scale_cols, drop = FALSE]

  # ---- CRITICAL FIX: renormalize weights per draw ----
  # If weights don't sum to 1 (numerical or model-definition issues),
  # force them onto the simplex. This prevents quantile bracketing failure.
  rs <- rowSums(W)
  bad <- !is.finite(rs) | rs <= 0
  if (any(bad)) {
    stop("Non-finite or non-positive mixture weight sums encountered in MCMC draws.", call. = FALSE)
  }
  W <- W / rs

  # (optional) tiny negative cleanup
  W[W < 0] <- 0
  W <- W / rowSums(W)

  list(
    W      = W,        # M x K
    Shape  = Shape,    # M x K
    Scale  = Scale,    # M x K
    n_iter = nrow(draws),
    K      = K
  )
}


# mixture CDF at x for one draw-row
.gamma_mix_cdf_1 <- function(x, w, shape, scale) {
  sum(w * stats::pgamma(x, shape = shape, scale = scale))
}

.gamma_mix_pdf_1 <- function(x, w, shape, scale) {
  sum(w * stats::dgamma(x, shape = shape, scale = scale))
}

.gamma_mix_quantile_1 <- function(p, w, shape, scale,
                                  upper0 = NULL, max_expand = 80L) {
  stopifnot(p >= 0, p <= 1)

  if (p == 0) {
    return(0)
  }
  if (p == 1) {
    return(Inf)
  }

  # ---- sanitize inputs ----
  w <- as.numeric(w)
  shape <- as.numeric(shape)
  scale <- as.numeric(scale)

  # bad params -> NA (don’t kill whole predict)
  if (any(!is.finite(w)) || any(!is.finite(shape)) || any(!is.finite(scale))) {
    return(NA_real_)
  }
  if (any(shape <= 0) || any(scale <= 0)) {
    return(NA_real_)
  }

  # normalize weights (defensive)
  sw <- sum(w)
  if (!is.finite(sw) || sw <= 0) {
    return(NA_real_)
  }
  w <- w / sw

  f <- function(z) {
    val <- sum(w * stats::pgamma(z, shape = shape, scale = scale))
    if (!is.finite(val)) {
      return(NA_real_)
    }
    val - p
  }

  lo <- 0
  flo <- f(lo)
  if (!is.finite(flo)) {
    return(NA_real_)
  } # can’t even evaluate at 0

  # ---- smarter initial upper bound ----
  if (is.null(upper0)) {
    # start from a high component-wise quantile
    qq <- suppressWarnings(stats::qgamma(min(0.999, p + 0.01), shape = shape, scale = scale))
    if (all(!is.finite(qq))) {
      # fallback to mean scale
      comp_mean <- shape * scale
      upper0 <- max(comp_mean) * 50
    } else {
      upper0 <- max(qq[is.finite(qq)]) * 2
    }
    if (!is.finite(upper0) || upper0 <= 0) upper0 <- 1
  }

  hi <- upper0
  fhi <- f(hi)

  # expand until bracket or until hi becomes effectively infinite
  expand <- 0L
  while ((is.na(fhi) || fhi < 0) && expand < max_expand) {
    # if hi gets huge, just try Inf once (pgamma(Inf)=1)
    if (!is.finite(hi) || hi > 1e308) {
      hi <- Inf
      fhi <- 1 - p
      break
    }
    hi <- hi * 2
    fhi <- f(hi)
    expand <- expand + 1L
  }

  if (!is.finite(fhi) && !is.infinite(hi)) {
    return(NA_real_)
  }
  if (is.na(fhi) || fhi < 0) {
    return(NA_real_)
  } # still not bracketed

  # uniroot needs finite interval; if hi is Inf, cap it
  if (is.infinite(hi)) {
    hi <- upper0
    # keep expanding but force finite cap for uniroot
    for (k in 1:60) {
      hi <- hi * 2
      fhi <- f(hi)
      if (is.finite(fhi) && fhi >= 0) break
      if (hi > 1e308) {
        return(NA_real_)
      }
    }
    if (!is.finite(fhi) || fhi < 0) {
      return(NA_real_)
    }
  }

  out <- tryCatch(
    stats::uniroot(f, interval = c(lo, hi), tol = 1e-10, maxiter = 2000)$root,
    error = function(e) NA_real_
  )
  out
}


# ============================================================================
# predict() for unconditional Gamma DP mixtures
# ============================================================================

.predict_gamma_uncond_dp <- function(object, newdata, type,
                                     probs = c(0.1, 0.5, 0.9),
                                     n_samples = 1000L) {
  params <- .extract_gamma_dp_params_uncond(object)
  W <- params$W
  Shape <- params$Shape
  Scale <- params$Scale
  M <- params$M
  K <- params$K

  type <- match.arg(type, c("density", "cdf", "sample", "quantile"))

  if (type %in% c("density", "cdf")) {
    x <- as.numeric(newdata)
    out <- numeric(length(x))

    for (i in seq_along(x)) {
      xi <- x[i]
      if (type == "density") {
        fm <- vapply(seq_len(M), function(m) {
          .gamma_mix_pdf_1(xi, W[m, ], Shape[m, ], Scale[m, ])
        }, numeric(1))
        out[i] <- mean(fm)
      } else {
        Fm <- vapply(seq_len(M), function(m) {
          .gamma_mix_cdf_1(xi, W[m, ], Shape[m, ], Scale[m, ])
        }, numeric(1))
        out[i] <- mean(Fm)
      }
    }
    return(out)
  }

  if (type == "sample") {
    n_samples <- as.integer(n_samples)
    if (n_samples <= 0L) stop("'n_samples' must be positive.", call. = FALSE)

    yrep <- numeric(n_samples)
    for (s in seq_len(n_samples)) {
      m <- sample.int(M, 1L)
      j <- sample.int(K, 1L, prob = W[m, ])
      yrep[s] <- stats::rgamma(1L, shape = Shape[m, j], scale = Scale[m, j])
    }
    return(yrep)
  }

  if (type == "quantile") {
    probs <- sort(as.numeric(probs))
    if (any(probs < 0 | probs > 1)) stop("'probs' must be in [0, 1].", call. = FALSE)

    alpha <- if (!is.null(object$alpha)) object$alpha else 0.05
    lo <- alpha/2
    hi <- 1 - alpha/2

    out <- lapply(probs, function(tau) {
      q_draw <- numeric(M)
      for (m in seq_len(M)) {
        q_draw[m] <- .qmix_gamma_dp_uncond(
          tau   = tau,
          w     = as.numeric(W[m, ]),
          shape = as.numeric(Shape[m, ]),
          scale = as.numeric(Scale[m, ])
        )
      }
      c(mean = mean(q_draw),
        sd   = stats::sd(q_draw),
        lo   = stats::quantile(q_draw, lo, names = FALSE),
        hi   = stats::quantile(q_draw, hi, names = FALSE))
    })

    mat <- do.call(rbind, out)
    rownames(mat) <- paste0("tau=", probs)
    return(mat)
  }

}
