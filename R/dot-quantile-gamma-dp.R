# ============================================================================
# Canonical quantile solver for Gamma DP mixtures (UNIROOT backbone)
# Everything else should call this.
# ============================================================================

#' @keywords internal
.normalize_weights <- function(w) {
  w[!is.finite(w)] <- NA_real_
  w[w < 0] <- NA_real_
  s <- sum(w, na.rm = TRUE)
  if (!is.finite(s) || s <= 0) stop("Invalid weights in a draw.", call. = FALSE)
  w / s
}

#' @keywords internal
.mix_cdf_gamma <- function(x, w, shape, scale) {
  # x scalar
  sum(w * stats::pgamma(x, shape = shape, scale = scale))
}

#' @keywords internal
.bracket_root <- function(fun, target = 0, lower = 0, upper = 1,
                          expand = 2, max_expand = 60) {
  # want fun(lower) < target and fun(upper) > target
  fL <- fun(lower) - target
  fU <- fun(upper) - target

  if (is.nan(fL) || is.nan(fU)) stop("NaN when bracketing root.", call. = FALSE)

  # If already bracketed, done
  if (is.finite(fL) && is.finite(fU) && fL <= 0 && fU >= 0) {
    return(c(lower, upper))
  }

  # If lower is too high (rare for CDF), try shrinking lower
  # (for our CDF use-case, lower=0 should already be fine)
  if (is.finite(fL) && fL > 0) {
    lower_try <- lower
    for (k in seq_len(max_expand)) {
      lower_try <- lower_try / expand
      fL <- fun(lower_try) - target
      if (is.finite(fL) && fL <= 0) {
        lower <- lower_try
        break
      }
    }
  }

  # Expand upper until it crosses target
  upper_try <- upper
  for (k in seq_len(max_expand)) {
    fU <- fun(upper_try) - target
    if (is.finite(fU) && fU >= 0) {
      return(c(lower, upper_try))
    }
    upper_try <- upper_try * expand
  }

  stop("Failed to bracket quantile (upper bound too small even after expansion).",
       call. = FALSE)
}

#' Quantile of a Gamma DP mixture for ONE posterior draw
#' @keywords internal
.q_gamma_dp_one_draw <- function(p, w, shape, scale,
                                 lower = 0,
                                 upper0 = 1,
                                 expand = 2,
                                 max_expand = 60,
                                 tol = .Machine$double.eps^0.5,
                                 maxiter = 1000) {

  if (!is.finite(p) || p <= 0 || p >= 1) {
    stop("'p' must be strictly between 0 and 1 for uniroot.", call. = FALSE)
  }

  w <- .normalize_weights(w)

  # CDF(x) - p = 0
  f <- function(x) .mix_cdf_gamma(x, w, shape, scale) - p

  # bracket then uniroot
  br <- .bracket_root(fun = function(x) .mix_cdf_gamma(x, w, shape, scale),
                      target = p,
                      lower = lower,
                      upper = upper0,
                      expand = expand,
                      max_expand = max_expand)

  stats::uniroot(f, interval = br, tol = tol, maxiter = maxiter)$root
}

#' Vectorized quantiles over posterior draws (returns M x length(p))
#' @keywords internal
.q_gamma_dp_over_draws <- function(p_vec, W, Shape, Scale,
                                   lower = 0,
                                   upper0 = 1,
                                   expand = 2,
                                   max_expand = 60,
                                   tol = .Machine$double.eps^0.5,
                                   maxiter = 1000) {
  p_vec <- as.numeric(p_vec)
  M <- nrow(W)
  Q <- matrix(NA_real_, nrow = M, ncol = length(p_vec))
  colnames(Q) <- paste0("q", p_vec)

  for (m in seq_len(M)) {
    w_m  <- as.numeric(W[m, ])
    sh_m <- as.numeric(Shape[m, ])
    sc_m <- as.numeric(Scale[m, ])

    for (k in seq_along(p_vec)) {
      Q[m, k] <- .q_gamma_dp_one_draw(
        p = p_vec[k], w = w_m, shape = sh_m, scale = sc_m,
        lower = lower, upper0 = upper0, expand = expand, max_expand = max_expand,
        tol = tol, maxiter = maxiter
      )
    }
  }
  Q
}
# ============================================================================
# Extract Gamma DP mixture params from a fitted object
# Supports: scale[j] OR beta_scale[1,j] intercept-only -> mu -> scale = mu/shape
# ============================================================================

#' @keywords internal
.extract_gamma_dp_params <- function(object) {
  draws <- .as_mcmc_matrix(object)
  cn <- colnames(draws)

  w_cols <- grep("^w\\[", cn, value = TRUE)
  sh_cols <- grep("^shape\\[", cn, value = TRUE)

  if (!length(w_cols)) stop("Cannot find mixture weights 'w[j]' in MCMC draws.", call. = FALSE)
  if (!length(sh_cols)) stop("Cannot find 'shape[j]' in MCMC draws.", call. = FALSE)

  # order by component index
  w_j  <- as.integer(sub("^w\\[([0-9]+)\\]$", "\\1", w_cols))
  sh_j <- as.integer(sub("^shape\\[([0-9]+)\\]$", "\\1", sh_cols))
  w_cols  <- w_cols[order(w_j)]
  sh_cols <- sh_cols[order(sh_j)]
  w_j  <- sort(w_j)
  sh_j <- sort(sh_j)

  if (!identical(w_j, sh_j)) {
    stop("Mismatch between 'w[j]' and 'shape[j]' component indices.", call. = FALSE)
  }

  K <- length(w_cols)

  # Prefer unconditional scale[j]
  sc_cols <- grep("^scale\\[", cn, value = TRUE)
  if (length(sc_cols) > 0L) {
    sc_j <- as.integer(sub("^scale\\[([0-9]+)\\]$", "\\1", sc_cols))
    sc_cols <- sc_cols[order(sc_j)]
    sc_j <- sort(sc_j)
    if (!identical(sc_j, sh_j)) {
      stop("Mismatch between 'scale[j]' and 'shape[j]' component indices.", call. = FALSE)
    }
    Scale <- draws[, sc_cols, drop = FALSE]

  } else {
    # Fallback: intercept-only beta_scale[1,j] gives log(mu_j) in your engine
    beta_cols <- grep("^beta_scale\\[1,", cn, value = TRUE)
    if (!length(beta_cols)) {
      stop("Cannot find Gamma scale parameters in MCMC draws.\n",
           "Expected either 'scale[j]' or 'beta_scale[1,j]'.", call. = FALSE)
    }
    beta_j <- as.integer(sub("^beta_scale\\[1,([0-9]+)\\]$", "\\1", beta_cols))
    beta_cols <- beta_cols[order(beta_j)]
    beta_j <- sort(beta_j)
    if (!identical(beta_j, sh_j)) {
      stop("Mismatch between 'shape[j]' and 'beta_scale[1,j]' component indices.", call. = FALSE)
    }
    Mu <- exp(draws[, beta_cols, drop = FALSE])
    Shape <- draws[, sh_cols, drop = FALSE]
    Scale <- Mu / Shape
  }

  list(
    W = draws[, w_cols, drop = FALSE],
    Shape = draws[, sh_cols, drop = FALSE],
    Scale = Scale
  )
}

#' @keywords internal
.qmix_gamma_dp_uncond <- function(tau, w, shape, scale,
                                  lower = 0, upper = NULL,
                                  expand = 2, max_expand = 60,
                                  tol = 1e-10, maxiter = 1000) {
  # Defensive
  if (!is.finite(tau) || tau < 0 || tau > 1) stop("'tau' must be in [0,1].", call. = FALSE)
  if (any(!is.finite(w)) || any(w < 0)) stop("Bad weights.", call. = FALSE)
  s <- sum(w)
  if (!is.finite(s) || s <= 0) stop("Sum of weights must be positive.", call. = FALSE)
  w <- w / s

  if (tau == 0) return(0)
  if (tau == 1) {
    # crude but safe: keep expanding until cdf ~ 1
    tau <- 1 - 1e-12
  }

  Fmix <- function(x) sum(w * stats::pgamma(x, shape = shape, scale = scale))

  # choose upper if not given: use a big multiple of mean as a starting guess
  if (is.null(upper)) {
    mu_mix <- sum(w * shape * scale)
    upper <- max(1, 10 * mu_mix)
  }

  fL <- Fmix(lower) - tau
  fU <- Fmix(upper) - tau

  n_expand <- 0L
  while (fU < 0 && n_expand < max_expand) {
    upper <- upper * expand
    fU <- Fmix(upper) - tau
    n_expand <- n_expand + 1L
  }

  if (fU < 0) {
    stop("Failed to bracket quantile (upper bound too small even after expansion).",
         call. = FALSE)
  }

  stats::uniroot(function(x) Fmix(x) - tau,
                 lower = lower, upper = upper,
                 tol = tol, maxiter = maxiter)$root
}
