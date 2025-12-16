#' CRP MCMC engine for unconditional Gamma DPM
#'
#' Runs an exact Chinese Restaurant Process (CRP) Dirichlet process mixture sampler
#' for the unconditional Gamma kernel using Neal's Algorithm 8 (nonconjugate).
#'
#' This engine targets the posterior of a Dirichlet process mixture with base prior
#' H on (shape, scale) and concentration parameter alpha. No truncation K is used.
#'
#' Important implementation note: the engine stores and updates parameters on their
#' natural positive scale. Any covariate-dependent transforms (e.g., exp links) belong
#' in the transformation layer used by regression models, not inside these likelihood
#' helpers.
#'
#' @param spec Model specification list produced by `build_model_spec()`.
#'   Must include `spec$kernel == "gamma"` and `spec$Y` (numeric vector).
#' @param mcmc MCMC control list. Uses `n_iter`, `burn_in`, `thin`.
#'
#' @return A list with elements:
#' \describe{
#'   \item{mcmc_draws}{A `coda::mcmc.list` of posterior draws with columns
#'   `alpha`, `shape[1:Kmax]`, `scale[1:Kmax]`, `v[1:Kmax]`, `w[1:Kmax]` where
#'   `Kmax` is the maximum number of occupied clusters across saved iterations.
#'   Components are ordered by decreasing weight within each draw; unused slots have
#'   `w=0` and placeholder `shape=1`, `scale=1`.}
#'   \item{mcmc_info}{Basic MCMC settings.}
#' }
#'
#' @keywords internal
run_mcmc_crp_gamma <- function(spec, mcmc) {
  if (!identical(spec$kernel, "gamma")) {
    stop("run_mcmc_crp_gamma: requires spec$kernel == 'gamma'.", call. = FALSE)
  }
  if (is.null(spec$Y)) {
    stop("run_mcmc_crp_gamma: spec$Y is required.", call. = FALSE)
  }
  y <- as.numeric(spec$Y)
  if (any(!is.finite(y))) y <- y[is.finite(y)]
  if (length(y) < 2L) stop("run_mcmc_crp_gamma: need at least 2 observations.", call. = FALSE)
  if (any(y < 0)) stop("run_mcmc_crp_gamma: Gamma kernel requires y >= 0.", call. = FALSE)

  n_iter  <- if (is.null(mcmc$n_iter)) 2000L else as.integer(mcmc$n_iter)
  burn_in <- if (is.null(mcmc$burn_in)) 1000L else as.integer(mcmc$burn_in)
  thin    <- if (is.null(mcmc$thin)) 1L else as.integer(mcmc$thin)
  chains  <- if (is.null(mcmc$chains)) 1L else as.integer(mcmc$chains)
  if (chains != 1L) stop("CRP engine currently supports chains = 1.", call. = FALSE)
  if (n_iter <= burn_in) stop("mcmc$n_iter must be > mcmc$burn_in.", call. = FALSE)
  if (thin < 1L) stop("mcmc$thin must be >= 1.", call. = FALSE)

  dp_ctrl <- if (is.null(spec$dp_ctrl)) list() else spec$dp_ctrl
  if (!is.null(dp_ctrl$K)) {
    warning("dp_ctrl$K is ignored for dp_rep = 'crp' (CRP does not use a fixed truncation).", call. = FALSE)
  }
  m_aux <- if (is.null(dp_ctrl$m_aux)) 5L else as.integer(dp_ctrl$m_aux)
  if (m_aux < 1L) stop("dp_ctrl$m_aux must be >= 1.", call. = FALSE)

  mh_sd <- dp_ctrl$mh_sd
  if (is.null(mh_sd)) mh_sd <- c(0.25, 0.25)
  mh_sd <- as.numeric(mh_sd)
  if (length(mh_sd) == 1L) mh_sd <- rep(mh_sd, 2L)
  if (length(mh_sd) != 2L || any(!is.finite(mh_sd)) || any(mh_sd <= 0)) {
    stop("dp_ctrl$mh_sd must be positive (length 1 or 2).", call. = FALSE)
  }

  pri <- .crp_gamma_priors(spec)

  alpha <- if (!is.null(spec$alpha)) as.numeric(spec$alpha) else 1
  if (!is.finite(alpha) || alpha <= 0) stop("alpha must be > 0.", call. = FALSE)

  N <- length(y)

  # ---- state ----
  z <- sample.int(2L, N, replace = TRUE)

  clusters <- new.env(parent = emptyenv())
  next_id <- 1L
  for (k in 1:2) {
    key <- as.character(next_id); next_id <- next_id + 1L
    th <- .crp_gamma_rH(1L, pri)
    clusters[[key]] <- list(shape = th$shape, scale = th$scale, members = integer(0))
  }
  keys <- ls(envir = clusters)
  for (i in seq_len(N)) {
    key <- keys[z[i]]
    clusters[[key]]$members <- c(clusters[[key]]$members, i)
  }

  drop_if_empty <- function(key) {
    if (length(clusters[[key]]$members) == 0L) rm(list = key, envir = clusters)
  }

  keep_idx <- seq.int(from = burn_in + 1L, to = n_iter, by = thin)

  # First pass: run and keep raw cluster draws, also record Kmax
  raw_draws <- vector("list", length(keep_idx))
  keep_pos <- 0L
  Kmax <- 0L

  for (iter in seq_len(n_iter)) {

    # --- allocation updates (Neal 8) ---
    for (i in seq_len(N)) {
      # find current cluster key
      cur_key <- NULL
      for (k in ls(envir = clusters)) {
        mem <- clusters[[k]]$members
        if (length(mem) && any(mem == i)) { cur_key <- k; break }
      }
      if (is.null(cur_key)) stop("CRP internal error: observation not assigned.", call. = FALSE)

      # remove i
      mem <- clusters[[cur_key]]$members
      clusters[[cur_key]]$members <- mem[mem != i]
      drop_if_empty(cur_key)

      keys_exist <- ls(envir = clusters)
      n_k <- vapply(keys_exist, function(k) length(clusters[[k]]$members), integer(1))

      logp_exist <- numeric(length(keys_exist))
      for (kk in seq_along(keys_exist)) {
        k <- keys_exist[kk]
        th <- clusters[[k]]
        logp_exist[kk] <- log(n_k[kk]) + .gamma_loglik1(y[i], th$shape, th$scale)
      }

      aux <- .crp_gamma_rH(m_aux, pri)
      logp_new <- numeric(m_aux)
      for (j in seq_len(m_aux)) {
        logp_new[j] <- log(alpha / m_aux) + .gamma_loglik1(y[i], aux$shape[j], aux$scale[j])
      }

      logp <- c(logp_exist, logp_new)
      p <- exp(logp - max(logp))
      p <- p / sum(p)

      draw <- sample.int(length(p), 1L, prob = p)
      if (draw <= length(keys_exist)) {
        k <- keys_exist[draw]
        clusters[[k]]$members <- c(clusters[[k]]$members, i)
      } else {
        j <- draw - length(keys_exist)
        key <- as.character(next_id); next_id <- next_id + 1L
        clusters[[key]] <- list(shape = aux$shape[j], scale = aux$scale[j], members = i)
      }
    }

    # --- parameter updates per cluster (RW-MH, lognormal proposals) ---
    for (k in ls(envir = clusters)) {
      mem <- clusters[[k]]$members
      yy <- y[mem]

      cur_s <- clusters[[k]]$shape
      cur_c <- clusters[[k]]$scale

      # lognormal random-walk proposals (no explicit exp in code)
      prop_s <- stats::rlnorm(1L, meanlog = log(cur_s), sdlog = mh_sd[1])
      prop_c <- stats::rlnorm(1L, meanlog = log(cur_c), sdlog = mh_sd[2])

      cur_lp <- .crp_gamma_logprior(cur_s, cur_c, pri) + .gamma_loglik(yy, cur_s, cur_c)
      prp_lp <- .crp_gamma_logprior(prop_s, prop_c, pri) + .gamma_loglik(yy, prop_s, prop_c)

      # symmetric in log-space? not exactly; include proposal ratio for lognormal RW
      # q(prop|cur)/q(cur|prop) cancels because both are lognormal centered at log(current):
      # include explicitly for correctness
      log_q_cur_given_prp <- stats::dlnorm(cur_s, meanlog = log(prop_s), sdlog = mh_sd[1], log = TRUE) +
        stats::dlnorm(cur_c, meanlog = log(prop_c), sdlog = mh_sd[2], log = TRUE)
      log_q_prp_given_cur <- stats::dlnorm(prop_s, meanlog = log(cur_s), sdlog = mh_sd[1], log = TRUE) +
        stats::dlnorm(prop_c, meanlog = log(cur_c), sdlog = mh_sd[2], log = TRUE)

      log_acc <- (prp_lp - cur_lp) + (log_q_cur_given_prp - log_q_prp_given_cur)

      if (log(stats::runif(1L)) < log_acc) {
        clusters[[k]]$shape <- prop_s
        clusters[[k]]$scale <- prop_c
      }
    }

    # --- optional alpha update (Escobar-West) ---
    if (isTRUE(dp_ctrl$update_alpha)) {
      alpha <- .crp_update_alpha(alpha, length(ls(envir = clusters)), N, pri$alpha_a, pri$alpha_b)
    }

    if (iter %in% keep_idx) {
      keep_pos <- keep_pos + 1L
      keys_exist <- ls(envir = clusters)
      counts <- vapply(keys_exist, function(k) length(clusters[[k]]$members), integer(1))
      shape <- vapply(keys_exist, function(k) clusters[[k]]$shape, numeric(1))
      scale <- vapply(keys_exist, function(k) clusters[[k]]$scale, numeric(1))
      raw_draws[[keep_pos]] <- list(alpha = alpha, shape = shape, scale = scale, counts = counts)
      Kmax <- max(Kmax, length(counts))
    }
  }

  # ---- convert raw draws to SB-compatible fixed-column matrices ----
  draw_mat <- matrix(NA_real_, nrow = length(raw_draws),
                     ncol = 1 + 4 * Kmax)
  colnames(draw_mat) <- c(
    "alpha",
    sprintf("shape[%d]", seq_len(Kmax)),
    sprintf("scale[%d]", seq_len(Kmax)),
    sprintf("v[%d]", seq_len(Kmax)),
    sprintf("w[%d]", seq_len(Kmax))
  )

  for (t in seq_along(raw_draws)) {
    dr <- raw_draws[[t]]
    w <- dr$counts / sum(dr$counts)
    ord <- order(w, decreasing = TRUE)
    w <- w[ord]; sh <- dr$shape[ord]; sc <- dr$scale[ord]

    if (length(w) < Kmax) {
      pad <- Kmax - length(w)
      w <- c(w, rep(0, pad))
      sh <- c(sh, rep(1, pad))
      sc <- c(sc, rep(1, pad))
    }
    v <- rep(NA_real_, Kmax) # CRP has no stick-breaking latent v

    draw_mat[t, ] <- c(dr$alpha, sh, sc, v, w)
  }

  # wrap as coda::mcmc.list (one chain)
  samples <- coda::mcmc(draw_mat)
  list(
    mcmc_draws = coda::mcmc.list(samples),
    mcmc_info = list(n_iter = n_iter, burn_in = burn_in, thin = thin, chains = 1L)
  )
}

# ---- Likelihood helpers on natural scale (no transforms) ---------------------

#' @keywords internal
.gamma_loglik1 <- function(y, shape, scale) {
  stats::dgamma(y, shape = shape, scale = scale, log = TRUE)
}

#' @keywords internal
.gamma_loglik <- function(y, shape, scale) {
  sum(stats::dgamma(y, shape = shape, scale = scale, log = TRUE))
}

# ---- Base prior H and related helpers ---------------------------------------

#' Extract/default priors for CRP Gamma engine.
#'
#' Base prior H is lognormal for shape and scale, and Gamma(a0,b0) for alpha if updated.
#'
#' @keywords internal
.crp_gamma_priors <- function(spec) {
  out <- list(
    shape_meanlog = 0, shape_sdlog = 1.5,
    scale_meanlog = 0, scale_sdlog = 1.5,
    alpha_a = 1, alpha_b = 1
  )

  pri <- spec$priors
  if (is.list(pri)) {
    if (is.list(pri$gamma)) pri <- pri$gamma
    if (is.list(pri$shape) && !is.null(pri$shape$meanlog)) out$shape_meanlog <- as.numeric(pri$shape$meanlog)
    if (is.list(pri$shape) && !is.null(pri$shape$sdlog))   out$shape_sdlog   <- as.numeric(pri$shape$sdlog)
    if (is.list(pri$scale) && !is.null(pri$scale$meanlog)) out$scale_meanlog <- as.numeric(pri$scale$meanlog)
    if (is.list(pri$scale) && !is.null(pri$scale$sdlog))   out$scale_sdlog   <- as.numeric(pri$scale$sdlog)
    if (is.list(pri$alpha)) {
      if (!is.null(pri$alpha$a)) out$alpha_a <- as.numeric(pri$alpha$a)
      if (!is.null(pri$alpha$b)) out$alpha_b <- as.numeric(pri$alpha$b)
    }
  }

  if (!is.finite(out$shape_meanlog) || !is.finite(out$scale_meanlog)) stop("CRP priors: meanlog must be finite.", call. = FALSE)
  if (!is.finite(out$shape_sdlog) || out$shape_sdlog <= 0) stop("CRP priors: shape sdlog must be > 0.", call. = FALSE)
  if (!is.finite(out$scale_sdlog) || out$scale_sdlog <= 0) stop("CRP priors: scale sdlog must be > 0.", call. = FALSE)
  if (!is.finite(out$alpha_a) || out$alpha_a <= 0) stop("CRP priors: alpha_a must be > 0.", call. = FALSE)
  if (!is.finite(out$alpha_b) || out$alpha_b <= 0) stop("CRP priors: alpha_b must be > 0.", call. = FALSE)

  out
}

#' Draw m auxiliary parameters from base prior H on (shape, scale).
#' @keywords internal
.crp_gamma_rH <- function(m, pri) {
  list(
    shape = stats::rlnorm(m, meanlog = pri$shape_meanlog, sdlog = pri$shape_sdlog),
    scale = stats::rlnorm(m, meanlog = pri$scale_meanlog, sdlog = pri$scale_sdlog)
  )
}

#' Log prior density for (shape, scale) under H (lognormal).
#' @keywords internal
.crp_gamma_logprior <- function(shape, scale, pri) {
  stats::dlnorm(shape, meanlog = pri$shape_meanlog, sdlog = pri$shape_sdlog, log = TRUE) +
    stats::dlnorm(scale, meanlog = pri$scale_meanlog, sdlog = pri$scale_sdlog, log = TRUE)
}

#' Escobar-West update for alpha
#' @keywords internal
.crp_update_alpha <- function(alpha, K, N, a0, b0) {
  eta <- stats::rbeta(1L, alpha + 1, N)
  pi_eta <- (a0 + K - 1) / (N * (b0 - log(eta)) + a0 + K - 1)
  if (stats::runif(1L) < pi_eta) {
    stats::rgamma(1L, shape = a0 + K, rate = b0 - log(eta))
  } else {
    stats::rgamma(1L, shape = a0 + K - 1, rate = b0 - log(eta))
  }
}


# ---- internal: extract CRP gamma regression parameters from draws
#' @keywords internal
.extract_crp_gamma_reg_params <- function(draws, p, N = NULL, renormalize_weights = TRUE) {
  draws <- as.matrix(draws)
  cn <- colnames(draws)

  if (is.null(N)) {
    z_cols0 <- grep("^z\\[", cn, value = TRUE)
    if (!length(z_cols0)) stop("CRP regression requires draws for z[i].", call. = FALSE)
    N <- length(z_cols0)
  }

  z_cols <- paste0("z[", seq_len(N), "]")
  if (!all(z_cols %in% cn)) stop("CRP regression requires draws for z[i] for i=1..N.", call. = FALSE)

  shape_cols_all <- grep("^shape\\[", cn, value = TRUE)
  if (!length(shape_cols_all)) stop("CRP regression requires draws for shape[j].", call. = FALSE)

  beta_cols_all <- grep("^beta_scale\\[", cn, value = TRUE)
  if (!length(beta_cols_all)) stop("CRP regression requires draws for beta_scale[*,*].", call. = FALSE)

  # --- helpers
  .idx1 <- function(x) as.integer(sub(".*\\[([0-9]+).*", "\\1", x))
  .parse_jq <- function(nm) {
    m <- regexec("^beta_scale\\[([0-9]+),([0-9]+)\\]$", nm)
    r <- regmatches(nm, m)[[1L]]
    if (length(r) != 3L) return(c(j = NA_integer_, q = NA_integer_))
    c(j = as.integer(r[2L]), q = as.integer(r[3L]))
  }

  # determine max component index present in parameter columns
  J_shape <- max(.idx1(shape_cols_all), na.rm = TRUE)

  # full regression beta_scale[j,q]?
  beta_jq <- do.call(rbind, lapply(beta_cols_all, .parse_jq))
  has_full <- any(is.finite(beta_jq[, "j"]) & is.finite(beta_jq[, "q"]))

  # intercept-only fallback beta_scale[1,j]
  beta_int_cols_all <- grep("^beta_scale\\[1,[0-9]+\\]$", beta_cols_all, value = TRUE)
  has_int_only <- length(beta_int_cols_all) > 0L

  if (!has_full && !has_int_only) {
    stop("CRP regression requires beta_scale[j,q] (or intercept-only beta_scale[1,j]).", call. = FALSE)
  }

  M <- nrow(draws)
  Z <- draws[, z_cols, drop = FALSE]
  Shape <- matrix(NA_real_, nrow = M, ncol = J_shape)

  # order and fill shapes
  shape_cols_all <- shape_cols_all[order(.idx1(shape_cols_all))]
  j_sh <- .idx1(shape_cols_all)
  for (k in seq_along(shape_cols_all)) {
    j <- j_sh[k]
    if (is.finite(j) && j >= 1L && j <= J_shape) {
      Shape[, j] <- draws[, shape_cols_all[k]]
    }
  }

  # Beta array [M, J, p]
  Beta <- array(0, dim = c(M, J_shape, p))

  if (has_full) {
    ok <- is.finite(beta_jq[, "j"]) & is.finite(beta_jq[, "q"])
    beta_cols <- beta_cols_all[ok]
    beta_jq <- beta_jq[ok, , drop = FALSE]
    ord <- order(beta_jq[, "j"], beta_jq[, "q"])
    beta_cols <- beta_cols[ord]
    beta_jq <- beta_jq[ord, , drop = FALSE]

    for (k in seq_along(beta_cols)) {
      j <- beta_jq[k, "j"]
      q <- beta_jq[k, "q"]
      if (j >= 1L && j <= J_shape && q >= 1L && q <= p) {
        Beta[, j, q] <- draws[, beta_cols[k]]
      }
    }
  } else {
    # intercept-only: beta_scale[1,j] maps to q=1
    beta_int_cols_all <- beta_int_cols_all[order(.idx1(beta_int_cols_all))]
    j_b <- as.integer(sub("^beta_scale\\[1,([0-9]+)\\]$", "\\1", beta_int_cols_all))
    for (k in seq_along(beta_int_cols_all)) {
      j <- j_b[k]
      if (is.finite(j) && j >= 1L && j <= J_shape && p >= 1L) {
        Beta[, j, 1L] <- draws[, beta_int_cols_all[k]]
      }
    }
  }

  # weights per draw derived from z-counts (CRP empirical weights)
  W <- matrix(0, nrow = M, ncol = J_shape)
  for (m in seq_len(M)) {
    zz <- as.integer(Z[m, ])
    zz <- zz[is.finite(zz) & zz >= 1L]
    if (!length(zz)) next
    tab <- table(zz)
    jj <- as.integer(names(tab))
    jj <- jj[jj >= 1L & jj <= J_shape]
    if (length(jj)) {
      w <- as.numeric(tab[as.character(jj)]) / length(zz)
      W[m, jj] <- w
    }
  }

  if (isTRUE(renormalize_weights)) {
    rs <- rowSums(W)
    rs[rs <= 0] <- NA_real_
    W <- W / rs
    W[is.na(W)] <- 0
  }

  list(W = W, Shape = Shape, Beta = Beta, M = M, J = J_shape, p = p, N = N)
}
