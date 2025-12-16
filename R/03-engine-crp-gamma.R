#' CRP MCMC engine for unconditional Gamma DPM
#'
#' Runs an exact Chinese Restaurant Process (CRP) Dirichlet process mixture sampler
#' for the unconditional Gamma kernel using Neal's Algorithm 8 (nonconjugate).
#'
#' This engine targets the correct posterior for a Dirichlet process mixture with
#' base prior \eqn{H} on \eqn{\theta = (\log(\mathrm{shape}), \log(\mathrm{scale}))}
#' and concentration parameter \eqn{\alpha}. No truncation \eqn{K} is used.
#'
#' @param spec Model specification list produced by `build_model_spec()`.
#'   Must include `spec$kernel == "gamma"` and `spec$Y` (numeric vector).
#' @param mcmc MCMC control list. Uses `n_iter`, `burn_in`, `thin`.
#'
#' @return A list with elements:
#' \describe{
#'   \item{crp_draws}{A list of posterior draws (after burn-in and thinning). Each
#'   draw is a list containing `log_shape`, `log_scale`, `counts` and `alpha`.}
#'   \item{mcmc_info}{Basic MCMC settings.}
#' }
#' @keywords internal
run_mcmc_crp_gamma <- function(spec, mcmc) {
  if (!identical(spec$kernel, "gamma")) {
    stop("run_mcmc_crp_gamma: requires spec$kernel == 'gamma'.", call. = FALSE)
  }
  if (is.null(spec$Y)) {
    stop("run_mcmc_crp_gamma: spec$Y is required.", call. = FALSE)
  }

  y <- as.numeric(spec$Y)
  y <- y[is.finite(y)]
  if (length(y) < 2L) stop("run_mcmc_crp_gamma: need at least 2 observations.", call. = FALSE)
  if (any(y < 0)) stop("run_mcmc_crp_gamma: Gamma kernel requires y >= 0.", call. = FALSE)

  n_iter  <- if (is.null(mcmc$n_iter)) 2000L else as.integer(mcmc$n_iter)
  burn_in <- if (is.null(mcmc$burn_in)) 1000L else as.integer(mcmc$burn_in)
  thin    <- if (is.null(mcmc$thin)) 1L else as.integer(mcmc$thin)
  if (n_iter <= burn_in) stop("mcmc$n_iter must be > mcmc$burn_in.", call. = FALSE)
  if (thin < 1L) stop("mcmc$thin must be >= 1.", call. = FALSE)

  dp_ctrl <- if (is.null(spec$dp_ctrl)) list() else spec$dp_ctrl
  m_aux   <- if (is.null(dp_ctrl$m_aux)) 5L else as.integer(dp_ctrl$m_aux)
  if (m_aux < 1L) stop("dp_ctrl$m_aux must be >= 1.", call. = FALSE)

  mh_sd <- dp_ctrl$mh_sd
  if (is.null(mh_sd)) mh_sd <- c(0.20, 0.20)
  mh_sd <- as.numeric(mh_sd)
  if (length(mh_sd) == 1L) mh_sd <- rep(mh_sd, 2L)
  if (length(mh_sd) != 2L || any(!is.finite(mh_sd)) || any(mh_sd <= 0)) {
    stop("dp_ctrl$mh_sd must be positive (length 1 or 2).", call. = FALSE)
  }

  pri <- .crp_gamma_priors(spec)

  alpha <- if (!is.null(spec$alpha)) as.numeric(spec$alpha) else 1
  if (!is.finite(alpha) || alpha <= 0) stop("alpha must be > 0.", call. = FALSE)

  # Initialize: random 2 clusters
  N <- length(y)
  z <- sample.int(2L, N, replace = TRUE)

  clusters <- new.env(parent = emptyenv())
  next_id <- 1L
  # map internal cluster labels to env keys
  cl_of_z <- integer(2L)
  for (k in 1:2) {
    key <- as.character(next_id); next_id <- next_id + 1L
    th <- .crp_gamma_rH(1L, pri)
    clusters[[key]] <- list(log_shape = th$log_shape, log_scale = th$log_scale, members = integer(0))
    cl_of_z[k] <- as.integer(key)
  }

  # assign members
  for (i in seq_len(N)) {
    key <- as.character(cl_of_z[z[i]])
    clusters[[key]]$members <- c(clusters[[key]]$members, i)
  }

  # helper to drop empty cluster
  drop_if_empty <- function(key) {
    if (length(clusters[[key]]$members) == 0L) rm(list = key, envir = clusters)
  }

  # storage
  keep_idx <- seq.int(from = burn_in + 1L, to = n_iter, by = thin)
  draws <- vector("list", length(keep_idx))
  keep_pos <- 0L

  for (iter in seq_len(n_iter)) {

    # --- Allocation updates (Neal 8) ---
    keys_exist <- ls(envir = clusters)
    for (i in seq_len(N)) {
      # current cluster key
      cur_key <- NULL
      # find cluster containing i
      # (we keep members vectors; remove by search)
      keys_exist <- ls(envir = clusters)
      for (k in keys_exist) {
        mem <- clusters[[k]]$members
        if (length(mem) && any(mem == i)) { cur_key <- k; break }
      }
      if (is.null(cur_key)) stop("CRP internal error: observation not assigned.", call. = FALSE)

      # remove i
      mem <- clusters[[cur_key]]$members
      clusters[[cur_key]]$members <- mem[mem != i]
      drop_if_empty(cur_key)

      # existing cluster probabilities
      keys_exist <- ls(envir = clusters)
      n_k <- vapply(keys_exist, function(k) length(clusters[[k]]$members), integer(1))
      logp_exist <- numeric(length(keys_exist))
      for (kk in seq_along(keys_exist)) {
        k <- keys_exist[kk]
        th <- clusters[[k]]
        logp_exist[kk] <- log(n_k[kk]) + .crp_gamma_loglik1(y[i], th$log_shape, th$log_scale)
      }

      # auxiliary new clusters
      aux <- .crp_gamma_rH(m_aux, pri) # vectors log_shape/log_scale
      logp_new <- numeric(m_aux)
      for (j in seq_len(m_aux)) {
        logp_new[j] <- log(alpha / m_aux) + .crp_gamma_loglik1(y[i], aux$log_shape[j], aux$log_scale[j])
      }

      # sample destination
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
        clusters[[key]] <- list(
          log_shape = aux$log_shape[j],
          log_scale = aux$log_scale[j],
          members = i
        )
      }
    }

    # --- Parameter updates per cluster (RW-MH on log-shape/log-scale) ---
    keys_exist <- ls(envir = clusters)
    for (k in keys_exist) {
      mem <- clusters[[k]]$members
      yy <- y[mem]

      cur_ls <- clusters[[k]]$log_shape
      cur_lc <- clusters[[k]]$log_scale

      prop_ls <- cur_ls + stats::rnorm(1L, 0, mh_sd[1])
      prop_lc <- cur_lc + stats::rnorm(1L, 0, mh_sd[2])

      cur_lp <- .crp_gamma_logprior(cur_ls, cur_lc, pri) + .crp_gamma_loglik(yy, cur_ls, cur_lc)
      prp_lp <- .crp_gamma_logprior(prop_ls, prop_lc, pri) + .crp_gamma_loglik(yy, prop_ls, prop_lc)

      if (log(stats::runif(1L)) < (prp_lp - cur_lp)) {
        clusters[[k]]$log_shape <- prop_ls
        clusters[[k]]$log_scale <- prop_lc
      }
    }

    # --- Optional alpha update (Escobar-West) ---
    if (isTRUE(dp_ctrl$update_alpha)) {
      alpha <- .crp_update_alpha(alpha, length(ls(envir = clusters)), N, pri$alpha_a, pri$alpha_b)
    }

    # --- store draw ---
    if (iter %in% keep_idx) {
      keep_pos <- keep_pos + 1L
      keys_exist <- ls(envir = clusters)
      counts <- vapply(keys_exist, function(k) length(clusters[[k]]$members), integer(1))
      log_shape <- vapply(keys_exist, function(k) clusters[[k]]$log_shape, numeric(1))
      log_scale <- vapply(keys_exist, function(k) clusters[[k]]$log_scale, numeric(1))

      draws[[keep_pos]] <- list(
        alpha = alpha,
        log_shape = log_shape,
        log_scale = log_scale,
        counts = counts
      )
    }
  }

  list(
    crp_draws = draws,
    mcmc_info = list(n_iter = n_iter, burn_in = burn_in, thin = thin, chains = 1L, parallel = FALSE)
  )
}

# ---- Helpers (Gamma kernel) -------------------------------------------------

#' @keywords internal
.crp_gamma_loglik1 <- function(y, log_shape, log_scale) {
  shape <- exp(log_shape)
  scale <- exp(log_scale)
  stats::dgamma(y, shape = shape, scale = scale, log = TRUE)
}

#' @keywords internal
.crp_gamma_loglik <- function(y, log_shape, log_scale) {
  shape <- exp(log_shape)
  scale <- exp(log_scale)
  sum(stats::dgamma(y, shape = shape, scale = scale, log = TRUE))
}

#' @keywords internal
.crp_gamma_logprior <- function(log_shape, log_scale, pri) {
  stats::dnorm(log_shape, mean = pri$ls_mu, sd = pri$ls_sd, log = TRUE) +
    stats::dnorm(log_scale, mean = pri$lc_mu, sd = pri$lc_sd, log = TRUE)
}

#' Draw m auxiliary parameters from base prior H on (log_shape, log_scale).
#' @keywords internal
.crp_gamma_rH <- function(m, pri) {
  list(
    log_shape = stats::rnorm(m, mean = pri$ls_mu, sd = pri$ls_sd),
    log_scale = stats::rnorm(m, mean = pri$lc_mu, sd = pri$lc_sd)
  )
}

#' Extract/default priors for CRP Gamma engine (Option B: log-params normal).
#' @keywords internal
.crp_gamma_priors <- function(spec) {
  # defaults
  out <- list(
    ls_mu = 0, ls_sd = 1.5,
    lc_mu = 0, lc_sd = 1.5,
    alpha_a = 1, alpha_b = 1
  )

  pri <- spec$priors
  if (is.list(pri)) {
    # accept a few plausible paths without guessing too hard
    if (is.list(pri$gamma)) pri <- pri$gamma
    if (!is.null(pri$log_shape) && is.list(pri$log_shape)) {
      if (!is.null(pri$log_shape$mean)) out$ls_mu <- as.numeric(pri$log_shape$mean)
      if (!is.null(pri$log_shape$sd))   out$ls_sd <- as.numeric(pri$log_shape$sd)
    }
    if (!is.null(pri$log_scale) && is.list(pri$log_scale)) {
      if (!is.null(pri$log_scale$mean)) out$lc_mu <- as.numeric(pri$log_scale$mean)
      if (!is.null(pri$log_scale$sd))   out$lc_sd <- as.numeric(pri$log_scale$sd)
    }
    if (!is.null(pri$alpha) && is.list(pri$alpha)) {
      if (!is.null(pri$alpha$a)) out$alpha_a <- as.numeric(pri$alpha$a)
      if (!is.null(pri$alpha$b)) out$alpha_b <- as.numeric(pri$alpha$b)
    }
  }

  # validate
  if (!is.finite(out$ls_mu) || !is.finite(out$lc_mu)) stop("CRP priors: means must be finite.", call. = FALSE)
  if (!is.finite(out$ls_sd) || out$ls_sd <= 0) stop("CRP priors: log_shape sd must be > 0.", call. = FALSE)
  if (!is.finite(out$lc_sd) || out$lc_sd <= 0) stop("CRP priors: log_scale sd must be > 0.", call. = FALSE)
  if (!is.finite(out$alpha_a) || out$alpha_a <= 0) stop("CRP priors: alpha_a must be > 0.", call. = FALSE)
  if (!is.finite(out$alpha_b) || out$alpha_b <= 0) stop("CRP priors: alpha_b must be > 0.", call. = FALSE)

  out
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
