# Simple MCMC engine -----------------------------------------------------------
# Currently a lightweight stochastic sampler; can be replaced later
# by a full DP + GPD implementation without changing the interface.

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
run_mcmc_engine <- function(spec, mcmc) {

  # Special case: Gamma kernel, no tail, stick-breaking DP
  if (identical(spec$kernel, "gamma") &&
      (is.null(spec$tail) || spec$tail == "none") &&
      identical(spec$dp_rep, "stick_breaking")) {

    return(run_mcmc_nimble_gamma(spec, mcmc))
  }

  # Fallback: simple generic engine (no regression yet)
  simulate_chain <- function(spec, n_iter, burn_in) {
    Y <- spec$Y
    N <- spec$N

    y_mean <- mean(Y)
    y_sd   <- stats::sd(Y)
    if (!is.finite(y_sd) || y_sd <= 0) y_sd <- 1

    n_keep <- max(1L, n_iter - burn_in)
    mu        <- numeric(n_keep)
    log_sigma <- numeric(n_keep)

    mu_curr    <- y_mean
    log_sig_cr <- log(y_sd)

    for (iter in seq_len(n_iter)) {
      mu_prop <- stats::rnorm(1, mu_curr, y_sd / sqrt(N))
      log_sig_prop <- stats::rnorm(1, log_sig_cr, 0.1)

      sigma_curr <- exp(log_sig_cr)
      sigma_prop <- exp(log_sig_prop)

      loglik <- function(mu, sigma) {
        sum(stats::dnorm(Y, mean = mu, sd = sigma, log = TRUE))
      }

      log_acc <- (loglik(mu_prop, sigma_prop) -
                    loglik(mu_curr, sigma_curr))

      if (log(stats::runif(1)) < log_acc) {
        mu_curr    <- mu_prop
        log_sig_cr <- log_sig_prop
      }

      if (iter > burn_in) {
        k <- iter - burn_in
        mu[k]        <- mu_curr
        log_sigma[k] <- log_sig_cr
      }
    }

    coda::mcmc(cbind(mu = mu, log_sigma = log_sigma))
  }

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


