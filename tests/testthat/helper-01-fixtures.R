# Helpers for test fixtures with caching to minimize expensive MCMC runs.
# These are test-only utilities.

.test_cache <- new.env(parent = emptyenv())

.get_cached <- function(key, expr) {
  if (exists(key, envir = .test_cache, inherits = FALSE)) {
    return(get(key, envir = .test_cache, inherits = FALSE))
  }
  val <- eval.parent(substitute(expr))
  assign(key, val, envir = .test_cache)
  val
}

# Minimal MCMC settings to keep tests fast.
.tiny_mcmc <- function() {
  list(n_iter = 300, burn_in = 150, chains = 1)
}

# Small synthetic dataset used across multiple tests.
.make_gamma_data <- function(n = 200, seed = 1) {
  set.seed(seed)
  data.frame(y = stats::rgamma(n, shape = 2, scale = 1))
}

.make_te_data <- function(n = 200, seed = 1) {
  set.seed(seed)
  data.frame(
    y  = stats::rgamma(n, shape = 2, rate = 1),
    A  = stats::rbinom(n, 1, 0.5),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
}

.get_fit_dpm_sb_gamma_uncond <- function(K = 5, n = 200, seed = 1) {
  key <- sprintf("fit_dpm_sb_gamma_uncond_K%s_n%s_seed%s", K, n, seed)
  .get_cached(key, {
    dat <- .make_gamma_data(n = n, seed = seed)
    DPmixGPD::fit.dpm(
      y ~ 0,
      data   = dat,
      kernel = "gamma",
      dp_rep = "stick_breaking",
      dp_ctrl = list(K = K),
      mcmc   = .tiny_mcmc(),
      alpha  = 0.05
    )
  })
}

.get_fit_te_sb_gamma_uncond <- function(K = 5, n = 200, seed = 1) {
  key <- sprintf("fit_te_sb_gamma_uncond_K%s_n%s_seed%s", K, n, seed)
  .get_cached(key, {
    dat <- .make_te_data(n = n, seed = seed)
    DPmixGPD::fit.TE(
      y ~ 0,
      data   = dat,
      A      = "A",
      kernel = "gamma",
      tail   = FALSE,
      priors = list(),
      trans  = list(),
      intercept = TRUE,
      dp_rep = "stick_breaking",
      dp_ctrl = list(K = K),
      mcmc   = .tiny_mcmc(),
      alpha  = 0.05
    )
  })
}


.get_fit_te_sb_gamma_reg <- function(K = 5, n = 200, seed = 1) {
  key <- sprintf("fit_te_sb_gamma_reg_K%s_n%s_seed%s", K, n, seed)
  .get_cached(key, {
    dat <- .make_te_data(n = n, seed = seed)
    DPmixGPD::fit.TE(
      y ~ x1 + x2,
      data   = dat,
      A      = "A",
      kernel = "gamma",
      tail   = FALSE,
      priors = list(),
      trans  = list(),
      intercept = TRUE,
      dp_rep = "stick_breaking",
      dp_ctrl = list(K = K),
      mcmc   = .tiny_mcmc(),
      alpha  = 0.05
    )
  })
}

