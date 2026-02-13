# test-ate-rmean.R
# Focused regression checks for ATE mean vs restricted mean under heavy tails.

test_that("ate(type='mean') is Inf when xi >= 1 and ate_rmean stays finite", {
  skip_if_not_test_level("ci")

  set.seed(77)
  n <- 20
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.4 * X[, 1]))
  y <- abs(stats::rnorm(n)) + 0.1

  mcmc_out <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)
  mcmc_ps  <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)

  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = c("sb", "sb"),
    kernel = c("gamma", "gamma"),
    GPD = c(TRUE, TRUE),
    components = c(4, 4),
    mcmc_outcome = mcmc_out,
    mcmc_ps = mcmc_ps
  )

  cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
  newx <- head(X, 3)

  trt_fit <- cf$outcome_fit$trt
  smp <- trt_fit$mcmc$samples %||% trt_fit$samples
  if (is.null(smp) || !("tail_shape" %in% colnames(as.matrix(smp[[1]])))) {
    skip("Outcome fit has no tail_shape draws to patch.")
  }
  ch <- as.matrix(smp[[1]])
  ch[1L, "tail_shape"] <- 1.5
  smp[[1]] <- coda::as.mcmc(ch)
  if (!is.null(trt_fit$mcmc$samples)) trt_fit$mcmc$samples <- smp
  if (!is.null(trt_fit$samples)) trt_fit$samples <- smp
  cf$outcome_fit$trt <- trt_fit

  expect_warning(
    a_mean <- DPmixGPD::cate(cf, newdata = newx, type = "mean", interval = NULL, nsim_mean = 20L),
    "infinite"
  )
  expect_true(all(!is.finite(a_mean$fit)) | any(a_mean$fit == Inf))

  a_rmean <- DPmixGPD::ate_rmean(cf, newdata = newx, cutoff = 10, interval = "credible", nsim_mean = 20L)
  expect_true(all(is.finite(a_rmean$fit)))
  expect_true(all(c("fit", "lower", "upper", "trt", "con", "meta", "grid") %in% names(a_rmean)))
})
