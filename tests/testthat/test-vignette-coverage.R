# test-vignette-coverage.R
# =============================================================================
# Tests that exercise code paths from vignettes for coverage
# These tests use short MCMC runs to be fast while still exercising the code
# =============================================================================

# Use package's test level system - these run at "ci" level and above
# They will run during coverage calculation (DPMIXGPD_TEST_LEVEL="ci")
# but skip during R CMD check (DPMIXGPD_TEST_LEVEL="cran")

# Short MCMC settings for fast execution
mcmc_short <- list(niter = 50, nburnin = 10, thin = 1, nchains = 1, seed = 1)

# Helper to suppress MCMC output
quiet_run <- function(expr) {

  nullfile <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  utils::capture.output(result <- force(expr), file = nullfile)
  result
}

# =============================================================================
# Unconditional Models (from v05-v09 vignettes)
# =============================================================================

test_that("unconditional CRP bulk model works (v05 coverage)", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  data("nc_pos200_k3", package = "DPmixGPD")
  y <- nc_pos200_k3$y[1:50]  # Use subset for speed

  bundle <- build_nimble_bundle(
    y = y,
    backend = "crp",
    kernel = "gamma",
    GPD = FALSE,
    components = 3,
    mcmc = mcmc_short
  )

  expect_s3_class(bundle, "dpmixgpd_bundle")

  # Test print and summary
  expect_output(print(bundle))
  expect_output(print(summary(bundle)))

  # Run MCMC
  fit <- quiet_run(run_mcmc_bundle_manual(bundle, show_progress = FALSE))
  expect_s3_class(fit, "mixgpd_fit")

  # Test S3 methods
  expect_output(print(fit))
  summ <- summary(fit)
  expect_s3_class(summ, "mixgpd_summary")
  expect_output(print(summ))

  # Test params
  p <- params(fit)
  expect_s3_class(p, "mixgpd_params")
  expect_output(print(p))

  # Test predict methods
  pred_q <- predict(fit, type = "quantile", index = c(0.5, 0.9))
  expect_s3_class(pred_q, "mixgpd_predict")

  pred_d <- predict(fit, y = y[1:5], type = "density")
  expect_s3_class(pred_d, "mixgpd_predict")

  pred_s <- predict(fit, y = y[1:5], type = "survival")
  expect_s3_class(pred_s, "mixgpd_predict")
})

test_that("unconditional SB bulk model works (v07 coverage)", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  data("nc_pos200_k3", package = "DPmixGPD")
  y <- nc_pos200_k3$y[1:50]

  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "lognormal",
    GPD = FALSE,
    components = 3,
    mcmc = mcmc_short
  )

  expect_s3_class(bundle, "dpmixgpd_bundle")

  fit <- quiet_run(run_mcmc_bundle_manual(bundle, show_progress = FALSE))
  expect_s3_class(fit, "mixgpd_fit")
  # fitted() not supported for unconditional; use predict() only
})

test_that("unconditional SB GPD model works (v09 coverage)", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  data("nc_pos200_k3", package = "DPmixGPD")
  y <- nc_pos200_k3$y[1:50]

  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "gamma",
    GPD = TRUE,
    components = 3,
    mcmc = mcmc_short
  )

  expect_s3_class(bundle, "dpmixgpd_bundle")

  fit <- quiet_run(run_mcmc_bundle_manual(bundle, show_progress = FALSE))
  expect_s3_class(fit, "mixgpd_fit")
})

# =============================================================================
# Conditional Models (from v10-v13 vignettes)
# =============================================================================

test_that("conditional SB bulk model works (v11 coverage)", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  data("nc_posX100_p3_k2", package = "DPmixGPD")
  y <- nc_posX100_p3_k2$y[1:40]
  X <- as.matrix(nc_posX100_p3_k2$X[1:40, ])

  bundle <- build_nimble_bundle(
    y = y,
    X = X,
    backend = "sb",
    kernel = "lognormal",
    GPD = FALSE,
    components = 3,
    mcmc = mcmc_short
  )

  expect_s3_class(bundle, "dpmixgpd_bundle")

  fit <- quiet_run(run_mcmc_bundle_manual(bundle, show_progress = FALSE))
  expect_s3_class(fit, "mixgpd_fit")

  # Test conditional predict
  x_new <- X[1:5, , drop = FALSE]
  pred_mean <- predict(fit, x = x_new, type = "mean", nsim_mean = 20)
  expect_s3_class(pred_mean, "mixgpd_predict")
})

test_that("conditional SB GPD model works (v13 coverage)", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  data("nc_posX100_p3_k2", package = "DPmixGPD")
  y <- nc_posX100_p3_k2$y[1:40]
  X <- as.matrix(nc_posX100_p3_k2$X[1:40, ])

  bundle <- build_nimble_bundle(
    y = y,
    X = X,
    backend = "sb",
    kernel = "lognormal",
    GPD = TRUE,
    components = 3,
    mcmc = mcmc_short
  )

  expect_s3_class(bundle, "dpmixgpd_bundle")

  fit <- quiet_run(run_mcmc_bundle_manual(bundle, show_progress = FALSE))
  expect_s3_class(fit, "mixgpd_fit")
})

# =============================================================================
# Causal Models (from v14-v19 vignettes)
# =============================================================================

test_that("causal no-X CRP model works (v14 coverage)", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  data("causal_alt_real500_p4_k2", package = "DPmixGPD")
  y <- abs(causal_alt_real500_p4_k2$y[1:80]) + 0.01
  T_vec <- causal_alt_real500_p4_k2$T[1:80]

  bundle <- build_causal_bundle(
    y = y,
    T = T_vec,
    X = NULL,
    kernel = "gamma",
    backend = "crp",
    PS = FALSE,
    GPD = FALSE,
    components = 3,
    mcmc_outcome = mcmc_short
  )

  expect_s3_class(bundle, "dpmixgpd_causal_bundle")
  expect_output(print(bundle))
  expect_output(print(summary(bundle)))

  fit <- quiet_run(run_mcmc_causal(bundle, show_progress = FALSE))
  expect_s3_class(fit, "dpmixgpd_causal_fit")

  # Test causal S3 methods
  expect_output(print(fit))
  expect_output(print(summary(fit)))

  # Test causal params
  p <- params(fit)
  expect_s3_class(p, "mixgpd_params_pair")
  expect_output(print(p))

  # Test causal predict
  pred <- predict(fit, type = "mean", nsim_mean = 20)
  expect_s3_class(pred, "dpmixgpd_causal_predict")

  # Test QTE
  qte_result <- qte(fit, probs = c(0.5), interval = "credible")
  expect_s3_class(qte_result, "dpmixgpd_qte")
  expect_output(print(qte_result))
  summ_qte <- summary(qte_result)
  expect_output(print(summ_qte))

  # Test ATE
  ate_result <- ate(fit, interval = "credible", nsim_mean = 20)
  expect_s3_class(ate_result, "dpmixgpd_ate")
  expect_output(print(ate_result))
  summ_ate <- summary(ate_result)
  expect_output(print(summ_ate))
})

test_that("causal X no-PS SB model works (v15 coverage)", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  data("causal_alt_real500_p4_k2", package = "DPmixGPD")
  y <- abs(causal_alt_real500_p4_k2$y[1:80]) + 0.01
  T_vec <- causal_alt_real500_p4_k2$T[1:80]
  X <- as.matrix(causal_alt_real500_p4_k2$X[1:80, 1:2])

  bundle <- build_causal_bundle(
    y = y,
    T = T_vec,
    X = X,
    kernel = "lognormal",
    backend = "sb",
    PS = FALSE,
    GPD = FALSE,
    components = 3,
    mcmc_outcome = mcmc_short
  )

  expect_s3_class(bundle, "dpmixgpd_causal_bundle")

  fit <- quiet_run(run_mcmc_causal(bundle, show_progress = FALSE))
  expect_s3_class(fit, "dpmixgpd_causal_fit")

  # Test causal predict with multiple types
  pred_q <- predict(fit, type = "quantile", p = c(0.25, 0.75))
  expect_s3_class(pred_q, "dpmixgpd_causal_predict")

  pred_d <- predict(fit, y = y[1:5], type = "density")
  expect_s3_class(pred_d, "dpmixgpd_causal_predict")

  pred_s <- predict(fit, y = y[1:5], type = "survival")
  expect_s3_class(pred_s, "dpmixgpd_causal_predict")
})

test_that("causal no-X SB model works (v16 coverage)", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  data("causal_alt_real500_p4_k2", package = "DPmixGPD")
  y <- abs(causal_alt_real500_p4_k2$y[1:80]) + 0.01
  T_vec <- causal_alt_real500_p4_k2$T[1:80]

  bundle <- build_causal_bundle(
    y = y,
    T = T_vec,
    X = NULL,
    kernel = "lognormal",
    backend = "sb",
    PS = FALSE,
    GPD = FALSE,
    components = 3,
    mcmc_outcome = mcmc_short
  )

  expect_s3_class(bundle, "dpmixgpd_causal_bundle")

  fit <- quiet_run(run_mcmc_causal(bundle, show_progress = FALSE))
  expect_s3_class(fit, "dpmixgpd_causal_fit")

  # Test QTE with multiple quantiles
  qte_result <- qte(fit, probs = c(0.25, 0.5, 0.75), interval = "credible")
  expect_s3_class(qte_result, "dpmixgpd_qte")

  # Test plot methods for QTE (returns plot objects)
  qte_plots <- plot(qte_result)
  expect_type(qte_plots, "list")

  # Test ATE with HPD interval
  ate_result <- ate(fit, interval = "hpd", nsim_mean = 20)
  expect_s3_class(ate_result, "dpmixgpd_ate")

  # Test plot methods for ATE
  ate_plots <- plot(ate_result)
  expect_type(ate_plots, "list")
})

test_that("causal with GPD tails works (v17 coverage)", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  data("causal_alt_real500_p4_k2", package = "DPmixGPD")
  y <- abs(causal_alt_real500_p4_k2$y[1:60]) + 0.01
  T_vec <- causal_alt_real500_p4_k2$T[1:60]

  bundle <- build_causal_bundle(
    y = y,
    T = T_vec,
    X = NULL,
    kernel = "gamma",
    backend = "sb",
    PS = FALSE,
    GPD = TRUE,
    components = 3,
    mcmc_outcome = mcmc_short
  )

  expect_s3_class(bundle, "dpmixgpd_causal_bundle")

  fit <- quiet_run(run_mcmc_causal(bundle, show_progress = FALSE))
  expect_s3_class(fit, "dpmixgpd_causal_fit")

  # Test predict with location type
  pred_loc <- predict(fit, type = "location")
  expect_s3_class(pred_loc, "dpmixgpd_causal_predict")
})

test_that("causal fit plot method works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  data("causal_alt_real500_p4_k2", package = "DPmixGPD")
  y <- abs(causal_alt_real500_p4_k2$y[1:60]) + 0.01
  T_vec <- causal_alt_real500_p4_k2$T[1:60]

  bundle <- build_causal_bundle(
    y = y,
    T = T_vec,
    X = NULL,
    kernel = "gamma",
    backend = "crp",
    PS = FALSE,
    GPD = FALSE,
    components = 3,
    mcmc_outcome = mcmc_short
  )

  fit <- quiet_run(run_mcmc_causal(bundle, show_progress = FALSE))

  # Test plot method for causal fit
  fit_plots <- plot(fit, arm = "both")
  expect_type(fit_plots, "list")

  # Test plot for treatment arm only
  fit_plots_trt <- plot(fit, arm = "trt")
  expect_type(fit_plots_trt, "list")

  # Test plot for control arm only
  fit_plots_con <- plot(fit, arm = "con")
  expect_type(fit_plots_con, "list")
})

test_that("causal predict plot method works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  data("causal_alt_real500_p4_k2", package = "DPmixGPD")
  y <- abs(causal_alt_real500_p4_k2$y[1:60]) + 0.01
  T_vec <- causal_alt_real500_p4_k2$T[1:60]

  bundle <- build_causal_bundle(
    y = y,
    T = T_vec,
    X = NULL,
    kernel = "gamma",
    backend = "crp",
    PS = FALSE,
    GPD = FALSE,
    components = 3,
    mcmc_outcome = mcmc_short
  )

  fit <- quiet_run(run_mcmc_causal(bundle, show_progress = FALSE))

  # Test predict plot
  pred <- predict(fit, type = "quantile", p = 0.5)
  pred_plots <- plot(pred)
  expect_type(pred_plots, "list")
})

# =============================================================================
# Kernel Coverage (all 7 kernels)
# =============================================================================

test_that("all kernel types work with SB backend", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  data("nc_pos200_k3", package = "DPmixGPD")
  y_pos <- nc_pos200_k3$y[1:30]
  y_real <- y_pos - mean(y_pos)  # Center for real-support kernels

  # Positive support kernels
  for (kernel in c("gamma", "lognormal", "invgauss")) {
    bundle <- build_nimble_bundle(
      y = y_pos,
      backend = "sb",
      kernel = kernel,
      GPD = FALSE,
      components = 2,
      mcmc = list(niter = 30, nburnin = 5, thin = 1, nchains = 1, seed = 1)
    )
    expect_s3_class(bundle, "dpmixgpd_bundle")

    fit <- quiet_run(run_mcmc_bundle_manual(bundle, show_progress = FALSE))
    expect_s3_class(fit, "mixgpd_fit")
  }

  # Real support kernels
  for (kernel in c("normal", "laplace", "cauchy")) {
    bundle <- build_nimble_bundle(
      y = y_real,
      backend = "sb",
      kernel = kernel,
      GPD = FALSE,
      components = 2,
      mcmc = list(niter = 30, nburnin = 5, thin = 1, nchains = 1, seed = 1)
    )
    expect_s3_class(bundle, "dpmixgpd_bundle")

    fit <- quiet_run(run_mcmc_bundle_manual(bundle, show_progress = FALSE))
    expect_s3_class(fit, "mixgpd_fit")
  }
})

test_that("amoroso kernel works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  data("nc_pos200_k3", package = "DPmixGPD")
  y <- nc_pos200_k3$y[1:30]

  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "amoroso",
    GPD = FALSE,
    components = 2,
    mcmc = list(niter = 30, nburnin = 5, thin = 1, nchains = 1, seed = 1)
  )
  expect_s3_class(bundle, "dpmixgpd_bundle")

  fit <- quiet_run(run_mcmc_bundle_manual(bundle, show_progress = FALSE))
  expect_s3_class(fit, "mixgpd_fit")
})
