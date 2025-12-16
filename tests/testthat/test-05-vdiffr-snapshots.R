# Snapshot tests for plots via vdiffr
# These are optional: they will be skipped unless {vdiffr} (and {svglite}) are installed.

testthat::skip_if_not_installed("vdiffr")
testthat::skip_on_ci()
testthat::test_that("plot.mixgpd_te_fit: default ATE plot is stable", {
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not_installed("svglite")
  testthat::skip_on_cran()

  set.seed(1)
  dat <- data.frame(
    y  = stats::rgamma(200, shape = 2, rate = 1),
    A  = stats::rbinom(200, 1, 0.5),
    x1 = stats::rnorm(200),
    x2 = stats::rnorm(200)
  )

  fit_te <- DPmixGPD::fit.TE(
    y ~ 0,
    data   = dat,
    A      = "A",
    kernel = "gamma",
    tail   = FALSE,
    dp_rep = "stick_breaking",
    dp_ctrl = list(K = 5),
    mcmc   = list(n_iter = 600, burn_in = 300, chains = 1),
    alpha  = 0.05
  )

  p <- graphics::plot(fit_te)  # ggplot object
  testthat::expect_true(inherits(p, "ggplot"))
  vdiffr::expect_doppelganger("mixgpd_te_fit-ate-default", p)
})

testthat::test_that("plot.mixgpd_te_fit: QTE plot is stable", {
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not_installed("svglite")
  testthat::skip_on_cran()

  set.seed(1)
  dat <- data.frame(
    y  = stats::rgamma(200, shape = 2, rate = 1),
    A  = stats::rbinom(200, 1, 0.5),
    x1 = stats::rnorm(200),
    x2 = stats::rnorm(200)
  )

  fit_te <- DPmixGPD::fit.TE(
    y ~ 0,
    data   = dat,
    A      = "A",
    kernel = "gamma",
    tail   = FALSE,
    dp_rep = "stick_breaking",
    dp_ctrl = list(K = 5),
    mcmc   = list(n_iter = 600, burn_in = 300, chains = 1),
    alpha  = 0.05
  )

  tau <- seq(0.1, 0.9, by = 0.1)
  p <- graphics::plot(fit_te, effect = "quantile", tau = tau)
  testthat::expect_true(inherits(p, "ggplot"))
  vdiffr::expect_doppelganger("mixgpd_te_fit-qte-default", p)
})

testthat::test_that("plot.mixgpd_fit: predictive density plot is stable", {
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not_installed("svglite")
  testthat::skip_on_cran()

  set.seed(1)
  dat <- data.frame(y = stats::rgamma(200, shape = 2, scale = 1))

  fit0 <- DPmixGPD::fit.dpm(
    y ~ 0,
    data   = dat,
    kernel = "gamma",
    dp_rep = "stick_breaking",
    dp_ctrl = list(K = 5),
    mcmc   = list(n_iter = 600, burn_in = 300, chains = 1),
    alpha  = 0.05
  )

  p <- graphics::plot(fit0, type = "density")
  testthat::expect_true(inherits(p, "ggplot"))
  vdiffr::expect_doppelganger("mixgpd_fit-density", p)
})

