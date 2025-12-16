testthat::test_that("fit.dpm CRP unconditional gamma runs and predict() works (density/cdf/sample/quantile)", {
  set.seed(1)
  dat <- data.frame(y = stats::rgamma(200, shape = 2, scale = 1))

  fit_crp <- DPmixGPD::fit.dpm(
    y ~ 0,
    data   = dat,
    kernel = "gamma",
    dp_rep = "crp",
    dp_ctrl = list(m_aux = 3),
    mcmc   = list(n_iter = 600, burn_in = 300, thin = 2, chains = 1),
    alpha  = 0.05
  )

  testthat::expect_true(inherits(fit_crp, "mixgpd_fit"))
  testthat::expect_true(inherits(fit_crp$mcmc_draws, "mcmc.list"))

  draws <- as.matrix(fit_crp$mcmc_draws)
  testthat::expect_true(is.matrix(draws))
  testthat::expect_true(nrow(draws) > 10)
  testthat::expect_true("alpha" %in% colnames(draws))

  # should be SB-compatible columns (w[.], shape[.], scale[.])
  testthat::expect_true(any(grepl("^w\\[",     colnames(draws))))
  testthat::expect_true(any(grepl("^shape\\[", colnames(draws))))
  testthat::expect_true(any(grepl("^scale\\[", colnames(draws))))

  grid <- seq(0, 8, length.out = 200)

  pd <- stats::predict(fit_crp, newdata = grid, type = "density")
  testthat::expect_true(is.numeric(pd))
  testthat::expect_length(pd, length(grid))
  testthat::expect_true(all(is.finite(pd)))
  testthat::expect_true(all(pd >= 0))

  pc <- stats::predict(fit_crp, newdata = grid, type = "cdf")
  testthat::expect_true(is.numeric(pc))
  testthat::expect_length(pc, length(grid))
  testthat::expect_true(all(is.finite(pc)))
  testthat::expect_true(all(pc >= 0 & pc <= 1))
  testthat::expect_true(all(diff(pc) >= -1e-8))

  ys <- stats::predict(fit_crp, type = "sample", n_samples = 1000L)
  testthat::expect_true(is.numeric(ys))
  testthat::expect_length(ys, 1000L)
  testthat::expect_true(all(is.finite(ys)))
  testthat::expect_true(all(ys >= 0))

  qq <- stats::predict(fit_crp, type = "quantile", probs = c(0.1, 0.5, 0.9))
  testthat::expect_true(is.matrix(qq))
  testthat::expect_true(.has_dimnames(qq))
  testthat::expect_true(all(c("mean","sd","lower","upper") %in% colnames(qq)))
  testthat::expect_equal(nrow(qq), 3L)
  .expect_ci_order(qq)

  testthat::expect_true(qq["q0.1","mean"] <= qq["q0.5","mean"])
  testthat::expect_true(qq["q0.5","mean"] <= qq["q0.9","mean"])
})
