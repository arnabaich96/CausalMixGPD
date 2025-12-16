
testthat::test_that("fit.dpm unconditional gamma runs and stats::predict() works (density/cdf/sample/quantile)", {
  set.seed(1)
  dat <- data.frame(y = stats::rgamma(200, shape = 2, scale = 1))

  fit0 <- DPmixGPD::fit.dpm(
    y ~ 0,
    data   = dat,
    kernel = "gamma",
    dp_rep = "stick_breaking",
    dp_ctrl = list(K = 5),
    mcmc   = list(n_iter = 1200, burn_in = 600, chains = 1),
    alpha  = 0.05
  )

  testthat::expect_true(inherits(fit0, "mixgpd_fit"))
  testthat::expect_true(!is.null(fit0$mcmc_draws))

  draws <- as.matrix(fit0$mcmc_draws)
  testthat::expect_true(is.matrix(draws))
  testthat::expect_true(nrow(draws) > 10)
  testthat::expect_true(ncol(draws) > 5)

  .expect_cols(draws, c("alpha"))
  testthat::expect_true(any(grepl("^shape\\[", colnames(draws))))
  testthat::expect_true(any(grepl("^scale\\[", colnames(draws))))
  testthat::expect_true(any(grepl("^w\\[",     colnames(draws))) ||
                          any(grepl("^v\\[",   colnames(draws))))

  grid <- seq(0, 8, length.out = 200)

  pd <- stats::predict(fit0, newdata = grid, type = "density")
  testthat::expect_true(is.numeric(pd))
  testthat::expect_length(pd, length(grid))
  testthat::expect_true(all(is.finite(pd)))
  testthat::expect_true(all(pd >= 0))

  pc <- stats::predict(fit0, newdata = grid, type = "cdf")
  testthat::expect_true(is.numeric(pc))
  testthat::expect_length(pc, length(grid))
  testthat::expect_true(all(is.finite(pc)))
  testthat::expect_true(all(pc >= 0 & pc <= 1))
  testthat::expect_true(all(diff(pc) >= -1e-8))

  ys <- stats::predict(fit0, type = "sample", n_samples = 2000L)
  testthat::expect_true(is.numeric(ys))
  testthat::expect_length(ys, 2000L)
  testthat::expect_true(all(is.finite(ys)))
  testthat::expect_true(all(ys >= 0))

  qq <- stats::predict(fit0, type = "quantile", probs = c(0.1, 0.5, 0.9))
  testthat::expect_true(is.matrix(qq))
  testthat::expect_true(.has_dimnames(qq))
  testthat::expect_true(all(c("mean","sd","lower","upper") %in% colnames(qq)))
  testthat::expect_equal(nrow(qq), 3L)
  .expect_ci_order(qq)

  rn <- rownames(qq)
  testthat::expect_true(all(c("q0.1","q0.5","q0.9") %in% rn))
  testthat::expect_true(qq["q0.1","mean"] <= qq["q0.5","mean"])
  testthat::expect_true(qq["q0.5","mean"] <= qq["q0.9","mean"])
})
