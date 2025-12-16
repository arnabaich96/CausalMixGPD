
testthat::test_that("mixture weights sum to ~1 and are finite (unconditional gamma)", {
  set.seed(1)
  dat <- data.frame(y = stats::rgamma(200, shape=2, scale=1))

  fit0 <- DPmixGPD::fit.dpm(
    y ~ 0, data = dat,
    kernel="gamma", dp_rep="stick_breaking",
    dp_ctrl=list(K=5),
    mcmc=list(n_iter=800, burn_in=400, chains=1),
    alpha=0.05
  )

  draws <- as.matrix(fit0$mcmc_draws)
  w_cols <- grep("^w\\[", colnames(draws), value=TRUE)

  testthat::expect_true(length(w_cols) > 0)
  w_ok <- rowSums(draws[, w_cols, drop=FALSE])

  testthat::expect_true(all(is.finite(w_ok)))
  testthat::expect_true(stats::median(w_ok) > 0.95)
  testthat::expect_true(stats::median(w_ok) <= 1.05)
})
