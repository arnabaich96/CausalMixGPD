testthat::test_that("fit.dpm CRP gamma runs without K and returns SB-compatible draws", {
  set.seed(1)
  dat <- data.frame(y = stats::rgamma(120, shape = 2, scale = 1))

  fit_crp <- DPmixGPD::fit.dpm(
    y ~ 0,
    data   = dat,
    kernel = "gamma",
    dp_rep = "crp",
    dp_ctrl = list(m_aux = 3),
    mcmc   = list(n_iter = 400, burn_in = 200, thin = 2, chains = 1),
    alpha  = 0.05
  )

  testthat::expect_true(inherits(fit_crp, "mixgpd_fit"))

  # S3 basics should not error
  testthat::expect_silent(print(fit_crp))
  testthat::expect_silent(summary(fit_crp))

  draws <- fit_crp$mcmc_draws
  testthat::expect_true(inherits(draws, "mcmc.list"))

  mat <- as.matrix(draws)
  testthat::expect_true("alpha" %in% colnames(mat))
  testthat::expect_true(any(grepl("^w\\[", colnames(mat))))

  wcols <- grep("^w\\[", colnames(mat), value = TRUE)
  wsum <- rowSums(mat[, wcols, drop = FALSE])
  testthat::expect_true(all(is.finite(wsum)))
  testthat::expect_true(all(abs(wsum - 1) < 1e-6))
})
