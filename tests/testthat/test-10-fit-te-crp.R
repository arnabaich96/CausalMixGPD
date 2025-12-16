testthat::test_that("fit.TE CRP unconditional gamma works and ate/qte/plot work", {
  set.seed(1)
  dat <- data.frame(
    y  = stats::rgamma(220, shape = 2, rate = 1),
    A  = stats::rbinom(220, 1, 0.5),
    x1 = stats::rnorm(220),
    x2 = stats::rnorm(220)
  )

  fit_te_crp <- DPmixGPD::fit.TE(
    y ~ 0,
    data   = dat,
    A      = "A",
    kernel = "gamma",
    tail   = FALSE,
    priors = list(),
    trans  = list(),
    intercept = TRUE,
    dp_rep = "crp",
    dp_ctrl = list(m_aux = 3),
    mcmc   = list(n_iter = 700, burn_in = 350, thin = 2, chains = 1),
    alpha  = 0.05
  )

  testthat::expect_true(inherits(fit_te_crp, "mixgpd_te_fit"))
  testthat::expect_true(all(c("fit_trt","fit_con","spec_trt","spec_con") %in% names(fit_te_crp)))

  d1 <- as.matrix(fit_te_crp$fit_trt$mcmc_draws)
  d0 <- as.matrix(fit_te_crp$fit_con$mcmc_draws)
  testthat::expect_true(nrow(d1) > 10 && nrow(d0) > 10)

  ate0 <- DPmixGPD::ate(fit_te_crp, level = 0.95)
  testthat::expect_true(is.matrix(ate0))
  testthat::expect_equal(nrow(ate0), 1L)
  testthat::expect_true(all(c("mean","sd","lower","upper") %in% colnames(ate0)))
  .expect_ci_order(ate0)

  qte0 <- DPmixGPD::qte(fit_te_crp, probs = c(0.1, 0.5, 0.9), level = 0.95)
  testthat::expect_true(is.matrix(qte0))
  testthat::expect_equal(nrow(qte0), 3L)
  testthat::expect_true(all(c("mean","sd","lower","upper") %in% colnames(qte0)))
  .expect_ci_order(qte0)

  p1 <- graphics::plot(fit_te_crp)
  testthat::expect_true(inherits(p1, "ggplot"))

  p2 <- graphics::plot(fit_te_crp, effect = "quantile", tau = seq(0.1, 0.9, by = 0.1))
  testthat::expect_true(inherits(p2, "ggplot"))
})
