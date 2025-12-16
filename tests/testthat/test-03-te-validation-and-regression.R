
testthat::test_that("fit.TE regression: ate/qte/plot fail cleanly if not implemented", {
  set.seed(1)
  dat <- data.frame(
    y  = stats::rgamma(200, shape = 2, rate = 1),
    A  = stats::rbinom(200, 1, 0.5),
    x1 = stats::rnorm(200),
    x2 = stats::rnorm(200)
  )

  fit_te1 <- DPmixGPD::fit.TE(
    y ~ x1 + x2,
    data   = dat,
    A      = "A",
    kernel = "gamma",
    tail   = FALSE,
    dp_rep = "stick_breaking",
    dp_ctrl = list(K = 5),
    mcmc   = list(n_iter = 600, burn_in = 300, chains = 1),
    alpha  = 0.05
  )

  testthat::expect_true(inherits(fit_te1, "mixgpd_te_fit"))

  testthat::expect_error(DPmixGPD::ate(fit_te1),
                         regexp = "unconditional|not implemented|Gamma",
                         ignore.case = TRUE)
  testthat::expect_error(DPmixGPD::qte(fit_te1, probs = c(0.25,0.5,0.75)),
                         regexp = "unconditional|not implemented|Gamma",
                         ignore.case = TRUE)
  testthat::expect_error(graphics::plot(fit_te1),
                         regexp = "unconditional|not implemented|Gamma",
                         ignore.case = TRUE)
})

testthat::test_that("fit.TE kernel length 1 vs 2 is respected", {
  set.seed(1)
  dat <- data.frame(
    y  = stats::rgamma(200, shape = 2, rate = 1),
    A  = stats::rbinom(200, 1, 0.5),
    x1 = stats::rnorm(200),
    x2 = stats::rnorm(200)
  )

  fit1 <- DPmixGPD::fit.TE(
    y ~ 0,
    data = dat,
    A = "A",
    kernel = "gamma",
    tail = FALSE,
    dp_ctrl = list(K=5),
    mcmc = list(n_iter=400, burn_in=200, chains=1)
  )
  testthat::expect_equal(fit1$spec_trt$kernel, "gamma")
  testthat::expect_equal(fit1$spec_con$kernel, "gamma")

  fit2 <- DPmixGPD::fit.TE(
    y ~ 0,
    data = dat,
    A = "A",
    kernel = c("gamma","lognormal"),
    tail = FALSE,
    dp_ctrl = list(K=5),
    mcmc = list(n_iter=300, burn_in=150, chains=1)
  )
  testthat::expect_equal(fit2$spec_trt$kernel, "gamma")
  testthat::expect_equal(fit2$spec_con$kernel, "lognormal")

  testthat::expect_error(DPmixGPD::ate(fit2),
                         regexp = "Gamma|not implemented",
                         ignore.case = TRUE)
  testthat::expect_error(DPmixGPD::qte(fit2, probs=c(0.1,0.5,0.9)),
                         regexp = "Gamma|not implemented",
                         ignore.case = TRUE)
})

testthat::test_that("fit.TE validates A (missing, non-binary)", {
  set.seed(1)
  dat <- data.frame(
    y  = stats::rgamma(100, shape = 2, rate = 1),
    A  = stats::rbinom(100, 1, 0.5),
    x1 = stats::rnorm(100)
  )

  testthat::expect_error(
    DPmixGPD::fit.TE(y ~ 0, data = dat, kernel="gamma", tail=FALSE),
    regexp = "A.*missing|argument.*A",
    ignore.case = TRUE
  )

  dat2 <- dat
  dat2$A <- sample(0:2, nrow(dat2), TRUE)
  testthat::expect_error(
    DPmixGPD::fit.TE(y ~ 0, data = dat2, A="A", kernel="gamma", tail=FALSE),
    regexp = "binary|0/1",
    ignore.case = TRUE
  )

  A_vec <- dat$A
  fit3 <- DPmixGPD::fit.TE(
    y ~ x1,
    data = dat,
    A = A_vec,
    kernel="gamma",
    tail=FALSE,
    dp_ctrl=list(K=5),
    mcmc=list(n_iter=300, burn_in=150, chains=1)
  )
  testthat::expect_true(inherits(fit3, "mixgpd_te_fit"))
})

testthat::test_that("fit.TE drops rows with missing model vars consistently", {
  set.seed(1)
  dat <- data.frame(
    y  = stats::rgamma(200, shape = 2, rate = 1),
    A  = stats::rbinom(200, 1, 0.5),
    x1 = stats::rnorm(200),
    x2 = stats::rnorm(200)
  )
  dat_na <- dat
  dat_na$x1[sample.int(nrow(dat_na), 5)] <- NA

  fit_na <- DPmixGPD::fit.TE(
    y ~ x1 + x2,
    data = dat_na,
    A = "A",
    kernel="gamma",
    tail=FALSE,
    dp_ctrl=list(K=5),
    mcmc=list(n_iter=250, burn_in=125, chains=1)
  )

  n_cc <- sum(stats::complete.cases(dat_na[, c("y","A","x1","x2")]))
  testthat::expect_equal(fit_na$spec_trt$N + fit_na$spec_con$N, n_cc)
})
