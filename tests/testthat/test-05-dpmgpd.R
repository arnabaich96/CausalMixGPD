.tiny_mcmc <- function() list(n_iter = 150L, burn_in = 50L, thin = 2L, chains = 1L)

testthat::test_that("DPM y-only fits run (CRP & SB) and predictions work", {
  set.seed(1)
  dat <- data.frame(y = stats::rgamma(120, shape = 2, scale = 1))
  mcmc <- .tiny_mcmc()

  # -------------------------
  # Stick-breaking
  # -------------------------
  fit_sb <- DPmixGPD::fit.dpm(
    y ~ 0, data = dat, kernel = "gamma",
    dp_rep = "stick_breaking",
    dp_ctrl = list(K = 5),
    mcmc = mcmc
  )
  testthat::expect_s3_class(fit_sb, "mixgpd_fit")

  q_sb <- stats::predict(fit_sb, type = "quantile", probs = c(0.1, 0.5, 0.9))
  testthat::expect_true(is.matrix(q_sb) || is.data.frame(q_sb))
  testthat::expect_true(all(c("mean", "sd", "lower", "upper") %in% colnames(q_sb)))

  d_sb <- stats::predict(fit_sb, type = "density", newdata = data.frame(y = c(0.5, 1)))
  testthat::expect_type(d_sb, "double")
  testthat::expect_length(d_sb, 2L)
  testthat::expect_true(all(is.finite(d_sb)))

  c_sb <- stats::predict(fit_sb, type = "cdf", newdata = data.frame(y = c(0.5, 1)))
  testthat::expect_type(c_sb, "double")
  testthat::expect_length(c_sb, 2L)
  testthat::expect_true(all(is.finite(c_sb)))
  testthat::expect_true(all(c_sb >= 0 & c_sb <= 1))

  s_sb <- stats::predict(fit_sb, type = "sample", n_samples = 20L)
  testthat::expect_type(s_sb, "double")
  testthat::expect_length(s_sb, 20L)
  testthat::expect_true(all(is.finite(s_sb)))

  # -------------------------
  # CRP
  # -------------------------
  fit_crp <- DPmixGPD::fit.dpm(
    y ~ 0, data = dat, kernel = "gamma",
    dp_rep = "crp",
    mcmc = mcmc
  )
  testthat::expect_s3_class(fit_crp, "mixgpd_fit")

  q_crp <- stats::predict(fit_crp, type = "quantile", probs = c(0.1, 0.5, 0.9))
  testthat::expect_true(is.matrix(q_crp) || is.data.frame(q_crp))
  testthat::expect_true(all(c("mean", "sd", "lower", "upper") %in% colnames(q_crp)))

  d_crp <- stats::predict(fit_crp, type = "density", newdata = data.frame(y = c(0.5, 1)))
  testthat::expect_type(d_crp, "double")
  testthat::expect_length(d_crp, 2L)
  testthat::expect_true(all(is.finite(d_crp)))

  c_crp <- stats::predict(fit_crp, type = "cdf", newdata = data.frame(y = c(0.5, 1)))
  testthat::expect_type(c_crp, "double")
  testthat::expect_length(c_crp, 2L)
  testthat::expect_true(all(is.finite(c_crp)))
  testthat::expect_true(all(c_crp >= 0 & c_crp <= 1))

  s_crp <- stats::predict(fit_crp, type = "sample", n_samples = 20L)
  testthat::expect_type(s_crp, "double")
  testthat::expect_length(s_crp, 20L)
  testthat::expect_true(all(is.finite(s_crp)))
})


.tiny_mcmc <- function() list(n_iter = 150L, burn_in = 50L, thin = 2L, chains = 1L)

 testthat::test_that("DPM regression fits run (CRP & SB) and newdata prediction works", {
  set.seed(2)
  n <- 140
  dat <- data.frame(
    y  = stats::rgamma(n, shape = 2, scale = 1),
    x1 = stats::rnorm(n),
    x2 = stats::runif(n)
  )
  xnew <- data.frame(x1 = c(0, 1), x2 = c(0.2, 0.8))
  mcmc <- .tiny_mcmc()

  fit_sb <- DPmixGPD::fit.dpm(y ~ x1 + x2, data = dat, kernel = "gamma",
                             dp_rep = "stick_breaking",
                             dp_ctrl = list(K = 5),
                             mcmc = mcmc)
  testthat::expect_s3_class(fit_sb, "mixgpd_fit")

  q_sb <- stats::predict(fit_sb, type = "quantile", probs = c(0.25, 0.5), newdata = xnew)
  testthat::expect_true(is.matrix(q_sb) || is.data.frame(q_sb))

  fit_crp <- DPmixGPD::fit.dpm(y ~ x1 + x2, data = dat, kernel = "gamma",
                              dp_rep = "crp",
                              dp_ctrl = list(m_aux = 5),
                              mcmc = mcmc)
  testthat::expect_s3_class(fit_crp, "mixgpd_fit")

  q_crp <- stats::predict(fit_crp, type = "quantile", probs = c(0.25, 0.5), newdata = xnew)
  testthat::expect_true(is.matrix(q_crp) || is.data.frame(q_crp))
 })


.tiny_mcmc <- function() list(n_iter = 160L, burn_in = 60L, thin = 2L, chains = 1L)

 testthat::test_that("DPMGPD y-only fits run (CRP & SB) and predictions work", {
  set.seed(3)
  y <- c(stats::rgamma(160, shape = 2, scale = 1), stats::rgamma(10, shape = 1, scale = 6))
  dat <- data.frame(y = y)
  mcmc <- .tiny_mcmc()

  fit_sb <- DPmixGPD::fit.dpmgpd(y ~ 0, data = dat, kernel = "gamma",
                                dp_rep = "stick_breaking",
                                dp_ctrl = list(K = 5),
                                mcmc = mcmc)
  testthat::expect_s3_class(fit_sb, "mixgpd_fit")
  testthat::expect_error(
    stats::predict(fit_sb, type = "cdf", newdata = data.frame(y = 1)),
    "predict\\(\\) is currently implemented for DPM",
    fixed = FALSE
  )

  fit_crp <- DPmixGPD::fit.dpmgpd(y ~ 0, data = dat, kernel = "gamma",
                                 dp_rep = "crp",
                                 mcmc = mcmc)
  testthat::expect_s3_class(fit_crp, "mixgpd_fit")
  testthat::expect_error(
    stats::predict(fit_crp, type = "cdf", newdata = data.frame(y = 1)),
    "predict\\(\\) is currently implemented for DPM",
    fixed = FALSE
  )
 })


.tiny_mcmc <- function() list(n_iter = 160L, burn_in = 60L, thin = 2L, chains = 1L)

 testthat::test_that("DPMGPD regression fits run (CRP & SB) and newdata prediction works", {
  set.seed(4)
  n <- 170
  dat <- data.frame(
    y  = c(stats::rgamma(n, shape = 2, scale = 1), stats::rgamma(8, shape = 1, scale = 7)),
    x1 = stats::rnorm(n + 8),
    x2 = stats::runif(n + 8)
  )
  xnew <- cbind(x1 = c(-0.5, 0.5), x2 = c(0.1, 0.9))
  mcmc <- .tiny_mcmc()

  fit_sb <- DPmixGPD::fit.dpmgpd(y ~ x1 + x2, data = dat, kernel = "gamma",
                                dp_rep = "stick_breaking", dp_ctrl = list(K = 5), mcmc = mcmc)
  testthat::expect_s3_class(fit_sb, "mixgpd_fit")
  testthat::expect_error(
    stats::predict(fit_sb, newdata = xnew, type = "quantile", p = 0.5),
    "predict\\(\\) is currently implemented for DPM",
    fixed = FALSE
  )

  fit_crp <- DPmixGPD::fit.dpmgpd(y ~ x1 + x2, data = dat, kernel = "gamma",
                                 dp_rep = "crp", dp_ctrl = list(m_aux = 3), mcmc = mcmc)
  testthat::expect_s3_class(fit_crp, "mixgpd_fit")
  testthat::expect_error(
    stats::predict(fit_crp, newdata = xnew, type = "quantile", p = 0.5),
    "predict\\(\\) is currently implemented for DPM",
    fixed = FALSE
  )
 })



testthat::test_that("ATE with GPD: xi >= 1 triggers warning and drops invalid draws", {
  K <- 3
  M <- 50

  make_draws <- function(xi_vals) {
    draws <- cbind(
      alpha = rep(0.05, M),
      matrix(2, nrow=M, ncol=K, dimnames=list(NULL, paste0("shape[",1:K,"]"))),
      matrix(1, nrow=M, ncol=K, dimnames=list(NULL, paste0("scale[",1:K,"]"))),
      matrix(1/K, nrow=M, ncol=K, dimnames=list(NULL, paste0("w[",1:K,"]"))),
      u = rep(1.0, M),
      sigma = rep(1.0, M),
      xi = xi_vals
    )
    colnames(draws)[1] <- "alpha"
    draws
  }

  xi_trt <- c(rep(0.2, M-5), rep(1.2, 5))
  xi_con <- rep(0.2, M)

  te_obj <- .make_fake_te_fit(make_draws(xi_trt), make_draws(xi_con), tail = c(TRUE, TRUE))

  testthat::expect_warning({
    out <- DPmixGPD::ate(te_obj, level = 0.95)
    testthat::expect_true(is.matrix(out))
    testthat::expect_true(all(c("mean","sd","lower","upper") %in% colnames(out)))
  }, regexp = "xi\\s*>=\\s*1|infinite|Dropping", ignore.case = TRUE)
})

testthat::test_that("ATE with GPD: all xi >= 1 returns NA with warning", {
  K <- 3
  M <- 30

  make_draws <- function(xi_vals) {
    draws <- cbind(
      alpha = rep(0.05, M),
      matrix(2, nrow=M, ncol=K, dimnames=list(NULL, paste0("shape[",1:K,"]"))),
      matrix(1, nrow=M, ncol=K, dimnames=list(NULL, paste0("scale[",1:K,"]"))),
      matrix(1/K, nrow=M, ncol=K, dimnames=list(NULL, paste0("w[",1:K,"]"))),
      u = rep(1.0, M),
      sigma = rep(1.0, M),
      xi = xi_vals
    )
    colnames(draws)[1] <- "alpha"
    draws
  }

  te_obj <- .make_fake_te_fit(make_draws(rep(1.5, M)), make_draws(rep(1.2, M)), tail = c(TRUE, TRUE))

  testthat::expect_warning({
    out <- DPmixGPD::ate(te_obj, level = 0.95)
    testthat::expect_true(is.matrix(out))
    testthat::expect_true(all(is.na(out)))
  }, regexp = "all posterior draws|returning NA|xi", ignore.case = TRUE)
})


