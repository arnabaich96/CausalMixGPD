test_that("PS utilities: design matrix alignment and scaling", {
  ps_design_matrix <- DPmixGPD:::.ps_design_matrix
  apply_ps_scale <- DPmixGPD:::.apply_ps_scale

  X_train <- cbind(`(Intercept)` = 1, x1 = c(0, 1), x2 = c(2, 3))
  ps_bundle <- list(
    spec = list(meta = list(include_intercept = TRUE), model = "logit"),
    data = list(X = X_train)
  )
  class(ps_bundle) <- "dpmixgpd_ps_bundle"

  X_new <- cbind(x2 = c(10, 11), x1 = c(4, 5))
  colnames(X_new) <- c("x2", "x1")
  dm <- ps_design_matrix(ps_bundle, X_new)
  expect_equal(colnames(dm), colnames(X_train))
  expect_equal(dm[, 1], c(1, 1))
  expect_equal(dm[, "x1"], c(4, 5))
  expect_equal(dm[, "x2"], c(10, 11))

  # mismatch column names error
  X_bad <- cbind(x1 = c(1, 2), x3 = c(3, 4))
  expect_error(ps_design_matrix(ps_bundle, X_bad), "do not match PS design")

  # intercept-only model expects 0 covariate columns
  X_train_int <- cbind(`(Intercept)` = 1)
  ps_bundle_int <- list(
    spec = list(meta = list(include_intercept = TRUE), model = "logit"),
    data = list(X = X_train_int)
  )
  class(ps_bundle_int) <- "dpmixgpd_ps_bundle"
  X0 <- matrix(numeric(0), nrow = 3, ncol = 0)
  dm0 <- ps_design_matrix(ps_bundle_int, X0)
  expect_equal(dim(dm0), c(3L, 1L))
  expect_error(ps_design_matrix(ps_bundle_int, matrix(1, nrow = 3, ncol = 1)), "PS-only intercept")

  # scaling: prob passthrough; logit clamps to finite
  expect_equal(apply_ps_scale(c(0.2, 0.8), scale = "prob"), c(0.2, 0.8))
  out <- apply_ps_scale(c(0, 1), scale = "logit", clamp = 1e-6)
  expect_true(all(is.finite(out)))
})

test_that("PS utilities: compute PS from fit (logit/probit/naive + error branches)", {
  compute_ps_from_fit <- DPmixGPD:::.compute_ps_from_fit

  # logit/probit paths
  X_train <- cbind(`(Intercept)` = 1, x1 = c(0, 1), x2 = c(2, 3))
  ps_bundle_logit <- list(
    spec = list(meta = list(include_intercept = TRUE), model = "logit"),
    data = list(X = X_train)
  )
  class(ps_bundle_logit) <- "dpmixgpd_ps_bundle"
  ps_bundle_probit <- ps_bundle_logit
  ps_bundle_probit$spec$model <- "probit"

  set.seed(1)
  S <- 10
  beta_draws <- matrix(stats::rnorm(S * 3, sd = 0.2), nrow = S)
  colnames(beta_draws) <- c("beta[1]", "beta[2]", "beta[3]")
  ps_fit_ok <- list(mcmc = list(samples = beta_draws))

  X_new <- cbind(x1 = c(-0.5, 0.5), x2 = c(0.1, -0.1))
  ps_l <- compute_ps_from_fit(ps_fit_ok, ps_bundle_logit, X_new, summary = "mean", clamp = 1e-6)
  ps_p <- compute_ps_from_fit(ps_fit_ok, ps_bundle_probit, X_new, summary = "median", clamp = 1e-6)
  expect_true(all(ps_l > 0 & ps_l < 1))
  expect_true(all(ps_p > 0 & ps_p < 1))
  expect_equal(length(ps_l), nrow(X_new))
  expect_equal(length(ps_p), nrow(X_new))

  # error: missing beta draws
  ps_fit_no_beta <- list(mcmc = list(samples = matrix(0, nrow = 2, ncol = 1, dimnames = list(NULL, "alpha"))))
  expect_error(compute_ps_from_fit(ps_fit_no_beta, ps_bundle_logit, X_new), "beta draws not found")

  # naive path
  ps_bundle_naive <- list(
    spec = list(meta = list(include_intercept = FALSE), model = "naive"),
    data = list(X = cbind(x1 = c(0, 1), x2 = c(2, 3)))
  )
  class(ps_bundle_naive) <- "dpmixgpd_ps_bundle"

  S2 <- 5
  samples_naive <- matrix(NA_real_, nrow = S2, ncol = 1 + 4 + 4)
  colnames(samples_naive) <- c(
    "pi_prior",
    "mu[1,1]", "mu[2,1]", "mu[1,2]", "mu[2,2]",
    "sigma[1,1]", "sigma[2,1]", "sigma[1,2]", "sigma[2,2]"
  )
  samples_naive[, "pi_prior"] <- 0.4
  samples_naive[, c("mu[1,1]", "mu[2,1]", "mu[1,2]", "mu[2,2]")] <- rep(c(0, 0.5, 0, 0.5), each = S2)
  samples_naive[, c("sigma[1,1]", "sigma[2,1]", "sigma[1,2]", "sigma[2,2]")] <- rep(c(1, 1, 1, 1), each = S2)
  ps_fit_naive <- list(mcmc = list(samples = samples_naive))

  Xn <- cbind(x1 = c(-0.2, 0.1, 0.4), x2 = c(0.0, 0.3, -0.1))
  ps_n <- compute_ps_from_fit(ps_fit_naive, ps_bundle_naive, Xn, summary = "mean", clamp = 1e-6)
  expect_true(all(ps_n > 0 & ps_n < 1))
  expect_equal(length(ps_n), nrow(Xn))

  # error: naive missing required columns
  ps_fit_naive_bad <- list(mcmc = list(samples = matrix(0, nrow = 2, ncol = 1, dimnames = list(NULL, "pi_prior"))))
  expect_error(compute_ps_from_fit(ps_fit_naive_bad, ps_bundle_naive, Xn), "Naive Bayes PS samples missing")
})

