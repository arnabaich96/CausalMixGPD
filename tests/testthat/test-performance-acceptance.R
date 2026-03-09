test_that("monitoring policy defaults exclude latent z and opt-in works", {
  skip_if_not_test_level("ci")
  set.seed(101)
  y <- abs(stats::rnorm(80)) + 0.1

  b_default <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 6
  )
  expect_false(any(grepl("^z\\[", b_default$monitors)))

  b_latent <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 6,
    monitor_latent = TRUE
  )
  expect_true(any(grepl("^z\\[", b_latent$monitors)))
})

test_that("compile cache exposes timing and reuses build/compile", {
  skip_if_not_test_level("ci")
  set.seed(102)
  y <- abs(stats::rnorm(80)) + 0.1
  m <- list(niter = 120, nburnin = 30, thin = 1, nchains = 1, seed = 42, timing = TRUE)

  b <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 6,
    mcmc = m
  )

  fit1 <- run_mcmc_bundle_manual(b, show_progress = FALSE, timing = TRUE)
  fit2 <- run_mcmc_bundle_manual(b, show_progress = FALSE, timing = TRUE)

  expect_true(is.list(fit1$timing))
  expect_true(is.list(fit2$timing))
  expect_true(all(c("build", "compile", "mcmc") %in% names(fit1$timing)))
  expect_true(all(c("build", "compile", "mcmc") %in% names(fit2$timing)))
  expect_true(isTRUE(fit2$timing$cache_hit))
  expect_lte(unname(fit2$timing$build), unname(fit1$timing$build) + 1e-8)
  expect_lte(unname(fit2$timing$compile), unname(fit1$timing$compile) + 1e-8)
})

test_that("parallel chains path preserves output contract", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  set.seed(103)
  y <- abs(stats::rnorm(60)) + 0.1
  b <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 5,
    mcmc = list(niter = 120, nburnin = 30, thin = 1, nchains = 2, seed = 11)
  )

  fit_seq <- run_mcmc_bundle_manual(b, show_progress = FALSE, parallel_chains = FALSE, timing = TRUE)
  fit_par <- run_mcmc_bundle_manual(b, show_progress = FALSE, parallel_chains = TRUE, workers = 2, timing = TRUE)

  expect_s3_class(fit_par, "mixgpd_fit")
  expect_equal(names(fit_par), names(fit_seq))
  expect_equal(fit_par$mcmc$nchains, 2)
  expect_true(length(unique(as.integer(fit_par$mcmc$seed))) > 1)
})

test_that("causal arms can run in parallel and preserve contract", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  set.seed(104)
  n <- 120
  x1 <- stats::rnorm(n)
  x2 <- stats::runif(n)
  A <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.4 * x1))
  y <- abs(0.7 * x1 + 0.2 * x2 + A + stats::rnorm(n)) + 0.1
  X <- cbind(x1 = x1, x2 = x2)

  cb <- build_causal_bundle(
    y = y, X = X, A = A,
    backend = "sb", kernel = "normal", GPD = FALSE, components = 6,
    mcmc_outcome = list(niter = 100, nburnin = 30, thin = 1, nchains = 1, seed = 21),
    mcmc_ps = list(niter = 80, nburnin = 20, thin = 1, nchains = 1, seed = 22),
    PS = "logit"
  )

  fit_seq <- run_mcmc_causal(cb, show_progress = FALSE, parallel_arms = FALSE, timing = TRUE)
  fit_par <- run_mcmc_causal(cb, show_progress = FALSE, parallel_arms = TRUE, workers = 2, timing = TRUE)

  expect_s3_class(fit_par, "causalmixgpd_causal_fit")
  expect_equal(names(fit_par), names(fit_seq))
  expect_true(is.list(fit_par$timing))
})

test_that("predict supports ndraws_pred/chunk_size/parallel aliases", {
  skip_if_not_test_level("ci")
  set.seed(105)
  n <- 120
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n))
  y <- abs(0.5 * X[, 1] + stats::rnorm(n)) + 0.1
  b <- build_nimble_bundle(
    y = y, X = X, backend = "sb", kernel = "normal", GPD = FALSE, components = 6,
    mcmc = list(niter = 120, nburnin = 30, thin = 1, nchains = 1, seed = 31),
    monitor_latent = TRUE
  )
  fit <- run_mcmc_bundle_manual(b, show_progress = FALSE)
  X_new <- X[1:40, , drop = FALSE]

  p_seq <- predict(
    fit,
    x = X_new,
    type = "quantile",
    index = c(0.5, 0.9),
    ndraws_pred = 50,
    chunk_size = 10,
    parallel = FALSE
  )
  p_par <- predict(
    fit,
    x = X_new,
    type = "quantile",
    index = c(0.5, 0.9),
    ndraws_pred = 50,
    chunk_size = 10,
    parallel = TRUE,
    workers = 2
  )

  expect_s3_class(p_seq, "mixgpd_predict")
  expect_s3_class(p_par, "mixgpd_predict")
  expect_equal(names(p_seq$fit), names(p_par$fit))
  expect_equal(dim(p_seq$fit), dim(p_par$fit))

  expect_error(
    predict(
      fit,
      x = X_new,
      type = "quantile",
      index = c(0.5, 0.9),
      cred.level = 0.9
    ),
    "'cred.level' is no longer supported; use 'level' instead."
  )
})

test_that("parallel runtime does not leave global future plan modified", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)

  set.seed(106)
  y <- abs(stats::rnorm(50)) + 0.1
  b <- build_nimble_bundle(
    y = y, backend = "sb", kernel = "normal", GPD = FALSE, components = 5,
    mcmc = list(niter = 80, nburnin = 20, thin = 1, nchains = 2, seed = 9)
  )
  invisible(run_mcmc_bundle_manual(b, show_progress = FALSE, parallel_chains = TRUE, workers = 2))
  after_plan <- future::plan()
  expect_identical(class(after_plan), class(old_plan))
})

test_that("functional equivalence and contracts on seeded runs", {
  skip_if_not_test_level("full")

  tol <- 0.2
  set.seed(107)
  y <- abs(stats::rnorm(140)) + 0.1
  X <- cbind(x1 = stats::rnorm(140), x2 = stats::runif(140))
  A <- stats::rbinom(140, 1, 0.5)

  m <- list(niter = 600, nburnin = 150, thin = 2, nchains = 1, seed = 77)
  fit_u_old <- dpmgpd(x = y, backend = "sb", kernel = "normal", components = 6, mcmc = m, monitor_latent = TRUE)
  fit_u_new <- dpmgpd(x = y, backend = "sb", kernel = "normal", components = 6,
                      monitor_latent = TRUE,
                      mcmc = utils::modifyList(m, list(parallel_chains = FALSE)))
  mu_old <- mean(as.numeric(.extract_draws_matrix(fit_u_old)[, "alpha"]), na.rm = TRUE)
  mu_new <- mean(as.numeric(.extract_draws_matrix(fit_u_new)[, "alpha"]), na.rm = TRUE)
  expect_lte(abs(mu_old - mu_new), tol)
  expect_equal(class(fit_u_old), class(fit_u_new))
  expect_equal(sort(names(fit_u_old)), sort(names(fit_u_new)))

  cb <- build_causal_bundle(
    y = y, X = X, A = A, backend = "sb", kernel = "normal", GPD = FALSE, components = 6,
    mcmc_outcome = m, mcmc_ps = m, PS = "logit"
  )
  cf_seq <- run_mcmc_causal(cb, show_progress = FALSE, parallel_arms = FALSE)
  cf_par <- run_mcmc_causal(cb, show_progress = FALSE, parallel_arms = TRUE, workers = 2)
  expect_equal(class(cf_seq), class(cf_par))

  p_seq <- predict(fit_u_new, type = "quantile", index = c(0.5, 0.9), parallel = FALSE)
  p_old <- predict(fit_u_old, type = "quantile", index = c(0.5, 0.9), parallel = FALSE)
  expect_equal(names(p_seq$fit), names(p_old$fit))
  expect_equal(dim(p_seq$fit), dim(p_old$fit))
})
