test_that("monitor profile supports core/full policy", {
  skip_if_not_test_level("ci")
  set.seed(511)
  y <- abs(stats::rnorm(80)) + 0.1
  b_core <- build_nimble_bundle(
    y = y, backend = "sb", kernel = "normal", GPD = FALSE, components = 6,
    monitor = "core"
  )
  b_full <- build_nimble_bundle(
    y = y, backend = "sb", kernel = "normal", GPD = FALSE, components = 6,
    monitor = "full"
  )
  expect_false(any(grepl("^z\\[", b_core$monitors)))
  expect_true(any(grepl("^z\\[", b_full$monitors)))
  expect_true(any(grepl("^v\\[", b_full$monitors)))
})

test_that("mcmc overrides accept z_update_every", {
  skip_if_not_test_level("ci")
  set.seed(512)
  y <- abs(stats::rnorm(80)) + 0.1
  fit <- dpmgpd(
    x = y,
    backend = "sb",
    kernel = "normal",
    components = 6,
    mcmc = list(niter = 100, nburnin = 30, thin = 1, nchains = 1, seed = 14, z_update_every = 2)
  )
  expect_equal(fit$mcmc$z_update_every, 2L)
})
