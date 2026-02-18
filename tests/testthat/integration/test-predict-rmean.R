# test-predict-rmean.R
# Focused regression checks for mean vs restricted mean under heavy tails.

test_that("predict(type='mean') returns Inf when xi >= 1 and rmean stays finite", {
  skip_if_not_test_level("ci")

  fit <- .load_fixture("fit_gpd_small.rds")

  smp <- fit$mcmc$samples %||% fit$samples
  if (is.null(smp) || !is.list(smp)) {
    skip("Fixture has no posterior samples to patch.")
  }

  ch <- as.matrix(smp[[1]])
  if (!("tail_shape" %in% colnames(ch))) {
    skip("Fixture has no tail_shape draws (non-GPD or unexpected structure).")
  }

  ch[1L, "tail_shape"] <- 1.5
  smp[[1]] <- coda::as.mcmc(ch)

  fit_patched <- fit
  if (!is.null(fit_patched$mcmc$samples)) fit_patched$mcmc$samples <- smp
  if (!is.null(fit_patched$samples)) fit_patched$samples <- smp

  expect_warning(
    pred_mean <- predict(fit_patched, type = "mean", interval = "none", store_draws = FALSE),
    "infinite"
  )
  expect_equal(pred_mean$fit$estimate, Inf)

  pred_rmean <- predict(fit_patched, type = "rmean", cutoff = 50, interval = "none", store_draws = FALSE)
  expect_true(all(is.finite(pred_rmean$fit$estimate)))
})
