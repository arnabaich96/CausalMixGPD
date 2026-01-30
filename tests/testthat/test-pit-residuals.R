# test-pit-residuals.R
# Focused regression checks for PIT diagnostics and draw filtering.

test_that("PIT residuals include diagnostics and drop invalid draws", {
  skip_if_not_test_level("ci")

  fit <- .load_fixture("fit_cond_small.rds")

  smp <- fit$mcmc$samples %||% fit$samples
  if (is.null(smp) || !is.list(smp)) {
    skip("Fixture has no posterior samples to patch.")
  }

  kernel <- fit$spec$meta$kernel %||% fit$spec$kernel$key
  kdef <- DPmixGPD::get_kernel_registry()[[kernel]] %||% list()
  bulk_support <- kdef$bulk_support %||% list()
  positive_params <- names(bulk_support)[bulk_support %in%
    c("positive_sd", "positive_scale", "positive_shape", "positive_location")]
  if (!length(positive_params)) {
    skip("No positive-support parameters found to invalidate.")
  }

  ch <- as.matrix(smp[[1]])
  target <- NULL
  for (nm in positive_params) {
    cand <- grep(paste0("^", nm, "\\["), colnames(ch), value = TRUE)
    if (length(cand)) {
      target <- cand[1]
      break
    }
  }
  if (is.null(target)) {
    skip("Could not find a bulk parameter column to invalidate.")
  }

  ch[1L, target] <- -abs(ch[1L, target]) - 1
  smp[[1]] <- coda::as.mcmc(ch)

  fit_patched <- fit
  if (!is.null(fit_patched$mcmc$samples)) fit_patched$mcmc$samples <- smp
  if (!is.null(fit_patched$samples)) fit_patched$samples <- smp

  res_mean <- residuals(fit_patched, type = "pit", pit = "bayes_mean", pit_seed = 1L)
  diag_mean <- attr(res_mean, "pit_diagnostics")
  expect_true(is.list(diag_mean))
  expect_true(diag_mean$n_draws_total >= diag_mean$n_draws_valid)
  expect_true(diag_mean$n_draws_dropped >= 1)
  expect_true(is.numeric(diag_mean$n_draws_used))
  expect_equal(length(diag_mean$n_draws_used), length(res_mean))
  expect_true(all(diag_mean$n_draws_used <= diag_mean$n_draws_total))
  expect_true(all(res_mean >= 0 & res_mean <= 1, na.rm = TRUE))

  res_draw <- residuals(fit_patched, type = "pit", pit = "bayes_draw", pit_seed = 1L)
  diag_draw <- attr(res_draw, "pit_diagnostics")
  expect_true(is.list(diag_draw))
  expect_true(diag_draw$n_draws_total >= diag_draw$n_draws_valid)
  expect_true(diag_draw$n_draws_dropped >= 1)
  expect_true(is.numeric(diag_draw$n_draws_used))
  expect_equal(length(diag_draw$n_draws_used), length(res_draw))
  expect_true(all(diag_draw$n_draws_used <= diag_draw$n_draws_total))
  expect_true(all(res_draw >= 0 & res_draw <= 1, na.rm = TRUE))
})
