strip_ansi <- function(x) {
  gsub("\u001b\\[[0-9;]*m", "", x, perl = TRUE)
}

test_that("progress helpers emit step messages when enabled", {
  ctx <- .cmgpd_progress_start(total_steps = 2L, enabled = TRUE, quiet = FALSE, label = "unit")
  msgs <- utils::capture.output(
    {
      .cmgpd_progress_step(ctx, "step one")
      .cmgpd_progress_step(ctx, "step two")
    },
    type = "message"
  )
  msgs <- strip_ansi(msgs)
  expect_true(any(grepl("^step one$", msgs)))
  expect_true(any(grepl("^step two$", msgs)))
  expect_silent(.cmgpd_progress_done(ctx))
})

test_that("progress helpers are silent when disabled", {
  ctx <- .cmgpd_progress_start(total_steps = 2L, enabled = FALSE, quiet = FALSE, label = "unit")
  expect_silent(.cmgpd_progress_step(ctx, "no-op"))
  expect_silent(.cmgpd_progress_done(ctx))
})

test_that("long-running APIs expose progress controls in formals", {
  expect_true("show_progress" %in% names(formals(predict.mixgpd_fit)))
  expect_true("show_progress" %in% names(formals(predict.causalmixgpd_causal_fit)))
  expect_true("show_progress" %in% names(formals(cqte)))
  expect_true("show_progress" %in% names(formals(cate)))
  expect_true("show_progress" %in% names(formals(qte)))
  expect_true("show_progress" %in% names(formals(qtt)))
  expect_true("show_progress" %in% names(formals(ate)))
  expect_true("show_progress" %in% names(formals(att)))
  expect_true("show_progress" %in% names(formals(ate_rmean)))
  expect_true("show_progress" %in% names(formals(allocation.mixgpd_fit)))
  expect_true("quiet" %in% names(formals(run_mcmc_causal)))
  expect_identical(formals(run_mcmc_bundle_manual)$quiet, FALSE)
})

test_that("run_mcmc_bundle_manual and predict honor progress toggles", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  set.seed(222)
  y <- abs(stats::rnorm(18)) + 0.1
  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,
    mcmc = mcmc_fast(seed = 222L)
  )

  expect_message(
    fit <- run_mcmc_bundle_manual(bundle, show_progress = TRUE, quiet = FALSE),
    "Validating configuration"
  )
  active_msg <- utils::capture.output(
    active_out <- utils::capture.output(
      run_mcmc_bundle_manual(bundle, show_progress = TRUE, quiet = FALSE),
      type = "output"
    ),
    type = "message"
  )
  active_msg <- strip_ansi(active_msg)
  active_lines <- c(active_out, active_msg)
  expect_true(any(grepl("Validating configuration", active_lines, fixed = TRUE)))
  expect_false(any(grepl("\\[[0-9]+/[0-9]+\\].*\\[[=-]+\\]\\s+[0-9]+%", active_lines)))
  expect_false(any(grepl("===== Monitors =====|running chain|Defining model", active_lines)))

  quiet_msg <- utils::capture.output(
    quiet_fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE, quiet = TRUE),
    type = "message"
  )
  quiet_out <- utils::capture.output(quiet_fit, type = "output")
  expect_false(any(grepl("\\[[0-9]+/[0-9]+\\]", c(quiet_msg, quiet_out))))

  expect_message(
    predict(fit, type = "quantile", index = 0.5, show_progress = TRUE),
    "Validating prediction inputs"
  )
  pred_quiet <- utils::capture.output(
    predict(fit, type = "quantile", index = 0.5, show_progress = FALSE),
    type = "message"
  )
  expect_false(any(grepl("Validating prediction inputs", pred_quiet, fixed = TRUE)))
})
