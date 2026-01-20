# Test unconditional predict() methods with all output formats
# Tests quantile, mean, density, survival, sample, and fitted predictions
#
# NOTE: All tests in this file require ci level due to MCMC fitting

# Setup: Create shared fit object for all tests (only at ci level or higher)
fit <- NULL

.build_unconditional_fit <- function() {
  set.seed(42)
  y <- abs(rnorm(60)) + 0.2

  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "gamma",
    GPD = TRUE,
    components = 8,
    mcmc = list(niter = 300, nburnin = 50, nchains = 1)
  )

  # Suppress output during fit
  nullfile <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  res <- NULL
  utils::capture.output(
    res <- run_mcmc_bundle_manual(bundle, show_progress = FALSE),
    file = nullfile
  )
  res
}

.get_fit <- function() {
  if (is.null(fit)) {
    fit <<- .build_unconditional_fit()
  }
  fit
}

test_that("Unconditional model fitted successfully", {
  skip_if_not_test_level("ci")  # Requires MCMC
  
  fit <- .get_fit()
  expect_s3_class(fit, "mixgpd_fit")
  expect_true(!is.null(fit$samples))
})


test_that("Quantile prediction with default quartiles", {
  skip_if_not_test_level("ci")  # Requires MCMC
  
  fit <- .get_fit()
  pred_quant <- predict(fit, type = "quantile", interval = "credible")

  expect_s3_class(pred_quant, "mixgpd_predict")
  expect_type(pred_quant, "list")
  expect_equal(pred_quant$type, "quantile")
  expect_s3_class(pred_quant$fit, "data.frame")
  expect_true(all(c("index", "estimate", "lower", "upper") %in% names(pred_quant$fit)))
  expect_equal(nrow(pred_quant$fit), 3)
  expect_true(all(is.finite(pred_quant$fit$estimate)))
  expect_true(all(pred_quant$fit$lower <= pred_quant$fit$estimate))
  expect_true(all(pred_quant$fit$estimate <= pred_quant$fit$upper))
  expect_silent(p <- plot(pred_quant))
})


test_that("Quantile prediction with custom index", {
  skip_if_not_test_level("ci")  # Requires MCMC
  
  fit <- .get_fit()
  custom_idx <- c(0.1, 0.5, 0.9)
  pred_quant <- predict(fit, type = "quantile", index = custom_idx, interval = "credible")

  expect_equal(nrow(pred_quant$fit), length(custom_idx))
  expect_equal(pred_quant$fit$index, custom_idx)
  expect_silent(p <- plot(pred_quant))
})


test_that("Mean prediction returns data frame format", {
  skip_if_not_test_level("ci")  # Requires MCMC
  
  fit <- .get_fit()
  pred_mean <- predict(fit, type = "mean", interval = "credible", nsim_mean = 200)

  expect_s3_class(pred_mean, "mixgpd_predict")
  expect_equal(pred_mean$type, "mean")
  expect_s3_class(pred_mean$fit, "data.frame")
  expect_true(all(c("estimate", "lower", "upper") %in% names(pred_mean$fit)))
  expect_equal(nrow(pred_mean$fit), 1)
  expect_true(!is.null(pred_mean$draws))
  expect_true(length(pred_mean$draws) > 0)
  expect_type(pred_mean$draws, "double")
  expect_true(pred_mean$fit$lower <= pred_mean$fit$estimate)
  expect_true(pred_mean$fit$estimate <= pred_mean$fit$upper)
  expect_silent(p <- plot(pred_mean))
})


test_that("Mean prediction with custom credible level", {
  skip_if_not_test_level("ci")  # Requires MCMC
  
  fit <- .get_fit()
  pred_mean_90 <- predict(fit, type = "mean", interval = "credible",
                          cred.level = 0.90, nsim_mean = 200)
  pred_mean_99 <- predict(fit, type = "mean", interval = "credible",
                          cred.level = 0.99, nsim_mean = 200)

  width_90 <- pred_mean_90$fit$upper - pred_mean_90$fit$lower
  width_99 <- pred_mean_99$fit$upper - pred_mean_99$fit$lower
  expect_true(width_99 > width_90)
})


test_that("Density prediction returns data frame with grid", {
  skip_if_not_test_level("ci")  # Requires MCMC
  
  fit <- .get_fit()
  y_grid <- seq(0.1, 2, length.out = 10)
  pred_dens <- predict(fit, type = "density", y = y_grid, interval = "credible")

  expect_s3_class(pred_dens, "mixgpd_predict")
  expect_equal(pred_dens$type, "density")
  expect_s3_class(pred_dens$fit, "data.frame")
  expect_equal(nrow(pred_dens$fit), length(y_grid))
  expect_equal(pred_dens$grid, y_grid)

  density_col <- which(names(pred_dens$fit) %in% c("density", "estimate"))[1]
  expect_true(all(pred_dens$fit[[density_col]] >= 0))
  expect_silent(p <- plot(pred_dens))
})


test_that("Survival prediction returns data frame with probabilities", {
  skip_if_not_test_level("ci")  # Requires MCMC
  
  fit <- .get_fit()
  y_grid <- seq(0.1, 2, length.out = 10)
  pred_surv <- predict(fit, type = "survival", y = y_grid, interval = "credible")

  expect_s3_class(pred_surv, "mixgpd_predict")
  expect_equal(pred_surv$type, "survival")
  expect_s3_class(pred_surv$fit, "data.frame")
  expect_true("survival" %in% names(pred_surv$fit))
  expect_equal(nrow(pred_surv$fit), length(y_grid))
  expect_true(all(pred_surv$fit$survival >= 0))
  expect_true(all(pred_surv$fit$survival <= 1))
  expect_true(cor(pred_surv$fit$y, pred_surv$fit$survival) < 0)
  expect_silent(p <- plot(pred_surv))
})


test_that("Sample prediction returns vector of samples", {
  skip_if_not_test_level("ci")  # Requires MCMC
  
  fit <- .get_fit()
  nsim <- 100
  pred_samp <- predict(fit, type = "sample", nsim = nsim)

  expect_s3_class(pred_samp, "mixgpd_predict")
  expect_equal(pred_samp$type, "sample")
  expect_type(pred_samp$fit, "double")
  expect_equal(length(pred_samp$fit), nsim)
  expect_true(all(pred_samp$fit > 0))
  expect_silent(p <- plot(pred_samp))
})


test_that("Fitted values return model-based estimates", {
  skip_if_not_test_level("ci")  # Requires MCMC
  
  fit <- .get_fit()
  set.seed(42)  # recreate y for comparison
  y <- abs(rnorm(60)) + 0.2
  fit_vals <- fitted(fit, level = 0.95)

  expect_s3_class(fit_vals, "mixgpd_fitted")
  expect_s3_class(fit_vals, "data.frame")
  expect_true(all(c("fit", "lower", "upper", "residuals") %in% names(fit_vals)))
  expect_equal(nrow(fit_vals), length(y))
  expect_equal(fit_vals$residuals, y - fit_vals$fit, tolerance = 1e-10)
  expect_true(all(fit_vals$lower <= fit_vals$fit))
  expect_true(all(fit_vals$fit <= fit_vals$upper))
  expect_true(length(unique(fit_vals$fit)) == 1)
  expect_silent(p <- plot(fit_vals))
})


test_that("Fitted values with different credible levels", {
  skip_if_not_test_level("ci")  # Requires MCMC
  
  fit <- .get_fit()
  fit_90 <- fitted(fit, level = 0.90)
  fit_99 <- fitted(fit, level = 0.99)

  width_90 <- mean(fit_90$upper - fit_90$lower)
  width_99 <- mean(fit_99$upper - fit_99$lower)
  expect_true(width_99 > width_90)
})


test_that("All prediction types have consistent class structure", {
  skip_if_not_test_level("ci")  # Requires MCMC
  
  fit <- .get_fit()
  pred_types <- list(
    quantile = predict(fit, type = "quantile"),
    mean = predict(fit, type = "mean", nsim_mean = 100),
    density = predict(fit, type = "density", y = seq(0.1, 2, length.out = 5)),
    survival = predict(fit, type = "survival", y = seq(0.1, 2, length.out = 5)),
    sample = predict(fit, type = "sample", nsim = 50)
  )

  for (pred_name in names(pred_types)) {
    expect_s3_class(pred_types[[pred_name]], "mixgpd_predict")
    expect_equal(pred_types[[pred_name]]$type, pred_name)
    expect_silent(plot(pred_types[[pred_name]]))
  }
})
