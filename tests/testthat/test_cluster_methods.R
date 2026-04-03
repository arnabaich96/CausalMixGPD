test_that("cluster S3 methods run without error", {
  skip_if_not_test_level("ci")
  skip_if_not(exists("dpmix.cluster", mode = "function"))

  set.seed(789)
  dat <- data.frame(
    y = abs(stats::rnorm(18)) + 0.2,
    x1 = stats::rnorm(18),
    x2 = stats::runif(18)
  )

  fit <- dpmix.cluster(
    y ~ x1 + x2,
    data = dat,
    kernel = "normal",
    components = 4,
    type = "weights",
    mcmc = mcmc_fast(seed = 30L)
  )
  b <- fit$bundle
  lbl <- predict(fit, type = "label", return_scores = TRUE)
  lbl_new <- predict(fit, newdata = dat[1:6, , drop = FALSE], type = "label", return_scores = TRUE)
  psm <- predict(fit, type = "psm")

  expect_output(print(b), "Cluster bundle")
  expect_silent(summary(b))
  expect_s3_class(plot(b, plotly = FALSE), "ggplot")

  expect_output(print(fit), "Cluster fit")
  fit_sum <- summary(fit)
  expect_s3_class(fit_sum, "summary.dpmixgpd_cluster_fit")
  expect_true(is.data.frame(fit_sum$cluster_profiles))
  expect_true(all(c("cluster", "n", "y_mean", "y_sd", "x1_mean", "x1_sd", "x2_mean", "x2_sd", "certainty_mean", "certainty_sd") %in% names(fit_sum$cluster_profiles)))
  expect_true(all(diff(as.integer(fit_sum$cluster_sizes)) <= 0))
  expect_s3_class(plot(fit, which = "psm", plotly = FALSE), "ggplot")
  expect_s3_class(plot(fit, which = "k", plotly = FALSE), "ggplot")
  expect_s3_class(plot(fit, which = "sizes", top_n = 2L, order_by = "label", plotly = FALSE), "ggplot")
  expect_s3_class(plot(fit, which = "summary", top_n = 2L, order_by = "label", plotly = FALSE), "ggplot")

  expect_output(print(lbl), "Cluster labels")
  lbl_sum <- summary(lbl, top_n = 2L)
  expect_s3_class(lbl_sum, "summary.dpmixgpd_cluster_labels")
  expect_true(is.data.frame(lbl_sum$cluster_profiles))
  expect_lte(nrow(lbl_sum$cluster_profiles), 2L)
  expect_true(all(diff(as.integer(lbl_sum$cluster_sizes)) <= 0))
  expect_s3_class(plot(lbl, type = "sizes", top_n = 2L, order_by = "label", plotly = FALSE), "ggplot")
  expect_s3_class(plot(lbl, type = "certainty", plotly = FALSE), "ggplot")
  expect_s3_class(plot(lbl, type = "summary", top_n = 2L, order_by = "label", plotly = FALSE), "ggplot")

  expect_s3_class(lbl_new, "dpmixgpd_cluster_labels")
  expect_true(is.list(lbl_new$train_reference))
  expect_s3_class(plot(lbl_new, type = "summary", top_n = 2L, order_by = "label", plotly = FALSE), "ggplot")

  expect_output(print(psm), "Cluster PSM")
  expect_silent(summary(psm))
  expect_s3_class(plot(psm, psm_max_n = nrow(psm$psm), plotly = FALSE), "ggplot")

  if (requireNamespace("plotly", quietly = TRUE)) {
    expect_true(inherits(plot(lbl, type = "sizes", plotly = TRUE), "plotly"))
  }
})

test_that("cluster fit summary rounds cluster profiles to 3 decimals", {
  skip_if_not(exists("dpmix.cluster", mode = "function"))

  set.seed(321)
  dat <- data.frame(
    y = abs(stats::rnorm(18)) + 0.2,
    x1 = stats::rnorm(18),
    x2 = stats::runif(18)
  )

  fit <- dpmix.cluster(
    y ~ x1 + x2,
    data = dat,
    kernel = "normal",
    components = 4,
    type = "weights",
    mcmc = mcmc_fast(seed = 31L)
  )

  fit_sum <- summary(fit, top_n = 2L)
  prof <- fit_sum$cluster_profiles
  num_cols <- vapply(prof, is.numeric, logical(1))

  expect_true(all(prof[num_cols] == lapply(prof[num_cols], round, digits = 3L)))
})
