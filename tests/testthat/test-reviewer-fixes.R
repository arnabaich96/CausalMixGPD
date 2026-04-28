test_that("reviewer-facing metadata and namespace are clean", {
  desc_path <- system.file("DESCRIPTION", package = "CausalMixGPD")
  if (!nzchar(desc_path)) {
    desc_path <- test_path("..", "..", "DESCRIPTION")
  }
  desc <- read.dcf(desc_path)[1, ]
  expect_false(grepl("zenodo.19620523", desc[["Description"]], fixed = TRUE))
  expect_false(grepl("nimble", desc[["Depends"]], fixed = TRUE))
  expect_true(grepl("nimble", desc[["Imports"]], fixed = TRUE))

  ns_path <- system.file("NAMESPACE", package = "CausalMixGPD")
  if (!nzchar(ns_path)) {
    ns_path <- test_path("..", "..", "NAMESPACE")
  }
  ns <- readLines(ns_path, warn = FALSE)
  expect_false(any(grepl("^export\\(build_(code|constants|dimensions|inits|monitors)_from_spec\\)", ns)))
  expect_true(any(ns == "export(cluster_profiles)"))
  expect_true(any(ns == "S3method(print,mixgpd_predict)"))
})

test_that("causal estimand validation happens before progress output", {
  bad_fit <- structure(list(), class = "not_a_causal_fit")
  expect_error(ate(bad_fit, show_progress = TRUE), "causalmixgpd_causal_fit")
  expect_error(qte(bad_fit, show_progress = TRUE), "causalmixgpd_causal_fit")
})

test_that("prediction print and rmean plotting methods are registered", {
  pr <- list(
    type = "rmean",
    fit = data.frame(id = 1, estimate = 1, lower = 0.5, upper = 1.5),
    draws = c(0.8, 1.0, 1.2)
  )
  class(pr) <- "mixgpd_predict"
  expect_output(print(pr), "MixGPD prediction")
  skip_if_not_installed("ggplot2")
  expect_s3_class(plot(pr), "mixgpd_predict_plots")
})

test_that("cluster profile accessor reads summary objects", {
  s <- structure(
    list(cluster_profiles = data.frame(cluster = 1L, y_mean = 2)),
    class = c("summary.dpmixgpd_cluster_labels", "list")
  )
  expect_equal(cluster_profiles(s)$y_mean, 2)
})
