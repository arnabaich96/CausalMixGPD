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

test_that("causal fit print omits unavailable timing instead of printing NA timings", {
  fit <- structure(
    list(
      bundle = list(
        meta = list(
          ps = list(enabled = FALSE),
          backend = list(trt = "sb", con = "sb"),
          kernel = list(trt = "normal", con = "normal"),
          GPD = list(trt = FALSE, con = FALSE)
        )
      ),
      timing = list(total = NA_real_, ps = NA_real_, con = NA_real_, trt = NA_real_)
    ),
    class = "causalmixgpd_causal_fit"
  )
  out <- capture.output(print(fit))
  expect_false(any(grepl("Timing \\(sec\\)", out)))
})

test_that("cluster profile accessor reads summary objects", {
  s <- structure(
    list(cluster_profiles = data.frame(cluster = 1L, y_mean = 2)),
    class = c("summary.dpmixgpd_cluster_labels", "list")
  )
  expect_equal(cluster_profiles(s)$y_mean, 2)
})

test_that("reviewer-facing classes include common S3 base classes", {
  one_arm <- structure(list(), class = c("mixgpd_fit", "causalmixgpd_fit", "list"))
  causal <- structure(list(), class = c("causalmixgpd_causal_fit", "causalmixgpd_fit", "list"))
  cluster <- structure(list(), class = c("dpmixgpd_cluster_fit", "causalmixgpd_fit", "list"))
  effect <- structure(list(), class = c("causalmixgpd_ate", "causalmixgpd_effect", "list"))

  expect_s3_class(one_arm, "causalmixgpd_fit")
  expect_s3_class(causal, "causalmixgpd_fit")
  expect_s3_class(cluster, "causalmixgpd_fit")
  expect_s3_class(effect, "causalmixgpd_effect")

  expect_identical(class(one_arm)[1], "mixgpd_fit")
  expect_identical(class(causal)[1], "causalmixgpd_causal_fit")
  expect_identical(class(cluster)[1], "dpmixgpd_cluster_fit")
  expect_identical(class(effect)[1], "causalmixgpd_ate")
})

test_that("reviewer-facing replication and examples use public entrypoints", {
  root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
  read_if_exists <- function(...) {
    path <- file.path(root, ...)
    if (!file.exists(path)) return(character())
    readLines(path, warn = FALSE)
  }

  article <- read_if_exists("manuscript", "CausalMixGPD_JSS_article.Rnw")
  if (length(article)) {
    expect_false(any(grepl("summary\\(z_test\\)\\$cluster_profiles", article)))
    expect_true(any(grepl("cluster_profiles\\(z_test\\)", article, fixed = FALSE)))
  }

  rd <- read_if_exists("man", "dpmix.causal.Rd")
  if (length(rd)) {
    expect_true(any(grepl("\\\\examples\\{", rd)))
  }

  man_dir <- file.path(root, "man")
  if (dir.exists(man_dir)) {
    titles <- unlist(lapply(list.files(man_dir, "\\.Rd$", full.names = TRUE), function(path) {
      raw <- paste(readLines(path, warn = FALSE), collapse = "\n")
      match <- regexec("\\\\title\\{([^}]*)\\}", raw, perl = TRUE)
      regmatches(raw, match)[[1]][2]
    }), use.names = FALSE)
    expect_false(any(grepl("^[a-z]", titles)))
  }
})
