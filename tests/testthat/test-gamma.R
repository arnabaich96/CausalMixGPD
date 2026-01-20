# test-gamma.R
# Integration tests for gamma kernel workflows (Tier B)

if (!exists(".run_predict_case")) {
  ofile <- sys.frame(1)$ofile
  if (is.null(ofile)) ofile <- file.path('tests', 'testthat', 'dummy')
  helper_path <- file.path(dirname(ofile), 'helper-predict-distribution.R')
  if (file.exists(helper_path)) source(helper_path)
}

test_that("SB gamma: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB gamma: no GPD, no X", "gamma", "sb", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB gamma: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB gamma: GPD, no X", "gamma", "sb", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB gamma: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB gamma: no GPD, with X", "gamma", "sb", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB gamma: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB gamma: GPD, with X", "gamma", "sb", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP gamma: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP gamma: no GPD, no X", "gamma", "crp", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP gamma: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP gamma: GPD, no X", "gamma", "crp", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP gamma: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP gamma: no GPD, with X", "gamma", "crp", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP gamma: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP gamma: GPD, with X", "gamma", "crp", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})
