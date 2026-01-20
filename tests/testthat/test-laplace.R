# test-laplace.R
# Integration tests for laplace kernel workflows (Tier B)

if (!exists(".run_predict_case")) {
  ofile <- sys.frame(1)$ofile
  if (is.null(ofile)) ofile <- file.path('tests', 'testthat', 'dummy')
  helper_path <- file.path(dirname(ofile), 'helper-predict-distribution.R')
  if (file.exists(helper_path)) source(helper_path)
}

test_that("SB laplace: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB laplace: no GPD, no X", "laplace", "sb", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB laplace: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB laplace: GPD, no X", "laplace", "sb", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB laplace: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB laplace: no GPD, with X", "laplace", "sb", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB laplace: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB laplace: GPD, with X", "laplace", "sb", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP laplace: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP laplace: no GPD, no X", "laplace", "crp", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP laplace: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP laplace: GPD, no X", "laplace", "crp", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP laplace: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP laplace: no GPD, with X", "laplace", "crp", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP laplace: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP laplace: GPD, with X", "laplace", "crp", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})
