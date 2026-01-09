if (!exists(".run_predict_case")) {
  ofile <- sys.frame(1)$ofile
  if (is.null(ofile)) ofile <- file.path('tests', 'testthat', 'dummy')
  helper_path <- file.path(dirname(ofile), 'helper-predict-distribution.R')
  if (file.exists(helper_path)) source(helper_path)
}
test_that("SB normal: no GPD, no X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("SB normal: no GPD, no X", "normal", "sb", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB normal: GPD, no X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("SB normal: GPD, no X", "normal", "sb", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB normal: no GPD, with X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("SB normal: no GPD, with X", "normal", "sb", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB normal: GPD, with X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("SB normal: GPD, with X", "normal", "sb", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP normal: no GPD, no X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("CRP normal: no GPD, no X", "normal", "crp", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP normal: GPD, no X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("CRP normal: GPD, no X", "normal", "crp", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP normal: no GPD, with X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("CRP normal: no GPD, with X", "normal", "crp", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP normal: GPD, with X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("CRP normal: GPD, with X", "normal", "crp", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

