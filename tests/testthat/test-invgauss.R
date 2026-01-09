if (!exists(".run_predict_case")) {
  ofile <- sys.frame(1)$ofile
  if (is.null(ofile)) ofile <- file.path('tests', 'testthat', 'dummy')
  helper_path <- file.path(dirname(ofile), 'helper-predict-distribution.R')
  if (file.exists(helper_path)) source(helper_path)
}
test_that("SB invgauss: no GPD, no X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("SB invgauss: no GPD, no X", "invgauss", "sb", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB invgauss: GPD, no X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("SB invgauss: GPD, no X", "invgauss", "sb", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB invgauss: no GPD, with X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("SB invgauss: no GPD, with X", "invgauss", "sb", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB invgauss: GPD, with X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("SB invgauss: GPD, with X", "invgauss", "sb", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP invgauss: no GPD, no X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("CRP invgauss: no GPD, no X", "invgauss", "crp", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP invgauss: GPD, no X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("CRP invgauss: GPD, no X", "invgauss", "crp", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP invgauss: no GPD, with X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("CRP invgauss: no GPD, with X", "invgauss", "crp", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP invgauss: GPD, with X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("CRP invgauss: GPD, with X", "invgauss", "crp", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

