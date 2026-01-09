if (!exists(".run_predict_case")) {
  ofile <- sys.frame(1)$ofile
  if (is.null(ofile)) ofile <- file.path('tests', 'testthat', 'dummy')
  helper_path <- file.path(dirname(ofile), 'helper-predict-distribution.R')
  if (file.exists(helper_path)) source(helper_path)
}
test_that("SB amoroso: no GPD, no X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("SB amoroso: no GPD, no X", "amoroso", "sb", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB amoroso: GPD, no X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("SB amoroso: GPD, no X", "amoroso", "sb", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB amoroso: no GPD, with X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("SB amoroso: no GPD, with X", "amoroso", "sb", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB amoroso: GPD, with X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("SB amoroso: GPD, with X", "amoroso", "sb", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP amoroso: no GPD, no X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("CRP amoroso: no GPD, no X", "amoroso", "crp", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP amoroso: GPD, no X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("CRP amoroso: GPD, no X", "amoroso", "crp", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP amoroso: no GPD, with X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("CRP amoroso: no GPD, with X", "amoroso", "crp", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP amoroso: GPD, with X", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  res <- .run_predict_case("CRP amoroso: GPD, with X", "amoroso", "crp", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

