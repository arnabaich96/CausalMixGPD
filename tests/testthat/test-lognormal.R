if (!exists(".run_predict_case")) {
  ofile <- sys.frame(1)$ofile
  if (is.null(ofile)) ofile <- file.path('tests', 'testthat', 'dummy')
  helper_path <- file.path(dirname(ofile), 'helper-predict-distribution.R')
  if (file.exists(helper_path)) source(helper_path)
}
test_that("SB lognormal: no GPD, no X", {
  res <- .run_predict_case("SB lognormal: no GPD, no X", "lognormal", "sb", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB lognormal: GPD, no X", {
  res <- .run_predict_case("SB lognormal: GPD, no X", "lognormal", "sb", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB lognormal: no GPD, with X", {
  res <- .run_predict_case("SB lognormal: no GPD, with X", "lognormal", "sb", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB lognormal: GPD, with X", {
  res <- .run_predict_case("SB lognormal: GPD, with X", "lognormal", "sb", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP lognormal: no GPD, no X", {
  res <- .run_predict_case("CRP lognormal: no GPD, no X", "lognormal", "crp", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP lognormal: GPD, no X", {
  res <- .run_predict_case("CRP lognormal: GPD, no X", "lognormal", "crp", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP lognormal: no GPD, with X", {
  res <- .run_predict_case("CRP lognormal: no GPD, with X", "lognormal", "crp", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP lognormal: GPD, with X", {
  res <- .run_predict_case("CRP lognormal: GPD, with X", "lognormal", "crp", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

