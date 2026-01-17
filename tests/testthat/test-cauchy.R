if (!exists(".run_predict_case")) {
  ofile <- sys.frame(1)$ofile
  if (is.null(ofile)) ofile <- file.path('tests', 'testthat', 'dummy')
  helper_path <- file.path(dirname(ofile), 'helper-predict-distribution.R')
  if (file.exists(helper_path)) source(helper_path)
}
test_that("SB cauchy: no GPD, no X", {
  res <- .run_predict_case("SB cauchy: no GPD, no X", "cauchy", "sb", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB cauchy: GPD, no X", {
  res <- .run_predict_case("SB cauchy: GPD, no X", "cauchy", "sb", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB cauchy: no GPD, with X", {
  res <- .run_predict_case("SB cauchy: no GPD, with X", "cauchy", "sb", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB cauchy: GPD, with X", {
  res <- .run_predict_case("SB cauchy: GPD, with X", "cauchy", "sb", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP cauchy: no GPD, no X", {
  res <- .run_predict_case("CRP cauchy: no GPD, no X", "cauchy", "crp", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP cauchy: GPD, no X", {
  res <- .run_predict_case("CRP cauchy: GPD, no X", "cauchy", "crp", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP cauchy: no GPD, with X", {
  res <- .run_predict_case("CRP cauchy: no GPD, with X", "cauchy", "crp", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP cauchy: GPD, with X", {
  res <- .run_predict_case("CRP cauchy: GPD, with X", "cauchy", "crp", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})





