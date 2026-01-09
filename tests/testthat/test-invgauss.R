testthat::test_that("SB invgauss: no GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB invgauss: no GPD, no X", "invgauss", "sb", FALSE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("SB invgauss: GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB invgauss: GPD, no X", "invgauss", "sb", TRUE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("SB invgauss: no GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB invgauss: no GPD, with X", "invgauss", "sb", FALSE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("SB invgauss: GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB invgauss: GPD, with X", "invgauss", "sb", TRUE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP invgauss: no GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP invgauss: no GPD, no X", "invgauss", "crp", FALSE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP invgauss: GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP invgauss: GPD, no X", "invgauss", "crp", TRUE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP invgauss: no GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP invgauss: no GPD, with X", "invgauss", "crp", FALSE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP invgauss: GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP invgauss: GPD, with X", "invgauss", "crp", TRUE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})
