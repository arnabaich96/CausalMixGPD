testthat::test_that("SB normal: no GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB normal: no GPD, no X", "normal", "sb", FALSE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("SB normal: GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB normal: GPD, no X", "normal", "sb", TRUE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("SB normal: no GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB normal: no GPD, with X", "normal", "sb", FALSE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("SB normal: GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB normal: GPD, with X", "normal", "sb", TRUE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP normal: no GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP normal: no GPD, no X", "normal", "crp", FALSE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP normal: GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP normal: GPD, no X", "normal", "crp", TRUE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP normal: no GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP normal: no GPD, with X", "normal", "crp", FALSE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP normal: GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP normal: GPD, with X", "normal", "crp", TRUE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})
