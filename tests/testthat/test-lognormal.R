testthat::test_that("SB lognormal: no GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB lognormal: no GPD, no X", "lognormal", "sb", FALSE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("SB lognormal: GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB lognormal: GPD, no X", "lognormal", "sb", TRUE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("SB lognormal: no GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB lognormal: no GPD, with X", "lognormal", "sb", FALSE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("SB lognormal: GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB lognormal: GPD, with X", "lognormal", "sb", TRUE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP lognormal: no GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP lognormal: no GPD, no X", "lognormal", "crp", FALSE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP lognormal: GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP lognormal: GPD, no X", "lognormal", "crp", TRUE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP lognormal: no GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP lognormal: no GPD, with X", "lognormal", "crp", FALSE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP lognormal: GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP lognormal: GPD, with X", "lognormal", "crp", TRUE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})
