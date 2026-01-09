testthat::test_that("SB gamma: no GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB gamma: no GPD, no X", "gamma", "sb", FALSE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("SB gamma: GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB gamma: GPD, no X", "gamma", "sb", TRUE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("SB gamma: no GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB gamma: no GPD, with X", "gamma", "sb", FALSE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("SB gamma: GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB gamma: GPD, with X", "gamma", "sb", TRUE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP gamma: no GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP gamma: no GPD, no X", "gamma", "crp", FALSE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP gamma: GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP gamma: GPD, no X", "gamma", "crp", TRUE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP gamma: no GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP gamma: no GPD, with X", "gamma", "crp", FALSE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP gamma: GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP gamma: GPD, with X", "gamma", "crp", TRUE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})
