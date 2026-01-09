testthat::test_that("SB cauchy: no GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB cauchy: no GPD, no X", "cauchy", "sb", FALSE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("SB cauchy: GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB cauchy: GPD, no X", "cauchy", "sb", TRUE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("SB cauchy: no GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB cauchy: no GPD, with X", "cauchy", "sb", FALSE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("SB cauchy: GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("SB cauchy: GPD, with X", "cauchy", "sb", TRUE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP cauchy: no GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP cauchy: no GPD, no X", "cauchy", "crp", FALSE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP cauchy: GPD, no X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP cauchy: GPD, no X", "cauchy", "crp", TRUE, FALSE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP cauchy: no GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP cauchy: no GPD, with X", "cauchy", "crp", FALSE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})

testthat::test_that("CRP cauchy: GPD, with X", {
  testthat::skip_if_not_installed("nimble")
  testthat::skip_on_cran()
  res <- .run_predict_case("CRP cauchy: GPD, with X", "cauchy", "crp", TRUE, TRUE)
  testthat::expect_true(res$ok, info = res$msg)
})
