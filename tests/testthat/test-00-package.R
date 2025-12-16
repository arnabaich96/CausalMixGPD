
testthat::test_that("package loads", {
  testthat::expect_true(requireNamespace("DPmixGPD", quietly = TRUE))
})
