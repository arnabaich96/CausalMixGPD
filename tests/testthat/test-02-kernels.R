testthat::test_that("input validation rejects missing/invalid data", {
  set.seed(1)
  dat <- data.frame(y = stats::rgamma(20, shape = 2, scale = 1), x = stats::rnorm(20))

  # missing response in formula
  testthat::expect_error(
    DPmixGPD::fit.dpm(zzz ~ 0, data = dat),
    regexp = "zzz",
    ignore.case = TRUE
  )

  # data must be a data.frame
  testthat::expect_error(
    DPmixGPD::fit.dpm(y ~ 0, data = 1:10),
    regexp = "data\\.frame",
    ignore.case = TRUE
  )

  # invalid kernel
  expect_error(
    DPmixGPD::fit.dpm(y ~ 0, data = dat, kernel = "nope"),
    regexp = "should be one of"
  )

})


