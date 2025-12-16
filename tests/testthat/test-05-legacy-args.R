
testthat::test_that("old tail_priors argument is not silently accepted (or is handled)", {
  set.seed(1)
  dat <- data.frame(y = stats::rgamma(50, 2, 1))

  ok <- FALSE
  msg <- NULL

  res <- tryCatch({
    DPmixGPD::fit.dpm(y ~ 0, data = dat, kernel="gamma", tail="none",
                      dp_ctrl=list(K=3),
                      mcmc=list(n_iter=200, burn_in=100, chains=1),
                      tail_priors=list())
  }, error = function(e) { msg <<- conditionMessage(e); NULL })

  if (!is.null(res)) ok <- TRUE
  if (!ok) {
    testthat::expect_true(
      grepl("tail_priors|unused argument|deprecated", msg, ignore.case = TRUE),
      info = paste("Unexpected error message:", msg)
    )
  } else {
    testthat::expect_true(inherits(res, "mixgpd_fit"))
  }
})
