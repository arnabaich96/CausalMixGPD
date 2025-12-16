
testthat::test_that("ATE with GPD: xi >= 1 triggers warning and drops invalid draws", {
  K <- 3
  M <- 50

  make_draws <- function(xi_vals) {
    draws <- cbind(
      alpha = rep(0.05, M),
      matrix(2, nrow=M, ncol=K, dimnames=list(NULL, paste0("shape[",1:K,"]"))),
      matrix(1, nrow=M, ncol=K, dimnames=list(NULL, paste0("scale[",1:K,"]"))),
      matrix(1/K, nrow=M, ncol=K, dimnames=list(NULL, paste0("w[",1:K,"]"))),
      u = rep(1.0, M),
      sigma = rep(1.0, M),
      xi = xi_vals
    )
    colnames(draws)[1] <- "alpha"
    draws
  }

  xi_trt <- c(rep(0.2, M-5), rep(1.2, 5))
  xi_con <- rep(0.2, M)

  te_obj <- .make_fake_te_fit(make_draws(xi_trt), make_draws(xi_con), tail = c(TRUE, TRUE))

  testthat::expect_warning({
    out <- DPmixGPD::ate(te_obj, level = 0.95)
    testthat::expect_true(is.matrix(out))
    testthat::expect_true(all(c("mean","sd","lower","upper") %in% colnames(out)))
  }, regexp = "xi\\s*>=\\s*1|infinite|Dropping", ignore.case = TRUE)
})

testthat::test_that("ATE with GPD: all xi >= 1 returns NA with warning", {
  K <- 3
  M <- 30

  make_draws <- function(xi_vals) {
    draws <- cbind(
      alpha = rep(0.05, M),
      matrix(2, nrow=M, ncol=K, dimnames=list(NULL, paste0("shape[",1:K,"]"))),
      matrix(1, nrow=M, ncol=K, dimnames=list(NULL, paste0("scale[",1:K,"]"))),
      matrix(1/K, nrow=M, ncol=K, dimnames=list(NULL, paste0("w[",1:K,"]"))),
      u = rep(1.0, M),
      sigma = rep(1.0, M),
      xi = xi_vals
    )
    colnames(draws)[1] <- "alpha"
    draws
  }

  te_obj <- .make_fake_te_fit(make_draws(rep(1.5, M)), make_draws(rep(1.2, M)), tail = c(TRUE, TRUE))

  testthat::expect_warning({
    out <- DPmixGPD::ate(te_obj, level = 0.95)
    testthat::expect_true(is.matrix(out))
    testthat::expect_true(all(is.na(out)))
  }, regexp = "all posterior draws|returning NA|xi", ignore.case = TRUE)
})
