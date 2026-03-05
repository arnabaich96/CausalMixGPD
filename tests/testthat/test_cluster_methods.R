test_that("cluster S3 methods run without error", {
  skip_if_not_test_level("ci")
  skip_if_not(exists("dpmix.cluster", mode = "function"))

  set.seed(789)
  dat <- data.frame(
    y = abs(stats::rnorm(18)) + 0.2,
    x1 = stats::rnorm(18),
    x2 = stats::runif(18)
  )

  fit <- dpmix.cluster(
    y ~ x1 + x2,
    data = dat,
    kernel = "normal",
    components = 4,
    type = "weights",
    mcmc = mcmc_fast(seed = 30L)
  )
  b <- fit$bundle
  lbl <- predict(fit, type = "label", return_scores = TRUE)
  psm <- predict(fit, type = "psm")

  expect_output(print(b), "Cluster bundle")
  expect_silent(summary(b))
  expect_silent(plot(b))

  expect_output(print(fit), "Cluster fit")
  expect_silent(summary(fit))
  expect_silent(plot(fit, which = "psm"))
  expect_silent(plot(fit, which = "k"))
  expect_silent(plot(fit, which = "sizes"))

  expect_output(print(lbl), "Cluster labels")
  expect_silent(summary(lbl))
  expect_silent(plot(lbl, type = "sizes"))
  expect_silent(plot(lbl, type = "certainty"))

  expect_output(print(psm), "Cluster PSM")
  expect_silent(summary(psm))
  expect_silent(plot(psm, psm_max_n = nrow(psm$psm)))
})
