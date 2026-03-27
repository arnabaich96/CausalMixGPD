test_that(".strip_covr_counts removes standalone covr counters", {
  txt <- paste(
    "{",
    "  covr:::count(\"build-run.R:1207:5:1207:22:5:22:3261:3261\")",
    "  alpha <- 1",
    "  covr:::count(\"build-run.R:1208:5:1208:22:5:22:3262:3262\")",
    "  y[i] ~ dnorm(mean = 0, sd = 1)",
    "}",
    sep = "\n"
  )

  stripped <- CausalMixGPD:::.strip_covr_counts(txt)

  expect_false(grepl("covr:::count", stripped, fixed = TRUE))
  expect_match(stripped, "alpha <- 1", fixed = TRUE)
  expect_match(stripped, "y[i] ~ dnorm(mean = 0, sd = 1)", fixed = TRUE)
})
