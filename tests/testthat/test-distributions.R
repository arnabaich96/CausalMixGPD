# test-distributions.R
# Consolidated per-kernel distribution integration tests (Tier B)
# Merged from: test-amoroso.R, test-cauchy.R, test-gamma.R, test-invgauss.R,
#             test-laplace.R, test-lognormal.R, test-normal.R

if (!exists(".run_predict_case")) {
  helper_path <- file.path("tests", "testthat", "helper-predict-distribution.R")
  if (file.exists(helper_path)) source(helper_path)
}

# =============================================================================
# Amoroso kernel
# =============================================================================
test_that("SB amoroso: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB amoroso: no GPD, no X", "amoroso", "sb", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB amoroso: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB amoroso: GPD, no X", "amoroso", "sb", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB amoroso: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB amoroso: no GPD, with X", "amoroso", "sb", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB amoroso: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB amoroso: GPD, with X", "amoroso", "sb", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP amoroso: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP amoroso: no GPD, no X", "amoroso", "crp", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP amoroso: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP amoroso: GPD, no X", "amoroso", "crp", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP amoroso: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP amoroso: no GPD, with X", "amoroso", "crp", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP amoroso: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP amoroso: GPD, with X", "amoroso", "crp", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

# =============================================================================
# Cauchy kernel
# =============================================================================
test_that("SB cauchy: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB cauchy: no GPD, no X", "cauchy", "sb", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB cauchy: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB cauchy: GPD, no X", "cauchy", "sb", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB cauchy: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB cauchy: no GPD, with X", "cauchy", "sb", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB cauchy: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB cauchy: GPD, with X", "cauchy", "sb", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP cauchy: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP cauchy: no GPD, no X", "cauchy", "crp", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP cauchy: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP cauchy: GPD, no X", "cauchy", "crp", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP cauchy: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP cauchy: no GPD, with X", "cauchy", "crp", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP cauchy: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP cauchy: GPD, with X", "cauchy", "crp", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

# =============================================================================
# Gamma kernel
# =============================================================================
test_that("SB gamma: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB gamma: no GPD, no X", "gamma", "sb", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB gamma: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB gamma: GPD, no X", "gamma", "sb", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB gamma: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB gamma: no GPD, with X", "gamma", "sb", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB gamma: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB gamma: GPD, with X", "gamma", "sb", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP gamma: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP gamma: no GPD, no X", "gamma", "crp", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP gamma: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP gamma: GPD, no X", "gamma", "crp", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP gamma: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP gamma: no GPD, with X", "gamma", "crp", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP gamma: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP gamma: GPD, with X", "gamma", "crp", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

# =============================================================================
# InvGauss kernel
# =============================================================================
test_that("SB invgauss: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB invgauss: no GPD, no X", "invgauss", "sb", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB invgauss: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB invgauss: GPD, no X", "invgauss", "sb", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB invgauss: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB invgauss: no GPD, with X", "invgauss", "sb", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB invgauss: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB invgauss: GPD, with X", "invgauss", "sb", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP invgauss: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP invgauss: no GPD, no X", "invgauss", "crp", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP invgauss: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP invgauss: GPD, no X", "invgauss", "crp", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP invgauss: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP invgauss: no GPD, with X", "invgauss", "crp", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP invgauss: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP invgauss: GPD, with X", "invgauss", "crp", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

# =============================================================================
# Laplace kernel
# =============================================================================
test_that("SB laplace: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB laplace: no GPD, no X", "laplace", "sb", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB laplace: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB laplace: GPD, no X", "laplace", "sb", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB laplace: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB laplace: no GPD, with X", "laplace", "sb", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB laplace: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB laplace: GPD, with X", "laplace", "sb", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP laplace: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP laplace: no GPD, no X", "laplace", "crp", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP laplace: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP laplace: GPD, no X", "laplace", "crp", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP laplace: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP laplace: no GPD, with X", "laplace", "crp", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP laplace: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP laplace: GPD, with X", "laplace", "crp", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

# =============================================================================
# Lognormal kernel
# =============================================================================
test_that("SB lognormal: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB lognormal: no GPD, no X", "lognormal", "sb", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB lognormal: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB lognormal: GPD, no X", "lognormal", "sb", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB lognormal: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB lognormal: no GPD, with X", "lognormal", "sb", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB lognormal: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB lognormal: GPD, with X", "lognormal", "sb", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP lognormal: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP lognormal: no GPD, no X", "lognormal", "crp", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP lognormal: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP lognormal: GPD, no X", "lognormal", "crp", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP lognormal: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP lognormal: no GPD, with X", "lognormal", "crp", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP lognormal: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP lognormal: GPD, with X", "lognormal", "crp", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

# =============================================================================
# Normal kernel
# =============================================================================
test_that("SB normal: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB normal: no GPD, no X", "normal", "sb", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB normal: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB normal: GPD, no X", "normal", "sb", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB normal: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB normal: no GPD, with X", "normal", "sb", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("SB normal: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("SB normal: GPD, with X", "normal", "sb", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP normal: no GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP normal: no GPD, no X", "normal", "crp", FALSE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP normal: GPD, no X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP normal: GPD, no X", "normal", "crp", TRUE, FALSE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP normal: no GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP normal: no GPD, with X", "normal", "crp", FALSE, TRUE)
  expect_true(res$ok, info = res$msg)
})

test_that("CRP normal: GPD, with X", {
  skip_if_not_test_level("ci")
  res <- .run_predict_case("CRP normal: GPD, with X", "normal", "crp", TRUE, TRUE)
  expect_true(res$ok, info = res$msg)
})
