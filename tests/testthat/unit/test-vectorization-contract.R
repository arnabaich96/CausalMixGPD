test_that("lowercase r* supports n = 0", {

  w <- c(0.60, 0.40)
  shape <- c(2.0, 5.0)
  scale <- c(1.0, 2.0)

  # Use lowercase vectorized wrapper
  draws <- rgammamix(0, w = w, shape = shape, scale = scale)
  expect_true(is.numeric(draws))
  expect_length(draws, 0)
})
