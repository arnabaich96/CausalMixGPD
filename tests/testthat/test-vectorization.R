test_that("d/p/q functions vectorize over first argument", {

  w <- c(0.60, 0.40)
  shape <- c(2.0, 5.0)
  scale <- c(1.0, 2.0)
  x <- seq(0.5, 2.0, length.out = 5)

  d_vec <- dGammaMix(x, w = w, shape = shape, scale = scale, log = 0)
  d_scalar <- vapply(x, function(xx) {
    dGammaMix(xx, w = w, shape = shape, scale = scale, log = 0)
  }, numeric(1))
  expect_equal(d_vec, d_scalar, tolerance = 1e-10)

  p_vec <- pGammaMix(q = x, w = w, shape = shape, scale = scale,
                     lower.tail = 1, log.p = 0)
  p_scalar <- vapply(x, function(xx) {
    pGammaMix(q = xx, w = w, shape = shape, scale = scale,
              lower.tail = 1, log.p = 0)
  }, numeric(1))
  expect_equal(p_vec, p_scalar, tolerance = 1e-10)

  p_vec_x <- pGammaMix(x = x, w = w, shape = shape, scale = scale,
                       lower.tail = 1, log.p = 0)
  expect_equal(p_vec_x, p_scalar, tolerance = 1e-10)

  probs <- c(0.1, 0.5, 0.9)
  q_vec <- qGammaMix(probs, w = w, shape = shape, scale = scale)
  q_scalar <- vapply(probs, function(pp) {
    qGammaMix(pp, w = w, shape = shape, scale = scale)
  }, numeric(1))
  expect_equal(q_vec, q_scalar, tolerance = 1e-10)
})

test_that("r functions support n > 1", {

  w <- c(0.60, 0.40)
  shape <- c(2.0, 5.0)
  scale <- c(1.0, 2.0)

  set.seed(123)
  draws <- rGammaMix(10, w = w, shape = shape, scale = scale)
  expect_length(draws, 10)
  expect_true(all(is.finite(draws)))
})
