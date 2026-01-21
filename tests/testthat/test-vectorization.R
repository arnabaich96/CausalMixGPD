test_that("lowercase d/p/q functions vectorize over first argument", {

  w <- c(0.60, 0.40)

  shape <- c(2.0, 5.0)
  scale <- c(1.0, 2.0)
  x <- seq(0.5, 2.0, length.out = 5)

  # Use lowercase vectorized wrappers
  d_vec <- dgammamix(x, w = w, shape = shape, scale = scale, log = FALSE)
  d_scalar <- vapply(x, function(xx) {
    as.numeric(dGammaMix(xx, w = w, shape = shape, scale = scale, log = 0L))
  }, numeric(1))
  expect_equal(d_vec, d_scalar, tolerance = 1e-10)

  p_vec <- pgammamix(q = x, w = w, shape = shape, scale = scale,
                     lower.tail = TRUE, log.p = FALSE)
  p_scalar <- vapply(x, function(xx) {
    as.numeric(pGammaMix(xx, w = w, shape = shape, scale = scale,
              lower.tail = 1L, log.p = 0L))
  }, numeric(1))
  expect_equal(p_vec, p_scalar, tolerance = 1e-10)

  probs <- c(0.1, 0.5, 0.9)
  q_vec <- qgammamix(probs, w = w, shape = shape, scale = scale)
  q_scalar <- vapply(probs, function(pp) {
    qGammaMix(pp, w = w, shape = shape, scale = scale)
  }, numeric(1))
  expect_equal(q_vec, q_scalar, tolerance = 1e-10)
})

test_that("lowercase r functions support n > 1", {

  w <- c(0.60, 0.40)
  shape <- c(2.0, 5.0)
  scale <- c(1.0, 2.0)

  set.seed(123)
  draws <- rgammamix(10, w = w, shape = shape, scale = scale)
  expect_length(draws, 10)
  expect_true(all(is.finite(draws)))
})

test_that("lowercase base wrappers vectorize over first argument", {

  x <- c(1.2, 1.5, 2.0)
  probs <- c(0.2, 0.5, 0.9)

  threshold <- 1.0
  scale <- 0.8
  shape <- 0.2

  expect_equal(
    dgpd(x, threshold = threshold, scale = scale, shape = shape, log = FALSE),
    vapply(x, function(xx) as.numeric(dGpd(xx, threshold, scale, shape, 0L)), numeric(1))
  )
  expect_equal(
    pgpd(x, threshold = threshold, scale = scale, shape = shape,
         lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pGpd(xx, threshold, scale, shape, 1L, 0L)), numeric(1))
  )
  expect_equal(
    qgpd(probs, threshold = threshold, scale = scale, shape = shape,
         lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qGpd(pp, threshold, scale, shape, lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )

  mean <- 2.0
  shape_ig <- 3.0

  expect_equal(
    dinvgauss(x, mean = mean, shape = shape_ig, log = FALSE),
    vapply(x, function(xx) as.numeric(dInvGauss(xx, mean, shape_ig, 0L)), numeric(1))
  )
  expect_equal(
    pinvgauss(x, mean = mean, shape = shape_ig, lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pInvGauss(xx, mean, shape_ig, 1L, 0L)), numeric(1))
  )
  expect_equal(
    qinvgauss(probs, mean = mean, shape = shape_ig, lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qInvGauss(pp, mean, shape_ig, lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )

  loc <- 0.0
  scale_am <- 1.2
  shape1 <- 2.0
  shape2 <- 1.5

  expect_equal(
    damoroso(x, loc = loc, scale = scale_am, shape1 = shape1, shape2 = shape2, log = FALSE),
    vapply(x, function(xx) as.numeric(dAmoroso(xx, loc, scale_am, shape1, shape2, 0L)), numeric(1))
  )
  expect_equal(
    pamoroso(x, loc = loc, scale = scale_am, shape1 = shape1, shape2 = shape2,
             lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pAmoroso(xx, loc, scale_am, shape1, shape2, 1L, 0L)),
           numeric(1))
  )
  expect_equal(
    qamoroso(probs, loc = loc, scale = scale_am, shape1 = shape1, shape2 = shape2,
             lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qAmoroso(pp, loc, scale_am, shape1, shape2,
                                        lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )

  location <- 0.0
  scale_c <- 1.1

  expect_equal(
    dcauchy_vec(x, location = location, scale = scale_c, log = FALSE),
    vapply(x, function(xx) as.numeric(dCauchy(xx, location, scale_c, 0L)), numeric(1))
  )
  expect_equal(
    pcauchy_vec(x, location = location, scale = scale_c, lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pCauchy(xx, location, scale_c, 1L, 0L)), numeric(1))
  )
  expect_equal(
    qcauchy_vec(probs, location = location, scale = scale_c, lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qCauchy(pp, location, scale_c, lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )
})

test_that("lowercase base r wrappers support n > 1", {

  set.seed(456)
  draws_gpd <- rgpd(5, threshold = 1.0, scale = 0.8, shape = 0.2)
  expect_length(draws_gpd, 5)
  expect_true(all(is.finite(draws_gpd)))

  draws_ig <- rinvgauss(5, mean = 2.0, shape = 3.0)
  expect_length(draws_ig, 5)
  expect_true(all(is.finite(draws_ig)))

  draws_am <- ramoroso(5, loc = 0.0, scale = 1.2, shape1 = 2.0, shape2 = 1.5)
  expect_length(draws_am, 5)
  expect_true(all(is.finite(draws_am)))

  draws_c <- rcauchy_vec(5, location = 0.0, scale = 1.1)
  expect_length(draws_c, 5)
  expect_true(all(is.finite(draws_c)))
})

test_that("lowercase mix wrappers vectorize over first argument", {

  x <- c(-1.0, 0.5, 1.2)
  probs <- c(0.2, 0.5, 0.9)

  w <- c(0.7, 0.3)
  location <- c(-0.5, 0.8)
  scale <- c(1.0, 1.5)

  expect_equal(
    dcauchymix(x, w = w, location = location, scale = scale, log = FALSE),
    vapply(x, function(xx) as.numeric(dCauchyMix(xx, w = w, location = location, scale = scale, log = 0L)),
           numeric(1))
  )
  expect_equal(
    pcauchymix(x, w = w, location = location, scale = scale, lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pCauchyMix(xx, w = w, location = location, scale = scale,
                                                 lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qcauchymix(probs, w = w, location = location, scale = scale, lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qCauchyMix(pp, w = w, location = location, scale = scale,
                                          lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )
})

test_that("lowercase mixgpd and gpd wrappers vectorize over first argument", {

  x <- c(-0.5, 0.5, 2.5)
  probs <- c(0.1, 0.5, 0.9)

  w <- c(0.6, 0.4)
  mean <- c(0.0, 1.5)
  sd <- c(1.0, 1.8)
  threshold <- 1.2
  tail_scale <- 1.0
  tail_shape <- 0.15

  expect_equal(
    dnormmixgpd(x, w = w, mean = mean, sd = sd, threshold = threshold,
                tail_scale = tail_scale, tail_shape = tail_shape, log = FALSE),
    vapply(x, function(xx) as.numeric(dNormMixGpd(xx, w = w, mean = mean, sd = sd,
                                                 threshold = threshold, tail_scale = tail_scale,
                                                 tail_shape = tail_shape, log = 0L)),
           numeric(1))
  )
  expect_equal(
    pnormmixgpd(x, w = w, mean = mean, sd = sd, threshold = threshold,
                tail_scale = tail_scale, tail_shape = tail_shape,
                lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pNormMixGpd(xx, w = w, mean = mean, sd = sd,
                                                 threshold = threshold, tail_scale = tail_scale,
                                                 tail_shape = tail_shape, lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qnormmixgpd(probs, w = w, mean = mean, sd = sd, threshold = threshold,
                tail_scale = tail_scale, tail_shape = tail_shape,
                lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qNormMixGpd(pp, w = w, mean = mean, sd = sd,
                                          threshold = threshold, tail_scale = tail_scale,
                                          tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )

  expect_equal(
    dnormgpd(x, mean = mean[1], sd = sd[1], threshold = threshold,
             tail_scale = tail_scale, tail_shape = tail_shape, log = FALSE),
    vapply(x, function(xx) as.numeric(dNormGpd(xx, mean = mean[1], sd = sd[1],
                                              threshold = threshold, tail_scale = tail_scale,
                                              tail_shape = tail_shape, log = 0L)),
           numeric(1))
  )
  expect_equal(
    pnormgpd(x, mean = mean[1], sd = sd[1], threshold = threshold,
             tail_scale = tail_scale, tail_shape = tail_shape,
             lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pNormGpd(xx, mean = mean[1], sd = sd[1],
                                              threshold = threshold, tail_scale = tail_scale,
                                              tail_shape = tail_shape, lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qnormgpd(probs, mean = mean[1], sd = sd[1], threshold = threshold,
             tail_scale = tail_scale, tail_shape = tail_shape,
             lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qNormGpd(pp, mean = mean[1], sd = sd[1],
                                        threshold = threshold, tail_scale = tail_scale,
                                        tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )
})

test_that("lowercase mixgpd and gpd r wrappers support n > 1", {

  w <- c(0.6, 0.4)
  mean <- c(0.0, 1.5)
  sd <- c(1.0, 1.8)
  threshold <- 1.2
  tail_scale <- 1.0
  tail_shape <- 0.15

  set.seed(789)
  draws_mixgpd <- rnormmixgpd(7, w = w, mean = mean, sd = sd, threshold = threshold,
                             tail_scale = tail_scale, tail_shape = tail_shape)
  expect_length(draws_mixgpd, 7)
  expect_true(all(is.finite(draws_mixgpd)))

  draws_gpd <- rnormgpd(7, mean = mean[1], sd = sd[1], threshold = threshold,
                        tail_scale = tail_scale, tail_shape = tail_shape)
  expect_length(draws_gpd, 7)
  expect_true(all(is.finite(draws_gpd)))
})
