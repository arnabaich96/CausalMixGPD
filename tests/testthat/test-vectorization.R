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

test_that("lowercase gamma mixgpd and gpd wrappers vectorize over first argument", {
  x <- c(1.0, 2.2, 4.5)
  probs <- c(0.2, 0.6, 0.9)

  w <- c(0.6, 0.4)
  shape <- c(2.0, 4.0)
  scale <- c(1.0, 2.0)
  threshold <- 2.5
  tail_scale <- 0.9
  tail_shape <- 0.2

  expect_equal(
    dgammamixgpd(x, w = w, shape = shape, scale = scale, threshold = threshold,
                 tail_scale = tail_scale, tail_shape = tail_shape, log = FALSE),
    vapply(x, function(xx) as.numeric(dGammaMixGpd(xx, w = w, shape = shape, scale = scale,
                                                  threshold = threshold, tail_scale = tail_scale,
                                                  tail_shape = tail_shape, log = 0L)),
           numeric(1))
  )
  expect_equal(
    pgammamixgpd(x, w = w, shape = shape, scale = scale, threshold = threshold,
                 tail_scale = tail_scale, tail_shape = tail_shape,
                 lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pGammaMixGpd(xx, w = w, shape = shape, scale = scale,
                                                  threshold = threshold, tail_scale = tail_scale,
                                                  tail_shape = tail_shape, lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qgammamixgpd(probs, w = w, shape = shape, scale = scale, threshold = threshold,
                 tail_scale = tail_scale, tail_shape = tail_shape,
                 lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qGammaMixGpd(pp, w = w, shape = shape, scale = scale,
                                           threshold = threshold, tail_scale = tail_scale,
                                           tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )

  expect_equal(
    dgammagpd(x, shape = shape[1], scale = scale[1], threshold = threshold,
              tail_scale = tail_scale, tail_shape = tail_shape, log = FALSE),
    vapply(x, function(xx) as.numeric(dGammaGpd(xx, shape = shape[1], scale = scale[1],
                                               threshold = threshold, tail_scale = tail_scale,
                                               tail_shape = tail_shape, log = 0L)),
           numeric(1))
  )
  expect_equal(
    pgammagpd(x, shape = shape[1], scale = scale[1], threshold = threshold,
              tail_scale = tail_scale, tail_shape = tail_shape,
              lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pGammaGpd(xx, shape = shape[1], scale = scale[1],
                                               threshold = threshold, tail_scale = tail_scale,
                                               tail_shape = tail_shape, lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qgammagpd(probs, shape = shape[1], scale = scale[1], threshold = threshold,
              tail_scale = tail_scale, tail_shape = tail_shape,
              lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qGammaGpd(pp, shape = shape[1], scale = scale[1],
                                        threshold = threshold, tail_scale = tail_scale,
                                        tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )
})

test_that("lowercase laplace wrappers vectorize over first argument", {
  x <- c(-1.0, 0.5, 2.0)
  probs <- c(0.2, 0.6, 0.9)

  w <- c(0.7, 0.3)
  location <- c(-0.5, 1.0)
  scale <- c(1.0, 1.4)
  threshold <- 0.8
  tail_scale <- 0.9
  tail_shape <- 0.15

  expect_equal(
    dlaplacemix(x, w = w, location = location, scale = scale, log = FALSE),
    vapply(x, function(xx) as.numeric(dLaplaceMix(xx, w = w, location = location, scale = scale, log = 0L)),
           numeric(1))
  )
  expect_equal(
    plaplacemix(x, w = w, location = location, scale = scale, lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pLaplaceMix(xx, w = w, location = location, scale = scale,
                                                 lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qlaplacemix(probs, w = w, location = location, scale = scale,
                lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qLaplaceMix(pp, w = w, location = location, scale = scale,
                                          lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )

  expect_equal(
    dlaplacemixgpd(x, w = w, location = location, scale = scale, threshold = threshold,
                   tail_scale = tail_scale, tail_shape = tail_shape, log = FALSE),
    vapply(x, function(xx) as.numeric(dLaplaceMixGpd(xx, w = w, location = location, scale = scale,
                                                    threshold = threshold, tail_scale = tail_scale,
                                                    tail_shape = tail_shape, log = 0L)),
           numeric(1))
  )
  expect_equal(
    plaplacemixgpd(x, w = w, location = location, scale = scale, threshold = threshold,
                   tail_scale = tail_scale, tail_shape = tail_shape,
                   lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pLaplaceMixGpd(xx, w = w, location = location, scale = scale,
                                                    threshold = threshold, tail_scale = tail_scale,
                                                    tail_shape = tail_shape, lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qlaplacemixgpd(probs, w = w, location = location, scale = scale, threshold = threshold,
                   tail_scale = tail_scale, tail_shape = tail_shape,
                   lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qLaplaceMixGpd(pp, w = w, location = location, scale = scale,
                                             threshold = threshold, tail_scale = tail_scale,
                                             tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )

  expect_equal(
    dlaplacegpd(x, location = location[1], scale = scale[1], threshold = threshold,
               tail_scale = tail_scale, tail_shape = tail_shape, log = FALSE),
    vapply(x, function(xx) as.numeric(dLaplaceGpd(xx, location = location[1], scale = scale[1],
                                                 threshold = threshold, tail_scale = tail_scale,
                                                 tail_shape = tail_shape, log = 0L)),
           numeric(1))
  )
  expect_equal(
    plaplacegpd(x, location = location[1], scale = scale[1], threshold = threshold,
               tail_scale = tail_scale, tail_shape = tail_shape,
               lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pLaplaceGpd(xx, location = location[1], scale = scale[1],
                                                 threshold = threshold, tail_scale = tail_scale,
                                                 tail_shape = tail_shape, lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qlaplacegpd(probs, location = location[1], scale = scale[1], threshold = threshold,
               tail_scale = tail_scale, tail_shape = tail_shape,
               lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qLaplaceGpd(pp, location = location[1], scale = scale[1],
                                          threshold = threshold, tail_scale = tail_scale,
                                          tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )
})

test_that("lowercase lognormal wrappers vectorize over first argument", {
  x <- c(0.5, 1.2, 3.0)
  probs <- c(0.2, 0.6, 0.9)

  w <- c(0.7, 0.3)
  meanlog <- c(0.0, 0.4)
  sdlog <- c(0.3, 0.6)
  threshold <- 1.8
  tail_scale <- 0.8
  tail_shape <- 0.1

  expect_equal(
    dlognormalmix(x, w = w, meanlog = meanlog, sdlog = sdlog, log = FALSE),
    vapply(x, function(xx) as.numeric(dLognormalMix(xx, w = w, meanlog = meanlog, sdlog = sdlog, log = 0L)),
           numeric(1))
  )
  expect_equal(
    plognormalmix(x, w = w, meanlog = meanlog, sdlog = sdlog, lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pLognormalMix(xx, w = w, meanlog = meanlog, sdlog = sdlog,
                                                   lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qlognormalmix(probs, w = w, meanlog = meanlog, sdlog = sdlog,
                  lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qLognormalMix(pp, w = w, meanlog = meanlog, sdlog = sdlog,
                                            lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )

  expect_equal(
    dlognormalmixgpd(x, w = w, meanlog = meanlog, sdlog = sdlog, threshold = threshold,
                     tail_scale = tail_scale, tail_shape = tail_shape, log = FALSE),
    vapply(x, function(xx) as.numeric(dLognormalMixGpd(xx, w = w, meanlog = meanlog, sdlog = sdlog,
                                                      threshold = threshold, tail_scale = tail_scale,
                                                      tail_shape = tail_shape, log = 0L)),
           numeric(1))
  )
  expect_equal(
    plognormalmixgpd(x, w = w, meanlog = meanlog, sdlog = sdlog, threshold = threshold,
                     tail_scale = tail_scale, tail_shape = tail_shape,
                     lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pLognormalMixGpd(xx, w = w, meanlog = meanlog, sdlog = sdlog,
                                                      threshold = threshold, tail_scale = tail_scale,
                                                      tail_shape = tail_shape, lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qlognormalmixgpd(probs, w = w, meanlog = meanlog, sdlog = sdlog, threshold = threshold,
                     tail_scale = tail_scale, tail_shape = tail_shape,
                     lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qLognormalMixGpd(pp, w = w, meanlog = meanlog, sdlog = sdlog,
                                               threshold = threshold, tail_scale = tail_scale,
                                               tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )

  expect_equal(
    dlognormalgpd(x, meanlog = meanlog[1], sdlog = sdlog[1], threshold = threshold,
                  tail_scale = tail_scale, tail_shape = tail_shape, log = FALSE),
    vapply(x, function(xx) as.numeric(dLognormalGpd(xx, meanlog = meanlog[1], sdlog = sdlog[1],
                                                   threshold = threshold, tail_scale = tail_scale,
                                                   tail_shape = tail_shape, log = 0L)),
           numeric(1))
  )
  expect_equal(
    plognormalgpd(x, meanlog = meanlog[1], sdlog = sdlog[1], threshold = threshold,
                  tail_scale = tail_scale, tail_shape = tail_shape,
                  lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pLognormalGpd(xx, meanlog = meanlog[1], sdlog = sdlog[1],
                                                   threshold = threshold, tail_scale = tail_scale,
                                                   tail_shape = tail_shape, lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qlognormalgpd(probs, meanlog = meanlog[1], sdlog = sdlog[1], threshold = threshold,
                  tail_scale = tail_scale, tail_shape = tail_shape,
                  lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qLognormalGpd(pp, meanlog = meanlog[1], sdlog = sdlog[1],
                                            threshold = threshold, tail_scale = tail_scale,
                                            tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )
})

test_that("lowercase invgauss wrappers vectorize over first argument", {
  x <- c(0.8, 1.5, 3.0)
  probs <- c(0.2, 0.6, 0.9)

  w <- c(0.6, 0.4)
  mean <- c(1.2, 2.0)
  shape <- c(2.5, 4.0)
  threshold <- 1.6
  tail_scale <- 0.7
  tail_shape <- 0.15

  expect_equal(
    dinvgaussmix(x, w = w, mean = mean, shape = shape, log = FALSE),
    vapply(x, function(xx) as.numeric(dInvGaussMix(xx, w = w, mean = mean, shape = shape, log = 0L)),
           numeric(1))
  )
  expect_equal(
    pinvgaussmix(x, w = w, mean = mean, shape = shape, lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pInvGaussMix(xx, w = w, mean = mean, shape = shape,
                                                  lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qinvgaussmix(probs, w = w, mean = mean, shape = shape,
                 lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qInvGaussMix(pp, w = w, mean = mean, shape = shape,
                                           lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )

  expect_equal(
    dinvgaussmixgpd(x, w = w, mean = mean, shape = shape, threshold = threshold,
                    tail_scale = tail_scale, tail_shape = tail_shape, log = FALSE),
    vapply(x, function(xx) as.numeric(dInvGaussMixGpd(xx, w = w, mean = mean, shape = shape,
                                                     threshold = threshold, tail_scale = tail_scale,
                                                     tail_shape = tail_shape, log = 0L)),
           numeric(1))
  )
  expect_equal(
    pinvgaussmixgpd(x, w = w, mean = mean, shape = shape, threshold = threshold,
                    tail_scale = tail_scale, tail_shape = tail_shape,
                    lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pInvGaussMixGpd(xx, w = w, mean = mean, shape = shape,
                                                     threshold = threshold, tail_scale = tail_scale,
                                                     tail_shape = tail_shape, lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qinvgaussmixgpd(probs, w = w, mean = mean, shape = shape, threshold = threshold,
                    tail_scale = tail_scale, tail_shape = tail_shape,
                    lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qInvGaussMixGpd(pp, w = w, mean = mean, shape = shape,
                                              threshold = threshold, tail_scale = tail_scale,
                                              tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )

  expect_equal(
    dinvgaussgpd(x, mean = mean[1], shape = shape[1], threshold = threshold,
                 tail_scale = tail_scale, tail_shape = tail_shape, log = FALSE),
    vapply(x, function(xx) as.numeric(dInvGaussGpd(xx, mean = mean[1], shape = shape[1],
                                                  threshold = threshold, tail_scale = tail_scale,
                                                  tail_shape = tail_shape, log = 0L)),
           numeric(1))
  )
  expect_equal(
    pinvgaussgpd(x, mean = mean[1], shape = shape[1], threshold = threshold,
                 tail_scale = tail_scale, tail_shape = tail_shape,
                 lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pInvGaussGpd(xx, mean = mean[1], shape = shape[1],
                                                  threshold = threshold, tail_scale = tail_scale,
                                                  tail_shape = tail_shape, lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qinvgaussgpd(probs, mean = mean[1], shape = shape[1], threshold = threshold,
                 tail_scale = tail_scale, tail_shape = tail_shape,
                 lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qInvGaussGpd(pp, mean = mean[1], shape = shape[1],
                                           threshold = threshold, tail_scale = tail_scale,
                                           tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )
})

test_that("lowercase amoroso wrappers vectorize over first argument", {
  x <- c(0.8, 1.4, 3.0)
  probs <- c(0.2, 0.6, 0.9)

  w <- c(0.5, 0.3, 0.2)
  loc <- c(0.0, 0.5, 1.0)
  scale <- c(1.0, 1.2, 1.5)
  shape1 <- c(2.0, 3.0, 4.0)
  shape2 <- c(1.1, 1.2, 1.3)
  threshold <- 1.6
  tail_scale <- 0.8
  tail_shape <- 0.15

  expect_equal(
    damorosomix(x, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2, log = FALSE),
    vapply(x, function(xx) as.numeric(dAmorosoMix(xx, w = w, loc = loc, scale = scale,
                                                 shape1 = shape1, shape2 = shape2, log = 0L)),
           numeric(1))
  )
  expect_equal(
    pamorosomix(x, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2,
                lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pAmorosoMix(xx, w = w, loc = loc, scale = scale,
                                                 shape1 = shape1, shape2 = shape2,
                                                 lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qamorosomix(probs, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2,
                lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qAmorosoMix(pp, w = w, loc = loc, scale = scale,
                                          shape1 = shape1, shape2 = shape2,
                                          lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )

  expect_equal(
    damorosomixgpd(x, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2,
                   threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape, log = FALSE),
    vapply(x, function(xx) as.numeric(dAmorosoMixGpd(xx, w = w, loc = loc, scale = scale,
                                                    shape1 = shape1, shape2 = shape2,
                                                    threshold = threshold, tail_scale = tail_scale,
                                                    tail_shape = tail_shape, log = 0L)),
           numeric(1))
  )
  expect_equal(
    pamorosomixgpd(x, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2,
                   threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape,
                   lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pAmorosoMixGpd(xx, w = w, loc = loc, scale = scale,
                                                    shape1 = shape1, shape2 = shape2,
                                                    threshold = threshold, tail_scale = tail_scale,
                                                    tail_shape = tail_shape, lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qamorosomixgpd(probs, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2,
                   threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape,
                   lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qAmorosoMixGpd(pp, w = w, loc = loc, scale = scale,
                                             shape1 = shape1, shape2 = shape2,
                                             threshold = threshold, tail_scale = tail_scale,
                                             tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )

  expect_equal(
    damorosogpd(x, loc = loc[1], scale = scale[1], shape1 = shape1[1], shape2 = shape2[1],
                threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape, log = FALSE),
    vapply(x, function(xx) as.numeric(dAmorosoGpd(xx, loc = loc[1], scale = scale[1],
                                                 shape1 = shape1[1], shape2 = shape2[1],
                                                 threshold = threshold, tail_scale = tail_scale,
                                                 tail_shape = tail_shape, log = 0L)),
           numeric(1))
  )
  expect_equal(
    pamorosogpd(x, loc = loc[1], scale = scale[1], shape1 = shape1[1], shape2 = shape2[1],
                threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape,
                lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pAmorosoGpd(xx, loc = loc[1], scale = scale[1],
                                                 shape1 = shape1[1], shape2 = shape2[1],
                                                 threshold = threshold, tail_scale = tail_scale,
                                                 tail_shape = tail_shape, lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qamorosogpd(probs, loc = loc[1], scale = scale[1], shape1 = shape1[1], shape2 = shape2[1],
                threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape,
                lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qAmorosoGpd(pp, loc = loc[1], scale = scale[1],
                                          shape1 = shape1[1], shape2 = shape2[1],
                                          threshold = threshold, tail_scale = tail_scale,
                                          tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE),
           numeric(1))
  )
})

test_that("lowercase normal mix wrappers vectorize over first argument", {
  x <- c(-1.0, 0.2, 1.5)
  probs <- c(0.2, 0.6, 0.9)

  w <- c(0.7, 0.3)
  mean <- c(0.0, 1.2)
  sd <- c(1.0, 1.6)

  expect_equal(
    dnormmix(x, w = w, mean = mean, sd = sd, log = FALSE),
    vapply(x, function(xx) as.numeric(dNormMix(xx, w = w, mean = mean, sd = sd, log = 0L)),
           numeric(1))
  )
  expect_equal(
    pnormmix(x, w = w, mean = mean, sd = sd, lower.tail = TRUE, log.p = FALSE),
    vapply(x, function(xx) as.numeric(pNormMix(xx, w = w, mean = mean, sd = sd,
                                              lower.tail = 1L, log.p = 0L)),
           numeric(1))
  )
  expect_equal(
    qnormmix(probs, w = w, mean = mean, sd = sd,
             lower.tail = TRUE, log.p = FALSE),
    vapply(probs, function(pp) qNormMix(pp, w = w, mean = mean, sd = sd,
                                       lower.tail = TRUE, log.p = FALSE),
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

test_that("vectorized d/p/q options match scalar versions", {
  d_specs <- list(
    list(vec = dgpd, scalar = dGpd, x = c(1.2, 1.6, 2.0),
         args = list(threshold = 1.0, scale = 0.8, shape = 0.2)),
    list(vec = dnormmix, scalar = dNormMix, x = c(-1.0, 0.5, 1.2),
         args = list(w = c(0.7, 0.3), mean = c(0.0, 1.2), sd = c(1.0, 1.6))),
    list(vec = dnormmixgpd, scalar = dNormMixGpd, x = c(-0.5, 0.5, 2.5),
         args = list(w = c(0.6, 0.4), mean = c(0.0, 1.5), sd = c(1.0, 1.8),
                     threshold = 1.2, tail_scale = 1.0, tail_shape = 0.15)),
    list(vec = dnormgpd, scalar = dNormGpd, x = c(-0.5, 0.5, 2.5),
         args = list(mean = 0.0, sd = 1.0, threshold = 1.2, tail_scale = 1.0, tail_shape = 0.15)),
    list(vec = dgammamix, scalar = dGammaMix, x = c(1.0, 2.2, 4.5),
         args = list(w = c(0.6, 0.4), shape = c(2.0, 4.0), scale = c(1.0, 2.0))),
    list(vec = dgammamixgpd, scalar = dGammaMixGpd, x = c(1.0, 2.2, 4.5),
         args = list(w = c(0.6, 0.4), shape = c(2.0, 4.0), scale = c(1.0, 2.0),
                     threshold = 2.5, tail_scale = 0.9, tail_shape = 0.2)),
    list(vec = dgammagpd, scalar = dGammaGpd, x = c(1.0, 2.2, 4.5),
         args = list(shape = 2.0, scale = 1.0, threshold = 2.5, tail_scale = 0.9, tail_shape = 0.2)),
    list(vec = dlognormalmix, scalar = dLognormalMix, x = c(0.5, 1.2, 3.0),
         args = list(w = c(0.7, 0.3), meanlog = c(0.0, 0.4), sdlog = c(0.3, 0.6))),
    list(vec = dlognormalmixgpd, scalar = dLognormalMixGpd, x = c(0.5, 1.2, 3.0),
         args = list(w = c(0.7, 0.3), meanlog = c(0.0, 0.4), sdlog = c(0.3, 0.6),
                     threshold = 1.8, tail_scale = 0.8, tail_shape = 0.1)),
    list(vec = dlognormalgpd, scalar = dLognormalGpd, x = c(0.5, 1.2, 3.0),
         args = list(meanlog = 0.0, sdlog = 0.3, threshold = 1.8, tail_scale = 0.8, tail_shape = 0.1)),
    list(vec = dlaplacemix, scalar = dLaplaceMix, x = c(-1.0, 0.5, 2.0),
         args = list(w = c(0.7, 0.3), location = c(-0.5, 1.0), scale = c(1.0, 1.4))),
    list(vec = dlaplacemixgpd, scalar = dLaplaceMixGpd, x = c(-1.0, 0.5, 2.0),
         args = list(w = c(0.7, 0.3), location = c(-0.5, 1.0), scale = c(1.0, 1.4),
                     threshold = 0.8, tail_scale = 0.9, tail_shape = 0.15)),
    list(vec = dlaplacegpd, scalar = dLaplaceGpd, x = c(-1.0, 0.5, 2.0),
         args = list(location = -0.5, scale = 1.0, threshold = 0.8, tail_scale = 0.9, tail_shape = 0.15)),
    list(vec = dinvgaussmix, scalar = dInvGaussMix, x = c(0.8, 1.5, 3.0),
         args = list(w = c(0.6, 0.4), mean = c(1.2, 2.0), shape = c(2.5, 4.0))),
    list(vec = dinvgaussmixgpd, scalar = dInvGaussMixGpd, x = c(0.8, 1.5, 3.0),
         args = list(w = c(0.6, 0.4), mean = c(1.2, 2.0), shape = c(2.5, 4.0),
                     threshold = 1.6, tail_scale = 0.7, tail_shape = 0.15)),
    list(vec = dinvgaussgpd, scalar = dInvGaussGpd, x = c(0.8, 1.5, 3.0),
         args = list(mean = 1.2, shape = 2.5, threshold = 1.6, tail_scale = 0.7, tail_shape = 0.15)),
    list(vec = dinvgauss, scalar = dInvGauss, x = c(1.2, 1.5, 2.0),
         args = list(mean = 2.0, shape = 3.0)),
    list(vec = damorosomix, scalar = dAmorosoMix, x = c(0.8, 1.4, 3.0),
         args = list(w = c(0.5, 0.3, 0.2), loc = c(0.0, 0.5, 1.0), scale = c(1.0, 1.2, 1.5),
                     shape1 = c(2.0, 3.0, 4.0), shape2 = c(1.1, 1.2, 1.3))),
    list(vec = damorosomixgpd, scalar = dAmorosoMixGpd, x = c(0.8, 1.4, 3.0),
         args = list(w = c(0.5, 0.3, 0.2), loc = c(0.0, 0.5, 1.0), scale = c(1.0, 1.2, 1.5),
                     shape1 = c(2.0, 3.0, 4.0), shape2 = c(1.1, 1.2, 1.3),
                     threshold = 1.6, tail_scale = 0.8, tail_shape = 0.15)),
    list(vec = damorosogpd, scalar = dAmorosoGpd, x = c(0.8, 1.4, 3.0),
         args = list(loc = 0.0, scale = 1.0, shape1 = 2.0, shape2 = 1.1,
                     threshold = 1.6, tail_scale = 0.8, tail_shape = 0.15)),
    list(vec = damoroso, scalar = dAmoroso, x = c(1.2, 1.5, 2.0),
         args = list(loc = 0.0, scale = 1.2, shape1 = 2.0, shape2 = 1.5)),
    list(vec = dcauchy_vec, scalar = dCauchy, x = c(-1.0, 0.0, 1.0),
         args = list(location = 0.0, scale = 1.1)),
    list(vec = dcauchymix, scalar = dCauchyMix, x = c(-1.0, 0.5, 1.2),
         args = list(w = c(0.7, 0.3), location = c(-0.5, 0.8), scale = c(1.0, 1.5)))
  )

  for (spec in d_specs) {
    vec <- do.call(spec$vec, c(list(spec$x), spec$args, list(log = TRUE)))
    scalar <- vapply(
      spec$x,
      function(xx) as.numeric(do.call(spec$scalar, c(list(xx), spec$args, list(log = 1L)))),
      numeric(1)
    )
    expect_equal(vec, scalar, tolerance = 1e-10)
  }

  p_specs <- list(
    list(vec = pgpd, scalar = pGpd, q = c(1.2, 1.6, 2.0),
         args = list(threshold = 1.0, scale = 0.8, shape = 0.2)),
    list(vec = pnormmix, scalar = pNormMix, q = c(-1.0, 0.5, 1.2),
         args = list(w = c(0.7, 0.3), mean = c(0.0, 1.2), sd = c(1.0, 1.6))),
    list(vec = pnormmixgpd, scalar = pNormMixGpd, q = c(-0.5, 0.5, 2.5),
         args = list(w = c(0.6, 0.4), mean = c(0.0, 1.5), sd = c(1.0, 1.8),
                     threshold = 1.2, tail_scale = 1.0, tail_shape = 0.15)),
    list(vec = pnormgpd, scalar = pNormGpd, q = c(-0.5, 0.5, 2.5),
         args = list(mean = 0.0, sd = 1.0, threshold = 1.2, tail_scale = 1.0, tail_shape = 0.15)),
    list(vec = pgammamix, scalar = pGammaMix, q = c(1.0, 2.2, 4.5),
         args = list(w = c(0.6, 0.4), shape = c(2.0, 4.0), scale = c(1.0, 2.0))),
    list(vec = pgammamixgpd, scalar = pGammaMixGpd, q = c(1.0, 2.2, 4.5),
         args = list(w = c(0.6, 0.4), shape = c(2.0, 4.0), scale = c(1.0, 2.0),
                     threshold = 2.5, tail_scale = 0.9, tail_shape = 0.2)),
    list(vec = pgammagpd, scalar = pGammaGpd, q = c(1.0, 2.2, 4.5),
         args = list(shape = 2.0, scale = 1.0, threshold = 2.5, tail_scale = 0.9, tail_shape = 0.2)),
    list(vec = plognormalmix, scalar = pLognormalMix, q = c(0.5, 1.2, 3.0),
         args = list(w = c(0.7, 0.3), meanlog = c(0.0, 0.4), sdlog = c(0.3, 0.6))),
    list(vec = plognormalmixgpd, scalar = pLognormalMixGpd, q = c(0.5, 1.2, 3.0),
         args = list(w = c(0.7, 0.3), meanlog = c(0.0, 0.4), sdlog = c(0.3, 0.6),
                     threshold = 1.8, tail_scale = 0.8, tail_shape = 0.1)),
    list(vec = plognormalgpd, scalar = pLognormalGpd, q = c(0.5, 1.2, 3.0),
         args = list(meanlog = 0.0, sdlog = 0.3, threshold = 1.8, tail_scale = 0.8, tail_shape = 0.1)),
    list(vec = plaplacemix, scalar = pLaplaceMix, q = c(-1.0, 0.5, 2.0),
         args = list(w = c(0.7, 0.3), location = c(-0.5, 1.0), scale = c(1.0, 1.4))),
    list(vec = plaplacemixgpd, scalar = pLaplaceMixGpd, q = c(-1.0, 0.5, 2.0),
         args = list(w = c(0.7, 0.3), location = c(-0.5, 1.0), scale = c(1.0, 1.4),
                     threshold = 0.8, tail_scale = 0.9, tail_shape = 0.15)),
    list(vec = plaplacegpd, scalar = pLaplaceGpd, q = c(-1.0, 0.5, 2.0),
         args = list(location = -0.5, scale = 1.0, threshold = 0.8, tail_scale = 0.9, tail_shape = 0.15)),
    list(vec = pinvgaussmix, scalar = pInvGaussMix, q = c(0.8, 1.5, 3.0),
         args = list(w = c(0.6, 0.4), mean = c(1.2, 2.0), shape = c(2.5, 4.0))),
    list(vec = pinvgaussmixgpd, scalar = pInvGaussMixGpd, q = c(0.8, 1.5, 3.0),
         args = list(w = c(0.6, 0.4), mean = c(1.2, 2.0), shape = c(2.5, 4.0),
                     threshold = 1.6, tail_scale = 0.7, tail_shape = 0.15)),
    list(vec = pinvgaussgpd, scalar = pInvGaussGpd, q = c(0.8, 1.5, 3.0),
         args = list(mean = 1.2, shape = 2.5, threshold = 1.6, tail_scale = 0.7, tail_shape = 0.15)),
    list(vec = pinvgauss, scalar = pInvGauss, q = c(1.2, 1.5, 2.0),
         args = list(mean = 2.0, shape = 3.0)),
    list(vec = pamorosomix, scalar = pAmorosoMix, q = c(0.8, 1.4, 3.0),
         args = list(w = c(0.5, 0.3, 0.2), loc = c(0.0, 0.5, 1.0), scale = c(1.0, 1.2, 1.5),
                     shape1 = c(2.0, 3.0, 4.0), shape2 = c(1.1, 1.2, 1.3))),
    list(vec = pamorosomixgpd, scalar = pAmorosoMixGpd, q = c(0.8, 1.4, 3.0),
         args = list(w = c(0.5, 0.3, 0.2), loc = c(0.0, 0.5, 1.0), scale = c(1.0, 1.2, 1.5),
                     shape1 = c(2.0, 3.0, 4.0), shape2 = c(1.1, 1.2, 1.3),
                     threshold = 1.6, tail_scale = 0.8, tail_shape = 0.15)),
    list(vec = pamorosogpd, scalar = pAmorosoGpd, q = c(0.8, 1.4, 3.0),
         args = list(loc = 0.0, scale = 1.0, shape1 = 2.0, shape2 = 1.1,
                     threshold = 1.6, tail_scale = 0.8, tail_shape = 0.15)),
    list(vec = pamoroso, scalar = pAmoroso, q = c(1.2, 1.5, 2.0),
         args = list(loc = 0.0, scale = 1.2, shape1 = 2.0, shape2 = 1.5)),
    list(vec = pcauchy_vec, scalar = pCauchy, q = c(-1.0, 0.0, 1.0),
         args = list(location = 0.0, scale = 1.1)),
    list(vec = pcauchymix, scalar = pCauchyMix, q = c(-1.0, 0.5, 1.2),
         args = list(w = c(0.7, 0.3), location = c(-0.5, 0.8), scale = c(1.0, 1.5)))
  )

  for (spec in p_specs) {
    vec <- do.call(spec$vec, c(list(spec$q), spec$args, list(lower.tail = FALSE, log.p = TRUE)))
    scalar <- vapply(
      spec$q,
      function(xx) as.numeric(do.call(spec$scalar, c(list(xx), spec$args, list(lower.tail = 0L, log.p = 1L)))),
      numeric(1)
    )
    expect_equal(vec, scalar, tolerance = 1e-10)
  }

  q_specs <- list(
    list(vec = qgpd, scalar = qGpd, p = c(0.1, 0.5, 0.9),
         args = list(threshold = 1.0, scale = 0.8, shape = 0.2)),
    list(vec = qnormmix, scalar = qNormMix, p = c(0.2, 0.5, 0.8),
         args = list(w = c(0.7, 0.3), mean = c(0.0, 1.2), sd = c(1.0, 1.6))),
    list(vec = qnormmixgpd, scalar = qNormMixGpd, p = c(0.2, 0.5, 0.8),
         args = list(w = c(0.6, 0.4), mean = c(0.0, 1.5), sd = c(1.0, 1.8),
                     threshold = 1.2, tail_scale = 1.0, tail_shape = 0.15)),
    list(vec = qnormgpd, scalar = qNormGpd, p = c(0.2, 0.5, 0.8),
         args = list(mean = 0.0, sd = 1.0, threshold = 1.2, tail_scale = 1.0, tail_shape = 0.15)),
    list(vec = qgammamix, scalar = qGammaMix, p = c(0.2, 0.5, 0.8),
         args = list(w = c(0.6, 0.4), shape = c(2.0, 4.0), scale = c(1.0, 2.0))),
    list(vec = qgammamixgpd, scalar = qGammaMixGpd, p = c(0.2, 0.5, 0.8),
         args = list(w = c(0.6, 0.4), shape = c(2.0, 4.0), scale = c(1.0, 2.0),
                     threshold = 2.5, tail_scale = 0.9, tail_shape = 0.2)),
    list(vec = qgammagpd, scalar = qGammaGpd, p = c(0.2, 0.5, 0.8),
         args = list(shape = 2.0, scale = 1.0, threshold = 2.5, tail_scale = 0.9, tail_shape = 0.2)),
    list(vec = qlognormalmix, scalar = qLognormalMix, p = c(0.2, 0.5, 0.8),
         args = list(w = c(0.7, 0.3), meanlog = c(0.0, 0.4), sdlog = c(0.3, 0.6))),
    list(vec = qlognormalmixgpd, scalar = qLognormalMixGpd, p = c(0.2, 0.5, 0.8),
         args = list(w = c(0.7, 0.3), meanlog = c(0.0, 0.4), sdlog = c(0.3, 0.6),
                     threshold = 1.8, tail_scale = 0.8, tail_shape = 0.1)),
    list(vec = qlognormalgpd, scalar = qLognormalGpd, p = c(0.2, 0.5, 0.8),
         args = list(meanlog = 0.0, sdlog = 0.3, threshold = 1.8, tail_scale = 0.8, tail_shape = 0.1)),
    list(vec = qlaplacemix, scalar = qLaplaceMix, p = c(0.2, 0.5, 0.8),
         args = list(w = c(0.7, 0.3), location = c(-0.5, 1.0), scale = c(1.0, 1.4))),
    list(vec = qlaplacemixgpd, scalar = qLaplaceMixGpd, p = c(0.2, 0.5, 0.8),
         args = list(w = c(0.7, 0.3), location = c(-0.5, 1.0), scale = c(1.0, 1.4),
                     threshold = 0.8, tail_scale = 0.9, tail_shape = 0.15)),
    list(vec = qlaplacegpd, scalar = qLaplaceGpd, p = c(0.2, 0.5, 0.8),
         args = list(location = -0.5, scale = 1.0, threshold = 0.8, tail_scale = 0.9, tail_shape = 0.15)),
    list(vec = qinvgaussmix, scalar = qInvGaussMix, p = c(0.2, 0.5, 0.8),
         args = list(w = c(0.6, 0.4), mean = c(1.2, 2.0), shape = c(2.5, 4.0))),
    list(vec = qinvgaussmixgpd, scalar = qInvGaussMixGpd, p = c(0.2, 0.5, 0.8),
         args = list(w = c(0.6, 0.4), mean = c(1.2, 2.0), shape = c(2.5, 4.0),
                     threshold = 1.6, tail_scale = 0.7, tail_shape = 0.15)),
    list(vec = qinvgaussgpd, scalar = qInvGaussGpd, p = c(0.2, 0.5, 0.8),
         args = list(mean = 1.2, shape = 2.5, threshold = 1.6, tail_scale = 0.7, tail_shape = 0.15)),
    list(vec = qinvgauss, scalar = qInvGauss, p = c(0.2, 0.5, 0.8),
         args = list(mean = 2.0, shape = 3.0)),
    list(vec = qamorosomix, scalar = qAmorosoMix, p = c(0.2, 0.5, 0.8),
         args = list(w = c(0.5, 0.3, 0.2), loc = c(0.0, 0.5, 1.0), scale = c(1.0, 1.2, 1.5),
                     shape1 = c(2.0, 3.0, 4.0), shape2 = c(1.1, 1.2, 1.3))),
    list(vec = qamorosomixgpd, scalar = qAmorosoMixGpd, p = c(0.2, 0.5, 0.8),
         args = list(w = c(0.5, 0.3, 0.2), loc = c(0.0, 0.5, 1.0), scale = c(1.0, 1.2, 1.5),
                     shape1 = c(2.0, 3.0, 4.0), shape2 = c(1.1, 1.2, 1.3),
                     threshold = 1.6, tail_scale = 0.8, tail_shape = 0.15)),
    list(vec = qamorosogpd, scalar = qAmorosoGpd, p = c(0.2, 0.5, 0.8),
         args = list(loc = 0.0, scale = 1.0, shape1 = 2.0, shape2 = 1.1,
                     threshold = 1.6, tail_scale = 0.8, tail_shape = 0.15)),
    list(vec = qamoroso, scalar = qAmoroso, p = c(0.2, 0.5, 0.8),
         args = list(loc = 0.0, scale = 1.2, shape1 = 2.0, shape2 = 1.5)),
    list(vec = qcauchy_vec, scalar = qCauchy, p = c(0.2, 0.5, 0.8),
         args = list(location = 0.0, scale = 1.1)),
    list(vec = qcauchymix, scalar = qCauchyMix, p = c(0.2, 0.5, 0.8),
         args = list(w = c(0.7, 0.3), location = c(-0.5, 0.8), scale = c(1.0, 1.5)))
  )

  for (spec in q_specs) {
    vec <- do.call(spec$vec, c(list(log(spec$p)), spec$args, list(lower.tail = TRUE, log.p = TRUE)))
    scalar <- vapply(
      log(spec$p),
      function(pp) do.call(spec$scalar, c(list(pp), spec$args, list(lower.tail = TRUE, log.p = TRUE))),
      numeric(1)
    )
    expect_equal(vec, scalar, tolerance = 1e-10)
  }
})

test_that("vectorized r functions match scalar draws with same seed", {
  r_specs <- list(
    list(vec = rgpd, scalar = rGpd, args = list(threshold = 1.0, scale = 0.8, shape = 0.2)),
    list(vec = rnormmix, scalar = rNormMix, args = list(w = c(0.7, 0.3), mean = c(0.0, 1.2), sd = c(1.0, 1.6))),
    list(vec = rnormmixgpd, scalar = rNormMixGpd, args = list(w = c(0.6, 0.4), mean = c(0.0, 1.5), sd = c(1.0, 1.8),
                                                               threshold = 1.2, tail_scale = 1.0, tail_shape = 0.15)),
    list(vec = rnormgpd, scalar = rNormGpd, args = list(mean = 0.0, sd = 1.0, threshold = 1.2, tail_scale = 1.0, tail_shape = 0.15)),
    list(vec = rgammamix, scalar = rGammaMix, args = list(w = c(0.6, 0.4), shape = c(2.0, 4.0), scale = c(1.0, 2.0))),
    list(vec = rgammamixgpd, scalar = rGammaMixGpd, args = list(w = c(0.6, 0.4), shape = c(2.0, 4.0), scale = c(1.0, 2.0),
                                                                 threshold = 2.5, tail_scale = 0.9, tail_shape = 0.2)),
    list(vec = rgammagpd, scalar = rGammaGpd, args = list(shape = 2.0, scale = 1.0, threshold = 2.5, tail_scale = 0.9, tail_shape = 0.2)),
    list(vec = rlognormalmix, scalar = rLognormalMix, args = list(w = c(0.7, 0.3), meanlog = c(0.0, 0.4), sdlog = c(0.3, 0.6))),
    list(vec = rlognormalmixgpd, scalar = rLognormalMixGpd, args = list(w = c(0.7, 0.3), meanlog = c(0.0, 0.4), sdlog = c(0.3, 0.6),
                                                                       threshold = 1.8, tail_scale = 0.8, tail_shape = 0.1)),
    list(vec = rlognormalgpd, scalar = rLognormalGpd, args = list(meanlog = 0.0, sdlog = 0.3, threshold = 1.8, tail_scale = 0.8, tail_shape = 0.1)),
    list(vec = rlaplacemix, scalar = rLaplaceMix, args = list(w = c(0.7, 0.3), location = c(-0.5, 1.0), scale = c(1.0, 1.4))),
    list(vec = rlaplacemixgpd, scalar = rLaplaceMixGpd, args = list(w = c(0.7, 0.3), location = c(-0.5, 1.0), scale = c(1.0, 1.4),
                                                                     threshold = 0.8, tail_scale = 0.9, tail_shape = 0.15)),
    list(vec = rlaplacegpd, scalar = rLaplaceGpd, args = list(location = -0.5, scale = 1.0, threshold = 0.8, tail_scale = 0.9, tail_shape = 0.15)),
    list(vec = rinvgaussmix, scalar = rInvGaussMix, args = list(w = c(0.6, 0.4), mean = c(1.2, 2.0), shape = c(2.5, 4.0))),
    list(vec = rinvgaussmixgpd, scalar = rInvGaussMixGpd, args = list(w = c(0.6, 0.4), mean = c(1.2, 2.0), shape = c(2.5, 4.0),
                                                                     threshold = 1.6, tail_scale = 0.7, tail_shape = 0.15)),
    list(vec = rinvgaussgpd, scalar = rInvGaussGpd, args = list(mean = 1.2, shape = 2.5, threshold = 1.6, tail_scale = 0.7, tail_shape = 0.15)),
    list(vec = rinvgauss, scalar = rInvGauss, args = list(mean = 2.0, shape = 3.0)),
    list(vec = ramorosomix, scalar = rAmorosoMix, args = list(w = c(0.5, 0.3, 0.2), loc = c(0.0, 0.5, 1.0), scale = c(1.0, 1.2, 1.5),
                                                             shape1 = c(2.0, 3.0, 4.0), shape2 = c(1.1, 1.2, 1.3))),
    list(vec = ramorosomixgpd, scalar = rAmorosoMixGpd, args = list(w = c(0.5, 0.3, 0.2), loc = c(0.0, 0.5, 1.0), scale = c(1.0, 1.2, 1.5),
                                                                   shape1 = c(2.0, 3.0, 4.0), shape2 = c(1.1, 1.2, 1.3),
                                                                   threshold = 1.6, tail_scale = 0.8, tail_shape = 0.15)),
    list(vec = ramorosogpd, scalar = rAmorosoGpd, args = list(loc = 0.0, scale = 1.0, shape1 = 2.0, shape2 = 1.1,
                                                             threshold = 1.6, tail_scale = 0.8, tail_shape = 0.15)),
    list(vec = ramoroso, scalar = rAmoroso, args = list(loc = 0.0, scale = 1.2, shape1 = 2.0, shape2 = 1.5)),
    list(vec = rcauchy_vec, scalar = rCauchy, args = list(location = 0.0, scale = 1.1)),
    list(vec = rcauchymix, scalar = rCauchyMix, args = list(w = c(0.7, 0.3), location = c(-0.5, 0.8), scale = c(1.0, 1.5)))
  )

  for (spec in r_specs) {
    set.seed(123)
    vec <- do.call(spec$vec, c(list(n = 7), spec$args))
    set.seed(123)
    scalar <- vapply(
      seq_len(7),
      function(i) do.call(spec$scalar, c(list(n = 1), spec$args)),
      numeric(1)
    )
    expect_equal(vec, scalar, tolerance = 1e-10)
  }
})
