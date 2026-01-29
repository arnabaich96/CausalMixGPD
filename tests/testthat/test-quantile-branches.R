test_that("Quantile helpers handle log.p and lower.tail branches (mix + mixgpd)", {
  w3 <- c(0.2, 0.3, 0.5)
  p <- c(0.1, 0.5, 0.9)

  check_q_branches <- function(qfun, args, tol = 1e-5) {
    q_plain <- do.call(qfun, c(list(p = p), args))
    q_logp <- do.call(qfun, c(list(p = log(p), log.p = TRUE), args))
    expect_equal(q_logp, q_plain, tolerance = tol)

    q_upper <- do.call(qfun, c(list(p = p, lower.tail = FALSE), args))
    q_ref <- do.call(qfun, c(list(p = 1 - p), args))
    expect_equal(q_upper, q_ref, tolerance = tol)

    # boundaries should not error and should return correct length
    q_edge <- do.call(qfun, c(list(p = c(0, 1)), args))
    expect_true(is.numeric(q_edge))
    expect_equal(length(q_edge), 2L)
  }

  # Mix (bulk only)
  check_q_branches(
    qCauchyMix,
    list(w = w3, location = c(-1, 1, 3), scale = c(1.2, 0.8, 0.6)),
    tol = 1e-4
  )
  check_q_branches(
    qNormMix,
    list(w = w3, mean = c(-1, 0.5, 2), sd = c(1.0, 0.6, 1.8)),
    tol = 1e-4
  )
  check_q_branches(
    qLaplaceMix,
    list(w = w3, location = c(-1, 0, 1.5), scale = c(0.6, 1.0, 1.4)),
    tol = 1e-4
  )
  check_q_branches(
    qGammaMix,
    list(w = w3, shape = c(2, 3, 5), scale = c(1.0, 0.7, 1.2)),
    tol = 1e-4
  )
  check_q_branches(
    qLognormalMix,
    list(w = w3, meanlog = c(0, 0.3, 0.6), sdlog = c(0.5, 0.7, 0.9)),
    tol = 1e-4
  )
  check_q_branches(
    qInvGaussMix,
    list(w = w3, mean = c(1.5, 2.5, 3.5), shape = c(4, 6, 8)),
    tol = 1e-4
  )
  check_q_branches(
    qAmorosoMix,
    list(
      w = w3,
      loc = c(0, 0.3, 0.6),
      scale = c(1, 1, 1),
      shape1 = c(2, 3, 5),
      shape2 = c(1.0, 1.3, 1.6)
    ),
    tol = 2e-4
  )

  # Mix + GPD tail (splice)
  gpd_tail <- list(threshold = 3, tail_scale = 1.0, tail_shape = 0.2)
  check_q_branches(
    qInvGaussMixGpd,
    c(list(w = w3, mean = c(1.5, 2.5, 3.5), shape = c(4, 6, 8)), gpd_tail),
    tol = 2e-4
  )
  check_q_branches(
    qAmorosoMixGpd,
    c(
      list(
        w = w3,
        loc = c(0, 0.3, 0.6),
        scale = c(1, 1, 1),
        shape1 = c(2, 3, 5),
        shape2 = c(1.0, 1.3, 1.6)
      ),
      gpd_tail
    ),
    tol = 2e-4
  )
})

