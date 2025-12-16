.extract_idx <- function(nms, prefix) {
  # prefix like "w", "shape", "scale"
  pat <- paste0("^", prefix, "\\[(\\d+)\\]$")
  hit <- regmatches(nms, regexec(pat, nms))
  idx <- vapply(hit, function(z) if (length(z) == 2) z[2] else NA_character_, character(1))
  idx <- idx[!is.na(idx)]
  as.integer(idx)
}

.expect_component_index_alignment <- function(draws_mat) {
  nms <- colnames(draws_mat)

  iw <- sort(unique(.extract_idx(nms, "w")))
  is <- sort(unique(.extract_idx(nms, "shape")))
  ic <- sort(unique(.extract_idx(nms, "scale")))

  testthat::expect_true(length(iw) > 0)
  testthat::expect_true(length(is) > 0)
  testthat::expect_true(length(ic) > 0)

  testthat::expect_equal(iw, is, info = "w[j] and shape[j] use different component index sets")
  testthat::expect_equal(iw, ic, info = "w[j] and scale[j] use different component index sets")
}

testthat::test_that("CRP fit.dpm has aligned component indices across w/shape/scale", {
  set.seed(1)
  dat <- data.frame(y = stats::rgamma(160, shape = 2, scale = 1))

  fit_crp <- DPmixGPD::fit.dpm(
    y ~ 0, data = dat,
    kernel = "gamma",
    dp_rep = "crp",
    dp_ctrl = list(m_aux = 3),
    mcmc = list(n_iter = 500, burn_in = 250, thin = 2, chains = 1)
  )

  draws <- as.matrix(fit_crp$mcmc_draws)
  .expect_component_index_alignment(draws)
})

testthat::test_that("CRP fit.TE has aligned component indices across w/shape/scale (both arms)", {
  set.seed(1)
  dat <- data.frame(
    y  = stats::rgamma(200, shape = 2, rate = 1),
    A  = stats::rbinom(200, 1, 0.5),
    x1 = stats::rnorm(200)
  )

  fit_te_crp <- DPmixGPD::fit.TE(
    y ~ 0,
    data = dat,
    A = "A",
    kernel = "gamma",
    tail = FALSE,
    dp_rep = "crp",
    dp_ctrl = list(m_aux = 3),
    mcmc = list(n_iter = 600, burn_in = 300, thin = 2, chains = 1)
  )

  d1 <- as.matrix(fit_te_crp$fit_trt$mcmc_draws)
  d0 <- as.matrix(fit_te_crp$fit_con$mcmc_draws)

  .expect_component_index_alignment(d1)
  .expect_component_index_alignment(d0)
})
