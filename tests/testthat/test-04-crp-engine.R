testthat::test_that("fit.dpm CRP gamma runs without K and returns SB-compatible draws", {
  set.seed(1)
  dat <- data.frame(y = stats::rgamma(120, shape = 2, scale = 1))

  fit_crp <- DPmixGPD::fit.dpm(
    y ~ 0,
    data   = dat,
    kernel = "gamma",
    dp_rep = "crp",
    dp_ctrl = list(m_aux = 3),
    mcmc   = list(n_iter = 400, burn_in = 200, thin = 2, chains = 1),
    alpha  = 0.05
  )

  testthat::expect_true(inherits(fit_crp, "mixgpd_fit"))

  # S3 basics should not error
  testthat::expect_output(print(fit_crp), "Dirichlet process mixture model")

  testthat::expect_silent(summary(fit_crp))

  draws <- fit_crp$mcmc_draws
  testthat::expect_true(inherits(draws, "mcmc.list"))

  mat <- as.matrix(draws)
  testthat::expect_true("alpha" %in% colnames(mat))
  testthat::expect_true(any(grepl("^w\\[", colnames(mat))))

  wcols <- grep("^w\\[", colnames(mat), value = TRUE)
  wsum <- rowSums(mat[, wcols, drop = FALSE])
  testthat::expect_true(all(is.finite(wsum)))
  testthat::expect_true(all(abs(wsum - 1) < 1e-6))
})


testthat::test_that("fit.dpm CRP unconditional gamma runs and predict() works (density/cdf/sample/quantile)", {
  set.seed(1)
  dat <- data.frame(y = stats::rgamma(200, shape = 2, scale = 1))

  fit_crp <- DPmixGPD::fit.dpm(
    y ~ 0,
    data   = dat,
    kernel = "gamma",
    dp_rep = "crp",
    dp_ctrl = list(m_aux = 3),
    mcmc   = list(n_iter = 600, burn_in = 300, thin = 2, chains = 1),
    alpha  = 0.05
  )

  testthat::expect_true(inherits(fit_crp, "mixgpd_fit"))
  testthat::expect_true(inherits(fit_crp$mcmc_draws, "mcmc.list"))

  draws <- as.matrix(fit_crp$mcmc_draws)
  testthat::expect_true(is.matrix(draws))
  testthat::expect_true(nrow(draws) > 10)
  testthat::expect_true("alpha" %in% colnames(draws))

  # should be SB-compatible columns (w[.], shape[.], scale[.])
  testthat::expect_true(any(grepl("^w\\[",     colnames(draws))))
  testthat::expect_true(any(grepl("^shape\\[", colnames(draws))))
  testthat::expect_true(any(grepl("^scale\\[", colnames(draws))))

  grid <- seq(0, 8, length.out = 200)

  pd <- stats::predict(fit_crp, newdata = grid, type = "density")
  testthat::expect_true(is.numeric(pd))
  testthat::expect_length(pd, length(grid))
  testthat::expect_true(all(is.finite(pd)))
  testthat::expect_true(all(pd >= 0))

  pc <- stats::predict(fit_crp, newdata = grid, type = "cdf")
  testthat::expect_true(is.numeric(pc))
  testthat::expect_length(pc, length(grid))
  testthat::expect_true(all(is.finite(pc)))
  testthat::expect_true(all(pc >= 0 & pc <= 1))
  testthat::expect_true(all(diff(pc) >= -1e-8))

  ys <- stats::predict(fit_crp, type = "sample", n_samples = 1000L)
  testthat::expect_true(is.numeric(ys))
  testthat::expect_length(ys, 1000L)
  testthat::expect_true(all(is.finite(ys)))
  testthat::expect_true(all(ys >= 0))

  qq <- stats::predict(fit_crp, type = "quantile", probs = c(0.1, 0.5, 0.9))
  testthat::expect_true(is.matrix(qq))
  testthat::expect_true(.has_dimnames(qq))
  testthat::expect_true(all(c("mean","sd","lower","upper") %in% colnames(qq)))
  testthat::expect_equal(nrow(qq), 3L)
  .expect_ci_order(qq)

  testthat::expect_true(qq["q0.1","mean"] <= qq["q0.5","mean"])
  testthat::expect_true(qq["q0.5","mean"] <= qq["q0.9","mean"])
})


testthat::test_that("fit.TE CRP unconditional gamma works and ate/qte/plot work", {
  set.seed(1)
  dat <- data.frame(
    y  = stats::rgamma(220, shape = 2, rate = 1),
    A  = stats::rbinom(220, 1, 0.5),
    x1 = stats::rnorm(220),
    x2 = stats::rnorm(220)
  )

  fit_te_crp <- DPmixGPD::fit.TE(
    y ~ 0,
    data   = dat,
    A      = "A",
    kernel = "gamma",
    tail   = FALSE,
    priors = list(),
    trans  = list(),
    intercept = TRUE,
    dp_rep = "crp",
    dp_ctrl = list(m_aux = 3),
    mcmc   = list(n_iter = 700, burn_in = 350, thin = 2, chains = 1),
    alpha  = 0.05
  )

  testthat::expect_true(inherits(fit_te_crp, "mixgpd_te_fit"))
  testthat::expect_true(all(c("fit_trt","fit_con","spec_trt","spec_con") %in% names(fit_te_crp)))

  d1 <- as.matrix(fit_te_crp$fit_trt$mcmc_draws)
  d0 <- as.matrix(fit_te_crp$fit_con$mcmc_draws)
  testthat::expect_true(nrow(d1) > 10 && nrow(d0) > 10)

  ate0 <- DPmixGPD::ate(fit_te_crp, level = 0.95)
  testthat::expect_true(is.matrix(ate0))
  testthat::expect_equal(nrow(ate0), 1L)
  testthat::expect_true(all(c("mean","sd","lower","upper") %in% colnames(ate0)))
  .expect_ci_order(ate0)

  qte0 <- DPmixGPD::qte(fit_te_crp, probs = c(0.1, 0.5, 0.9), level = 0.95)
  testthat::expect_true(is.matrix(qte0))
  testthat::expect_equal(nrow(qte0), 3L)
  testthat::expect_true(all(c("mean","sd","lower","upper") %in% colnames(qte0)))
  .expect_ci_order(qte0)

  p1 <- graphics::plot(fit_te_crp)
  testthat::expect_true(inherits(p1, "ggplot"))

  p2 <- graphics::plot(fit_te_crp, effect = "quantile", tau = seq(0.1, 0.9, by = 0.1))
  testthat::expect_true(inherits(p2, "ggplot"))
})


.tiny_mcmc <- function() list(n_iter = 180L, burn_in = 60L, thin = 2L, chains = 1L)

 testthat::test_that("TE model CRP: unconditional ATE/QTE run; conditional CATE/CQTE error", {
  set.seed(5)
  n <- 200
  dat <- data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::runif(n),
    A  = rbinom(n, 1, 0.5)
  )
  # make the two arms differ a bit
  mu <- 1 + 0.5 * dat$x1 + 0.25 * dat$x2 + 0.75 * dat$A
  dat$y <- stats::rgamma(n, shape = 2, scale = exp(mu) / 2)

  fit_te <- DPmixGPD::fit.TE(y ~ x1 + x2, data = dat, A = "A",
                            kernel = "gamma",
                            dp_rep = "crp",
                            dp_ctrl = list(m_aux = 5),
                            mcmc = .tiny_mcmc())

	  # Regression TE with dp_rep='crp' currently does not retain the per-observation
	  # cluster indicators needed for arm-averaged effects.
	  testthat::expect_error(DPmixGPD::ate(fit_te),
	                         regexp = "CRP regression requires draws for z", ignore.case = TRUE)
	  testthat::expect_error(DPmixGPD::qte(fit_te, tau = c(0.25, 0.5, 0.9)),
	                         regexp = "CRP regression requires draws for z", ignore.case = TRUE)

  xnew <- data.frame(x1 = c(0, 1), x2 = c(0.2, 0.8))
	  testthat::expect_error(DPmixGPD::cate(fit_te, x = xnew),
	                         regexp = "CRP regression requires draws for z|stick_breaking", ignore.case = TRUE)
	  testthat::expect_error(DPmixGPD::cqte(fit_te, x = xnew, tau = 0.5),
	                         regexp = "CRP regression requires draws for z|stick_breaking", ignore.case = TRUE)
})


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


