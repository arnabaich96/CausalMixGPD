# Helper utilities for DPmixGPD tests
# Keep these lightweight and deterministic.

.make_data_yonly <- function(n = 120, shape = 2, scale = 1) {
  set.seed(1)
  data.frame(y = stats::rgamma(n, shape = shape, scale = scale))
}

.make_data_reg <- function(n = 140) {
  set.seed(2)
  x1 <- stats::rnorm(n)
  x2 <- stats::runif(n, -1, 1)
  mu <- exp(0.2 + 0.5 * x1 - 0.3 * x2)
  y <- stats::rgamma(n, shape = 2, scale = mu / 2)
  data.frame(y = y, x1 = x1, x2 = x2)
}

.make_data_te <- function(n = 200) {
  set.seed(3)
  x1 <- stats::rnorm(n)
  x2 <- stats::runif(n, -1, 1)
  pA <- stats::plogis(-0.2 + 0.6 * x1 - 0.4 * x2)
  A <- stats::rbinom(n, 1, pA)
  mu0 <- exp(0.1 + 0.4 * x1)
  mu1 <- exp(0.3 + 0.4 * x1 + 0.2 * x2)
  mu <- ifelse(A == 1, mu1, mu0)
  y <- stats::rgamma(n, shape = 2, scale = mu / 2)
  data.frame(y = y, A = A, x1 = x1, x2 = x2)
}

.tiny_mcmc <- function(n_iter = 80, burn_in = 30, thin = 1, chains = 1) {
  # DPmixGPD expects these exact names; keep tiny for fast unit tests.
  list(n_iter = n_iter, burn_in = burn_in, thin = thin, chains = chains)
}

.sb_dp_ctrl <- function(K = 5L) {
  # stick_breaking backend requires K
  list(K = as.integer(K))
}

.crp_dp_ctrl <- function(m_aux = 5L) {
  # CRP backend uses Neal's auxiliary components; K is not used.
  list(m_aux = as.integer(m_aux))
}

.assert_predict_shapes <- function(pred) {
  stopifnot(is.list(pred))
  stopifnot(length(pred) >= 1)
}

# Small pre-fitted objects for snapshot tests
.get_fit_te_sb_gamma_uncond <- function(K = 5L, n = 200, seed = 1) {
  set.seed(seed)
  dat <- .make_data_te(n)
  DPmixGPD::fit.TE(y ~ 0, data = dat, A = "A",
                  kernel = "gamma",
                  dp_rep = "stick_breaking",
                  dp_ctrl = .sb_dp_ctrl(K),
                  mcmc = .tiny_mcmc(n_iter = 60, burn_in = 20),
                  compile = FALSE)
}

.get_fit_dpm_sb_gamma_uncond <- function(K = 5L, n = 200, seed = 1) {
  set.seed(seed)
  dat <- .make_data_yonly(n)
  DPmixGPD::fit.dpm(y ~ 0, data = dat,
                   kernel = "gamma",
                   dp_rep = "stick_breaking",
                   dp_ctrl = .sb_dp_ctrl(K),
                   mcmc = .tiny_mcmc(n_iter = 60, burn_in = 20))
}
.expect_cols <- function(x, cols) {
  testthat::expect_true(is.data.frame(x) || is.matrix(x))
  nm <- colnames(x)
  testthat::expect_true(!is.null(nm))
  missing <- setdiff(cols, nm)
  testthat::expect_length(missing, 0L)
  invisible(TRUE)
}

.expect_ci_order <- function(obj) {
  # works for both named numeric vectors and 1-row data.frames
  if (is.data.frame(obj)) obj <- unlist(obj[1, , drop = TRUE])
  testthat::expect_true(is.numeric(obj))
  nms <- names(obj)

  # common patterns you use in your summaries
  if (!is.null(nms) && all(c("lwr","est","upr") %in% nms)) {
    testthat::expect_lte(obj[["lwr"]], obj[["est"]])
    testthat::expect_lte(obj[["est"]], obj[["upr"]])
    return(invisible(TRUE))
  }

  if (!is.null(nms) && all(c("lower","estimate","upper") %in% nms)) {
    testthat::expect_lte(obj[["lower"]], obj[["estimate"]])
    testthat::expect_lte(obj[["estimate"]], obj[["upper"]])
    return(invisible(TRUE))
  }

  # fallback: assume first three are (lwr, est, upr)
  if (length(obj) >= 3) {
    testthat::expect_lte(obj[[1]], obj[[2]])
    testthat::expect_lte(obj[[2]], obj[[3]])
  }

  invisible(TRUE)
}
.make_fake_te_fit <- function(draws_trt, draws_con, tail = c(FALSE, FALSE)) {
  structure(
    list(
      fit_trt = structure(list(mcmc_draws = draws_trt), class = "mixgpd_fit"),
      fit_con = structure(list(mcmc_draws = draws_con), class = "mixgpd_fit"),
      tail    = c(trt = isTRUE(tail[1]), con = isTRUE(tail[2])),
      spec_trt = list(mode = "response_only"),
      spec_con = list(mode = "response_only")
    ),
    class = "mixgpd_te_fit"
  )
}
