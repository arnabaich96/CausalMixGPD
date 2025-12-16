
# Helper utilities shared across DPmixGPD tests
testthat::local_edition(3)

.has_dimnames <- function(x) {
  !is.null(dimnames(x)) && length(dimnames(x)) == 2L
}

.expect_ci_order <- function(mat) {
  testthat::expect_true(all(mat[, "lower"] <= mat[, "mean"]))
  testthat::expect_true(all(mat[, "mean"]  <= mat[, "upper"]))
}

.expect_cols <- function(x, cols) {
  testthat::expect_true(all(cols %in% colnames(x)))
}

# Fast constructor for a minimal mixgpd_fit object (for unit tests w/o MCMC)
.make_fake_mixgpd_fit <- function(draws, kernel = "gamma", tail = "none") {
  stopifnot(is.matrix(draws))
  spec <- list(kernel = kernel, tail = tail)
  structure(
    list(
      call = match.call(),
      formula = y ~ 0,
      kernel = kernel,
      tail = tail,
      dp_rep = "stick_breaking",
      priors = list(),
      dp_ctrl = list(K = NA_integer_),
      trans = list(),
      alpha = 0.05,
      spec = spec,
      N = NA_integer_,
      x_range = NULL,
      mcmc = list(),
      mcmc_draws = draws
    ),
    class = "mixgpd_fit"
  )
}

.make_fake_te_fit <- function(draws_trt, draws_con, tail = c(FALSE, FALSE)) {
  fit_trt <- .make_fake_mixgpd_fit(draws_trt, kernel = "gamma", tail = ifelse(isTRUE(tail[1]), "gpd", "none"))
  fit_con <- .make_fake_mixgpd_fit(draws_con, kernel = "gamma", tail = ifelse(isTRUE(tail[2]), "gpd", "none"))
  out <- list(
    fit_trt  = fit_trt,
    fit_con  = fit_con,
    spec_trt = fit_trt$spec,
    spec_con = fit_con$spec,
    tail     = c(trt = isTRUE(tail[1]), con = isTRUE(tail[2]))
  )
  class(out) <- "mixgpd_te_fit"
  out
}
