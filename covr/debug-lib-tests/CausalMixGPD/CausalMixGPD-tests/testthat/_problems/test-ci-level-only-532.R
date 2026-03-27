# Extracted from test-ci-level-only.R:532

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "CausalMixGPD", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
if (!identical(Sys.getenv("DPMIXGPD_CI_COVERAGE_ONLY"), "1")) {
  testthat::skip("Coverage-only suite disabled. Set DPMIXGPD_CI_COVERAGE_ONLY=1 to run.")
}
`%||%` <- function(a, b) if (!is.null(a)) a else b
.coverage_quiet <- function(expr) {
  nullfile <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  utils::capture.output(result <- force(expr), file = nullfile)
  result
}
.coverage_cache <- new.env(parent = emptyenv())
.coverage_cached <- function(key, expr) {
  if (exists(key, envir = .coverage_cache, inherits = FALSE)) {
    return(get(key, envir = .coverage_cache, inherits = FALSE))
  }
  value <- force(expr)
  assign(key, value, envir = .coverage_cache)
  value
}
.coverage_mcmc <- function(seed = 1L, niter = 20L, nburnin = 5L) {
  list(niter = niter, nburnin = nburnin, thin = 1L, nchains = 1L, seed = seed)
}
.coverage_kernel_case <- function(kernel) {
  switch(
    kernel,
    normal = list(
      prefix = "norm",
      values = list(mean = c(-0.75, 0.25, 1.5), sd = c(0.6, 1.0, 1.4)),
      scalar = list(mean = 0.3, sd = 1.1),
      x = c(-1.5, 0.0, 1.5)
    ),
    gamma = list(
      prefix = "gamma",
      values = list(shape = c(2, 4, 6), scale = c(0.8, 1.2, 1.8)),
      scalar = list(shape = 3, scale = 1.4),
      x = c(0.5, 1.5, 3.0)
    ),
    lognormal = list(
      prefix = "lognormal",
      values = list(meanlog = c(-0.2, 0.3, 0.9), sdlog = c(0.4, 0.7, 1.0)),
      scalar = list(meanlog = 0.4, sdlog = 0.7),
      x = c(0.4, 1.2, 3.5)
    ),
    invgauss = list(
      prefix = "invgauss",
      values = list(mean = c(0.8, 1.5, 2.5), shape = c(2, 4, 8)),
      scalar = list(mean = 1.2, shape = 5),
      x = c(0.5, 1.0, 2.5)
    ),
    laplace = list(
      prefix = "laplace",
      values = list(location = c(-1, 0.5, 2), scale = c(0.6, 1.0, 1.4)),
      scalar = list(location = 0.4, scale = 1.0),
      x = c(-1.5, 0.0, 1.8)
    ),
    cauchy = list(
      prefix = "cauchy",
      values = list(location = c(-1, 0.5, 1.5), scale = c(0.8, 1.2, 1.6)),
      scalar = list(location = 0.25, scale = 1.1),
      x = c(-2.0, 0.0, 2.0)
    ),
    amoroso = list(
      prefix = "amoroso",
      values = list(
        loc = c(0, 0.2, 0.5),
        scale = c(1.0, 1.5, 2.0),
        shape1 = c(1.5, 2.0, 3.0),
        shape2 = c(0.8, 1.2, 1.6)
      ),
      scalar = list(loc = 0.1, scale = 1.4, shape1 = 2.0, shape2 = 1.2),
      x = c(0.3, 1.0, 3.0)
    )
  )
}
.coverage_numeric <- function(x, n) {
  testthat::expect_type(x, "double")
  testthat::expect_length(x, n)
  testthat::expect_true(all(is.finite(x) | is.infinite(x)))
}
.coverage_run_kernel_wrappers <- function(kernel) {
  case <- .coverage_kernel_case(kernel)
  prefix <- case$prefix
  w <- c(0.5, 0.3, 0.2)
  probs <- c(0.25, 0.75)
  tail_args <- list(threshold = max(case$x) * 0.7, tail_scale = 1.1, tail_shape = 0.15)

  d_mix <- get(paste0("d", prefix, "mix"), mode = "function")
  p_mix <- get(paste0("p", prefix, "mix"), mode = "function")
  q_mix <- get(paste0("q", prefix, "mix"), mode = "function")
  r_mix <- get(paste0("r", prefix, "mix"), mode = "function")

  .coverage_numeric(do.call(d_mix, c(list(x = case$x, w = w), case$values, list(log = FALSE))), length(case$x))
  .coverage_numeric(do.call(p_mix, c(list(q = case$x, w = w), case$values, list(lower.tail = TRUE, log.p = FALSE))), length(case$x))
  .coverage_numeric(do.call(q_mix, c(list(p = probs, w = w), case$values, list(lower.tail = TRUE, log.p = FALSE))), length(probs))
  .coverage_numeric(do.call(r_mix, c(list(n = 4L, w = w), case$values)), 4L)

  if (!identical(kernel, "cauchy")) {
    d_mix_gpd <- get(paste0("d", prefix, "mixgpd"), mode = "function")
    p_mix_gpd <- get(paste0("p", prefix, "mixgpd"), mode = "function")
    q_mix_gpd <- get(paste0("q", prefix, "mixgpd"), mode = "function")
    r_mix_gpd <- get(paste0("r", prefix, "mixgpd"), mode = "function")

    d_gpd <- get(paste0("d", prefix, "gpd"), mode = "function")
    p_gpd <- get(paste0("p", prefix, "gpd"), mode = "function")
    q_gpd <- get(paste0("q", prefix, "gpd"), mode = "function")
    r_gpd <- get(paste0("r", prefix, "gpd"), mode = "function")

    .coverage_numeric(do.call(d_mix_gpd, c(list(x = case$x, w = w), case$values, tail_args, list(log = FALSE))), length(case$x))
    .coverage_numeric(do.call(p_mix_gpd, c(list(q = case$x, w = w), case$values, tail_args, list(lower.tail = TRUE, log.p = FALSE))), length(case$x))
    .coverage_numeric(do.call(q_mix_gpd, c(list(p = probs, w = w), case$values, tail_args, list(lower.tail = TRUE, log.p = FALSE))), length(probs))
    .coverage_numeric(do.call(r_mix_gpd, c(list(n = 4L, w = w), case$values, tail_args)), 4L)

    .coverage_numeric(do.call(d_gpd, c(list(x = case$x), case$scalar, tail_args, list(log = FALSE))), length(case$x))
    .coverage_numeric(do.call(p_gpd, c(list(q = case$x), case$scalar, tail_args, list(lower.tail = TRUE, log.p = FALSE))), length(case$x))
    .coverage_numeric(do.call(q_gpd, c(list(p = probs), case$scalar, tail_args, list(lower.tail = TRUE, log.p = FALSE))), length(probs))
    .coverage_numeric(do.call(r_gpd, c(list(n = 4L), case$scalar, tail_args)), 4L)
  }
}
.coverage_conditional_fit <- function() {
  .coverage_cached("conditional_fit", {
    set.seed(41)
    n <- 24L
    X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
    y <- abs(1 + X[, 1] + 0.4 * X[, 2] + stats::rnorm(n, sd = 0.25)) + 0.2

    fit <- dpmgpd(
      x = y,
      X = X,
      backend = "crp",
      kernel = "gamma",
      components = 3,
      mcmc = c(.coverage_mcmc(seed = 41L), list(show_progress = FALSE, quiet = TRUE))
    )

    list(fit = fit, y = y, X = X)
  })
}
.coverage_unconditional_fit <- function() {
  .coverage_cached("unconditional_fit", {
    y <- sim_bulk_tail(n = 24, seed = 7)
    fit <- dpmix(
      x = y,
      backend = "sb",
      kernel = "normal",
      components = 3,
      mcmc = c(.coverage_mcmc(seed = 7L), list(show_progress = FALSE, quiet = TRUE))
    )

    list(fit = fit, y = y)
  })
}
.coverage_spliced_bundle <- function() {
  .coverage_cached("spliced_bundle", {
    set.seed(13)
    y <- sim_bulk_tail(n = 28, seed = 13)
    X <- cbind(x1 = stats::rnorm(28), x2 = stats::runif(28))

    build_nimble_bundle(
      y = y,
      X = X,
      backend = "spliced",
      kernel = "gamma",
      GPD = TRUE,
      components = 3,
      param_specs = list(
        gpd = list(
          threshold = list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1)),
          tail_scale = list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1)),
          tail_shape = list(mode = "fixed", value = 0.1)
        )
      ),
      mcmc = .coverage_mcmc(seed = 13L)
    )
  })
}
.coverage_causal_fit <- function() {
  .coverage_cached("causal_fit", {
    sim <- sim_causal_qte(n = 26, seed = 29)
    sim$y <- abs(sim$y) + 0.2

    fit <- dpmix.causal(
      x = sim$y,
      X = as.matrix(sim$X),
      treat = sim$t,
      backend = c("sb", "crp"),
      kernel = c("normal", "gamma"),
      components = c(3, 3),
      PS = "logit",
      mcmc = c(.coverage_mcmc(seed = 29L), list(show_progress = FALSE))
    )

    list(fit = fit, sim = sim)
  })
}
.coverage_cluster_fit <- function() {
  .coverage_cached("cluster_fit", {
    set.seed(53)
    dat <- data.frame(
      y = abs(stats::rnorm(20)) + 0.2,
      x1 = stats::rnorm(20),
      x2 = stats::runif(20)
    )

    fit <- dpmix.cluster(
      y ~ x1 + x2,
      data = dat,
      kernel = "normal",
      components = 4,
      type = "weights",
      mcmc = .coverage_mcmc(seed = 53L)
    )

    list(fit = fit, data = dat)
  })
}
.coverage_ns_fun <- function(name) {
  getFromNamespace(name, "CausalMixGPD")
}
.coverage_mock_mixgpd_params <- function(empty = FALSE) {
  out <- if (empty) {
    list()
  } else {
    list(
      alpha = 1.5,
      w = c(0.6, 0.3, 0.1),
      mu = c(1, 2, 3),
      sigma = c(0.5, 1, 1.5),
      beta_mu = matrix(
        c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
        nrow = 3,
        ncol = 2,
        dimnames = list(paste0("comp", 1:3), c("x1", "x2"))
      )
    )
  }
  class(out) <- "mixgpd_params"
  out
}
.coverage_mock_mixgpd_params_pair <- function() {
  out <- list(
    treated = .coverage_mock_mixgpd_params(),
    control = .coverage_mock_mixgpd_params()
  )
  class(out) <- "mixgpd_params_pair"
  out
}
.coverage_mock_qte <- function() {
  out <- list(
    type = "qte",
    probs = c(0.25, 0.5, 0.75),
    grid = c(0.25, 0.5, 0.75),
    n_pred = 5,
    level = 0.95,
    interval = "credible",
    meta = list(
      backend = list(trt = "sb", con = "sb"),
      kernel = list(trt = "normal", con = "normal"),
      GPD = list(trt = FALSE, con = FALSE)
    ),
    ps = stats::runif(5),
    x = matrix(stats::rnorm(10), nrow = 5, ncol = 2),
    qte = list(
      fit = data.frame(
        id = rep(1:5, each = 3),
        index = rep(c(0.25, 0.5, 0.75), times = 5),
        estimate = stats::rnorm(15),
        lower = stats::rnorm(15) - 0.5,
        upper = stats::rnorm(15) + 0.5
      )
    ),
    fit = matrix(stats::rnorm(15), nrow = 5, ncol = 3),
    lower = matrix(stats::rnorm(15) - 0.5, nrow = 5, ncol = 3),
    upper = matrix(stats::rnorm(15) + 0.5, nrow = 5, ncol = 3),
    trt = list(
      fit = data.frame(
        estimate = stats::rnorm(15),
        id = rep(1:5, 3),
        index = rep(c(0.25, 0.5, 0.75), each = 5)
      )
    ),
    con = list(
      fit = data.frame(
        estimate = stats::rnorm(15),
        id = rep(1:5, 3),
        index = rep(c(0.25, 0.5, 0.75), each = 5)
      )
    )
  )
  class(out) <- "causalmixgpd_qte"
  out
}
.coverage_mock_ate <- function() {
  out <- list(
    type = "ate",
    n_pred = 5,
    level = 0.95,
    interval = "credible",
    nsim_mean = 200,
    meta = list(
      backend = list(trt = "sb", con = "sb"),
      kernel = list(trt = "normal", con = "normal"),
      GPD = list(trt = FALSE, con = FALSE)
    ),
    ps = stats::runif(5),
    x = matrix(stats::rnorm(10), nrow = 5, ncol = 2),
    ate = list(
      fit = data.frame(
        id = 1:5,
        estimate = stats::rnorm(5),
        lower = stats::rnorm(5) - 0.5,
        upper = stats::rnorm(5) + 0.5
      )
    ),
    fit = stats::rnorm(5),
    lower = stats::rnorm(5) - 0.5,
    upper = stats::rnorm(5) + 0.5,
    trt = list(fit = data.frame(estimate = stats::rnorm(5), id = 1:5)),
    con = list(fit = data.frame(estimate = stats::rnorm(5), id = 1:5))
  )
  class(out) <- "causalmixgpd_ate"
  out
}
.coverage_mock_mixgpd_predict <- function(type = "quantile") {
  out <- switch(
    type,
    quantile = list(
      fit = data.frame(
        estimate = c(1, 2, 3),
        index = c(0.25, 0.5, 0.75),
        lower = c(0.5, 1.5, 2.5),
        upper = c(1.5, 2.5, 3.5)
      ),
      type = "quantile",
      grid = c(0.25, 0.5, 0.75)
    ),
    sample = list(fit = stats::rnorm(100), type = "sample", grid = NULL),
    mean = list(fit = data.frame(estimate = 5.2, lower = 4.8, upper = 5.6), type = "mean", grid = NULL),
    density = {
      y_grid <- seq(0, 5, length.out = 20)
      list(
        fit = data.frame(
          id = rep(1L, 20),
          y = y_grid,
          density = stats::dnorm(y_grid, mean = 2.5, sd = 1),
          lower = stats::dnorm(y_grid, mean = 2.5, sd = 1) - 0.05,
          upper = stats::dnorm(y_grid, mean = 2.5, sd = 1) + 0.05
        ),
        type = "density",
        grid = y_grid
      )
    },
    survival = {
      y_grid <- seq(0, 5, length.out = 20)
      list(
        fit = data.frame(
          id = rep(1L, 20),
          y = y_grid,
          survival = 1 - stats::pnorm(y_grid, mean = 2.5, sd = 1),
          lower = pmax(0, 1 - stats::pnorm(y_grid, mean = 2.5, sd = 1) - 0.05),
          upper = pmin(1, 1 - stats::pnorm(y_grid, mean = 2.5, sd = 1) + 0.05)
        ),
        type = "survival",
        grid = y_grid
      )
    },
    location = list(
      fit = data.frame(
        mean = 5.0, mean_lower = 4.5, mean_upper = 5.5,
        median = 4.9, median_lower = 4.4, median_upper = 5.4
      ),
      type = "location",
      grid = NULL
    )
  )
  class(out) <- "mixgpd_predict"
  out
}
.coverage_mock_causal_predict_plots <- function() {
  out <- list(
    trt_control = ggplot2::ggplot() + ggplot2::ggtitle("trt_control"),
    treatment_effect = ggplot2::ggplot() + ggplot2::ggtitle("treatment_effect")
  )
  class(out) <- c("causalmixgpd_causal_predict_plots", "list")
  out
}
.coverage_mock_mixgpd_predict_plots <- function() {
  p <- ggplot2::ggplot() + ggplot2::ggtitle("predict_plot")
  class(p) <- c("mixgpd_predict_plots", class(p))
  p
}
.coverage_mock_mixgpd_fit_plots <- function() {
  out <- list(
    traceplot = ggplot2::ggplot() + ggplot2::ggtitle("traceplot"),
    density = ggplot2::ggplot() + ggplot2::ggtitle("density")
  )
  class(out) <- c("mixgpd_fit_plots", "list")
  out
}
.coverage_mock_mixgpd_fitted <- function() {
  n <- 20L
  fit_vals <- stats::rnorm(n, mean = 5)
  y_obs <- fit_vals + stats::rnorm(n, sd = 0.5)
  out <- data.frame(
    fit = fit_vals,
    lower = fit_vals - 0.5,
    upper = fit_vals + 0.5,
    residuals = y_obs - fit_vals
  )
  class(out) <- c("mixgpd_fitted", "data.frame")
  attr(out, "object") <- list(
    data = list(y = y_obs),
    spec = list(meta = list(backend = "sb", kernel = "normal", GPD = FALSE))
  )
  attr(out, "level") <- 0.95
  attr(out, "interval") <- "credible"
  out
}
.coverage_mock_causal_fit_plots <- function() {
  treated <- .coverage_mock_mixgpd_fit_plots()
  control <- .coverage_mock_mixgpd_fit_plots()
  out <- list(treated = treated, control = control)
  class(out) <- c("causalmixgpd_causal_fit_plots", "list")
  out
}

# test -------------------------------------------------------------------------
skip_if_not_test_level("ci")
skip_if_not_installed("ggplot2")
skip_if_not_installed("nimble")
print_causal_bundle <- .coverage_ns_fun("print.causalmixgpd_causal_bundle")
summary_causal_bundle <- .coverage_ns_fun("summary.causalmixgpd_causal_bundle")
print_params <- .coverage_ns_fun("print.mixgpd_params")
print_params_pair <- .coverage_ns_fun("print.mixgpd_params_pair")
causal <- .coverage_causal_fit()
