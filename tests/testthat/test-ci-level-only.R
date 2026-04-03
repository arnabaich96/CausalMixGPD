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

test_that("coverage-only suite exercises internal summary and visualization helpers directly", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")
  if (identical(Sys.getenv("DPMIXGPD_SKIP_COVR_HELPERS_BLOCK"), "1")) {
    skip("Helper coverage block disabled for isolation.")
  }

  validate_reserved <- .coverage_ns_fun(".validate_nimble_reserved_names")
  coerce_fit_df <- .coverage_ns_fun(".coerce_fit_df")
  compute_interval <- .coverage_ns_fun(".compute_interval")
  posterior_summarize <- .coverage_ns_fun(".posterior_summarize")
  truncate_one_draw <- .coverage_ns_fun(".truncate_components_one_draw")
  wrap_plotly <- .coverage_ns_fun(".wrap_plotly")
  plot_palette <- .coverage_ns_fun(".plot_palette")
  plot_theme <- .coverage_ns_fun(".plot_theme")
  extract_nimble_code <- .coverage_ns_fun(".extract_nimble_code")
  wrap_nimble_code <- .coverage_ns_fun(".wrap_nimble_code")
  plot_quantile_pred <- .coverage_ns_fun(".plot_quantile_pred")
  plot_sample_pred <- .coverage_ns_fun(".plot_sample_pred")
  plot_mean_pred <- .coverage_ns_fun(".plot_mean_pred")
  plot_density_pred <- .coverage_ns_fun(".plot_density_pred")
  plot_survival_pred <- .coverage_ns_fun(".plot_survival_pred")
  plot_location_pred <- .coverage_ns_fun(".plot_location_pred")

  expect_invisible(validate_reserved(c("alpha", "beta")))
  expect_error(validate_reserved(c("if", "alpha")), "reserved NIMBLE keywords")

  df <- data.frame(estimate = 1:3, lower = 0:2, upper = 2:4)
  expect_true(all(c("estimate", "lower", "upper", "id") %in% names(coerce_fit_df(df))))
  expect_equal(coerce_fit_df(c(1, 2, 3))$estimate, c(1, 2, 3))

  iv_cred <- compute_interval(stats::rnorm(1000), level = 0.95, type = "credible")
  expect_named(iv_cred, c("lower", "upper"))
  expect_true(iv_cred["lower"] < iv_cred["upper"])

  if (requireNamespace("coda", quietly = TRUE)) {
    iv_hpd <- compute_interval(stats::rnorm(1000), level = 0.95, type = "hpd")
    expect_true(iv_hpd["lower"] < iv_hpd["upper"])
  }

  summ_vec <- posterior_summarize(stats::rnorm(100), interval = "credible")
  summ_mat <- posterior_summarize(matrix(stats::rnorm(300), nrow = 3), interval = NULL)
  expect_named(summ_vec, c("estimate", "lower", "upper", "q"))
  expect_true(all(is.na(summ_mat$lower)))

  trunc_out <- truncate_one_draw(c(0.1, 0.6, 0.3), list(mu = c(1, 2, 3)), epsilon = 0.01)
  expect_equal(trunc_out$ord, c(2, 3, 1))

  wrapped <- wrap_nimble_code(quote({ a <- 1 }))
  expect_true(is.list(wrapped))
  expect_equal(deparse(extract_nimble_code(wrapped)), deparse(quote({ a <- 1 })))

  expect_length(plot_palette(12), 12)
  expect_s3_class(plot_theme(), "theme")

  p <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(1, 1))
  expect_true(inherits(wrap_plotly(p), "ggplot") || is.list(wrap_plotly(p)))

  pred_quant <- list(fit = data.frame(index = c(0.25, 0.5, 0.75), estimate = c(1, 2, 3), lower = c(0.5, 1.5, 2.5), upper = c(1.5, 2.5, 3.5)))
  pred_sample <- list(fit = stats::rnorm(100))
  pred_mean <- list(fit = data.frame(estimate = 5, lower = 4.5, upper = 5.5), draws = stats::rnorm(100, mean = 5))
  pred_density <- list(fit = data.frame(y = seq(0, 5, length.out = 20), density = stats::dnorm(seq(0, 5, length.out = 20), mean = 2.5)))
  pred_survival <- list(fit = data.frame(y = seq(0, 5, length.out = 20), survival = 1 - stats::pnorm(seq(0, 5, length.out = 20), mean = 2.5)))
  pred_location <- list(fit = data.frame(mean = 5, mean_lower = 4.5, mean_upper = 5.5, median = 4.9, median_lower = 4.4, median_upper = 5.4))

  expect_true(inherits(plot_quantile_pred(pred_quant), "gg"))
  expect_true(inherits(plot_sample_pred(pred_sample), "gg"))
  expect_true(inherits(plot_mean_pred(pred_mean), "gg"))
  expect_true(inherits(plot_density_pred(pred_density), "gg"))
  expect_true(inherits(plot_survival_pred(pred_survival), "gg"))
  expect_true(inherits(plot_location_pred(pred_location), "gg"))
})

test_that("coverage-only suite exercises causal bundle and parameter methods directly", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("nimble")

  print_causal_bundle <- .coverage_ns_fun("print.causalmixgpd_causal_bundle")
  summary_causal_bundle <- .coverage_ns_fun("summary.causalmixgpd_causal_bundle")
  print_params <- .coverage_ns_fun("print.mixgpd_params")
  print_params_pair <- .coverage_ns_fun("print.mixgpd_params_pair")

  causal <- .coverage_causal_fit()
  causal_bundle <- build_causal_bundle(
    y = causal$sim$y,
    X = as.matrix(causal$sim$X),
    A = causal$sim$t,
    backend = c("sb", "sb"),
    kernel = c("normal", "gamma"),
    GPD = FALSE,
    components = c(3, 3),
    PS = FALSE,
    mcmc_outcome = .coverage_mcmc(seed = 111L)
  )

  expect_output(print_causal_bundle(causal_bundle), "CausalMixGPD causal bundle")
  expect_output(summary_causal_bundle(causal_bundle), "CausalMixGPD causal bundle summary")
  expect_output(print_params(.coverage_mock_mixgpd_params()), "Posterior mean parameters")
  expect_output(print_params_pair(params(causal$fit)), "Posterior mean parameters")
  expect_output(print_params(.coverage_mock_mixgpd_params(empty = TRUE)), "empty")
})

test_that("coverage-only suite exercises mocked effect and plot methods directly", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  print_qte <- .coverage_ns_fun("print.causalmixgpd_qte")
  print_ate <- .coverage_ns_fun("print.causalmixgpd_ate")
  summary_qte <- .coverage_ns_fun("summary.causalmixgpd_qte")
  summary_ate <- .coverage_ns_fun("summary.causalmixgpd_ate")
  print_summary_qte <- .coverage_ns_fun("print.summary.causalmixgpd_qte")
  print_summary_ate <- .coverage_ns_fun("print.summary.causalmixgpd_ate")
  plot_predict <- .coverage_ns_fun("plot.mixgpd_predict")
  print_predict_plots <- .coverage_ns_fun("print.mixgpd_predict_plots")
  print_causal_predict_plots <- .coverage_ns_fun("print.causalmixgpd_causal_predict_plots")
  print_fit_plots <- .coverage_ns_fun("print.mixgpd_fit_plots")
  print_causal_fit_plots <- .coverage_ns_fun("print.causalmixgpd_causal_fit_plots")
  plot_fitted <- .coverage_ns_fun("plot.mixgpd_fitted")
  print_fitted_plots <- .coverage_ns_fun("print.mixgpd_fitted_plots")
  plot_qte <- .coverage_ns_fun("plot.causalmixgpd_qte")
  plot_ate <- .coverage_ns_fun("plot.causalmixgpd_ate")

  qte_obj <- .coverage_mock_qte()
  ate_obj <- .coverage_mock_ate()
  expect_output(print_qte(qte_obj), "QTE")
  expect_output(print_ate(ate_obj), "ATE")
  expect_s3_class(summary_qte(qte_obj), "summary.causalmixgpd_qte")
  expect_s3_class(summary_ate(ate_obj), "summary.causalmixgpd_ate")
  expect_output(print_summary_qte(summary_qte(qte_obj)), "QTE Summary")
  expect_output(print_summary_ate(summary_ate(ate_obj)), "ATE Summary")

  expect_true(inherits(plot_predict(.coverage_mock_mixgpd_predict("quantile")), "gg"))
  expect_true(inherits(plot_predict(.coverage_mock_mixgpd_predict("sample")), "gg"))
  expect_true(inherits(plot_predict(.coverage_mock_mixgpd_predict("mean")), "gg"))
  expect_true(inherits(plot_predict(.coverage_mock_mixgpd_predict("density")), "gg"))
  expect_true(inherits(plot_predict(.coverage_mock_mixgpd_predict("survival")), "gg"))
  expect_true(inherits(plot_predict(.coverage_mock_mixgpd_predict("location")), "gg") || is.list(plot_predict(.coverage_mock_mixgpd_predict("location"))))
  expect_silent(invisible(utils::capture.output(print_predict_plots(.coverage_mock_mixgpd_predict_plots()))))
  expect_output(print_causal_predict_plots(.coverage_mock_causal_predict_plots()), ".")
  expect_output(print_fit_plots(.coverage_mock_mixgpd_fit_plots()), "traceplot")
  expect_output(print_causal_fit_plots(.coverage_mock_causal_fit_plots()), "treated")
  expect_s3_class(plot_fitted(.coverage_mock_mixgpd_fitted()), "mixgpd_fitted_plots")
  expect_silent(invisible(utils::capture.output(print_fitted_plots(plot_fitted(.coverage_mock_mixgpd_fitted())))))

  expect_true(inherits(plot_qte(qte_obj, type = "effect"), "gg"))
  expect_true(inherits(plot_ate(ate_obj, type = "effect"), "gg"))
})

test_that("coverage-only suite exercises cluster S3 methods directly", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("nimble")

  print_cluster_bundle <- .coverage_ns_fun("print.dpmixgpd_cluster_bundle")
  summary_cluster_bundle <- .coverage_ns_fun("summary.dpmixgpd_cluster_bundle")
  plot_cluster_bundle <- .coverage_ns_fun("plot.dpmixgpd_cluster_bundle")
  print_cluster_fit <- .coverage_ns_fun("print.dpmixgpd_cluster_fit")
  summary_cluster_fit <- .coverage_ns_fun("summary.dpmixgpd_cluster_fit")
  plot_cluster_fit <- .coverage_ns_fun("plot.dpmixgpd_cluster_fit")
  print_cluster_labels <- .coverage_ns_fun("print.dpmixgpd_cluster_labels")
  summary_cluster_labels <- .coverage_ns_fun("summary.dpmixgpd_cluster_labels")
  plot_cluster_labels <- .coverage_ns_fun("plot.dpmixgpd_cluster_labels")
  print_cluster_psm <- .coverage_ns_fun("print.dpmixgpd_cluster_psm")
  summary_cluster_psm <- .coverage_ns_fun("summary.dpmixgpd_cluster_psm")
  plot_cluster_psm <- .coverage_ns_fun("plot.dpmixgpd_cluster_psm")

  cluster <- .coverage_cluster_fit()
  lbl <- predict(cluster$fit, type = "label", return_scores = TRUE)
  psm <- predict(cluster$fit, type = "psm")

  expect_output(print_cluster_bundle(cluster$fit$bundle), "Cluster bundle")
  expect_s3_class(summary_cluster_bundle(cluster$fit$bundle), "summary.dpmixgpd_cluster_bundle")
  expect_silent(plot_cluster_bundle(cluster$fit$bundle))
  expect_output(print_cluster_fit(cluster$fit), "Cluster fit")
  expect_s3_class(summary_cluster_fit(cluster$fit), "summary.dpmixgpd_cluster_fit")
  expect_silent(plot_cluster_fit(cluster$fit, which = "sizes"))
  expect_silent(plot_cluster_fit(cluster$fit, which = "summary"))
  expect_output(print_cluster_labels(lbl), "Cluster labels")
  expect_s3_class(summary_cluster_labels(lbl), "summary.dpmixgpd_cluster_labels")
  expect_silent(plot_cluster_labels(lbl, type = "certainty"))
  expect_silent(plot_cluster_labels(lbl, type = "summary"))
  expect_output(print_cluster_psm(psm), "Cluster PSM")
  expect_s3_class(summary_cluster_psm(psm), "summary.dpmixgpd_cluster_psm")
  expect_silent(plot_cluster_psm(psm, psm_max_n = nrow(psm$psm)))
})

test_that("coverage-only suite exercises registries, simulations, and distribution wrappers", {
  skip_if_not_test_level("ci")

  init_kernel_registry()
  reg <- get_kernel_registry()
  tail_reg <- get_tail_registry()
  support <- kernel_support_table(round = FALSE)

  expect_true(is.list(reg))
  expect_true(length(reg) >= 7L)
  expect_true(is.list(tail_reg))
  expect_true(is.data.frame(support))

  .coverage_numeric(dgpd(c(1.2, 1.8), threshold = 1, scale = 0.8, shape = 0.2), 2L)
  .coverage_numeric(pgpd(c(1.2, 1.8), threshold = 1, scale = 0.8, shape = 0.2), 2L)
  .coverage_numeric(qgpd(c(0.25, 0.75), threshold = 1, scale = 0.8, shape = 0.2), 2L)
  .coverage_numeric(rgpd(4L, threshold = 1, scale = 0.8, shape = 0.2), 4L)

  .coverage_numeric(dinvgauss(c(0.8, 1.4), mean = 1.5, shape = 5), 2L)
  .coverage_numeric(pinvgauss(c(0.8, 1.4), mean = 1.5, shape = 5), 2L)
  .coverage_numeric(qinvgauss(c(0.3, 0.7), mean = 1.5, shape = 5), 2L)
  .coverage_numeric(rinvgauss(4L, mean = 1.5, shape = 5), 4L)

  .coverage_numeric(damoroso(c(0.5, 1.5), loc = 0, scale = 1.4, shape1 = 2, shape2 = 1.2), 2L)
  .coverage_numeric(pamoroso(c(0.5, 1.5), loc = 0, scale = 1.4, shape1 = 2, shape2 = 1.2), 2L)
  .coverage_numeric(qamoroso(c(0.25, 0.75), loc = 0, scale = 1.4, shape1 = 2, shape2 = 1.2), 2L)
  .coverage_numeric(ramoroso(4L, loc = 0, scale = 1.4, shape1 = 2, shape2 = 1.2), 4L)

  .coverage_numeric(dcauchy_vec(c(-1, 0, 1), location = 0, scale = 1.2), 3L)
  .coverage_numeric(pcauchy_vec(c(-1, 0, 1), location = 0, scale = 1.2), 3L)
  .coverage_numeric(qcauchy_vec(c(0.25, 0.75), location = 0, scale = 1.2), 2L)
  .coverage_numeric(rcauchy_vec(4L, location = 0, scale = 1.2), 4L)

  for (kernel in names(reg)) {
    .coverage_run_kernel_wrappers(kernel)
  }

  sim_bulk <- sim_bulk_tail(n = 30, seed = 1)
  sim_causal <- sim_causal_qte(n = 20, seed = 2)
  sim_surv <- sim_survival_tail(n = 20, seed = 3)

  expect_equal(length(sim_bulk), 30L)
  expect_named(sim_causal, c("y", "t", "X", "A"))
  expect_true(is.data.frame(sim_surv))
  expect_true(all(c("time", "status", "x1", "x2") %in% names(sim_surv)))
})

test_that("coverage-only suite exercises non-causal build, fit, predict, and glue paths", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  uncond <- .coverage_unconditional_fit()
  fit_u <- uncond$fit
  cond <- .coverage_conditional_fit()
  fit_c <- cond$fit
  y <- cond$y
  X <- cond$X

  expect_s3_class(bundle(y = y, X = X, backend = "sb", kernel = "normal", components = 3, GPD = FALSE), "causalmixgpd_bundle")
  expect_s3_class(.coverage_spliced_bundle(), "causalmixgpd_bundle")

  expect_output(print(fit_u))
  expect_s3_class(summary(fit_u), "mixgpd_summary")
  expect_s3_class(params(fit_u), "mixgpd_params")

  pred_u_q <- predict(fit_u, type = "quantile", index = c(0.25, 0.75))
  pred_u_d <- predict(fit_u, y = uncond$y[1:5], type = "density")
  pred_u_s <- predict(fit_u, y = uncond$y[1:5], type = "survival")

  expect_s3_class(pred_u_q, "mixgpd_predict")
  expect_s3_class(pred_u_d, "mixgpd_predict")
  expect_s3_class(pred_u_s, "mixgpd_predict")

  expect_output(print(fit_c))
  expect_s3_class(summary(fit_c), "mixgpd_summary")
  expect_s3_class(params(fit_c), "mixgpd_params")

  pred_c_mean <- predict(fit_c, newdata =X[1:4, , drop = FALSE], type = "mean", nsim_mean = 20L)
  pred_c_q <- predict(fit_c, newdata =X[1:4, , drop = FALSE], type = "quantile", index = c(0.25, 0.75))
  pred_c_d <- predict(fit_c, newdata =X[1:2, , drop = FALSE], y = y[1:2], type = "density")
  pred_c_s <- predict(fit_c, newdata =X[1:2, , drop = FALSE], y = y[1:2], type = "survival")

  expect_s3_class(pred_c_mean, "mixgpd_predict")
  expect_s3_class(pred_c_q, "mixgpd_predict")
  expect_s3_class(pred_c_d, "mixgpd_predict")
  expect_s3_class(pred_c_s, "mixgpd_predict")
  expect_true(is.numeric(residuals(fit_c)))
  expect_true(is.data.frame(fitted(fit_c)))

  glue_check <- check_glue_validity(
    fit_c,
    grid = seq(min(y), stats::quantile(y, 0.9), length.out = 16L),
    n_draws = 5L,
    check_continuity = FALSE
  )
  expect_true(is.list(glue_check))
  expect_true(all(c("pass", "violations", "n_checked_draws") %in% names(glue_check)))

  if (requireNamespace("coda", quietly = TRUE)) {
    ess <- tryCatch(ess_summary(fit_c), error = function(e) NULL)
    if (!is.null(ess)) {
      expect_s3_class(ess, "mixgpd_ess_summary")
    }
  }
})

test_that("coverage-only suite exercises direct bundle and mcmc wrappers", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  dat <- .coverage_cached("bundle_dat", {
    set.seed(67)
    data.frame(
      y = abs(stats::rnorm(18)) + 0.25,
      x1 = stats::rnorm(18),
      x2 = stats::runif(18)
    )
  })

  b_formula <- bundle(
    formula = y ~ x1 + x2,
    data = dat,
    backend = "sb",
    kernel = "normal",
    components = 3,
    GPD = FALSE
  )
  expect_s3_class(b_formula, "causalmixgpd_bundle")

  fit_formula <- .coverage_quiet(mcmc(b_formula, niter = 20L, nburnin = 5L, thin = 1L, nchains = 1L, seed = 67L, show_progress = FALSE))
  expect_s3_class(fit_formula, "mixgpd_fit")

  b_causal <- bundle(
    formula = y ~ x1 + x2,
    data = transform(dat, A = rep(c(0L, 1L), length.out = nrow(dat))),
    treat = "A",
    backend = c("sb", "sb"),
    kernel = c("normal", "gamma"),
    components = c(3, 3),
    GPD = FALSE
  )
  expect_s3_class(b_causal, "causalmixgpd_causal_bundle")
})

test_that("coverage-only suite exercises causal workflows and treatment-effect methods", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  obj <- .coverage_causal_fit()
  fit <- obj$fit
  X <- as.matrix(obj$sim$X)

  expect_output(print(fit))
  expect_output(summary(fit), "Outcome fits")
  expect_s3_class(params(fit), "mixgpd_params_pair")

  pred_mean <- predict(fit, newdata =X[1:3, , drop = FALSE], type = "mean", nsim_mean = 20L)
  pred_quant <- predict(fit, newdata =X[1:3, , drop = FALSE], type = "quantile", p = c(0.25, 0.75))
  pred_density <- predict(fit, newdata =X[1:2, , drop = FALSE], y = obj$sim$y[1:2], type = "density")
  pred_survival <- predict(fit, newdata =X[1:2, , drop = FALSE], y = obj$sim$y[1:2], type = "survival")

  expect_s3_class(pred_mean, "causalmixgpd_causal_predict")
  expect_s3_class(pred_quant, "causalmixgpd_causal_predict")
  expect_s3_class(pred_density, "causalmixgpd_causal_predict")
  expect_s3_class(pred_survival, "causalmixgpd_causal_predict")

  cate_res <- cate(fit, newdata = X[1:3, , drop = FALSE], nsim_mean = 20L, interval = "credible")
  cqte_res <- cqte(fit, probs = c(0.25, 0.75), newdata = X[1:3, , drop = FALSE], interval = "credible")
  ate_res <- ate(fit, nsim_mean = 20L, interval = "credible")
  att_res <- att(fit, nsim_mean = 20L, interval = "credible")
  qte_res <- qte(fit, probs = c(0.25, 0.75), interval = "credible")
  qtt_res <- qtt(fit, probs = c(0.25, 0.75), interval = "credible")
  rmean_res <- ate_rmean(fit, newdata = X[1:3, , drop = FALSE], cutoff = 10, nsim_mean = 20L, interval = "credible")

  expect_s3_class(cate_res, "causalmixgpd_ate")
  expect_s3_class(cqte_res, "causalmixgpd_qte")
  expect_s3_class(ate_res, "causalmixgpd_ate")
  expect_s3_class(att_res, "causalmixgpd_ate")
  expect_s3_class(qte_res, "causalmixgpd_qte")
  expect_s3_class(qtt_res, "causalmixgpd_qte")
  expect_s3_class(rmean_res, "causalmixgpd_ate")

  expect_silent(summary(ate_res))
  expect_silent(summary(qte_res))
  expect_type(plot(ate_res), "list")
  expect_type(plot(qte_res), "list")
  expect_type(plot(pred_mean), "list")
  expect_type(plot(fit, arm = "both"), "list")
})

test_that("coverage-only suite exercises causal GPD wrapper path", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  fit <- .coverage_cached("causal_gpd_fit", {
    sim <- sim_causal_qte(n = 20, seed = 71)
    sim$y <- abs(sim$y) + 0.2

    dpmgpd.causal(
      x = sim$y,
      X = as.matrix(sim$X[, 1:2, drop = FALSE]),
      treat = sim$t,
      backend = c("sb", "sb"),
      kernel = c("gamma", "gamma"),
      components = c(3, 3),
      PS = FALSE,
      mcmc = c(.coverage_mcmc(seed = 71L), list(show_progress = FALSE))
    )
  })

  expect_s3_class(fit, "causalmixgpd_causal_fit")
})

test_that("coverage-only suite exercises cluster builders, predictors, and S3 methods", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")

  obj <- .coverage_cluster_fit()
  fit <- obj$fit
  dat <- obj$data

  psm <- predict(fit, type = "psm")
  lbl_train <- predict(fit, type = "label", return_scores = TRUE)
  lbl_new <- predict(fit, newdata = dat[1:4, , drop = FALSE], type = "label", return_scores = TRUE)

  expect_s3_class(fit, "dpmixgpd_cluster_fit")
  expect_s3_class(psm, "dpmixgpd_cluster_psm")
  expect_s3_class(lbl_train, "dpmixgpd_cluster_labels")
  expect_s3_class(lbl_new, "dpmixgpd_cluster_labels")

  expect_output(print(fit))
  expect_silent(summary(fit))
  expect_silent(plot(fit, which = "psm"))
  expect_silent(plot(fit, which = "k"))
  expect_silent(plot(fit, which = "sizes"))

  expect_output(print(lbl_train))
  expect_silent(summary(lbl_train))
  expect_silent(plot(lbl_train, type = "sizes"))
  expect_silent(plot(lbl_train, type = "certainty"))

  expect_output(print(psm))
  expect_silent(summary(psm))
  expect_silent(plot(psm, psm_max_n = nrow(psm$psm)))
})
