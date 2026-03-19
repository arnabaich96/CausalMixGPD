# Coverage expansion tests targeting low-coverage helper and S3 files.

fmt3 <- get("fmt3", mode = "function")
fmt3_sci <- get("fmt3_sci", mode = "function")
fmt3_vec <- get("fmt3_vec", mode = "function")
format_df3 <- get("format_df3", mode = "function")
format_df3_sci <- get("format_df3_sci", mode = "function")
format_mat3 <- get("format_mat3", mode = "function")
format_mat3_sci <- get("format_mat3_sci", mode = "function")
.knitr_asis <- get(".knitr_asis", mode = "function")
.kable_table <- get(".kable_table", mode = "function")
.dt_view_table <- get(".dt_view_table", mode = "function")
print_fmt3 <- get("print_fmt3", mode = "function")
print_fmt3_sci <- get("print_fmt3_sci", mode = "function")
.bundle_has_any_gpd <- get(".bundle_has_any_gpd", mode = "function")
.bundle_all_gpd <- get(".bundle_all_gpd", mode = "function")
.strip_gpd_single_bundle <- get(".strip_gpd_single_bundle", mode = "function")
.bundle_strip_gpd <- get(".bundle_strip_gpd", mode = "function")
.codegen_prior_call <- get(".codegen_prior_call", mode = "function")
.codegen_link_expr <- get(".codegen_link_expr", mode = "function")
build_data_from_inputs <- get("build_data_from_inputs", mode = "function")
build_constants_from_spec <- get("build_constants_from_spec", mode = "function")
build_inits_from_spec <- get("build_inits_from_spec", mode = "function")
build_prior_table_from_spec <- get("build_prior_table_from_spec", mode = "function")
.mcmc_cache_key <- get(".mcmc_cache_key", mode = "function")
.mcmc_cache_get <- get(".mcmc_cache_get", mode = "function")
.mcmc_cache_set <- get(".mcmc_cache_set", mode = "function")
.configure_samplers <- get(".configure_samplers", mode = "function")
.coerce_treat <- get(".coerce_treat", mode = "function")
.parse_formula_yX <- get(".parse_formula_yX", mode = "function")
.extract_treat_from_data <- get(".extract_treat_from_data", mode = "function")
.normalize_mcmc_inputs <- get(".normalize_mcmc_inputs", mode = "function")
.apply_mcmc_overrides <- get(".apply_mcmc_overrides", mode = "function")
.silent_wrapper <- get(".silent_wrapper", mode = "function")
.cmgpd_capture_nimble <- get(".cmgpd_capture_nimble", mode = "function")
.validate_nimble_reserved_names <- get(".validate_nimble_reserved_names", mode = "function")
.extract_nimble_code <- get(".extract_nimble_code", mode = "function")
.wrap_nimble_code <- get(".wrap_nimble_code", mode = "function")
.plot_palette <- get(".plot_palette", mode = "function")
.strip_fill_scales <- get(".strip_fill_scales", mode = "function")
.wrap_plotly <- get(".wrap_plotly", mode = "function")
.resolve_predict_id <- get(".resolve_predict_id", mode = "function")
.reorder_predict_cols <- get(".reorder_predict_cols", mode = "function")
.coerce_fit_df <- get(".coerce_fit_df", mode = "function")
.extract_weights <- get(".extract_weights", mode = "function")
.extract_bulk_params <- get(".extract_bulk_params", mode = "function")
.get_epsilon <- get(".get_epsilon", mode = "function")
.validate_fit <- get(".validate_fit", mode = "function")
.get_samples_mcmclist <- get(".get_samples_mcmclist", mode = "function")
.get_nobs <- get(".get_nobs", mode = "function")
.posterior_summarize <- get(".posterior_summarize", mode = "function")
.extract_draws <- get(".extract_draws", mode = "function")
.truncation_info <- get(".truncation_info", mode = "function")
.format_fit_header <- get(".format_fit_header", mode = "function")
.summarize_posterior <- get(".summarize_posterior", mode = "function")
.get_dispatch_scalar <- get(".get_dispatch_scalar", mode = "function")
.get_dispatch <- get(".get_dispatch", mode = "function")
.compute_interval <- get(".compute_interval", mode = "function")
.detect_first_present <- get(".detect_first_present", mode = "function")
.wrap_scalar_first_arg <- get(".wrap_scalar_first_arg", mode = "function")
.wrap_scalar_p <- get(".wrap_scalar_p", mode = "function")
.wrap_scalar_r <- get(".wrap_scalar_r", mode = "function")
.truncate_components_one_draw <- get(".truncate_components_one_draw", mode = "function")

.coverage_heavy_cached <- function(key, expr) {
  hit <- .cache_get(key)
  if (!is.null(hit)) return(hit)
  val <- force(expr)
  .cache_set(key, val)
  val
}

.coverage_heavy_fit <- function() {
  .coverage_heavy_cached("coverage-heavy-fit", {
    set.seed(901)
    n <- 24L
    X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n))
    y <- abs(0.6 + 0.4 * X[, 1] + stats::rnorm(n)) + 0.2
    bundle <- build_nimble_bundle(
      y = y,
      X = X,
      backend = "sb",
      kernel = "normal",
      GPD = TRUE,
      components = 4L,
      mcmc = list(niter = 60L, nburnin = 20L, thin = 1L, nchains = 1L, seed = 901L, waic = TRUE)
    )
    run_mcmc_bundle_manual(bundle, show_progress = FALSE, quiet = TRUE, timing = TRUE, z_update_every = 2L)
  })
}

.coverage_heavy_uncond_fit <- function() {
  .coverage_heavy_cached("coverage-heavy-fit-uncond", {
    set.seed(902)
    y <- abs(stats::rnorm(20L)) + 0.2
    bundle <- build_nimble_bundle(
      y = y,
      backend = "crp",
      kernel = "gamma",
      GPD = FALSE,
      components = 4L,
      mcmc = list(niter = 50L, nburnin = 15L, thin = 1L, nchains = 1L, seed = 902L, waic = FALSE)
    )
    run_mcmc_bundle_manual(bundle, show_progress = FALSE, quiet = TRUE, timing = TRUE)
  })
}

.coverage_heavy_cluster_fit <- function() {
  .coverage_heavy_cached("coverage-heavy-fit-cluster", {
    set.seed(903)
    dat <- data.frame(
      y = abs(stats::rnorm(18L)) + 0.2,
      x1 = stats::rnorm(18L),
      x2 = stats::runif(18L)
    )
    fit <- dpmix.cluster(
      y ~ x1 + x2,
      data = dat,
      kernel = "normal",
      components = 4L,
      type = "weights",
      mcmc = list(niter = 50L, nburnin = 15L, thin = 1L, nchains = 1L, seed = 903L, waic = FALSE)
    )
    list(fit = fit, data = dat)
  })
}

.coverage_heavy_fit_2chain <- function() {
  .coverage_heavy_cached("coverage-heavy-fit-2chain", {
    set.seed(906)
    n <- 18L
    X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n))
    y <- abs(0.4 + 0.3 * X[, 1] + stats::rnorm(n, sd = 0.4)) + 0.2
    bundle <- build_nimble_bundle(
      y = y,
      X = X,
      backend = "sb",
      kernel = "normal",
      GPD = TRUE,
      components = 4L,
      mcmc = list(niter = 45L, nburnin = 10L, thin = 1L, nchains = 2L, seed = 906L, waic = FALSE)
    )
    run_mcmc_bundle_manual(bundle, show_progress = FALSE, quiet = TRUE, timing = TRUE)
  })
}

.coverage_heavy_causal_fit <- function() {
  .coverage_heavy_cached("coverage-heavy-causal-fit", {
    set.seed(903)
    n <- 24L
    X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n))
    A <- stats::rbinom(n, 1L, plogis(0.2 + 0.5 * X[, 1]))
    y <- abs(0.5 + A + 0.4 * X[, 1] + stats::rnorm(n)) + 0.2
    bundle <- build_causal_bundle(
      y = y,
      X = X,
      A = A,
      backend = c("sb", "sb"),
      kernel = c("normal", "normal"),
      GPD = c(FALSE, FALSE),
      components = c(4L, 4L),
      PS = "logit",
      mcmc_outcome = list(niter = 50L, nburnin = 15L, thin = 1L, nchains = 1L, seed = 903L),
      mcmc_ps = list(niter = 40L, nburnin = 10L, thin = 1L, nchains = 1L, seed = 904L)
    )
    run_mcmc_causal(bundle, show_progress = FALSE, quiet = TRUE, timing = TRUE)
  })
}

.fake_sampler_conf <- function() {
  conf <- new.env(parent = emptyenv())
  conf$samplerConfs <- list(
    list(name = "RW", target = "beta[1]", control = list()),
    list(
      name = "RW",
      target = "beta[2]",
      control = list(
        clusterVarInfo = list(
          clusterNodes = list(c("beta[1]", "bad_node"), c("z[1]", "bad_z")),
          numNodesPerCluster = c(2L, 2L)
        )
      )
    ),
    list(name = "slice", target = "z[1]", control = list()),
    list(name = "RW", target = "tail_scale[1]", control = list())
  )
  model <- list(
    getNodeNames = function(stochOnly = TRUE, includeData = FALSE) {
      c("beta[1]", "beta[2]", "z[1]", "tail_scale[1]")
    }
  )
  conf$getModel <- function() model
  conf$getSamplers <- function() {
    lapply(conf$samplerConfs, function(x) list(target = x$target))
  }
  conf$removed <- list()
  conf$added <- list()
  conf$removeSamplers <- function(target) {
    conf$removed[[length(conf$removed) + 1L]] <<- target
    invisible(NULL)
  }
  conf$addSampler <- function(target, type, control = list()) {
    conf$added[[length(conf$added) + 1L]] <<- list(target = target, type = type, control = control)
    invisible(NULL)
  }
  conf
}

test_that("coverage-heavy formatting helpers cover knitr dt and print branches", {
  expect_s3_class(.kable_table(data.frame(a = 1:2), row.names = FALSE), "knitr_kable")
  expect_match(as.character(.knitr_asis("alpha", c("beta", "gamma"))), "alpha")
  expect_true(grepl("e", fmt3_sci(c(1, 1e5), big = 1000)[2], fixed = TRUE))
  expect_identical(format_df3("x"), "x")
  expect_identical(format_mat3("x"), "x")

  dt_fun <- .dt_view_table
  environment(dt_fun) <- list2env(
    list(
      interactive = function() TRUE,
      print = function(x, ...) invisible(x),
      .cmgpd_message = function(...) invisible(NULL)
    ),
    parent = environment(.dt_view_table)
  )
  expect_silent(dt_fun(as.data.frame(matrix(runif(120), nrow = 12, ncol = 10)), row.names = FALSE))

  old_knitr <- getOption("knitr.in.progress")
  old_kable <- getOption("causalmixgpd.knitr.kable")
  options(knitr.in.progress = TRUE, causalmixgpd.knitr.kable = TRUE)
  on.exit(options(knitr.in.progress = old_knitr, causalmixgpd.knitr.kable = old_kable), add = TRUE)

  expect_gt(length(utils::capture.output(print_fmt3(
    data.frame(a = c(1.2345, 2.3456), b = c("x", "y")),
    row.names = FALSE
  ))), 0L)
  expect_gt(length(utils::capture.output(print_fmt3(matrix(c(1.2, 3.4), nrow = 1)))), 0L)
  expect_gt(length(utils::capture.output(print_fmt3(1.234))), 0L)
  expect_gt(length(utils::capture.output(print_fmt3_sci(
    data.frame(a = c(1, 100000)),
    row.names = FALSE
  ))), 0L)
  expect_gt(length(utils::capture.output(print_fmt3_sci(
    matrix(c(1, 200000), nrow = 1),
    big = 1000
  ))), 0L)
  expect_gt(length(utils::capture.output(print_fmt3_sci(100000, big = 1000))), 0L)
})

test_that("coverage-heavy wrapper helpers cover GPD stripping and wrapper dispatch", {
  bundle_one <- structure(
    list(
      spec = list(meta = list(GPD = TRUE), plan = list(GPD = TRUE, gpd = list())),
      data = list(y = c(1, 2, 3)),
      monitor_policy = list(monitor_v = TRUE, monitor_latent = TRUE),
      mcmc = list(niter = 20L)
    ),
    class = "causalmixgpd_bundle"
  )
  causal_bundle <- structure(
    list(
      outcome = list(con = bundle_one, trt = bundle_one),
      meta = list(GPD = list(con = TRUE, trt = FALSE))
    ),
    class = "causalmixgpd_causal_bundle"
  )

  testthat::local_mocked_bindings(
    build_code_from_spec = function(spec) list(code = "fake"),
    build_constants_from_spec = function(spec) list(N = 3L),
    build_dimensions_from_spec = function(spec) list(z = 3L),
    build_inits_from_spec = function(spec, y = NULL) list(z = c(1L, 1L, 1L)),
    build_monitors_from_spec = function(spec, ...) c("alpha", "z[1:3]"),
    .package = "CausalMixGPD"
  )

  stripped <- .strip_gpd_single_bundle(bundle_one)
  expect_false(isTRUE(stripped$spec$meta$GPD))
  expect_equal(stripped$spec$plan$gpd, list())
  expect_true(.bundle_has_any_gpd(bundle_one))
  expect_true(.bundle_has_any_gpd(causal_bundle))
  expect_false(.bundle_all_gpd(causal_bundle))

  stripped_causal <- .bundle_strip_gpd(causal_bundle)
  expect_false(isTRUE(stripped_causal$meta$GPD$trt))
  expect_false(isTRUE(stripped_causal$meta$GPD$con))

  testthat::local_mocked_bindings(
    bundle = function(...) structure(list(tag = "built", spec = list(meta = list(GPD = TRUE))), class = "causalmixgpd_bundle"),
    .run_bundle_mcmc = function(b, mcmc_args = list()) structure(list(bundle = b, args = mcmc_args), class = "wrapped_fit"),
    .package = "CausalMixGPD"
  )
  expect_s3_class(dpmix(x = c(1, 2, 3), kernel = "normal", components = 3L, mcmc = list(seed = 1L)), "wrapped_fit")
  expect_s3_class(dpmgpd(x = c(1, 2, 3), kernel = "normal", components = 3L, mcmc = list(seed = 1L)), "wrapped_fit")
  expect_s3_class(dpmix.causal(x = c(1, 2, 3), X = cbind(x1 = c(0, 1, 0)), treat = c(0L, 1L, 0L), kernel = "normal", components = c(3L, 3L), mcmc = list(seed = 1L)), "wrapped_fit")
  expect_s3_class(dpmgpd.causal(x = c(1, 2, 3), X = cbind(x1 = c(0, 1, 0)), treat = c(0L, 1L, 0L), kernel = "normal", components = c(3L, 3L), mcmc = list(seed = 1L)), "wrapped_fit")
})

test_that("coverage-heavy build-run helpers cover codegen priors dimensions and sampler tuning", {
  expect_match(.codegen_prior_call("normal", list(mean = 0, sd = 1)), "dnorm")
  expect_match(.codegen_prior_call("gamma", list(shape = 2, rate = 1)), "dgamma")
  expect_match(.codegen_prior_call("invgamma", list(shape = 2, scale = 1)), "dinvgamma")
  expect_match(.codegen_prior_call("lognormal", list(meanlog = 0, sdlog = 1)), "dlnorm")
  expect_error(.codegen_prior_call("bad", list()), "Unsupported prior dist")

  expect_equal(.codegen_link_expr("eta", "identity"), "eta")
  expect_equal(.codegen_link_expr("eta", "exp"), "exp(eta)")
  expect_equal(.codegen_link_expr("eta", "log"), "log(eta)")
  expect_equal(.codegen_link_expr("eta", "softplus"), "log(1 + exp(eta))")
  expect_match(.codegen_link_expr("eta", "power", link_power = 2), "pow")
  expect_error(.codegen_link_expr("eta", "power"), "link requires numeric link_power")
  expect_error(.codegen_link_expr("eta", "bad"), "Unsupported link")

  y <- abs(stats::rnorm(8)) + 0.1
  X <- cbind(x1 = stats::rnorm(8), x2 = stats::runif(8))
  spec_link <- compile_model_spec(
    y = y,
    X = X,
    ps = rep(0.5, length(y)),
    backend = "spliced",
    kernel = "normal",
    GPD = TRUE,
    components = 4L,
    param_specs = list(
      bulk = list(mean = list(mode = "link", link = "identity")),
      gpd = list(
        threshold = list(mode = "link", link = "identity"),
        tail_scale = list(mode = "link", link = "exp"),
        tail_shape = list(mode = "dist", dist = "normal", args = list(mean = 0, sd = 0.3))
      )
    )
  )

  dims <- build_dimensions_from_spec(spec_link)
  mons <- build_monitors_from_spec(spec_link, monitor_v = TRUE, monitor_latent = TRUE)
  inits <- build_inits_from_spec(spec_link, seed = 1L, y = y)
  priors <- build_prior_table_from_spec(spec_link)

  expect_true(all(c("v", "w", "z", "beta_mean", "beta_ps_mean") %in% names(build_dimensions_from_spec(
    compile_model_spec(y = y, X = X, ps = rep(0.5, length(y)), backend = "sb", kernel = "normal", GPD = FALSE, components = 4L,
                       param_specs = list(bulk = list(mean = list(mode = "link", link = "identity"))))
  ))))
  expect_true(all(c("beta_threshold", "threshold_i", "beta_tail_scale", "tail_shape") %in% names(dims)))
  expect_true(any(grepl("^beta_mean", mons)))
  expect_true(any(grepl("^beta_threshold", mons)))
  expect_true(all(c("alpha", "z", "beta_mean", "beta_ps_mean", "beta_threshold", "beta_tail_scale", "tail_shape") %in% names(inits)))
  expect_true(any(priors$parameter == "tail_scale" & priors$mode == "link"))

  key <- .mcmc_cache_key(list(code = "x"), list(N = 1L), list(y = 1), list(), "alpha", TRUE)
  expect_null(.mcmc_cache_get(key))
  .mcmc_cache_set(key, list(answer = 42))
  expect_equal(.mcmc_cache_get(key)$answer, 42)

  conf <- .fake_sampler_conf()
  tuned <- .configure_samplers(conf, spec = spec_link, z_update_every = 3L)
  expect_identical(tuned, conf)
  expect_true(length(conf$removed) >= 2L)
  expect_true(any(vapply(conf$added, function(x) identical(x$type, "RW_block"), logical(1))))
  expect_true(any(vapply(conf$added, function(x) identical(x$type, "slice"), logical(1))))
  expect_true(all(vapply(conf$samplerConfs, function(x) isFALSE(is.null(x$control$checkConjugacy)), logical(1))))
})

test_that("coverage-heavy internal helpers cover silent wrappers and capture helpers", {
  wrapped <- .silent_wrapper(
    "demo_fun",
    function(x) {
      warning("suppressed warning", call. = FALSE)
      message("suppressed message")
      x + 1
    },
    "CausalMixGPD.silent"
  )
  options(CausalMixGPD.silent = TRUE)
  on.exit(options(CausalMixGPD.silent = NULL), add = TRUE)
  expect_equal(wrapped(1), 2)

  expect_equal(.cmgpd_capture_nimble({ 1 + 1 }, suppress = FALSE), 2)
  expect_equal(.cmgpd_capture_nimble({ message("hidden"); 3 }, suppress = TRUE), 3)
})

test_that("coverage-heavy runner and predictive methods cover build-run methods and internal branches", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggmcmc")
  skip_if_not_installed("coda")

  fit <- .coverage_heavy_fit()
  bundle_cached <- build_nimble_bundle(
    y = fit$data$y,
    X = fit$data$X,
    backend = "sb",
    kernel = "normal",
    GPD = TRUE,
    components = 4L,
    mcmc = list(niter = 60L, nburnin = 20L, thin = 1L, nchains = 1L, seed = 901L, waic = TRUE)
  )
  fit_cached <- run_mcmc_bundle_manual(bundle_cached, show_progress = FALSE, quiet = TRUE)
  expect_s3_class(fit, "mixgpd_fit")
  expect_s3_class(fit_cached, "mixgpd_fit")
  expect_true(is.list(fit$timing))
  expect_warning(
    run_mcmc_bundle_manual(
      build_nimble_bundle(
        y = fit$data$y,
        X = fit$data$X,
        backend = "sb",
        kernel = "normal",
        GPD = TRUE,
        components = 4L,
        mcmc = list(niter = 30L, nburnin = 10L, thin = 1L, nchains = 2L, seed = 910L, waic = FALSE)
      ),
      show_progress = FALSE,
      quiet = TRUE,
      parallel_chains = TRUE
    ),
    "falls back to sequential execution"
  )

  Xp <- fit$data$X[1:4, , drop = FALSE]
  yp <- fit$data$y[1:4]
  pred_mean <- predict(fit, x = Xp, type = "mean", nsim_mean = 20L, interval = "credible", show_progress = FALSE)
  pred_location <- predict(fit, x = Xp, type = "location", interval = "hpd", show_progress = FALSE)
  pred_quant <- predict(fit, x = Xp, type = "quantile", index = c(0.25, 0.75), interval = "credible", show_progress = FALSE)
  pred_median <- predict(fit, x = Xp, type = "median", show_progress = FALSE)
  pred_rmean <- predict(fit, x = Xp, type = "rmean", cutoff = 3, show_progress = FALSE)
  pred_density <- predict(fit, x = Xp[1:2, , drop = FALSE], y = yp[1:2], type = "density", interval = NULL, show_progress = FALSE)
  pred_survival <- predict(fit, x = Xp[1:2, , drop = FALSE], y = yp[1:2], type = "survival", interval = NULL, show_progress = FALSE)
  pred_sample <- predict(fit, x = Xp, type = "sample", nsim = 5L, store_draws = FALSE, show_progress = FALSE)

  expect_s3_class(pred_mean, "mixgpd_predict")
  expect_s3_class(pred_location, "mixgpd_predict")
  expect_s3_class(pred_quant, "mixgpd_predict")
  expect_s3_class(pred_median, "mixgpd_predict")
  expect_s3_class(pred_rmean, "mixgpd_predict")
  expect_s3_class(pred_density, "mixgpd_predict")
  expect_s3_class(pred_survival, "mixgpd_predict")
  expect_s3_class(pred_sample, "mixgpd_predict")

  fit_loc <- fitted(fit, type = "location", interval = "credible")
  fit_q <- fitted(fit, type = "quantile", p = 0.75, interval = NULL)
  expect_s3_class(fit_loc, "mixgpd_fitted")
  expect_s3_class(fit_q, "mixgpd_fitted")

  res_raw <- residuals(fit, type = "raw", fitted_type = "median")
  res_pit_plugin <- residuals(fit, type = "pit", pit = "plugin")
  expect_length(res_raw, nrow(fit$data$X))
  expect_length(res_pit_plugin, nrow(fit$data$X))
  expect_error(residuals(fit, type = "pit", pit = "bayes_mean", pit_seed = 1L), "argument \"mean\" is missing")
  expect_error(residuals(fit, type = "pit", pit = "bayes_draw", pit_seed = 1L), "argument \"mean\" is missing")

  pars <- params(fit)
  fit_sum <- summary(fit)
  ess <- ess_summary(fit, per_chain = TRUE)
  expect_s3_class(pars, "mixgpd_params")
  expect_s3_class(fit_sum, "mixgpd_summary")
  expect_s3_class(ess, "mixgpd_ess_summary")
  expect_output(print(pars))
  expect_output(print(fit_sum))
  expect_output(print(ess))
  expect_true(is.data.frame(summary(ess)))

  expect_s3_class(plot(fit, family = c("traceplot", "density"), params = "alpha"), "mixgpd_fit_plots")
  expect_s3_class(plot(pred_mean), "mixgpd_predict_plots")
  expect_s3_class(plot(fit_loc), "mixgpd_fitted_plots")
})

test_that("coverage-heavy causal methods and summaries cover methods branches", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")
  skip_if_not_installed("ggplot2")

  fit <- .coverage_heavy_causal_fit()
  Xp <- fit$bundle$data$X[1:4, , drop = FALSE]
  yp <- fit$bundle$data$y[1:2]

  expect_output(print(fit), "CausalMixGPD causal fit")
  expect_output(summary(fit), "Outcome fits")

  pars <- params(fit)
  expect_s3_class(pars, "mixgpd_params_pair")

  pred_mean <- predict(fit, x = Xp, type = "mean", nsim_mean = 20L, interval = "credible", show_progress = FALSE)
  pred_quant <- predict(fit, x = Xp, type = "quantile", p = c(0.25, 0.75), interval = "credible", show_progress = FALSE)
  pred_density <- predict(fit, x = Xp[1:2, , drop = FALSE], y = yp, type = "density", interval = NULL, show_progress = FALSE)
  expect_s3_class(pred_mean, "causalmixgpd_causal_predict")
  expect_s3_class(pred_quant, "causalmixgpd_causal_predict")
  expect_s3_class(pred_density, "causalmixgpd_causal_predict")

  qte_obj <- qte(fit, probs = c(0.25, 0.75), interval = "credible", show_progress = FALSE)
  ate_obj <- ate(fit, interval = "credible", nsim_mean = 20L, show_progress = FALSE)
  cqte_obj <- cqte(fit, probs = c(0.25, 0.75), newdata = Xp, interval = "credible", show_progress = FALSE)
  cate_obj <- cate(fit, newdata = Xp, interval = "credible", nsim_mean = 20L, show_progress = FALSE)
  expect_output(print(qte_obj), "QTE")
  expect_output(print(ate_obj), "ATE")
  expect_output(print(cqte_obj), "CQTE")
  expect_output(print(cate_obj), "CATE")
  expect_output(print(summary(qte_obj)), "QTE Summary")
  expect_output(print(summary(ate_obj)), "ATE Summary")

  pred_plot <- plot(pred_mean)
  fit_plot <- plot(fit, arm = "both")
  qte_plot <- plot(qte_obj)
  ate_plot <- plot(ate_obj)
  qte_effect_plot <- plot(qte_obj, type = "effect")
  qte_arms_plot <- plot(qte_obj, type = "arms")
  ate_effect_plot <- plot(ate_obj, type = "effect")
  ate_arms_plot <- plot(ate_obj, type = "arms")
  cqte_plot <- plot(cqte_obj, type = "both")
  cate_plot <- plot(cate_obj, type = "both")
  expect_s3_class(pred_plot, "causalmixgpd_causal_predict_plots")
  expect_s3_class(fit_plot, "causalmixgpd_causal_fit_plots")
  expect_true(is.list(qte_plot))
  expect_true(is.list(ate_plot))
  expect_s3_class(qte_effect_plot, "ggplot")
  expect_s3_class(qte_arms_plot, "ggplot")
  expect_s3_class(ate_effect_plot, "ggplot")
  expect_s3_class(ate_arms_plot, "ggplot")
  expect_true(is.list(cqte_plot))
  expect_true(is.list(cate_plot))
})

test_that("coverage-heavy bundle methods cover knitr presentation branches", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("kableExtra")

  old_knitr <- getOption("knitr.in.progress")
  old_kable <- getOption("causalmixgpd.knitr.kable")
  options(knitr.in.progress = TRUE, causalmixgpd.knitr.kable = TRUE)
  on.exit(options(knitr.in.progress = old_knitr, causalmixgpd.knitr.kable = old_kable), add = TRUE)

  set.seed(905)
  y <- abs(stats::rnorm(12L)) + 0.1
  X <- cbind(x1 = stats::rnorm(12L), x2 = stats::runif(12L))
  A <- rep(c(0L, 1L), length.out = 12L)
  b <- build_nimble_bundle(y = y, X = X, backend = "sb", kernel = "normal", GPD = FALSE, components = 3L)
  cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", GPD = FALSE, components = 3L)

  expect_silent(print(b))
  expect_silent(summary(b))
  expect_s3_class(print(cb), "knit_asis")
  expect_s3_class(summary(cb, code = TRUE, max_code_lines = 5L), "knit_asis")
})

test_that("coverage-heavy wrappers cover parsing treatment normalization and mcmc dispatch", {
  dat <- data.frame(
    y = c(1, 2, 3, 4, 5),
    x1 = c(0.1, 0.2, NA, 0.4, 0.5),
    id = 11:15,
    trt = factor(c("control", "treated", "control", "treated", "control"))
  )

  parsed <- .parse_formula_yX(y ~ x1 + id, data = dat)
  parsed_uncond <- .parse_formula_yX(y ~ 1, data = dat)
  expect_equal(colnames(parsed$X), "x1")
  expect_true(parsed_uncond$is_unconditional)

  expect_equal(.coerce_treat(factor(c("control", "treated", "control"))), c(0L, 1L, 0L))
  expect_equal(.coerce_treat(c(FALSE, TRUE, FALSE)), c(0L, 1L, 0L))
  expect_equal(.coerce_treat(c("control", "treated", "treated")), c(0L, 1L, 1L))
  expect_error(.coerce_treat(c(0, 2)), "binary")
  expect_error(.coerce_treat(c(0, NA)), "cannot contain NA")

  expect_equal(.extract_treat_from_data(parsed$mf, data = dat, treat = "trt"), c(0L, 1L, 1L, 0L))
  expect_equal(
    .extract_treat_from_data(parsed$mf, data = dat, treat = c(0L, 1L, 0L, 1L, 0L)),
    c(0L, 1L, 1L, 0L)
  )
  expect_error(.extract_treat_from_data(parsed$mf, data = dat, treat = 0:1), "length does not match")

  parsed_mcmc <- .normalize_mcmc_inputs(list(niter = 10L, nburn = 3L, quiet = TRUE, timing = TRUE))
  expect_equal(parsed_mcmc$overrides$nburnin, 3L)
  expect_true(isTRUE(parsed_mcmc$runner$quiet))
  expect_error(.normalize_mcmc_inputs(list(1L)), "must be named")
  expect_error(.normalize_mcmc_inputs(list(bad = 1L)), "Unknown mcmc argument")

  b <- structure(list(mcmc = list(niter = 10L), spec = list(meta = list(GPD = FALSE))), class = "causalmixgpd_bundle")
  cb <- structure(
    list(
      outcome = list(con = b, trt = b),
      design = structure(list(mcmc = list(niter = 5L)), class = "causalmixgpd_ps_bundle"),
      meta = list(GPD = list(con = FALSE, trt = FALSE))
    ),
    class = "causalmixgpd_causal_bundle"
  )

  expect_equal(.apply_mcmc_overrides(b, list(seed = 3L))$mcmc$seed, 3L)
  cb_over <- .apply_mcmc_overrides(cb, list(seed = 9L))
  expect_equal(cb_over$outcome$con$mcmc$seed, 9L)
  expect_equal(cb_over$design$mcmc$seed, 9L)

  testthat::local_mocked_bindings(
    build_nimble_bundle = function(y, X = NULL, GPD = FALSE, ...) {
      structure(list(kind = "one-arm", y = y, X = X, GPD = GPD), class = "causalmixgpd_bundle")
    },
    build_causal_bundle = function(y, X = NULL, A = NULL, GPD = FALSE, ...) {
      structure(list(kind = "causal", y = y, X = X, A = A, GPD = GPD), class = "causalmixgpd_causal_bundle")
    },
    run_mcmc_bundle_manual = function(bundle, ...) structure(list(bundle = bundle), class = "mixgpd_fit"),
    run_mcmc_causal = function(bundle, ...) structure(list(bundle = bundle), class = "causalmixgpd_causal_fit"),
    .package = "CausalMixGPD"
  )

  built_one <- bundle(formula = y ~ x1 + id, data = dat, backend = "sb", kernel = "normal", components = 3L)
  built_causal <- bundle(
    formula = y ~ x1 + id,
    data = dat,
    treat = "trt",
    backend = "sb",
    kernel = "normal",
    components = 3L
  )
  expect_s3_class(built_one, "causalmixgpd_bundle")
  expect_s3_class(built_causal, "causalmixgpd_causal_bundle")
  expect_s3_class(mcmc(b, quiet = TRUE), "mixgpd_fit")
  expect_s3_class(mcmc(cb, quiet = TRUE), "causalmixgpd_causal_fit")
  expect_error(mcmc(b, parallel_arms = TRUE), "Unsupported runner argument")
  expect_error(dpmix.causal(x = c(1, 2, 3), kernel = "normal", components = 3L, mcmc = list()), "requires 'treat'")
  expect_error(dpmgpd.causal(x = b, mcmc = list()), "requires a causal bundle")
  expect_error(dpmgpd(x = b, mcmc = list()), "requires a bundle with GPD enabled")
})

test_that("coverage-heavy internal helpers cover draw coercion id handling and plotting helpers", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("coda")

  fit <- .coverage_heavy_fit()
  fit_u <- .coverage_heavy_uncond_fit()

  expect_silent(.validate_nimble_reserved_names(c("alpha", "beta"), context = "columns"))
  expect_error(.validate_nimble_reserved_names(c("if", "alpha"), context = "columns"), "reserved NIMBLE keywords")

  wrapped_code <- .wrap_nimble_code(quote(alpha <- 1))
  expect_true(is.list(wrapped_code))
  expect_identical(.extract_nimble_code(list(code = quote(alpha <- 2))), quote(alpha <- 2))

  expect_length(.plot_palette(12L), 12L)
  p_fill <- ggplot2::ggplot(data.frame(x = c("a", "b"), y = c(1, 2), g = c(1, 2)),
                            ggplot2::aes(x = x, y = y, fill = g)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_viridis_c()
  p_stripped <- .strip_fill_scales(p_fill)
  expect_false(any(vapply(p_stripped$scales$scales, function(s) "fill" %in% (s$aesthetics %||% character(0)), logical(1))))
  expect_identical(.wrap_plotly("plain-object"), "plain-object")

  id_info <- .resolve_predict_id(data.frame(id = letters[1:2], x1 = 1:2), id = "id")
  expect_equal(id_info$id, letters[1:2])
  expect_false("id" %in% names(id_info$x))
  expect_error(.resolve_predict_id(matrix(1:4, ncol = 2), id = "id"), "data.frame")
  expect_error(.resolve_predict_id(NULL, id = 1:2), "requires 'x'/'newdata'")

  reordered <- .reorder_predict_cols(data.frame(lower = 1, estimate = 2, id = 3, upper = 4, y = 5, misc = 6))
  expect_identical(names(reordered), c("id", "y", "estimate", "lower", "upper", "misc"))

  fit_df_matrix <- .coerce_fit_df(matrix(c(1, 2, 3, 4), nrow = 2), probs = c(0.25, 0.75))
  fit_df_vector <- .coerce_fit_df(c(1, 2, 3))
  expect_true(all(c("id", "index", "estimate", "lower", "upper") %in% names(fit_df_matrix)))
  expect_true(all(c("id", "estimate", "lower", "upper") %in% names(fit_df_vector)))
  expect_error(.coerce_fit_df(list(a = 1)), "unsupported type")

  sb_weights <- matrix(c(0.6, 0.4, 0.7, 0.3), nrow = 2, byrow = TRUE,
                       dimnames = list(NULL, c("w[1]", "w[2]")))
  crp_weights <- matrix(c(1, 2, 1, 2, 2, 1), nrow = 2, byrow = TRUE,
                        dimnames = list(NULL, c("z[1]", "z[2]", "z[3]")))
  bulk_draws <- cbind(
    "mu[1]" = c(1, 2),
    "mu[2]" = c(3, 4),
    "sigma[1]" = c(0.5, 0.6)
  )
  expect_equal(.extract_weights(sb_weights, backend = "sb")[1, 1], 0.6)
  expect_equal(ncol(.extract_weights(crp_weights, backend = "crp")), 2L)
  expect_true(all(c("mu", "sigma") %in% names(.extract_bulk_params(bulk_draws, bulk_params = c("mu", "sigma")))))

  expect_equal(.get_epsilon(fit), fit$epsilon)
  expect_true(isTRUE(.validate_fit(fit)))
  expect_s3_class(.get_samples_mcmclist(fit), "mcmc.list")
  expect_equal(.get_nobs(fit_u), length(fit_u$data$y))

  post_vec <- .posterior_summarize(c(1, 2, 3), interval = NULL)
  post_mat <- .posterior_summarize(matrix(1:6, nrow = 2), interval = "credible")
  expect_equal(post_vec$estimate, 2)
  expect_length(post_mat$estimate, 2L)
})

test_that("coverage-heavy internal advanced helpers cover dispatch truncation and scalar wrappers", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("coda")

  fit <- .coverage_heavy_fit()

  add_one <- .wrap_scalar_first_arg(function(x, offset = 0) x + offset, "x")
  cdf_wrap <- .wrap_scalar_p(function(q, shift = 0) q + shift)
  rng_wrap <- .wrap_scalar_r(function(n, mu = 1) rep(mu, 1L))

  expect_equal(add_one(x = c(1, 2), offset = 2), c(3, 4))
  expect_equal(cdf_wrap(x = c(1, 2), shift = 1), c(2, 3))
  expect_equal(rng_wrap(n = 3L, mu = 2), c(2, 2, 2))
  expect_equal(length(rng_wrap(n = 0L, mu = 2)), 0L)

  expect_equal(.detect_first_present(list(q = 1), candidates = c("q", "x")), "q")
  expect_error(.detect_first_present(list(), candidates = c("q", "x")), "Expected one of")

  trunc <- .truncate_components_one_draw(
    w = c(0.6, 0.3, 0.1),
    params = list(mu = c(1, 2, 3)),
    epsilon = 0.2
  )
  expect_equal(trunc$k, 2L)
  expect_error(.truncate_components_one_draw(w = c(0.6, 0.4), params = list(mu = 1), epsilon = 0.2), "length K")

  ci <- .compute_interval(c(1, 2, 3, 4), level = 0.5, type = "credible")
  expect_true(all(c("lower", "upper") %in% names(ci)))

  draws_first <- .extract_draws(fit, chains = "first")
  trunc_info <- .truncation_info(fit)
  fit_header <- .format_fit_header(fit)
  post_sum <- .summarize_posterior(fit, pars = "alpha")
  dispatch_scalar <- .get_dispatch_scalar(fit)
  dispatch <- .get_dispatch(fit)

  expect_true(is.matrix(draws_first))
  expect_true(is.list(trunc_info))
  expect_true(length(fit_header) >= 2L)
  expect_true(is.data.frame(post_sum))
  expect_true(is.function(dispatch_scalar$d))
  expect_true(is.function(dispatch$d))
  expect_error(.extract_draws(fit, pars = "missing_param"), "Unknown params")
  expect_error(.summarize_posterior(fit, pars = "missing_param"), "Unknown params")
})

test_that("coverage-heavy build-run helpers cover data constants priors and error branches", {
  y <- abs(stats::rnorm(8L)) + 0.1
  X <- cbind(x1 = stats::rnorm(8L), x2 = stats::runif(8L))
  ps <- rep(0.5, length(y))

  data_obj <- build_data_from_inputs(y = y, X = X, ps = ps)
  expect_true(all(c("y", "X", "ps") %in% names(data_obj)))
  expect_error(build_data_from_inputs(y = numeric(0)), "non-empty")
  expect_error(build_data_from_inputs(y = y, X = matrix(1, nrow = 7, ncol = 1)), "nrow\\(X\\)")
  expect_error(build_data_from_inputs(y = y, X = matrix(numeric(0), nrow = 8, ncol = 0)), "at least one column")
  expect_error(build_data_from_inputs(y = y, ps = 1:3), "same length")

  spec_sb <- compile_model_spec(
    y = y,
    X = X,
    ps = ps,
    backend = "sb",
    kernel = "normal",
    GPD = TRUE,
    components = 4L,
    param_specs = list(
      bulk = list(
        mean = list(mode = "link", link = "identity"),
        sd = list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1))
      ),
      gpd = list(
        threshold = list(mode = "link", link = "identity", link_dist = list(dist = "lognormal")),
        tail_scale = list(mode = "link", link = "exp"),
        tail_shape = list(mode = "dist", dist = "normal", args = list(mean = 0, sd = 0.2))
      )
    )
  )
  spec_spliced <- compile_model_spec(
    y = y,
    X = X,
    backend = "spliced",
    kernel = "normal",
    GPD = TRUE,
    components = 4L,
    param_specs = list(
      bulk = list(mean = list(mode = "link", link = "identity")),
      gpd = list(
        threshold = list(mode = "link", link = "identity"),
        tail_scale = list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1)),
        tail_shape = list(mode = "link", link = "identity")
      )
    )
  )

  const_spec <- spec_sb
  const_spec$plan$concentration <- list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1))
  const_spec$plan$bulk$mean <- list(
    mode = "link",
    beta_prior = list(dist = "normal", args = list(mean = 0, sd = 2))
  )
  const_spec$plan$ps <- list(prior = list(dist = "normal", args = list(mean = 0, sd = 2)))
  const_spec$plan$gpd$threshold <- list(
    mode = "link",
    beta_prior = list(dist = "normal", args = list(mean = 0, sd = 0.2)),
    link_dist = list(dist = "lognormal")
  )
  const_spec$plan$gpd$sdlog_u <- list(mode = "dist", dist = "invgamma", args = list(shape = 2, scale = 1))
  const_spec$plan$gpd$tail_scale <- list(
    mode = "link",
    beta_prior = list(dist = "normal", args = list(mean = 0, sd = 0.5))
  )
  const_sb <- build_constants_from_spec(const_spec)
  dims_sp <- build_dimensions_from_spec(spec_spliced)
  mons_sp <- build_monitors_from_spec(spec_spliced, monitor_v = TRUE, monitor_latent = TRUE)
  inits_sp <- build_inits_from_spec(spec_spliced, seed = 2L, y = y)
  prior_sp <- build_prior_table_from_spec(spec_spliced)

  expect_true(all(c("N", "P", "components") %in% names(const_sb)))
  expect_true(all(c("beta_threshold", "tail_scale", "beta_tail_shape") %in% names(dims_sp)))
  expect_true(any(grepl("^beta_tail_shape", mons_sp)))
  expect_true(all(c("beta_threshold", "tail_scale", "beta_tail_shape") %in% names(inits_sp)))
  expect_true(any(prior_sp$parameter == "tail_shape" & prior_sp$mode == "link"))

  bad_const <- spec_sb
  bad_const$plan$bulk$mean$beta_prior$dist <- "bad"
  expect_error(build_constants_from_spec(bad_const), "Unsupported prior distribution")

  bad_mon <- spec_spliced
  bad_mon$plan$bulk$mean$mode <- "bad"
  expect_error(build_monitors_from_spec(bad_mon), "Invalid bulk plan mode")

  bad_inits <- spec_spliced
  bad_inits$plan$gpd$tail_scale$mode <- "bad"
  expect_error(build_inits_from_spec(bad_inits, y = y), "Invalid gpd\\$tail_scale mode")
})

test_that("coverage-heavy methods cover bundle ps summary and cluster printers", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("nimble")

  set.seed(907)
  y <- abs(stats::rnorm(10L)) + 0.1
  X <- cbind(x1 = stats::rnorm(10L), x2 = stats::runif(10L))
  bundle_obj <- build_nimble_bundle(y = y, X = X, backend = "sb", kernel = "normal", GPD = FALSE, components = 3L)
  ps_bundle <- structure(
    list(
      spec = list(meta = list(type = "ps_logit", include_intercept = TRUE)),
      code = quote({
        beta0 ~ dnorm(0, sd = 1)
      })
    ),
    class = "causalmixgpd_ps_bundle"
  )
  ps_fit <- structure(list(bundle = ps_bundle), class = "causalmixgpd_ps_fit")
  empty_params <- structure(list(), class = "mixgpd_params")
  param_pair <- structure(
    list(
      treated = structure(list(alpha = 1, w = c(0.6, 0.4)), class = "mixgpd_params"),
      control = structure(list(alpha = 2, w = c(0.5, 0.5)), class = "mixgpd_params")
    ),
    class = "mixgpd_params_pair"
  )
  summary_obj <- structure(
    list(
      model = list(
        backend = "sb",
        kernel = "normal",
        gpd = FALSE,
        epsilon = 0.1,
        truncation = list(Kt = 2L),
        n = 10L,
        components = 3L
      ),
      waic = list(WAIC = 1.23, lppd = 0.4, pWAIC = 0.2),
      table = data.frame(
        parameter = paste0("p", 1:3),
        mean = 1:3,
        sd = rep(0.1, 3),
        q0.025 = rep(0.5, 3),
        q0.500 = 1:3,
        q0.975 = rep(3.5, 3)
      )
    ),
    class = "mixgpd_summary"
  )
  cluster_obj <- .coverage_heavy_cluster_fit()
  cluster_fit <- cluster_obj$fit
  cluster_lbl <- predict(cluster_fit, type = "label", return_scores = TRUE)
  cluster_psm <- predict(cluster_fit, type = "psm")

  expect_output(print(bundle_obj, code = TRUE, max_code_lines = 3L), "CausalMixGPD bundle")
  expect_output(summary(bundle_obj), "Parameter specification")
  expect_output(print(ps_bundle, code = TRUE, max_code_lines = 2L), "PS bundle")
  expect_output(summary(ps_bundle, code = TRUE), "PS bundle")
  expect_output(print(ps_fit), "CausalMixGPD PS fit")
  expect_output(summary(ps_fit), "CausalMixGPD PS fit")
  expect_output(print(empty_params), "<empty>")
  expect_output(print(param_pair), "treated")
  expect_output(print(summary_obj, max_rows = 2L), "Showing first 2")
  expect_output(print(structure(list(table = data.frame(), overall = data.frame(), meta = list()), class = "mixgpd_ess_summary")), "No matched parameters")
  expect_output(print(.coverage_heavy_fit()), "MixGPD fit")
  expect_output(print(cluster_fit), "Cluster fit")
  expect_output(print(cluster_lbl), "Cluster labels")
  expect_output(print(cluster_psm), "Cluster PSM")
})

test_that("coverage-heavy methods cover plotting families causal prediction and cluster branches", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggmcmc")
  skip_if_not_installed("coda")

  fit <- .coverage_heavy_fit()
  fit2 <- .coverage_heavy_fit_2chain()
  fit_loc <- fitted(fit, type = "mean", interval = "credible")
  fit_q <- fitted(fit, type = "quantile", p = 0.5, interval = "credible")
  pred_density <- predict(
    fit,
    x = fit$data$X[1:2, , drop = FALSE],
    y = fit$data$y[1:2],
    type = "density",
    interval = "credible",
    show_progress = FALSE
  )
  pred_sample <- predict(fit, x = fit$data$X[1:3, , drop = FALSE], type = "sample", nsim = 3L, show_progress = FALSE)
  pred_survival <- predict(
    fit,
    x = fit$data$X[1:2, , drop = FALSE],
    y = fit$data$y[1:2],
    type = "survival",
    interval = "credible",
    show_progress = FALSE
  )
  fit_summary <- summary(fit, pars = "alpha")
  fit_params <- params(.coverage_heavy_uncond_fit())
  cluster_obj <- .coverage_heavy_cluster_fit()
  cluster_fit <- cluster_obj$fit
  cluster_lbl <- predict(cluster_fit, type = "label", return_scores = TRUE)
  cluster_psm <- predict(cluster_fit, type = "psm")

  expect_s3_class(
    suppressWarnings(plot(fit2, family = c("histogram", "running", "compare_partial", "autocorrelation", "geweke", "caterpillar"), params = "alpha")),
    "mixgpd_fit_plots"
  )
  expect_s3_class(
    suppressWarnings(plot(fit2, family = c("crosscorrelation", "Rhat", "effective"), params = c("alpha", "w\\[1\\]"))),
    "mixgpd_fit_plots"
  )
  expect_output(print(fit_summary), "MixGPD summary")
  expect_output(print(fit_params), "Posterior mean parameters")
  expect_s3_class(plot(fit_loc), "mixgpd_fitted_plots")
  expect_silent(print(plot(fit_loc)))
  expect_s3_class(plot(fit_q), "mixgpd_fitted_plots")
  expect_s3_class(plot(pred_density), "mixgpd_predict_plots")
  expect_silent(print(plot(pred_density)))
  expect_s3_class(plot(pred_sample), "mixgpd_predict_plots")
  expect_s3_class(plot(pred_survival), "mixgpd_predict_plots")

  expect_silent(summary(cluster_fit))
  expect_s3_class(plot(cluster_fit, which = "psm"), "dpmixgpd_cluster_psm")
  expect_true(is.numeric(plot(cluster_fit, which = "k")))
  expect_s3_class(plot(cluster_fit, which = "sizes"), "dpmixgpd_cluster_labels")
  expect_silent(summary(cluster_lbl))
  expect_silent(plot(cluster_lbl, type = "sizes"))
  expect_silent(plot(cluster_lbl, type = "certainty"))
  expect_silent(summary(cluster_psm))
  expect_silent(plot(cluster_psm, psm_max_n = nrow(cluster_psm$psm)))

  cp_mean <- structure(
    data.frame(ps = c(0.2, 0.8), estimate = c(1, 2), lower = c(0.8, 1.8), upper = c(1.2, 2.2)),
    class = c("causalmixgpd_causal_predict", "data.frame")
  )
  attr(cp_mean, "type") <- "mean"
  attr(cp_mean, "trt") <- list(fit = data.frame(id = 1:2, estimate = c(2, 3), lower = c(1.5, 2.5), upper = c(2.5, 3.5)))
  attr(cp_mean, "con") <- list(fit = data.frame(id = 1:2, estimate = c(1, 1.5), lower = c(0.5, 1.0), upper = c(1.5, 2.0)))
  cp_density <- structure(
    data.frame(
      y = c(0.2, 0.4),
      trt_estimate = c(0.4, 0.3),
      trt_lower = c(0.3, 0.2),
      trt_upper = c(0.5, 0.4),
      con_estimate = c(0.2, 0.15),
      con_lower = c(0.1, 0.05),
      con_upper = c(0.3, 0.25)
    ),
    class = c("causalmixgpd_causal_predict", "data.frame")
  )
  attr(cp_density, "type") <- "density"

  expect_s3_class(plot(cp_mean), "causalmixgpd_causal_predict_plots")
  expect_s3_class(plot(cp_density), "ggplot")
  expect_output(print(plot(cp_mean)))
  expect_s3_class(plot(.coverage_heavy_causal_fit(), arm = 1L), "mixgpd_fit_plots")
  expect_s3_class(plot(.coverage_heavy_causal_fit(), arm = 0L), "mixgpd_fit_plots")
})

test_that("coverage-heavy glue diagnostics cover link-mode, unconditional, and validation branches", {
  check_glue_validity <- get("check_glue_validity", mode = "function")

  dispatch_stub <- list(
    bulk_params = c("mean", "sd"),
    d = function(x, w, mean, sd, threshold = NULL, tail_scale = NULL, tail_shape = NULL, log = 0L) {
      dens <- stats::dnorm(x, mean = mean[1], sd = max(sd[1], 1e-6))
      if (log == 1L) log(pmax(dens, 1e-300)) else pmax(dens, 1e-300)
    },
    p = function(q, w, mean, sd, threshold = NULL, tail_scale = NULL, tail_shape = NULL,
                 lower.tail = 1L, log.p = 0L) {
      cdf <- stats::pnorm(q, mean = mean[1], sd = max(sd[1], 1e-6))
      if (lower.tail == 0L) cdf <- 1 - cdf
      if (log.p == 1L) log(pmax(cdf, 1e-300)) else cdf
    }
  )

  fake_fit <- structure(
    list(
      spec = list(
        meta = list(backend = "sb", GPD = TRUE, has_X = TRUE),
        dispatch = list(
          backend = "sb",
          GPD = TRUE,
          gpd = list(
            threshold = list(mode = "link", link = "identity"),
            tail_scale = list(mode = "link", link = "exp")
          )
        )
      ),
      data = list(
        X = matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE, dimnames = list(NULL, c("x1", "x2"))),
        y = c(0.25, 1.1)
      )
    ),
    class = "mixgpd_fit"
  )
  fake_uncond <- structure(
    list(
      spec = list(
        meta = list(backend = "sb", GPD = FALSE, has_X = FALSE),
        dispatch = list(backend = "sb", GPD = FALSE)
      ),
      data = list(y = c(0.2, 0.6, 1.0))
    ),
    class = "mixgpd_fit"
  )

  testthat::local_mocked_bindings(
    .get_dispatch = function(...) dispatch_stub,
    .extract_draws_matrix = function(fit) {
      if (isTRUE(fit$spec$meta$GPD)) {
        matrix(c(0.15, 0.2, 0.25), ncol = 1, dimnames = list(NULL, "tail_shape"))
      } else {
        matrix(1:6, nrow = 3, dimnames = list(NULL, c("alpha", "beta")))
      }
    },
    .extract_weights = function(draw_mat, backend) {
      matrix(c(0.7, 0.3, 0.6, 0.4, 0.55, 0.45), nrow = nrow(draw_mat), byrow = TRUE)
    },
    .extract_bulk_params = function(draw_mat, bulk_params) {
      list(
        mean = matrix(c(0.0, 0.8, 0.2, 1.0, 0.4, 1.2), nrow = nrow(draw_mat), byrow = TRUE),
        sd = matrix(c(0.9, 1.1, 1.0, 1.2, 1.1, 1.3), nrow = nrow(draw_mat), byrow = TRUE)
      )
    },
    .indexed_block = function(draw_mat, name, K) {
      if (identical(name, "beta_threshold")) {
        return(matrix(c(0.4, 0.1, 0.5, 0.15, 0.6, 0.2), nrow = nrow(draw_mat), byrow = TRUE))
      }
      if (identical(name, "beta_tail_scale")) {
        return(matrix(log(c(0.9, 1.1, 1.0, 1.2, 1.1, 1.3)), nrow = nrow(draw_mat), byrow = TRUE))
      }
      stop("unexpected indexed block request")
    },
    .package = "CausalMixGPD"
  )

  set.seed(11)
  res <- check_glue_validity(fake_fit, grid = c(-1, 0, 1, 2), n_draws = 2L, check_continuity = TRUE)
  expect_true(all(unlist(res$pass), na.rm = TRUE))
  expect_equal(res$n_checked_draws, 2L)
  expect_equal(res$n_x, 2L)

  res_uncond <- check_glue_validity(fake_uncond, grid = c(0.1, 0.5, 0.9), n_draws = 1L, check_continuity = FALSE)
  expect_true(all(unlist(res_uncond$pass[1:3]), na.rm = TRUE))
  expect_true(is.na(res_uncond$pass$continuity))
  expect_equal(res_uncond$n_x, 1L)

  expect_error(check_glue_validity(fake_uncond, x = matrix(1, ncol = 1), grid = c(0.1, 0.5)), "Unconditional model")
  expect_error(check_glue_validity(fake_uncond, grid = c(0.1, NA_real_)), "finite numeric")
  expect_error(check_glue_validity(fake_uncond, grid = c(0.1, 0.5), n_draws = 0L), ">= 1")
})

test_that("coverage-heavy runner mocks cover cache, compile fallback, and validation branches", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("nimble")
  skip_if(nzchar(Sys.getenv("COVERAGE")), "Runner mock coverage block is unstable under covr instrumentation")

  cache_store <- new.env(parent = emptyenv())
  run_mode <- "fallback"

  testthat::local_mocked_bindings(
    .cmgpd_progress_start = function(...) list(),
    .cmgpd_progress_step = function(...) invisible(NULL),
    .cmgpd_progress_done = function(...) invisible(NULL),
    .cmgpd_message = function(...) invisible(NULL),
    .cmgpd_capture_nimble = function(x, suppress = FALSE) x,
    .extract_nimble_code = function(code) code,
    .mcmc_cache_key = function(...) "coverage-heavy-runner-key",
    .mcmc_cache_get = function(key) {
      if (exists(key, envir = cache_store, inherits = FALSE)) {
        get(key, envir = cache_store, inherits = FALSE)
      } else {
        NULL
      }
    },
    .mcmc_cache_set = function(key, value) {
      assign(key, value, envir = cache_store)
      invisible(value)
    },
    .configure_samplers = function(conf, ...) {
      conf$configured <- TRUE
      conf
    },
    .package = "CausalMixGPD"
  )
  testthat::local_mocked_bindings(
    nimbleModel = function(...) structure(list(kind = "Rmodel"), class = "fake_nimble_model"),
    configureMCMC = function(...) list(kind = "conf"),
    buildMCMC = function(conf) list(kind = "Rmcmc", conf = conf),
    compileNimble = function(...) stop("mock compile failure"),
    runMCMC = function(...) {
      if (identical(run_mode, "fallback")) {
        run_mode <<- "matrix"
        stop("mock run failure")
      }
      if (identical(run_mode, "list")) {
        return(list(
          samples = matrix(c(5, 6, 7), ncol = 1, dimnames = list(NULL, "alpha")),
          WAIC = list(WAIC = 2.34, lppd = 0.4, pWAIC = 0.2)
        ))
      }
      matrix(c(1, 2, 3), ncol = 1, dimnames = list(NULL, "alpha"))
    },
    calculateWAIC = function(...) list(WAIC = 1.23, lppd = 0.3, pWAIC = 0.1),
    .package = "nimble"
  )

  bundle_stub <- structure(
    list(
      spec = list(meta = list()),
      mcmc = list(niter = 8L, nburnin = 2L, thin = 1L, nchains = 1L, seed = 101L, waic = TRUE),
      code = quote(alpha ~ dnorm(0, 1)),
      constants = list(N = 2L),
      data = list(y = c(1, 2)),
      dimensions = list(),
      monitors = "alpha",
      inits = list(alpha = 0),
      epsilon = 0.05
    ),
    class = "causalmixgpd_bundle"
  )

  expect_warning(
    fit1 <- run_mcmc_bundle_manual(bundle_stub, show_progress = FALSE, quiet = TRUE, timing = TRUE),
    "compilation failed"
  )
  expect_s3_class(fit1, "mixgpd_fit")
  expect_identical(fit1$mcmc$engine, "uncompiled")
  expect_equal(fit1$waic$WAIC, 1.23)
  expect_true(isFALSE(fit1$timing$cache_hit))

  run_mode <- "list"
  fit2 <- run_mcmc_bundle_manual(bundle_stub, show_progress = FALSE, quiet = TRUE, timing = TRUE)
  expect_s3_class(fit2, "mixgpd_fit")
  expect_true(isTRUE(fit2$timing$cache_hit))
  expect_equal(fit2$waic$WAIC, 2.34)

  bad_seed_bundle <- bundle_stub
  bad_seed_bundle$mcmc$nchains <- 2L
  bad_seed_bundle$mcmc$seed <- 1:3
  expect_error(run_mcmc_bundle_manual(bad_seed_bundle, show_progress = FALSE, quiet = TRUE), "length 1 or length nchains")
  expect_error(run_mcmc_bundle_manual(bundle_stub, show_progress = FALSE, quiet = TRUE, z_update_every = 0L), ">= 1")
})

test_that("coverage-heavy prediction internals cover chunking unconditional summaries and gpd guards", {
  predict_impl <- get(".predict_mixgpd", mode = "function")

  posterior_summary_mock <- function(x, probs = c(0.025, 0.5, 0.975), interval = "credible") {
    dims <- dim(x)
    if (is.null(dims)) {
      est <- mean(x, na.rm = TRUE)
      low <- min(x, na.rm = TRUE)
      up <- max(x, na.rm = TRUE)
    } else if (length(dims) == 2L) {
      est <- rowMeans(x, na.rm = TRUE)
      low <- apply(x, 1, min, na.rm = TRUE)
      up <- apply(x, 1, max, na.rm = TRUE)
    } else {
      est <- apply(x, c(1, 2), mean, na.rm = TRUE)
      low <- apply(x, c(1, 2), min, na.rm = TRUE)
      up <- apply(x, c(1, 2), max, na.rm = TRUE)
    }
    if (is.null(interval)) {
      low[] <- NA_real_
      up[] <- NA_real_
    }
    list(estimate = est, lower = low, upper = up, q = probs)
  }

  dispatch_stub <- list(
    bulk_params = c("mean", "sd"),
    d = function(x, w, mean, sd, threshold = NULL, tail_scale = NULL, tail_shape = NULL, log = 0L) {
      dens <- stats::dnorm(x, mean = mean[1], sd = max(sd[1], 1e-6))
      if (log == 1L) log(pmax(dens, 1e-300)) else pmax(dens, 1e-300)
    },
    p = function(q, w, mean, sd, threshold = NULL, tail_scale = NULL, tail_shape = NULL,
                 lower.tail = 1L, log.p = 0L) {
      cdf <- stats::pnorm(q, mean = mean[1], sd = max(sd[1], 1e-6))
      if (lower.tail == 0L) cdf <- 1 - cdf
      if (log.p == 1L) log(pmax(cdf, 1e-300)) else cdf
    },
    q = function(p, w, mean, sd, threshold = NULL, tail_scale = NULL, tail_shape = NULL) {
      stats::qnorm(p, mean = mean[1], sd = max(sd[1], 1e-6))
    },
    r = function(n, w, mean, sd, threshold = NULL, tail_scale = NULL, tail_shape = NULL) {
      rep(mean[1], n)
    }
  )

  fake_fit <- structure(
    list(
      spec = list(
        meta = list(backend = "sb", kernel = "normal", GPD = FALSE, has_X = TRUE),
        dispatch = list(backend = "sb", GPD = FALSE, link_params = list())
      ),
      data = list(
        X = matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE, dimnames = list(NULL, c("x1", "x2"))),
        y = c(0.25, 1.25)
      )
    ),
    class = "mixgpd_fit"
  )
  fake_uncond <- structure(
    list(
      spec = list(
        meta = list(backend = "sb", kernel = "normal", GPD = FALSE, has_X = FALSE),
        dispatch = list(backend = "sb", GPD = FALSE, link_params = list())
      ),
      data = list(y = c(0.25, 1.25))
    ),
    class = "mixgpd_fit"
  )
  fake_gpd <- structure(
    list(
      spec = list(
        meta = list(backend = "sb", kernel = "normal", GPD = TRUE, has_X = FALSE),
        dispatch = list(
          backend = "sb",
          GPD = TRUE,
          link_params = list(),
          gpd = list(
            threshold = list(mode = "constant"),
            tail_scale = list(mode = "constant"),
            tail_shape = list(mode = "dist")
          )
        )
      ),
      data = list(y = c(0.25, 1.25))
    ),
    class = "mixgpd_fit"
  )
  fake_spliced <- structure(
    list(
      spec = list(
        meta = list(backend = "spliced", kernel = "normal", GPD = TRUE, has_X = TRUE),
        dispatch = list(
          backend = "spliced",
          GPD = TRUE,
          link_params = list(),
          gpd = list(
            threshold = list(mode = "link", link = "identity"),
            tail_scale = list(mode = "dist"),
            tail_shape = list(mode = "dist")
          )
        )
      ),
      data = list(
        X = fake_fit$data$X,
        y = fake_fit$data$y
      )
    ),
    class = "mixgpd_fit"
  )

  testthat::local_mocked_bindings(
    .validate_fit = function(object) TRUE,
    .cmgpd_progress_start = function(...) list(),
    .cmgpd_progress_step = function(...) invisible(NULL),
    .cmgpd_progress_done = function(...) invisible(NULL),
    .extract_draws_matrix = function(object) {
      if (isTRUE(object$spec$meta$GPD)) {
        matrix(
          c(0.4, 0.5, 0.9,
            1.1, 0.6, 1.0,
            0.3, 0.7, 1.1),
          ncol = 3,
          byrow = TRUE,
          dimnames = list(NULL, c("tail_shape", "threshold", "tail_scale"))
        )
      } else {
        matrix(1:6, ncol = 2, dimnames = list(NULL, c("draw1", "draw2")))
      }
    },
    .extract_weights = function(draw_mat, backend) {
      matrix(c(0.7, 0.3, 0.6, 0.4, 0.5, 0.5), nrow = nrow(draw_mat), byrow = TRUE)
    },
    .extract_bulk_params = function(draw_mat, bulk_params) {
      list(
        mean = matrix(c(0.0, 0.8, 0.2, 1.0, 0.4, 1.2), nrow = nrow(draw_mat), byrow = TRUE),
        sd = matrix(c(1.0, 1.2, 1.1, 1.3, 1.2, 1.4), nrow = nrow(draw_mat), byrow = TRUE)
      )
    },
    .get_dispatch = function(...) dispatch_stub,
    get_kernel_registry = function() list(normal = list(bulk_support = list(mean = "", sd = "positive_sd"))),
    .posterior_summarize = posterior_summary_mock,
    .compute_interval = function(x, level, type) c(lower = min(x, na.rm = TRUE), upper = max(x, na.rm = TRUE)),
    .package = "CausalMixGPD"
  )

  pred_density <- predict_impl(
    fake_fit,
    x = fake_fit$data$X,
    y = c(0.2, 0.9),
    id = 1:2,
    type = "density",
    interval = NULL,
    chunk_size = 1L,
    show_progress = FALSE
  )
  pred_survival <- predict_impl(fake_fit, x = fake_fit$data$X, y = c(0.2, 0.9), type = "survival", show_progress = FALSE)
  pred_quant <- predict_impl(fake_fit, x = fake_fit$data$X, type = "quantile", index = c(0.25, 0.75), store_draws = TRUE, show_progress = FALSE)
  pred_sample <- predict_impl(fake_fit, x = fake_fit$data$X, type = "sample", nsim = 3L, show_progress = FALSE)
  pred_fit <- predict_impl(fake_fit, x = fake_fit$data$X, type = "fit", show_progress = FALSE)
  pred_mean <- predict_impl(fake_fit, x = fake_fit$data$X, type = "mean", nsim_mean = 12L, show_progress = FALSE)
  pred_rmean <- predict_impl(fake_fit, x = fake_fit$data$X, type = "rmean", cutoff = 1.1, nsim_mean = 12L, show_progress = FALSE)

  pred_quant_u <- predict_impl(fake_uncond, type = "quantile", index = c(0.25, 0.75), show_progress = FALSE)
  pred_sample_u <- predict_impl(fake_uncond, type = "sample", nsim = 4L, show_progress = FALSE)
  pred_mean_u <- predict_impl(fake_uncond, type = "mean", nsim_mean = 12L, show_progress = FALSE)
  pred_rmean_u <- predict_impl(fake_uncond, type = "rmean", cutoff = 1.1, nsim_mean = 12L, show_progress = FALSE)

  expect_s3_class(pred_density, "mixgpd_predict")
  expect_s3_class(pred_survival, "mixgpd_predict")
  expect_s3_class(pred_quant, "mixgpd_predict")
  expect_s3_class(pred_sample, "mixgpd_predict")
  expect_s3_class(pred_fit, "mixgpd_predict")
  expect_s3_class(pred_mean, "mixgpd_predict")
  expect_s3_class(pred_rmean, "mixgpd_predict")
  expect_s3_class(pred_quant_u, "mixgpd_predict")
  expect_s3_class(pred_sample_u, "mixgpd_predict")
  expect_s3_class(pred_mean_u, "mixgpd_predict")
  expect_s3_class(pred_rmean_u, "mixgpd_predict")
  expect_equal(pred_density$diagnostics$n_chunks, 2L)
  expect_true(all(c("id", "density") %in% names(pred_density$fit)))
  expect_true(all(c("id", "survival") %in% names(pred_survival$fit)))
  expect_true(all(c("id", "index", "estimate") %in% names(pred_quant$fit)))

  expect_warning(
    pred_mean_inf <- predict_impl(fake_gpd, type = "mean", nsim_mean = 12L, show_progress = FALSE),
    "Posterior mean is infinite"
  )
  expect_true(all(is.infinite(pred_mean_inf$fit$estimate)))

  expect_error(
    predict_impl(fake_spliced, x = fake_spliced$data$X, type = "mean", nsim_mean = 12L, show_progress = FALSE),
    "not yet fully implemented"
  )
})

test_that("coverage-heavy predict and residual methods cover wrapper branches with mocked internals", {
  predict_method <- get("predict.mixgpd_fit", mode = "function")
  residuals_method <- get("residuals.mixgpd_fit", mode = "function")
  fake_fit <- structure(
    list(
      spec = list(meta = list(backend = "sb", kernel = "normal", GPD = FALSE)),
      data = list(
        X = matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE, dimnames = list(NULL, c("x1", "x2"))),
        y = c(0.2, 0.8)
      )
    ),
    class = "mixgpd_fit"
  )

  testthat::local_mocked_bindings(
    .validate_fit = function(object) TRUE,
    .predict_mixgpd = function(object, x = NULL, y = NULL, ps = NULL, id = NULL, type = "mean", ...) {
      if (identical(type, "mean")) {
        return(list(fit = data.frame(id = 1:2, estimate = c(1.0, 2.0), lower = c(0.8, 1.8), upper = c(1.2, 2.2)), type = type))
      }
      if (identical(type, "median")) {
        return(list(fit = data.frame(id = 1:2, estimate = c(0.9, 1.9), lower = c(0.7, 1.7), upper = c(1.1, 2.1)), type = type))
      }
      list(fit = data.frame(id = 1:2, estimate = c(1.0, 2.0), lower = c(0.8, 1.8), upper = c(1.2, 2.2)), type = type)
    },
    .package = "CausalMixGPD"
  )

  loc <- predict_method(
    fake_fit,
    x = fake_fit$data$X,
    id = 1:2,
    type = "location",
    interval = "none"
  )
  expect_s3_class(loc, "mixgpd_predict")
  expect_true(all(c("mean", "median") %in% names(loc$fit)))

  expect_warning(predict_method(fake_fit, type = "mean", p = 0.2), "ignoring")
  expect_error(predict_method(fake_fit, x = fake_fit$data$X, newdata = fake_fit$data$X, type = "mean"), "Provide only one")
  expect_error(predict_method(fake_fit, type = "median", index = 0.25), "index = 0.5")
  expect_error(predict_method(fake_fit, type = "mean", level = 1.5), "between 0 and 1")
  expect_error(predict_method(fake_fit, type = "mean", ncores = 0L), ">= 1")

  testthat::local_mocked_bindings(
    fitted.mixgpd_fit = function(object, type = "mean", ...) {
      structure(data.frame(residuals = c(0.1, -0.2)), class = c("mixgpd_fitted", "data.frame"))
    },
    predict.mixgpd_fit = function(object, x = NULL, y = NULL, type = c("density", "survival", "quantile", "sample", "mean", "rmean", "median", "location", "fit"), ...) {
      list(fit = data.frame(survival = c(0.8, 0.4)))
    },
    .get_dispatch = function(...) {
      list(
        p = function(q, w, mean, sd, lower.tail = 1L, log.p = 0L) stats::pnorm(q, mean = mean[1], sd = sd[1]),
        bulk_params = c("mean", "sd")
      )
    },
    .extract_draws_matrix = function(object) matrix(1:6, ncol = 2, dimnames = list(NULL, c("draw1", "draw2"))),
    .extract_weights = function(draw_mat, backend) matrix(c(0.7, 0.3, 0.6, 0.4, 0.5, 0.5), nrow = nrow(draw_mat), byrow = TRUE),
    .extract_bulk_params = function(draw_mat, bulk_params) {
      list(
        mean = matrix(c(0.0, 0.8, 0.2, 1.0, 0.4, 1.2), nrow = nrow(draw_mat), byrow = TRUE),
        sd = matrix(c(1.0, 1.2, 1.1, 1.3, 1.2, 1.4), nrow = nrow(draw_mat), byrow = TRUE)
      )
    },
    get_kernel_registry = function() list(normal = list(bulk_support = list(mean = "", sd = "positive_sd"))),
    .package = "CausalMixGPD"
  )

  expect_equal(residuals_method(fake_fit, type = "raw", fitted_type = "median"), c(0.1, -0.2))
  expect_equal(as.numeric(residuals_method(fake_fit, type = "pit", pit = "plugin")), c(0.2, 0.6))

  pit_mean <- residuals_method(fake_fit, type = "pit", pit = "bayes_mean", pit_seed = 7L)
  pit_draw <- residuals_method(fake_fit, type = "pit", pit = "bayes_draw", pit_seed = 7L)
  expect_length(pit_mean, 2L)
  expect_length(pit_draw, 2L)
  expect_true(!is.null(attr(pit_mean, "pit_diagnostics")))
  expect_true(!is.null(attr(pit_draw, "pit_diagnostics")))
})

test_that("coverage-heavy kernel quantiles and lowercase wrappers cover boundary branches", {
  w <- c(0.6, 0.4)
  norm_mean <- c(-0.5, 1.0)
  norm_sd <- c(0.9, 1.2)

  expect_equal(qNormMix(c(0, 1), w = w, mean = norm_mean, sd = norm_sd), c(-Inf, Inf))
  expect_true(all(is.finite(qNormMix(log(c(0.2, 0.8)), w = w, mean = norm_mean, sd = norm_sd, log.p = TRUE))))
  expect_true(all(is.finite(qNormMixGpd(c(0.2, 0.95), w = w, mean = norm_mean, sd = norm_sd,
                                        threshold = 1.5, tail_scale = 0.8, tail_shape = 0.1))))
  expect_true(all(is.finite(qNormGpd(log(c(0.2, 0.8)), mean = 0.3, sd = 1.1,
                                     threshold = 1.5, tail_scale = 0.8, tail_shape = 0.1,
                                     log.p = TRUE))))
  expect_identical(dnormmix(numeric(0), w = w, mean = norm_mean, sd = norm_sd), numeric(0))
  expect_identical(dnormmixgpd(numeric(0), w = w, mean = norm_mean, sd = norm_sd,
                               threshold = 1.5, tail_scale = 0.8, tail_shape = 0.1), numeric(0))
  expect_identical(dnormgpd(numeric(0), mean = 0.3, sd = 1.1,
                            threshold = 1.5, tail_scale = 0.8, tail_shape = 0.1), numeric(0))
  expect_identical(rnormmix(0L, w = w, mean = norm_mean, sd = norm_sd), numeric(0))
  expect_identical(rnormmixgpd(0L, w = w, mean = norm_mean, sd = norm_sd,
                               threshold = 1.5, tail_scale = 0.8, tail_shape = 0.1), numeric(0))
  expect_identical(rnormgpd(0L, mean = 0.3, sd = 1.1,
                            threshold = 1.5, tail_scale = 0.8, tail_shape = 0.1), numeric(0))
  expect_error(rnormmix(NA_integer_, w = w, mean = norm_mean, sd = norm_sd), "single integer")

  expect_equal(qGpd(c(0, 1), threshold = 1, scale = 0.8, shape = -0.2), c(1, 5))
  expect_true(is.finite(qGpd(log(0.25), threshold = 1, scale = 0.8, shape = 0.2, log.p = TRUE)))
  expect_equal(qInvGauss(c(0, 1), mean = 1.5, shape = 5), c(0, Inf))
  expect_true(is.finite(qInvGauss(log(0.25), mean = 1.5, shape = 5, log.p = TRUE)))
  expect_equal(qAmoroso(c(0, 1), loc = c(0, 0), scale = c(1, -1), shape1 = c(2, 2), shape2 = c(1.2, 1.2)), c(0, -Inf))
  expect_equal(qCauchy(c(0, 1), location = 0, scale = 1), c(-Inf, Inf))
  expect_identical(dgpd(numeric(0), threshold = 1, scale = 0.8, shape = 0.2), numeric(0))
  expect_identical(dinvgauss(numeric(0), mean = 1.5, shape = 5), numeric(0))
  expect_identical(damoroso(numeric(0), loc = 0, scale = 1.4, shape1 = 2, shape2 = 1.2), numeric(0))
  expect_identical(dcauchy_vec(numeric(0), location = 0, scale = 1), numeric(0))
  expect_identical(rgpd(0L, threshold = 1, scale = 0.8, shape = 0.2), numeric(0))
  expect_identical(rinvgauss(0L, mean = 1.5, shape = 5), numeric(0))
  expect_identical(ramoroso(0L, loc = 0, scale = 1.4, shape1 = 2, shape2 = 1.2), numeric(0))
  expect_identical(rcauchy_vec(0L, location = 0, scale = 1), numeric(0))
  expect_error(rgpd(NA_integer_, threshold = 1, scale = 0.8, shape = 0.2), "single integer")

  amoroso_loc <- c(0.0, 0.2)
  amoroso_scale <- c(1.0, 1.5)
  amoroso_shape1 <- c(2.0, 2.5)
  amoroso_shape2 <- c(1.1, 1.3)
  amoroso_mix <- qAmorosoMix(c(0, 1), w = w, loc = amoroso_loc, scale = amoroso_scale,
                             shape1 = amoroso_shape1, shape2 = amoroso_shape2)
  amoroso_mix_gpd <- qAmorosoMixGpd(c(0.2, 0.95), w = w, loc = amoroso_loc, scale = amoroso_scale,
                                    shape1 = amoroso_shape1, shape2 = amoroso_shape2,
                                    threshold = 2.5, tail_scale = 0.9, tail_shape = 0.1)
  amoroso_gpd <- qAmorosoGpd(c(0.2, 0.95), loc = 0.1, scale = 1.3, shape1 = 2.2, shape2 = 1.1,
                             threshold = 2.5, tail_scale = 0.9, tail_shape = 0.1)

  expect_equal(amoroso_mix, c(-Inf, Inf))
  expect_true(all(is.finite(amoroso_mix_gpd)))
  expect_true(all(is.finite(amoroso_gpd)))
  expect_identical(damorosomix(numeric(0), w = w, loc = amoroso_loc, scale = amoroso_scale,
                               shape1 = amoroso_shape1, shape2 = amoroso_shape2), numeric(0))
  expect_identical(damorosomixgpd(numeric(0), w = w, loc = amoroso_loc, scale = amoroso_scale,
                                  shape1 = amoroso_shape1, shape2 = amoroso_shape2,
                                  threshold = 2.5, tail_scale = 0.9, tail_shape = 0.1), numeric(0))
  expect_identical(damorosogpd(numeric(0), loc = 0.1, scale = 1.3, shape1 = 2.2, shape2 = 1.1,
                               threshold = 2.5, tail_scale = 0.9, tail_shape = 0.1), numeric(0))
  expect_identical(ramorosomix(0L, w = w, loc = amoroso_loc, scale = amoroso_scale,
                               shape1 = amoroso_shape1, shape2 = amoroso_shape2), numeric(0))
  expect_identical(ramorosomixgpd(0L, w = w, loc = amoroso_loc, scale = amoroso_scale,
                                  shape1 = amoroso_shape1, shape2 = amoroso_shape2,
                                  threshold = 2.5, tail_scale = 0.9, tail_shape = 0.1), numeric(0))
  expect_identical(ramorosogpd(0L, loc = 0.1, scale = 1.3, shape1 = 2.2, shape2 = 1.1,
                               threshold = 2.5, tail_scale = 0.9, tail_shape = 0.1), numeric(0))
  expect_error(ramorosomix(NA_integer_, w = w, loc = amoroso_loc, scale = amoroso_scale,
                           shape1 = amoroso_shape1, shape2 = amoroso_shape2), "single integer")
})

test_that("coverage-heavy build-run advanced helpers cover gating and standard gpd branches", {
  skip_if_not_test_level("ci")

  y <- abs(stats::rnorm(10L)) + 0.1
  X <- cbind(x1 = stats::rnorm(10L), x2 = stats::runif(10L))
  ps <- rep(0.5, length(y))
  dat <- data.frame(y = y, x1 = X[, 1], x2 = X[, 2])

  expect_error(build_code_from_spec(list(meta = list(backend = "bad"))), "Unknown backend")
  expect_error(
    build_nimble_bundle(y = y, backend = "sb", kernel = "normal", GPD = FALSE, components = c(2L, 3L)),
    "single integer"
  )

  auto_bundle <- build_nimble_bundle(
    y = y,
    X = X,
    backend = "spliced",
    kernel = "normal",
    GPD = TRUE,
    param_specs = list(
      bulk = list(mean = list(mode = "dist", dist = "normal", args = list(mean = 0, sd = 1))),
      gpd = list(
        threshold = list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1)),
        tail_scale = list(mode = "fixed", value = 1.2),
        tail_shape = list(mode = "dist", dist = "normal", args = list(mean = 0, sd = 0.2))
      )
    ),
    monitor = "full"
  )
  expect_equal(auto_bundle$spec$meta$components, length(y))
  expect_identical(auto_bundle$spec$dispatch$backend, "spliced")
  expect_true(isTRUE(auto_bundle$monitor_policy$monitor_latent))

  spec_std <- compile_model_spec(
    y = y,
    X = X,
    ps = ps,
    backend = "sb",
    kernel = "normal",
    GPD = TRUE,
    components = 4L,
    param_specs = list(
      bulk = list(
        mean = list(
          mode = "link",
          link = "identity",
          beta_prior = list(dist = "normal", args = list(mean = 0, sd = 1.5))
        ),
        sd = list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1))
      ),
      gpd = list(
        threshold = list(
          mode = "link",
          link = "identity",
          beta_prior = list(dist = "normal", args = list(mean = 0, sd = 0.2)),
          link_dist = list(dist = "lognormal")
        ),
        sdlog_u = list(mode = "dist", dist = "invgamma", args = list(shape = 2, scale = 1)),
        tail_scale = list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1)),
        tail_shape = list(
          mode = "link",
          link = "identity",
          beta_prior = list(dist = "normal", args = list(mean = 0, sd = 0.3))
        )
      )
    )
  )

  const_std <- build_constants_from_spec(spec_std)
  dims_std <- build_dimensions_from_spec(spec_std)
  mons_std <- build_monitors_from_spec(spec_std, monitor_v = TRUE, monitor_latent = TRUE)
  inits_std <- build_inits_from_spec(spec_std, y = y)
  prior_std <- build_prior_table_from_spec(spec_std)

  expect_true(all(c("N", "P", "components") %in% names(const_std)))
  expect_true(all(c("beta_mean", "beta_ps_mean", "threshold", "beta_threshold", "beta_tail_shape") %in% names(dims_std)))
  expect_true(any(grepl("^beta_tail_shape", mons_std)))
  expect_true(all(c("beta_mean", "beta_ps_mean", "beta_threshold", "threshold", "sdlog_u", "tail_scale", "beta_tail_shape") %in% names(inits_std)))
  expect_true(any(prior_std$parameter == "threshold" & prior_std$mode == "link+dist"))
  expect_true(any(prior_std$parameter == "tail_shape" & prior_std$mode == "link"))

  cluster_bundle <- build_cluster_bundle(
    y ~ x1 + x2,
    data = dat,
    kernel = "normal",
    GPD = TRUE,
    type = "weights",
    components = 4L,
    link = list(
      bulk = list(mean = "identity"),
      gpd = list(
        threshold = list(link = "identity"),
        tail_shape = list(link = "identity")
      )
    ),
    priors = list(
      bulk = list(sd = list(dist = "gamma", args = list(shape = 2, rate = 1))),
      gpd = list(
        tail_scale = list(dist = "gamma", args = list(shape = 2, rate = 1)),
        tail_shape = list(dist = "normal", args = list(mean = 0, sd = 0.3))
      ),
      concentration = list(dist = "gamma", args = list(shape = 3, rate = 2))
    ),
    mcmc = mcmc_fast(seed = 17L)
  )
  gating_spec <- cluster_bundle$spec
  dims_gating <- build_dimensions_from_spec(gating_spec)
  mons_gating <- build_monitors_from_spec(gating_spec, monitor_latent = TRUE)
  inits_gating <- build_inits_from_spec(gating_spec, y = y)
  const_gating <- build_constants_from_spec(gating_spec)
  prior_gating <- build_prior_table_from_spec(gating_spec)

  expect_true(all(c("eta", "B", "logit_ij", "w_x") %in% names(dims_gating)))
  expect_true(any(grepl("^eta", mons_gating)))
  expect_true(any(grepl("^B", mons_gating)))
  expect_true(all(c("eta", "B", "z") %in% names(inits_gating)))
  expect_true(any(prior_gating$parameter == "threshold"))
  expect_true(all(c("N", "P", "components") %in% names(const_gating)))
})

test_that("coverage-heavy methods cover params and ESS summary branches with mocked draws", {
  skip_if_not_installed("coda")

  draw_mat_params <- matrix(
    c(
      1.0, 0.7, 0.3, 0.1, 0.2, -0.1, 0.0, 0.05, -0.05, 0.2, 0.3, 0.4, 1.1, 0.1, 0.2,
      1.2, 0.6, 0.4, 0.2, 0.1, -0.2, 0.1, 0.00, -0.02, 0.1, 0.2, 0.5, 1.3, 0.2, 0.1,
      0.8, 0.5, 0.5, 0.3, 0.0, -0.3, 0.2, -0.01, 0.01, 0.3, 0.1, 0.6, 1.2, 0.15, 0.05
    ),
    nrow = 3L,
    byrow = TRUE,
    dimnames = list(
      NULL,
      c(
        "alpha", "w[1]", "w[2]",
        "beta_mean[1,1]", "beta_mean[1,2]", "beta_mean[2,1]", "beta_mean[2,2]",
        "beta_ps_mean[1]", "beta_ps_mean[2]",
        "beta_threshold[1]", "beta_threshold[2]", "sdlog_u",
        "tail_scale",
        "beta_tail_shape[1]", "beta_tail_shape[2]"
      )
    )
  )

  fit_params <- structure(
    list(
      spec = list(
        meta = list(backend = "sb", kernel = "normal", GPD = TRUE, has_X = TRUE),
        plan = list(
          bulk = list(
            mean = list(mode = "link"),
            sd = list(mode = "fixed", value = 1)
          ),
          ps = list(prior = list(dist = "normal", args = list(mean = 0, sd = 1))),
          gpd = list(
            threshold = list(mode = "link", link_dist = list(dist = "lognormal")),
            tail_scale = list(mode = "dist"),
            tail_shape = list(mode = "link")
          )
        )
      ),
      data = list(X = structure(matrix(c(1, 0, 0, 1), nrow = 2L), dimnames = list(NULL, c("x1", "x2"))))
    ),
    class = "mixgpd_fit"
  )

  testthat::local_mocked_bindings(
    .extract_draws_matrix = function(...) draw_mat_params,
    .package = "CausalMixGPD"
  )

  param_obj <- params(fit_params)
  expect_s3_class(param_obj, "mixgpd_params")
  expect_true(all(c("alpha", "w", "beta_mean", "beta_ps_mean", "beta_threshold", "sdlog_u", "tail_scale") %in% names(param_obj)))
  expect_equal(dim(param_obj$beta_mean), c(2L, 2L))
  expect_identical(colnames(param_obj$beta_mean), c("x1", "x2"))

  chain1 <- coda::mcmc(
    matrix(
      c(
        1.0, 0.2, 0.5, 1.2, 0.1,
        1.1, 0.3, 0.6, 1.1, 0.2,
        0.9, 0.4, 0.7, 1.0, 0.3,
        1.2, 0.5, 0.8, 1.3, 0.4
      ),
      nrow = 4L,
      byrow = TRUE,
      dimnames = list(NULL, c("alpha", "tail_shape", "mu[1]", "sigma[1]", "beta[1]"))
    )
  )
  chain2 <- coda::mcmc(
    matrix(
      c(
        0.8, 0.1, 0.4, 1.0, 0.0,
        0.9, 0.2, 0.5, 1.1, 0.1,
        1.0, 0.3, 0.6, 1.2, 0.2,
        1.1, 0.4, 0.7, 1.3, 0.3
      ),
      nrow = 4L,
      byrow = TRUE,
      dimnames = list(NULL, c("alpha", "tail_shape", "mu[1]", "sigma[1]", "beta[1]"))
    )
  )
  fit_ess <- structure(
    list(
      mcmc = list(samples = coda::mcmc.list(chain1, chain2)),
      timing = list(mcmc = 6)
    ),
    class = "mixgpd_fit"
  )

  ess_obj <- ess_summary(fit_ess, per_chain = TRUE)
  expect_s3_class(ess_obj, "mixgpd_ess_summary")
  expect_true(nrow(ess_obj$table) >= 10L)
  expect_true(nrow(summary(ess_obj)) >= 1L)
  expect_output(print(ess_obj, max_rows = 2L), "ESS summary")
  expect_error(ess_summary(fit_ess, params = "missing_param", robust = FALSE), "No parameters matched")

  fit_ess_no_time <- fit_ess
  fit_ess_no_time$timing <- list()
  expect_warning(no_time_res <- try(ess_summary(fit_ess_no_time, params = "alpha", per_chain = FALSE), silent = TRUE), "No wall-time available")
  expect_true(inherits(no_time_res, "try-error"))

  causal_ess <- structure(list(outcome_fit = list(con = fit_ess, trt = fit_ess)), class = "causalmixgpd_causal_fit")
  ess_causal <- ess_summary(causal_ess, params = c("alpha", "mu\\[1\\]"), wall_time = 8, per_chain = FALSE)
  expect_s3_class(ess_causal, "mixgpd_ess_summary")
  expect_true(all(c("control", "treated") %in% ess_causal$table$arm))
})

test_that("coverage-heavy methods cover predict wrappers fitted and residual PIT branches", {
  fit_stub <- structure(list(), class = "mixgpd_fit")
  x_new <- matrix(c(1, 2, 3, 4), ncol = 2L)
  predict_calls <- character(0)

  testthat::local_mocked_bindings(
    .validate_fit = function(...) invisible(TRUE),
    .predict_mixgpd = function(object, x = NULL, type = c("mean", "median", "quantile"), ...) {
      type <- match.arg(type)
      predict_calls <<- c(predict_calls, type)
      if (type == "mean") {
        return(list(fit = data.frame(estimate = c(1.0, 2.0), lower = c(0.5, 1.5), upper = c(1.5, 2.5))))
      }
      if (type == "median") {
        return(list(fit = data.frame(estimate = c(0.8, 1.8), lower = c(0.3, 1.3), upper = c(1.3, 2.3))))
      }
      list(fit = data.frame(id = c(2L, 1L), estimate = c(1.5, 0.5), lower = c(1.0, 0.1), upper = c(2.0, 0.9)))
    },
    .package = "CausalMixGPD"
  )

  loc_obj <- predict(fit_stub, newdata = x_new, type = "location", workers = 2L, parallel = TRUE)
  expect_true(is.list(loc_obj) || is.data.frame(loc_obj))
  expect_equal(predict_calls, c("mean", "median"))
  expect_true(all(c("mean", "median") %in% names(loc_obj$fit)))
  qpred <- predict(fit_stub, newdata = x_new, type = "quantile", p = c(0.2, 0.8))
  qfit <- if (is.list(qpred) && !is.data.frame(qpred)) qpred$fit else qpred
  expect_true(is.data.frame(qfit))
  expect_error(predict(fit_stub, x = x_new, newdata = x_new, type = "mean"), "Provide only one")
  expect_error(predict(fit_stub, newdata = x_new, type = "quantile", p = 0.2, index = 0.3), "Provide only one")
  expect_error(predict(fit_stub, newdata = x_new, type = "median", index = 0.2), "index = 0.5")
  expect_error(predict(fit_stub, newdata = x_new, type = "mean", cred.level = 0.9), "cred.level")
  expect_warning(predict(fit_stub, newdata = x_new, type = "mean", p = 0.25), "only used for type = 'quantile'")

  plugin_with_id <- FALSE
  fit_cond <- structure(
    list(
      data = list(
        y = c(2.0, 3.0),
        X = structure(matrix(c(1, 0, 0, 1), nrow = 2L), dimnames = list(NULL, c("x1", "x2")))
      ),
      spec = list(
        meta = list(backend = "sb", kernel = "normal", GPD = TRUE, has_X = TRUE),
        dispatch = list(
          backend = "sb",
          GPD = TRUE,
          gpd = list(
            threshold = list(mode = "dist"),
            tail_scale = list(mode = "dist")
          )
        )
      )
    ),
    class = "mixgpd_fit"
  )

  draw_mat_pit <- matrix(
    c(
      0.7, 0.3, 1.0, 2.0, 1.0, 1.2, 0.1, 1.0, 1.0,
      0.6, 0.4, 1.2, 2.2, 1.0, 1.3, 0.2, 1.1, -1.0,
      0.5, 0.5, 1.4, 2.4, 1.0, 1.4, 0.3, 1.2, 1.1
    ),
    nrow = 3L,
    byrow = TRUE,
    dimnames = list(NULL, c("w[1]", "w[2]", "mean[1]", "mean[2]", "sd[1]", "sd[2]", "tail_shape", "threshold", "tail_scale"))
  )

  testthat::local_mocked_bindings(
    predict.mixgpd_fit = function(object, x = NULL, y = NULL, type = c("mean", "median", "quantile", "survival"), ...) {
      type <- match.arg(type)
      if (type == "mean") {
        return(list(fit = data.frame(id = c(2L, 1L), estimate = c(3.0, 2.0), lower = c(2.5, 1.5), upper = c(3.5, 2.5))))
      }
      if (type == "median") {
        return(list(fit = data.frame(id = c(2L, 1L), estimate = c(2.8, 1.8), lower = c(2.3, 1.3), upper = c(3.3, 2.3))))
      }
      if (type == "quantile") {
        return(list(fit = data.frame(id = c(2L, 1L), estimate = c(2.6, 1.6), lower = c(2.1, 1.1), upper = c(3.1, 2.1))))
      }
      if (isTRUE(plugin_with_id)) {
        return(list(fit = data.frame(id = c(1L, 1L, 2L, 2L), survival = c(0.8, 0.7, 0.6, 0.5))))
      }
      list(fit = data.frame(survival = c(0.8, 0.6)))
    },
    .get_dispatch = function(...) {
      list(
        p = function(q, w, mean, sd, threshold = NULL, tail_scale = NULL, tail_shape = NULL, lower.tail = 1L, log.p = 0L) {
          cdf <- stats::pnorm(q, mean = mean[1], sd = max(sd[1], 1e-6))
          if (lower.tail == 0L) cdf <- 1 - cdf
          if (log.p == 1L) log(pmax(cdf, 1e-12)) else cdf
        },
        bulk_params = c("mean", "sd")
      )
    },
    .extract_draws_matrix = function(...) draw_mat_pit,
    .extract_weights = function(...) matrix(c(0.7, 0.3, 0.6, 0.4, 0.5, 0.5), nrow = 3L, byrow = TRUE),
    .extract_bulk_params = function(...) {
      list(
        mean = matrix(c(1.0, 2.0, 1.2, 2.2, 1.4, 2.4), nrow = 3L, byrow = TRUE),
        sd = matrix(c(1.0, 1.2, 1.0, 1.3, 1.0, 1.4), nrow = 3L, byrow = TRUE)
      )
    },
    get_kernel_registry = function() list(normal = list(bulk_support = list(mean = "", sd = "positive_sd"))),
    .package = "CausalMixGPD"
  )

  fitted_loc <- fitted(fit_cond, type = "location", interval = "credible", seed = 1L)
  expect_s3_class(fitted_loc, "mixgpd_fitted")
  expect_true(all(c("mean", "median", "mean_lower", "median_upper") %in% names(fitted_loc)))
  expect_s3_class(fitted(fit_cond, type = "quantile", p = 0.4, interval = "credible", seed = 1L), "mixgpd_fitted")
  expect_equal(residuals(fit_cond, type = "raw", fitted_type = "median"), fit_cond$data$y - c(1.8, 2.8))

  plugin_plain <- residuals(fit_cond, type = "pit", pit = "plugin")
  expect_equal(as.numeric(plugin_plain), c(0.2, 0.4))
  plugin_with_id <- TRUE
  plugin_diag <- residuals(fit_cond, type = "pit", pit = "plugin")
  expect_equal(as.numeric(plugin_diag), c(0.2, 0.5))

  bayes_mean <- residuals(fit_cond, type = "pit", pit = "bayes_mean")
  expect_equal(attr(bayes_mean, "pit_type"), "bayes_mean")
  expect_equal(attr(bayes_mean, "pit_diagnostics")$n_draws_dropped, 1L)
  bayes_draw <- residuals(fit_cond, type = "pit", pit = "bayes_draw", pit_seed = 1L)
  expect_equal(attr(bayes_draw, "pit_type"), "bayes_draw")
  expect_equal(length(bayes_draw), 2L)

  fit_uncond <- fit_cond
  fit_uncond$data$X <- NULL
  fit_uncond$spec$meta$has_X <- FALSE
  expect_error(fitted(fit_uncond), "not supported for unconditional models")
  expect_error(residuals(fit_uncond), "not supported for unconditional models")
})

test_that("coverage-heavy cluster helper internals cover density scoring and design branches", {
  skip_if_not_installed("coda")

  ns_fun <- function(name) getFromNamespace(name, "CausalMixGPD")
  cluster_default_link <- ns_fun(".cluster_default_link")
  cluster_link_bulk_specs <- ns_fun(".cluster_link_bulk_specs")
  cluster_default_beta_prior <- ns_fun(".cluster_default_beta_prior")
  cluster_split_overrides <- ns_fun(".cluster_split_overrides")
  cluster_normalize_link_entry <- ns_fun(".cluster_normalize_link_entry")
  cluster_normalize_prior_entry <- ns_fun(".cluster_normalize_prior_entry")
  cluster_parse_formula <- ns_fun(".cluster_parse_formula")
  cluster_check_new_factor_levels <- ns_fun(".cluster_check_new_factor_levels")
  cluster_build_design <- ns_fun(".cluster_build_design")
  cluster_link_apply <- ns_fun(".cluster_link_apply")
  cluster_extract_beta_component <- ns_fun(".cluster_extract_beta_component")
  cluster_extract_beta_global <- ns_fun(".cluster_extract_beta_global")
  cluster_extract_beta_auto <- ns_fun(".cluster_extract_beta_auto")
  cluster_softmax <- ns_fun(".cluster_softmax")
  cluster_extract_gating_draw <- ns_fun(".cluster_extract_gating_draw")
  cluster_gating_weights <- ns_fun(".cluster_gating_weights")
  cluster_resolve_density_fun <- ns_fun(".cluster_resolve_density_fun")
  cluster_component_density <- ns_fun(".cluster_component_density")
  cluster_compute_scores <- ns_fun(".cluster_compute_scores")

  expect_equal(cluster_default_beta_prior("threshold")$args$sd, 0.2)
  expect_equal(cluster_default_link(get_kernel_registry()[["gamma"]], "scale"), "exp")
  expect_true(is.list(cluster_link_bulk_specs("normal", beta_sd = 1.5)))

  split1 <- cluster_split_overrides(list(mean = list(mode = "link"), tail_shape = list(mode = "dist")), c("mean"), c("tail_shape"))
  expect_true("mean" %in% names(split1$bulk))
  expect_true("tail_shape" %in% names(split1$gpd))
  expect_equal(cluster_normalize_link_entry("exp")$link, "exp")
  expect_error(cluster_normalize_link_entry(list(mode = "dist")), "mode='link'")

  expect_equal(cluster_normalize_prior_entry(1.2, current_mode = "dist")$mode, "fixed")
  expect_equal(cluster_normalize_prior_entry("gamma", current_mode = "dist")$dist, "gamma")
  expect_equal(cluster_normalize_prior_entry(list(beta_prior = list(dist = "normal", args = list(mean = 0, sd = 1))), current_mode = "link")$mode, "link")
  expect_error(cluster_normalize_prior_entry(list(foo = 1), current_mode = "dist"), "Could not interpret")

  dat <- data.frame(
    y = c(1, 2, 3, 4),
    x1 = c(0.1, 0.2, 0.3, 0.4),
    grp = factor(c("a", "b", "a", "b")),
    id = 1:4
  )
  parsed <- cluster_parse_formula(y ~ x1 + grp + id, data = dat)
  expect_equal(parsed$response, "y")
  expect_true(isTRUE(parsed$has_X))
  expect_true(length(parsed$X_cols) >= 2L)

  meta <- list(
    terms = stats::terms(y ~ grp + x1, data = dat),
    xlevels = list(grp = c("a", "b")),
    contrasts = attr(stats::model.matrix(y ~ grp + x1, data = dat), "contrasts"),
    X_cols = c("grpb", "x1"),
    response = "y"
  )
  expect_silent(cluster_check_new_factor_levels(meta, data.frame(y = 1, grp = factor("a", levels = c("a", "b")), x1 = 0.5)))
  expect_error(cluster_check_new_factor_levels(meta, data.frame(y = 1, grp = factor("c", levels = c("a", "b", "c")), x1 = 0.5)), "unseen factor levels")
  design_ok <- cluster_build_design(meta, data.frame(y = 5, grp = factor("b", levels = c("a", "b")), x1 = 0.8))
  expect_true(is.matrix(design_ok$X))
  expect_error(cluster_build_design(meta, data.frame(grp = factor("a", levels = c("a", "b")), x1 = 0.1)), "response column")
  expect_error(cluster_build_design(meta, data.frame(y = 1, grp = factor("c", levels = c("a", "b", "c")), x1 = 0.1)), "unseen factor levels")

  expect_equal(cluster_link_apply(1, "identity"), 1)
  expect_equal(cluster_link_apply(0, "exp"), 1)
  expect_equal(cluster_link_apply(2, "power", link_power = 2), 4)
  expect_error(cluster_link_apply(2, "power"), "Invalid power link exponent")

  draw_row <- c(
    "beta_mean[1,1]" = 0.3,
    "beta_mean[1,2]" = -0.2,
    "beta_mean[2,1]" = 0.1,
    "beta_mean[2,2]" = 0.4,
    "beta_threshold[1]" = 0.5,
    "beta_threshold[2]" = -0.1,
    "eta[1]" = 0.2,
    "B[1,1]" = 0.1,
    "B[1,2]" = -0.2,
    "mean[1]" = 1.5,
    "mean[2]" = 2.5,
    "sd[1]" = 1.0,
    "sd[2]" = 1.2
  )
  expect_equal(cluster_extract_beta_component(draw_row, "beta_mean", 1L, 2L), c(0.3, -0.2))
  expect_equal(cluster_extract_beta_global(draw_row, "beta_threshold", 2L), c(0.5, -0.1))
  expect_equal(cluster_extract_beta_auto(draw_row, "beta_mean", 2L, 2L), c(0.1, 0.4))
  expect_equal(cluster_softmax(numeric(0)), numeric(0))
  expect_equal(round(sum(cluster_softmax(c(1, 2, 3))), 8), 1)

  gating_draw <- cluster_extract_gating_draw(draw_row, K = 2L, P = 2L)
  expect_true(is.list(gating_draw))
  gating_w <- cluster_gating_weights(gating_draw, x_row = c(1, 2))
  expect_equal(round(sum(gating_w), 8), 1)

  cluster_bundle <- build_cluster_bundle(
    y ~ x1 + grp,
    data = transform(dat, grp = factor(grp)),
    kernel = "normal",
    GPD = FALSE,
    type = "param",
    components = 3L,
    mcmc = mcmc_fast(seed = 19L)
  )
  density_fun <- cluster_resolve_density_fun(cluster_bundle$spec)
  dens_val <- cluster_component_density(
    spec = cluster_bundle$spec,
    draw_row = draw_row,
    k = 1L,
    x_row = c(1, 0.5),
    y_val = 1.2,
    density_fun = density_fun
  )
  expect_true(is.numeric(dens_val) && dens_val >= 0)

  z_draws <- matrix(c(1, 1, 2, 2, 1, 2, 1, 2), nrow = 2L, byrow = TRUE)
  psm <- compute_psm(z_draws)
  dahl <- dahl_labels(z_draws, psm)
  scores <- cluster_compute_scores(z_draws, dahl$labels, psm)
  expect_true(is.matrix(scores))
  expect_equal(round(rowSums(scores), 8), rep(1, nrow(scores)))

  z_mcmc <- coda::mcmc.list(coda::mcmc(matrix(c(1, 1, 2, 2, 1, 2), ncol = 2L, byrow = TRUE, dimnames = list(NULL, c("z[1]", "z[2]")))))
  expect_true(is.matrix(extract_z_draws(z_mcmc, burnin = 1L, thin = 1L)))
})

test_that("coverage-heavy glue diagnostics cover missing-data and gpd failure branches", {
  check_glue_validity <- get("check_glue_validity", mode = "function")

  bad_fit <- structure(
    list(
      spec = list(
        meta = list(backend = "sb", GPD = TRUE, has_X = TRUE),
        dispatch = list(
          backend = "sb",
          GPD = TRUE,
          gpd = list(
            threshold = list(mode = "link", link = "power"),
            tail_scale = list(mode = "constant")
          )
        )
      ),
      data = list(X = NULL, y = NULL)
    ),
    class = "mixgpd_fit"
  )

  testthat::local_mocked_bindings(
    .get_dispatch = function(...) list(
      d = function(x, ...) rep(0.1, length(x)),
      p = function(q, ...) rep(0.5, length(q)),
      bulk_params = c("mean")
    ),
    .extract_draws_matrix = function(...) {
      matrix(c(1, 0.5, 0.1, 1, 0.6, 0.2), nrow = 2L, byrow = TRUE,
             dimnames = list(NULL, c("w[1]", "tail_shape", "beta_threshold[1]")))
    },
    .extract_weights = function(...) matrix(1, nrow = 1L, ncol = 1L),
    .extract_bulk_params = function(...) list(mean = matrix(1, nrow = 1L, ncol = 1L)),
    .package = "CausalMixGPD"
  )

  expect_error(check_glue_validity(bad_fit, grid = c(0.1, 0.5)), "Training X not found")
  expect_error(
    check_glue_validity(
      structure(
        list(
          spec = list(meta = list(backend = "sb", GPD = FALSE, has_X = FALSE), dispatch = list(backend = "sb", GPD = FALSE)),
          data = list(y = NULL)
        ),
        class = "mixgpd_fit"
      )
    ),
    "Training y not found"
  )

  bad_fit$data$X <- matrix(1, nrow = 1L, ncol = 1L)
  expect_error(check_glue_validity(bad_fit, grid = c(0.1, 0.5)), "power link requires numeric link_power")

  bad_fit$spec$dispatch$gpd$threshold <- list(mode = "constant")
  expect_error(check_glue_validity(bad_fit, grid = c(0.1, 0.5)), "threshold not found")

  testthat::local_mocked_bindings(
    .extract_draws_matrix = function(...) matrix(c(1, 0.5, 1, 1, 0.6, 1.1), nrow = 2L, byrow = TRUE, dimnames = list(NULL, c("w[1]", "tail_shape", "threshold"))),
    .package = "CausalMixGPD"
  )
  expect_error(check_glue_validity(bad_fit, grid = c(0.1, 0.5)), "tail_scale not found")
})

test_that("coverage-heavy internal progress helpers cover colorized cli step and done branches", {
  skip_if_not_installed("cli")

  progress_colorize <- get(".cmgpd_progress_colorize", mode = "function")
  progress_format <- get(".cmgpd_progress_format", mode = "function")
  progress_start <- get(".cmgpd_progress_start", mode = "function")
  progress_step <- get(".cmgpd_progress_step", mode = "function")
  progress_done <- get(".cmgpd_progress_done", mode = "function")

  progress_updates <- list()
  progress_messages <- character(0)

  testthat::local_mocked_bindings(
    col_blue = function(x) paste0("<", x, ">"),
    num_ansi_colors = function() 8L,
    cli_progress_update = function(id, inc) {
      progress_updates <<- c(progress_updates, list(list(id = id, inc = inc)))
      invisible(NULL)
    },
    cli_progress_done = function(id) {
      progress_updates <<- c(progress_updates, list(list(id = id, inc = NA_integer_)))
      invisible(NULL)
    },
    .package = "cli"
  )
  testthat::local_mocked_bindings(
    .cmgpd_message = function(...) {
      progress_messages <<- c(progress_messages, paste0(..., collapse = ""))
      invisible(NULL)
    },
    .package = "CausalMixGPD"
  )

  expect_equal(progress_colorize("", step_index = 1L, enabled = TRUE), "")
  expect_equal(progress_colorize("step", step_index = 1L, enabled = TRUE), "<step>")
  expect_equal(progress_format(1L, 3L, "   ", label = "mix", color = TRUE), "<[mix] Working...>")

  ctx <- progress_start(total_steps = 0L, enabled = TRUE, quiet = FALSE, label = "mix")
  ctx$total <- 2L
  ctx$enabled <- TRUE
  ctx$color_enabled <- TRUE
  ctx$live_backend <- "cli"
  ctx$live_id <- "bar-1"

  progress_step(ctx, "First")
  progress_done(ctx, final_label = "Done")

  expect_equal(ctx$current, ctx$total)
  expect_true(any(grepl("First", progress_messages, fixed = TRUE)))
  expect_true(any(grepl("Done", progress_messages, fixed = TRUE)))
  expect_true(length(progress_updates) >= 3L)
})

test_that("coverage-heavy causal summary printers cover knitr fallback and raw summaries", {
  knitr_mode <- TRUE
  old_kable_opt <- getOption("causalmixgpd.knitr.kable")
  options(causalmixgpd.knitr.kable = TRUE)
  on.exit(options(causalmixgpd.knitr.kable = old_kable_opt), add = TRUE)

  testthat::local_mocked_bindings(
    .is_knitr_output = function() knitr_mode,
    .kable_table = function(x, ...) {
      if (is.data.frame(x)) {
        paste(names(x), collapse = "|")
      } else {
        paste(as.character(x), collapse = "\n")
      }
    },
    .knitr_asis = function(...) paste(unlist(list(...)), collapse = "\n"),
    .package = "CausalMixGPD"
  )

  effect_meta <- list(
    ps_enabled = TRUE,
    ps_scale = "logit",
    backend = list(trt = "sb", con = "crp"),
    kernel = list(trt = "normal", con = "gamma"),
    GPD = list(trt = TRUE, con = FALSE)
  )

  qte_draw_obj <- structure(
    list(
      type = "cqte",
      probs = c(0.1, 0.9),
      n_pred = 2L,
      level = 0.9,
      interval = "hpd",
      x = matrix(c(1, 0, 0, 1), nrow = 2L),
      ps = c(0.2, 0.8),
      meta = effect_meta,
      qte = list(
        fit = data.frame(index = 0.9, estimate = 0.8, lower = 0.5, upper = 1.1),
        draws = matrix(c(NA_real_, NA_real_, 0.7, 0.9), nrow = 2L)
      )
    ),
    class = "causalmixgpd_qte"
  )

  qte_df_obj <- structure(
    list(
      type = "qte",
      probs = c(0.1, 0.5, 0.9),
      n_pred = 8L,
      level = 0.95,
      interval = "credible",
      x = matrix(1, nrow = 8L, ncol = 1L),
      ps = rep(0.4, 8L),
      meta = effect_meta,
      qte = list(
        fit = data.frame(
          id = seq_len(8L),
          index = rep(c(0.1, 0.5, 0.9, 0.5), each = 2L),
          estimate = seq(0.2, 1.6, length.out = 8L),
          lower = seq(0.1, 1.5, length.out = 8L),
          upper = seq(0.3, 1.7, length.out = 8L)
        )
      )
    ),
    class = "causalmixgpd_qte"
  )

  qte_matrix_obj <- structure(
    list(
      type = "cqte",
      probs = c(0.25, 0.75),
      n_pred = 4L,
      level = 0.9,
      interval = "hpd",
      x = matrix(1, nrow = 4L, ncol = 1L),
      ps = rep(0.5, 4L),
      meta = effect_meta,
      qte = list(fit = NULL, draws = NULL),
      fit = matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 4L, ncol = 2L)
    ),
    class = "causalmixgpd_qte"
  )

  ate_df_obj <- structure(
    list(
      type = "cate",
      n_pred = 7L,
      level = 0.95,
      interval = "credible",
      nsim_mean = 15L,
      x = matrix(1, nrow = 7L, ncol = 1L),
      ps = rep(0.6, 7L),
      meta = effect_meta,
      ate = list(
        fit = data.frame(
          id = seq_len(7L),
          estimate = seq(0.5, 1.7, length.out = 7L),
          lower = seq(0.3, 1.5, length.out = 7L),
          upper = seq(0.7, 1.9, length.out = 7L)
        )
      )
    ),
    class = "causalmixgpd_ate"
  )

  ate_vec_obj <- structure(
    list(
      type = "ate",
      n_pred = 5L,
      level = 0.9,
      interval = "hpd",
      nsim_mean = 12L,
      x = matrix(1, nrow = 5L, ncol = 1L),
      ps = rep(0.3, 5L),
      meta = effect_meta,
      ate = list(fit = NULL),
      fit = c(0.4, 0.6, 0.8, 1.0, 1.2),
      lower = c(0.2, 0.4, 0.6, 0.8, 1.0),
      upper = c(0.6, 0.8, 1.0, 1.2, 1.4)
    ),
    class = "causalmixgpd_ate"
  )

  qte_draw_sum <- summary(qte_draw_obj)
  expect_s3_class(qte_draw_sum, "summary.causalmixgpd_qte")
  expect_true(all(is.na(qte_draw_sum$quantile_summary[1, c("estimate_qte", "mean_qte", "ci_lower")])))
  expect_equal(qte_draw_sum$quantile_summary$estimate_qte[2], 0.8)

  qte_matrix_sum <- summary(qte_matrix_obj)
  expect_s3_class(qte_matrix_sum, "summary.causalmixgpd_qte")
  expect_equal(nrow(qte_matrix_sum$quantile_summary), 2L)

  ate_vec_sum <- summary(ate_vec_obj)
  expect_s3_class(ate_vec_sum, "summary.causalmixgpd_ate")
  expect_equal(ate_vec_sum$ate_stats$mean_ate, mean(ate_vec_obj$fit))
  expect_true(is.list(ate_vec_sum$ci_summary))

  qte_knitr <- print(qte_df_obj, max_rows = 2L)
  expect_true(is.character(qte_knitr))
  expect_match(qte_knitr, "PS scale: logit")
  expect_match(qte_knitr, "Credible interval: credible")

  qte_matrix_knitr <- print(qte_matrix_obj, max_rows = 2L)
  expect_true(is.character(qte_matrix_knitr))
  expect_match(qte_matrix_knitr, "\\(matrix: 4 x 2\\)")
  expect_match(qte_matrix_knitr, "\\.\\.\\. \\(2 more rows\\)")

  qte_summary_knitr <- print(qte_draw_sum, digits = 2L)
  expect_true(is.character(qte_summary_knitr))
  expect_match(qte_summary_knitr, "Model specification:")
  expect_match(qte_summary_knitr, "Interval: hpd")

  ate_knitr <- print(ate_df_obj, max_rows = 2L)
  expect_true(is.character(ate_knitr))
  expect_match(ate_knitr, "Posterior mean draws: 15")
  expect_match(ate_knitr, "Credible interval: credible")

  ate_vec_knitr <- print(ate_vec_obj, max_rows = 2L)
  expect_true(is.character(ate_vec_knitr))
  expect_match(ate_vec_knitr, "\\(vector: 5\\)")
  expect_match(ate_vec_knitr, "\\.\\.\\. \\(3 more\\)")

  ate_summary_knitr <- print(ate_vec_sum, digits = 2L)
  expect_true(is.character(ate_summary_knitr))
  expect_match(ate_summary_knitr, "Credible interval width:")
  expect_match(ate_summary_knitr, "Model specification:")

  knitr_mode <- FALSE
  expect_output(print(qte_df_obj, max_rows = 2L), "Credible interval: credible")
  expect_output(print(qte_matrix_obj, max_rows = 2L), "\\(matrix: 4 x 2\\)")
  expect_output(print(qte_draw_sum, digits = 2L), "Model specification:")
  expect_output(print(ate_df_obj, max_rows = 2L), "Posterior mean draws: 15")
  expect_output(print(ate_vec_obj, max_rows = 2L), "\\(vector: 5\\)")
  expect_output(print(ate_vec_sum, digits = 2L), "Credible interval width:")
})

test_that("coverage-heavy glue diagnostics cover grid defaults violation summaries and malformed draws", {
  check_glue_validity <- get("check_glue_validity", mode = "function")

  crp_fit <- structure(
    list(
      spec = list(
        meta = list(backend = "crp", GPD = TRUE, has_X = TRUE),
        dispatch = list(
          backend = "crp",
          GPD = TRUE,
          gpd = list(
            threshold = list(mode = "constant"),
            tail_scale = list(mode = "constant", value = 1.5)
          )
        )
      ),
      data = list(
        X = matrix(c(1, 0, 0, 1), nrow = 2L),
        y = c(0.1, 0.5, 0.9, 1.4)
      )
    ),
    class = "mixgpd_fit"
  )

  testthat::local_mocked_bindings(
    .get_dispatch = function(...) {
      list(
        bulk_params = c("mean"),
        d = function(x, w, mean, tail_shape = NULL, threshold = NULL, tail_scale = NULL, log = 0L) {
          out <- rep(0.1, length(x))
          out[2L] <- -0.2
          if (log == 1L) log(pmax(out, 1e-8)) else out
        },
        p = function(q, w, mean, tail_shape = NULL, threshold = NULL, tail_scale = NULL, lower.tail = 1L, log.p = 0L) {
          if (length(q) == 1L) {
            out <- if (q < threshold) 0.2 else 0.6
          } else {
            out <- c(0.2, 0.7, 0.5, 1.2)[seq_along(q)]
          }
          if (log.p == 1L) log(pmax(out, 1e-8)) else out
        }
      )
    },
    .extract_draws_matrix = function(...) {
      matrix(
        c(
          0.2, 0.4, 0.8, 0.1,
          0.3, 0.5, 0.9, 0.2,
          0.4, 0.6, 1.0, 0.3
        ),
        nrow = 3L,
        byrow = TRUE,
        dimnames = list(NULL, c("tail_shape", "threshold_1", "threshold_2", "mean[1]"))
      )
    },
    .extract_weights = function(...) matrix(c(0.7, 0.3, Inf, 0, 0.6, 0.4), nrow = 3L, byrow = TRUE),
    .extract_bulk_params = function(...) list(mean = matrix(c(0.1, 0.2, 0.3), nrow = 3L)),
    .package = "CausalMixGPD"
  )

  res <- check_glue_validity(crp_fit, x = matrix(c(1, 0, 0, 1), nrow = 2L), grid = NULL, n_draws = 5L, check_continuity = TRUE)
  expect_equal(res$n_checked_draws, 3L)
  expect_equal(res$grid_n, 200L)
  expect_true(res$violations$cdf_range > 0L)
  expect_true(res$violations$cdf_monotone > 0L)
  expect_true(res$violations$density_nonneg > 0L)
  expect_true(res$violations$continuity > 0L)
  expect_true(length(res$details$bad_draws) >= 1L)
  expect_true(length(res$details$examples) >= 1L)
  expect_false(all(unlist(res$pass), na.rm = TRUE))

  testthat::local_mocked_bindings(
    .extract_draws_matrix = function(...) matrix(1, nrow = 1L, ncol = 1L),
    .package = "CausalMixGPD"
  )
  expect_error(check_glue_validity(crp_fit, x = matrix(c(1, 0, 0, 1), nrow = 2L), grid = c(0, 1)), "Posterior draws not found or malformed")
})

test_that("coverage-heavy build-run helper edge modes cover spliced scalar and standard branches", {
  build_dimensions_from_spec <- get("build_dimensions_from_spec", mode = "function")
  build_monitors_from_spec <- get("build_monitors_from_spec", mode = "function")

  kernel_info <- list(
    param_types = list(mean = "location", scale = "scale", shape = "shape", sd = "sd", rate = "scale"),
    bulk_support = list(mean = "", scale = "positive_scale", shape = "positive_shape", sd = "positive_sd", rate = "positive_scale")
  )

  spliced_link_spec <- list(
    meta = list(backend = "spliced", N = 4L, P = 2L, components = 3L, GPD = TRUE, kernel = "normal", has_X = TRUE),
    plan = list(
      concentration = list(mode = "dist", dist = "lognormal", args = list(meanlog = 0.2, sdlog = 0.4)),
      bulk = list(
        mean = list(mode = "link", link = "power", link_power = 2, beta_prior = list(dist = "lognormal", args = list(meanlog = -0.2, sdlog = 0.3))),
        scale = list(mode = "dist", dist = "lognormal", args = list(meanlog = 0.1, sdlog = 0.2))
      ),
      ps = list(prior = list(dist = "lognormal", args = list(meanlog = -1, sdlog = 0.5))),
      gpd = list(
        threshold = list(mode = "link", link = "power", link_power = 2, beta_prior = list(dist = "lognormal", args = list(meanlog = -2, sdlog = 0.2))),
        tail_scale = list(mode = "link", link = "power", link_power = 1.5, beta_prior = list(dist = "lognormal", args = list(meanlog = -0.5, sdlog = 0.2))),
        tail_shape = list(mode = "link", link = "power", link_power = 3, beta_prior = list(dist = "lognormal", args = list(meanlog = -0.3, sdlog = 0.2)))
      )
    ),
    kernel_info = kernel_info
  )

  spliced_scalar_spec <- list(
    meta = list(backend = "spliced", N = 4L, P = 2L, components = 3L, GPD = TRUE, kernel = "normal", has_X = TRUE),
    plan = list(
      concentration = list(mode = "fixed", value = 0.8),
      bulk = list(
        shape = list(mode = "fixed", value = 0.4),
        sd = list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1))
      ),
      gpd = list(
        threshold = list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1)),
        tail_scale = list(mode = "fixed", value = 1.3),
        tail_shape = list(mode = "dist", dist = "normal", args = list(mean = 0, sd = 0.3))
      )
    ),
    kernel_info = kernel_info
  )

  standard_link_spec <- list(
    meta = list(backend = "sb", N = 5L, P = 2L, components = 3L, GPD = TRUE, kernel = "normal", has_X = TRUE),
    plan = list(
      concentration = list(mode = "fixed", value = 1.2),
      bulk = list(
        mean = list(mode = "link", link = "power", link_power = 2, beta_prior = list(dist = "normal", args = list(mean = 0, sd = 1))),
        sd = list(mode = "fixed", value = 1)
      ),
      ps = list(prior = list(dist = "normal", args = list(mean = 0, sd = 1))),
      gpd = list(
        threshold = list(mode = "link", link = "power", link_power = 2, link_dist = list(dist = "lognormal"), beta_prior = list(dist = "normal", args = list(mean = 0, sd = 0.2))),
        sdlog_u = list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1)),
        tail_scale = list(mode = "link", link = "power", link_power = 1.5, beta_prior = list(dist = "normal", args = list(mean = 0, sd = 0.4))),
        tail_shape = list(mode = "link", link = "power", link_power = 3, beta_prior = list(dist = "normal", args = list(mean = 0, sd = 0.3)))
      )
    ),
    kernel_info = kernel_info
  )

  standard_scalar_spec <- list(
    meta = list(backend = "crp", N = 4L, P = 0L, components = 4L, GPD = TRUE, kernel = "gamma", has_X = FALSE),
    plan = list(
      concentration = list(mode = "dist", dist = "gamma", args = list()),
      bulk = list(
        shape = list(mode = "dist", dist = "gamma", args = list()),
        rate = list(mode = "fixed", value = 1)
      ),
      gpd = list(
        threshold = list(mode = "fixed", value = 1.1),
        tail_scale = list(mode = "dist", dist = "gamma", args = list()),
        tail_shape = list(mode = "dist", dist = "normal", args = list())
      )
    ),
    kernel_info = kernel_info
  )

  standard_no_ld_spec <- list(
    meta = list(backend = "sb", N = 4L, P = 1L, components = 3L, GPD = TRUE, kernel = "normal", has_X = TRUE),
    plan = list(
      concentration = list(mode = "fixed", value = 1),
      bulk = list(mean = list(mode = "dist", dist = "normal", args = list(mean = 0, sd = 1))),
      gpd = list(
        threshold = list(mode = "link", link = "identity", beta_prior = list(dist = "normal", args = list(mean = 0, sd = 0.2))),
        tail_scale = list(mode = "fixed", value = 1.1),
        tail_shape = list(mode = "fixed", value = 0.2)
      )
    ),
    kernel_info = kernel_info
  )

  sb_small_spec <- list(
    meta = list(backend = "sb", N = 4L, P = 0L, components = 2L, GPD = FALSE, kernel = "normal", has_X = FALSE),
    plan = list(
      concentration = list(mode = "fixed", value = 1),
      bulk = list(mean = list(mode = "dist", dist = "normal", args = list(mean = 0, sd = 1)))
    ),
    kernel_info = kernel_info
  )

  gating_bad_spec <- list(
    meta = list(backend = "sb", N = 4L, P = 0L, components = 3L, GPD = FALSE, kernel = "normal", has_X = FALSE),
    plan = list(
      concentration = list(mode = "fixed", value = 1),
      bulk = list()
    ),
    cluster = list(gating = TRUE),
    kernel_info = kernel_info
  )

  unknown_backend_spec <- list(
    meta = list(backend = "mystery", N = 4L, P = 1L, components = 3L, GPD = FALSE, kernel = "normal", has_X = TRUE),
    plan = list(
      concentration = list(mode = "fixed", value = 1),
      bulk = list(mean = list(mode = "dist", dist = "normal", args = list(mean = 0, sd = 1)))
    ),
    kernel_info = kernel_info
  )

  bad_const_spec <- standard_scalar_spec
  bad_const_spec$plan$concentration$mode <- "weird"

  bad_prior_spec <- standard_scalar_spec
  bad_prior_spec$plan$bulk$shape$mode <- "weird"

  bad_dims_spec <- standard_scalar_spec
  bad_dims_spec$plan$gpd$tail_shape$mode <- "weird"

  bad_mon_spec <- standard_no_ld_spec
  bad_mon_spec$plan$bulk$mean$mode <- "weird"

  mons_sp_link <- build_monitors_from_spec(spliced_link_spec, monitor_latent = TRUE)
  dims_sp_link <- build_dimensions_from_spec(spliced_link_spec)
  inits_sp_link <- build_inits_from_spec(spliced_link_spec, y = c(1, 2, 3, 4))
  const_sp_link <- build_constants_from_spec(spliced_link_spec)
  prior_sp_link <- build_prior_table_from_spec(spliced_link_spec)

  expect_true(all(c("alpha", "z[1:4]", "beta_mean[1:3,1:2]", "beta_ps_mean[1:3]", "scale[1:3]", "beta_threshold[1:3,1:2]", "beta_tail_scale[1:3,1:2]", "beta_tail_shape[1:3,1:2]") %in% mons_sp_link))
  expect_true(all(c("z", "beta_mean", "beta_ps_mean", "scale", "beta_threshold", "eta_threshold", "threshold_i", "beta_tail_scale", "eta_tail_scale", "tail_scale_i", "beta_tail_shape", "eta_tail_shape", "tail_shape_i") %in% names(dims_sp_link)))
  expect_true(all(c("alpha", "z", "beta_mean", "beta_ps_mean", "scale", "beta_threshold", "beta_tail_scale", "beta_tail_shape") %in% names(inits_sp_link)))
  expect_equal(unname(unlist(const_sp_link[c("N", "P", "components")])), c(4, 2, 3))
  expect_true(any(grepl("power=2", prior_sp_link$notes, fixed = TRUE)))
  expect_true(any(grepl("power=1.5", prior_sp_link$notes, fixed = TRUE)))
  expect_true(any(grepl("power=3", prior_sp_link$notes, fixed = TRUE)))

  mons_sp_scalar <- build_monitors_from_spec(spliced_scalar_spec, monitor_latent = TRUE)
  dims_sp_scalar <- build_dimensions_from_spec(spliced_scalar_spec)
  inits_sp_scalar <- build_inits_from_spec(spliced_scalar_spec, y = c(2, 3, 4, 5))
  const_sp_scalar <- build_constants_from_spec(spliced_scalar_spec)
  prior_sp_scalar <- build_prior_table_from_spec(spliced_scalar_spec)

  expect_true(all(c("alpha", "z[1:4]", "sd[1:3]", "threshold[1:3]", "tail_scale[1:3]", "tail_shape[1:3]") %in% mons_sp_scalar))
  expect_true(all(c("z", "sd", "threshold", "tail_scale", "tail_shape") %in% names(dims_sp_scalar)))
  expect_true(all(c("z", "sd", "threshold", "tail_shape") %in% names(inits_sp_scalar)))
  expect_false("tail_scale" %in% names(inits_sp_scalar))
  expect_equal(unname(unlist(const_sp_scalar[c("N", "P", "components")])), c(4, 2, 3))
  expect_true(any(prior_sp_scalar$parameter == "tail_scale" & prior_sp_scalar$mode == "fixed"))
  expect_true(any(prior_sp_scalar$parameter == "threshold" & prior_sp_scalar$mode == "dist"))

  mons_std_link <- build_monitors_from_spec(standard_link_spec, monitor_v = TRUE, monitor_latent = TRUE)
  dims_std_link <- build_dimensions_from_spec(standard_link_spec)
  inits_std_link <- build_inits_from_spec(standard_link_spec, y = rep(NA_real_, 5L))
  const_std_link <- build_constants_from_spec(standard_link_spec)
  prior_std_link <- build_prior_table_from_spec(standard_link_spec)

  expect_true(all(c("alpha", "w[1:3]", "z[1:5]", "v[1:2]", "beta_mean[1:3,1:2]", "beta_ps_mean[1:3]", "threshold[1:5]", "beta_threshold[1:2]", "sdlog_u", "beta_tail_scale[1:2]", "beta_tail_shape[1:2]") %in% mons_std_link))
  expect_true(all(c("v", "w", "z", "beta_mean", "beta_ps_mean", "threshold", "beta_threshold", "beta_tail_scale", "beta_tail_shape") %in% names(dims_std_link)))
  expect_true(all(c("v", "z", "beta_mean", "beta_ps_mean", "beta_threshold", "threshold", "sdlog_u", "beta_tail_scale", "beta_tail_shape") %in% names(inits_std_link)))
  expect_equal(inits_std_link$threshold, rep(1, 5L))
  expect_equal(inits_std_link$sdlog_u, 0.2)
  expect_equal(unname(unlist(const_std_link[c("N", "P", "components")])), c(5, 2, 3))
  expect_true(any(prior_std_link$mode == "link+dist"))
  expect_true(any(grepl("Lognormal", prior_std_link$notes, fixed = TRUE)))

  mons_std_scalar <- build_monitors_from_spec(standard_scalar_spec, monitor_latent = TRUE)
  dims_std_scalar <- build_dimensions_from_spec(standard_scalar_spec)
  inits_std_scalar <- build_inits_from_spec(standard_scalar_spec, y = c(1, 2, 3, 4))
  const_std_scalar <- build_constants_from_spec(standard_scalar_spec)
  prior_std_scalar <- build_prior_table_from_spec(standard_scalar_spec)

  expect_true(all(c("alpha", "z[1:4]", "shape[1:4]", "threshold", "tail_scale", "tail_shape") %in% mons_std_scalar))
  expect_true(all(c("z", "shape") %in% names(dims_std_scalar)))
  expect_true(all(c("alpha", "z", "shape", "threshold", "tail_scale", "tail_shape") %in% names(inits_std_scalar)))
  expect_equal(unname(unlist(const_std_scalar[c("N", "P", "components")])), c(4, 0, 4))
  expect_true(any(prior_std_scalar$parameter == "threshold" & prior_std_scalar$mode == "fixed"))
  expect_true(any(prior_std_scalar$parameter == "tail_scale" & prior_std_scalar$mode == "dist"))

  inits_std_no_ld <- build_inits_from_spec(standard_no_ld_spec, y = rep(NA_real_, 4L))
  inits_small <- build_inits_from_spec(sb_small_spec, y = c(1, 2, 3, 4))
  expect_equal(inits_std_no_ld$threshold, rep(1, 4L))
  expect_length(inits_small$v, 1L)

  expect_error(build_monitors_from_spec(gating_bad_spec, monitor_latent = TRUE), "Cluster gating requires P > 0")
  expect_error(build_dimensions_from_spec(gating_bad_spec), "Cluster gating requires P > 0")
  expect_error(build_inits_from_spec(gating_bad_spec), "Cluster gating requires P > 0")
  expect_error(build_monitors_from_spec(unknown_backend_spec), "Unknown backend")
  expect_error(build_dimensions_from_spec(unknown_backend_spec), "Unknown backend")
  expect_error(build_inits_from_spec(unknown_backend_spec), "Unknown backend")
  expect_error(build_constants_from_spec(bad_const_spec), "Invalid plan\\$concentration\\$mode")
  expect_error(build_prior_table_from_spec(bad_prior_spec), "Invalid bulk mode")
  expect_error(build_dimensions_from_spec(bad_dims_spec), "Invalid gpd\\$tail_shape mode")
  expect_error(build_monitors_from_spec(bad_mon_spec), "Invalid bulk plan mode")
})

test_that("coverage-heavy build-run helper guard rails cover remaining mode errors and sampler fallbacks", {
  build_dimensions_from_spec <- get("build_dimensions_from_spec", mode = "function")
  build_monitors_from_spec <- get("build_monitors_from_spec", mode = "function")

  kernel_info <- list(
    param_types = list(mean = "location", scale = "scale", weird = "other"),
    bulk_support = list(mean = "", scale = "", weird = "")
  )

  make_spec <- function(backend = "sb", P = 1L, GPD = FALSE, bulk = list(), gpd = list(),
                        concentration = list(mode = "fixed", value = 1), N = 4L, K = 3L,
                        kernel_info_override = kernel_info) {
    list(
      meta = list(
        backend = backend,
        N = N,
        P = P,
        components = K,
        GPD = GPD,
        kernel = "normal",
        has_X = P > 0L
      ),
      plan = list(
        concentration = concentration,
        bulk = bulk,
        gpd = gpd
      ),
      kernel_info = kernel_info_override
    )
  }

  make_gpd_only_spec <- function(backend, P, param, mode, N = 4L, K = 3L) {
    gpd <- setNames(list(list(mode = mode)), param)
    make_spec(backend = backend, P = P, GPD = TRUE, gpd = gpd, N = N, K = K)
  }

  link_p0_spec <- make_spec(
    backend = "sb",
    P = 0L,
    bulk = list(scale = list(mode = "link"))
  )
  expect_error(build_monitors_from_spec(link_p0_spec), "link-mode but P=0")
  expect_error(build_dimensions_from_spec(link_p0_spec), "link-mode but P=0")
  expect_error(build_inits_from_spec(link_p0_spec), "link-mode but P=0")

  init_type_spec <- make_spec(
    backend = "sb",
    P = 0L,
    bulk = list(
      scale = list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1)),
      weird = list(mode = "dist", dist = "normal", args = list(mean = 0, sd = 1))
    )
  )
  init_type <- build_inits_from_spec(init_type_spec, y = c(1, 2, 3, 4))
  expect_equal(init_type$scale, rep(1, 3L))
  expect_equal(init_type$weird, rep(0, 3L))

  bad_inits_conc <- init_type_spec
  bad_inits_conc$plan$concentration$mode <- "odd"
  expect_error(build_inits_from_spec(bad_inits_conc), "Invalid plan\\$concentration\\$mode")
  expect_error(build_prior_table_from_spec(bad_inits_conc), "Invalid plan\\$concentration\\$mode")

  spliced_thr_dist_na <- make_spec(
    backend = "spliced",
    P = 1L,
    GPD = TRUE,
    gpd = list(threshold = list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1)))
  )
  expect_equal(build_inits_from_spec(spliced_thr_dist_na, y = rep(NA_real_, 4L))$threshold, rep(1, 3L))

  spliced_thr_fixed <- make_spec(
    backend = "spliced",
    P = 1L,
    GPD = TRUE,
    gpd = list(threshold = list(mode = "fixed", value = 1.2))
  )
  expect_false("threshold" %in% names(build_inits_from_spec(spliced_thr_fixed, y = c(1, 2, 3, 4))))

  spliced_tsh_fixed <- make_spec(
    backend = "spliced",
    P = 1L,
    GPD = TRUE,
    gpd = list(tail_shape = list(mode = "fixed", value = 0.2))
  )
  expect_false("tail_shape" %in% names(build_inits_from_spec(spliced_tsh_fixed, y = c(1, 2, 3, 4))))

  for (param in c("threshold", "tail_scale", "tail_shape")) {
    sp_p0 <- make_gpd_only_spec("spliced", 0L, param, "link")
    std_p0 <- make_gpd_only_spec("sb", 0L, param, "link")
    expect_error(build_monitors_from_spec(sp_p0), "P=0")
    expect_error(build_dimensions_from_spec(sp_p0), "P=0")
    expect_error(build_inits_from_spec(sp_p0), "P=0")
    expect_error(build_monitors_from_spec(std_p0), "P=0")
    expect_error(build_dimensions_from_spec(std_p0), "P=0")
    expect_error(build_inits_from_spec(std_p0), "P=0")
  }

  for (param in c("threshold", "tail_scale", "tail_shape")) {
    sp_bad <- make_gpd_only_spec("spliced", 1L, param, "weird")
    std_bad <- make_gpd_only_spec("sb", 1L, param, "weird")
    expect_error(build_monitors_from_spec(sp_bad), "Invalid gpd")
    expect_error(build_dimensions_from_spec(sp_bad), "Invalid gpd")
    expect_error(build_inits_from_spec(sp_bad), "Invalid gpd")
    if (param != "tail_shape") {
      expect_error(build_monitors_from_spec(std_bad), "Invalid gpd")
    }
    expect_error(build_dimensions_from_spec(std_bad), "Invalid gpd")
    if (param != "tail_shape") {
      expect_error(build_inits_from_spec(std_bad), "Invalid gpd")
    }
    expect_error(build_prior_table_from_spec(std_bad), "Invalid gpd")
  }

  bad_const_bulk <- make_spec(
    backend = "sb",
    P = 1L,
    bulk = list(mean = list(mode = "weird"))
  )
  expect_error(build_constants_from_spec(bad_const_bulk), "Invalid bulk plan mode")

  bad_const_sdlog <- make_spec(
    backend = "sb",
    P = 1L,
    GPD = TRUE,
    gpd = list(
      threshold = list(mode = "link", link_dist = list(dist = "lognormal")),
      sdlog_u = list(mode = "fixed", value = 0.2)
    )
  )
  expect_error(build_constants_from_spec(bad_const_sdlog), "sdlog_u must be dist-mode")

  bad_const_thr <- make_gpd_only_spec("sb", 1L, "threshold", "weird")
  bad_const_ts <- make_gpd_only_spec("sb", 1L, "tail_scale", "weird")
  bad_const_tsh <- make_gpd_only_spec("sb", 1L, "tail_shape", "weird")
  expect_error(build_constants_from_spec(bad_const_thr), "Invalid gpd\\$threshold mode")
  expect_error(build_constants_from_spec(bad_const_ts), "Invalid gpd\\$tail_scale mode")
  expect_error(build_constants_from_spec(bad_const_tsh), "Invalid gpd\\$tail_shape mode")

  std_tail_shape_fixed <- make_spec(
    backend = "sb",
    P = 1L,
    GPD = TRUE,
    gpd = list(tail_shape = list(mode = "fixed", value = 0.1))
  )
  expect_equal(unname(unlist(build_constants_from_spec(std_tail_shape_fixed)[c("N", "P", "components")])), c(4, 1, 3))
  prior_tail_shape_fixed <- build_prior_table_from_spec(std_tail_shape_fixed)
  expect_true(any(prior_tail_shape_fixed$parameter == "tail_shape" & prior_tail_shape_fixed$mode == "fixed"))

  conf_empty <- list(samplerConfs = NULL)
  expect_identical(.configure_samplers(conf_empty, spec = list()), conf_empty)

  conf_fallback <- list(
    samplerConfs = list(
      list(
        name = "RW",
        target = "beta_mean[1]",
        control = list(
          clusterVarInfo = list(
            clusterNodes = c("z[1]", "junk"),
            numNodesPerCluster = 2L
          )
        )
      )
    ),
    getModel = function() stop("no model"),
    model = list(
      getNodeNames = function(stochOnly = TRUE, includeData = FALSE) c("z[1]", "beta_mean[1]")
    ),
    getSamplers = function() list(list(target = "beta_mean[1]")),
    removeSamplers = function(...) invisible(NULL),
    addSampler = function(...) invisible(NULL)
  )
  conf_norm <- .configure_samplers(conf_fallback, spec = list(), z_update_every = 2L)
  expect_true(is.list(conf_norm$samplerConfs[[1]]$control$clusterVarInfo$clusterNodes))
  expect_equal(conf_norm$samplerConfs[[1]]$control$adaptInterval, 200L)
  expect_true(isTRUE(conf_norm$samplerConfs[[1]]$control$adaptScaleOnly))
})
