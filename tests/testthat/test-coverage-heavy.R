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
  alloc <- allocation(.coverage_heavy_uncond_fit(), show_progress = FALSE)
  expect_s3_class(pars, "mixgpd_params")
  expect_s3_class(fit_sum, "mixgpd_summary")
  expect_s3_class(ess, "mixgpd_ess_summary")
  expect_s3_class(alloc, "mixgpd_allocation")
  expect_output(print(pars))
  expect_output(print(fit_sum))
  expect_output(print(ess))
  expect_true(is.data.frame(summary(ess)))
  expect_output(print(alloc))

  expect_s3_class(plot(fit, family = c("traceplot", "density"), params = "alpha"), "mixgpd_fit_plots")
  expect_s3_class(plot(pred_mean), "mixgpd_predict_plots")
  expect_s3_class(plot(fit_loc), "mixgpd_fitted_plots")
  expect_s3_class(plot(alloc, overlay = FALSE), "mixgpd_allocation_plots")
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
  expect_equal(colnames(parsed$X), c("x1", "id"))
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

test_that("coverage-heavy methods cover bundle ps summary and allocation printers", {
  skip_if_not_installed("ggplot2")

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
})

test_that("coverage-heavy methods cover plotting families causal prediction and allocation branches", {
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
  alloc_new <- allocation(.coverage_heavy_uncond_fit(), newdata = data.frame(y = c(0.3, 0.6, 1.1)), show_progress = FALSE)

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

  expect_output(print(alloc_new, return = "prob"), "certainty")
  expect_output(print(summary(alloc_new)), "Cluster Allocation Summary")
  expect_s3_class(plot(alloc_new, overlay = TRUE), "mixgpd_allocation_plots")
  expect_s3_class(plot(alloc_new, overlay = FALSE), "mixgpd_allocation_plots")
  expect_output(print(plot(alloc_new, overlay = FALSE)))

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
