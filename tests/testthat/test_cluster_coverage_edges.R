cluster_parse_formula <- getFromNamespace(".cluster_parse_formula", "CausalMixGPD")
cluster_default_link <- getFromNamespace(".cluster_default_link", "CausalMixGPD")
cluster_link_bulk_specs <- getFromNamespace(".cluster_link_bulk_specs", "CausalMixGPD")
cluster_default_beta_prior <- getFromNamespace(".cluster_default_beta_prior", "CausalMixGPD")
cluster_split_overrides <- getFromNamespace(".cluster_split_overrides", "CausalMixGPD")
cluster_normalize_link_entry <- getFromNamespace(".cluster_normalize_link_entry", "CausalMixGPD")
cluster_normalize_prior_entry <- getFromNamespace(".cluster_normalize_prior_entry", "CausalMixGPD")
cluster_apply_prior_overrides <- getFromNamespace(".cluster_apply_prior_overrides", "CausalMixGPD")
cluster_order_levels <- getFromNamespace(".cluster_order_levels", "CausalMixGPD")
cluster_size_table <- getFromNamespace(".cluster_size_table", "CausalMixGPD")
cluster_data_frame_from_design <- getFromNamespace(".cluster_data_frame_from_design", "CausalMixGPD")
cluster_training_reference <- getFromNamespace(".cluster_training_reference", "CausalMixGPD")
cluster_response_name <- getFromNamespace(".cluster_response_name", "CausalMixGPD")
cluster_response_values <- getFromNamespace(".cluster_response_values", "CausalMixGPD")
cluster_plot_ylim <- getFromNamespace(".cluster_plot_ylim", "CausalMixGPD")
cluster_scale_values <- getFromNamespace(".cluster_scale_values", "CausalMixGPD")
cluster_hist_breaks <- getFromNamespace(".cluster_hist_breaks", "CausalMixGPD")
cluster_hist_max_counts <- getFromNamespace(".cluster_hist_max_counts", "CausalMixGPD")
cluster_summary_plot_data <- getFromNamespace(".cluster_summary_plot_data", "CausalMixGPD")
cluster_profile_table <- getFromNamespace(".cluster_profile_table", "CausalMixGPD")
cluster_build_design <- getFromNamespace(".cluster_build_design", "CausalMixGPD")
cluster_softmax <- getFromNamespace(".cluster_softmax", "CausalMixGPD")
cluster_gating_weights <- getFromNamespace(".cluster_gating_weights", "CausalMixGPD")
cluster_resolve_density_fun <- getFromNamespace(".cluster_resolve_density_fun", "CausalMixGPD")
cluster_component_density <- getFromNamespace(".cluster_component_density", "CausalMixGPD")

test_that("cluster helper edge cases cover additional validation branches", {
  dat <- data.frame(
    y = c(1, 2, 3),
    id = c("a", "b", "c")
  )

  expect_error(cluster_parse_formula(~ id, data = dat), "Could not extract response")
  parsed_id <- cluster_parse_formula(y ~ id, data = dat)
  expect_equal(parsed_id$response, "y")

  expect_equal(cluster_default_link(list(bulk_support = list(sd = "positive_sd")), "sd"), "exp")
  expect_equal(cluster_default_link(list(bulk_support = list(mean = "real")), "mean"), "identity")
  expect_error(cluster_link_bulk_specs("not-a-kernel"), "Kernel 'not-a-kernel' not found")
  expect_equal(cluster_default_beta_prior("tail_scale")$args$sd, 0.5)
  expect_equal(cluster_default_beta_prior("tail_shape")$args$sd, 0.3)

  expect_error(cluster_split_overrides("bad", bulk_names = "mean", gpd_names = "tail_scale"), "supplied as a list")
  expect_error(cluster_split_overrides(list(1), bulk_names = "mean", gpd_names = "tail_scale"), "Unnamed override entries")
  empty_overrides <- cluster_split_overrides(list(), bulk_names = "mean", gpd_names = "tail_scale")
  expect_equal(empty_overrides$bulk, list())
  expect_equal(empty_overrides$gpd, list())
  expect_null(empty_overrides$concentration)

  expect_error(cluster_normalize_link_entry(list(mode = "fixed")), "mode='link'")
  expect_equal(cluster_normalize_prior_entry("gamma", current_mode = "link")$beta_prior$dist, "gamma")
  expect_equal(cluster_normalize_prior_entry(list(mode = "dist", dist = "gamma"), current_mode = "dist")$dist, "gamma")
  expect_equal(
    cluster_normalize_prior_entry(
      list(beta_prior = list(dist = "normal", args = list(sd = 1))),
      current_mode = "dist"
    )$mode,
    "link"
  )
  expect_equal(
    cluster_normalize_prior_entry(
      list(args = list(mean = 1)),
      current_mode = "dist"
    )$dist,
    "normal"
  )
  expect_equal(
    cluster_normalize_prior_entry(
      list(args = list(mean = 0)),
      current_mode = "link",
      param_name = "tail_scale"
    )$beta_prior$dist,
    "normal"
  )

  spec <- list(
    plan = list(
      bulk = list(mean = list(mode = "dist")),
      gpd = list(),
      concentration = list(mode = "dist", dist = "gamma")
    )
  )

  out_char <- cluster_apply_prior_overrides(spec, priors = list(concentration = "gamma"))
  expect_equal(out_char$plan$concentration$dist, "gamma")

  out_mode <- cluster_apply_prior_overrides(
    spec,
    priors = list(concentration = list(mode = "fixed", value = 2))
  )
  expect_equal(out_mode$plan$concentration$value, 2)

  out_dist <- cluster_apply_prior_overrides(
    spec,
    priors = list(concentration = list(args = list(shape = 2, rate = 3)))
  )
  expect_equal(out_dist$plan$concentration$mode, "dist")
  expect_equal(out_dist$plan$concentration$args$shape, 2)
  expect_equal(out_dist$plan$concentration$args$rate, 3)

  expect_error(
    cluster_apply_prior_overrides(spec, priors = list(concentration = TRUE)),
    "Invalid concentration prior override"
  )
})

test_that("cluster fallback matrix helpers run when fast-path helpers are unavailable", {
  z_draws <- matrix(
    c(
      1, 1, 2,
      1, 2, 2
    ),
    nrow = 2,
    byrow = TRUE
  )

  testthat::local_mocked_bindings(
    .compute_psm = 0,
    .dahl_representative = 0,
    .compute_cluster_probs = 0,
    .package = "CausalMixGPD"
  )

  psm <- compute_psm(z_draws)
  expect_equal(
    psm,
    matrix(
      c(
        1, 0.5, 0,
        0.5, 1, 0.5,
        0, 0.5, 1
      ),
      nrow = 3,
      byrow = TRUE
    )
  )

  dahl <- dahl_labels(z_draws, psm)
  expect_equal(dahl$draw_index, 1L)
  expect_equal(dahl$labels, c(1L, 1L, 2L))

  scores <- getFromNamespace(".cluster_compute_scores", "CausalMixGPD")(z_draws, dahl$labels, psm)
  expect_equal(rowSums(scores), rep(1, 3), tolerance = 1e-8)
  expect_gt(scores[1, 1], scores[1, 2])
})

test_that("cluster summary and profile helpers cover non-plot edge branches", {
  expect_equal(cluster_order_levels(c("10", "2", "x"), order_by = "label"), c("2", "10", "x"))

  tab <- cluster_size_table(c("a", "b", "a"), levels = c("b", "a", "c"))
  expect_equal(as.integer(tab), c(1L, 2L, 0L))

  expect_null(cluster_data_frame_from_design(NULL))

  design_df <- cluster_data_frame_from_design(
    design = list(y = c(1, 2), X = matrix(c(10, 20, 30, 40), nrow = 2)),
    formula_meta = list(response = "resp", X_cols = c("x1", "x2"))
  )
  expect_equal(names(design_df), c("resp", "x1", "x2"))

  fit_stub <- structure(
    list(
      bundle = list(data = NULL),
      spec = list(cluster = list(formula_meta = list()))
    ),
    class = c("dpmixgpd_cluster_fit", "list")
  )
  expect_null(cluster_training_reference(fit_stub, integer(0)))

  blank_name <- data.frame(y = c(1, 2), check.names = FALSE)
  names(blank_name) <- ""
  expect_equal(cluster_response_name(blank_name), "y")
  expect_equal(cluster_response_values(data.frame(a = 1:2), response_name = "missing"), numeric(0))
  expect_equal(cluster_plot_ylim(c(NA_real_, NA_real_)), c(0, 1))
  expect_length(cluster_scale_values(character(0)), 0L)
  expect_equal(cluster_hist_breaks(c(NA_real_, NA_real_)), c(0, 1))

  hist_counts <- cluster_hist_max_counts(
    data.frame(cluster = c("1", "1"), response = c(1, 2)),
    levels = c("1", "2"),
    breaks = seq(0, 2, by = 1)
  )
  expect_equal(unname(hist_counts), c(1, 0))

  lbl_no_data <- structure(
    list(labels = c("1", "2"), components = 2L, source = "train"),
    class = c("dpmixgpd_cluster_labels", "list")
  )
  expect_warning(expect_null(cluster_summary_plot_data(lbl_no_data)), "No attached data available")

  lbl_mismatch <- structure(
    list(labels = c("1", "2"), data = data.frame(y = 1), source = "train"),
    class = c("dpmixgpd_cluster_labels", "list")
  )
  expect_warning(expect_null(cluster_summary_plot_data(lbl_mismatch)), "same number of rows")

  lbl_nonfinite <- structure(
    list(labels = c("1", "2"), data = data.frame(y = c(NA_real_, NaN)), source = "train"),
    class = c("dpmixgpd_cluster_labels", "list")
  )
  expect_warning(expect_null(cluster_summary_plot_data(lbl_nonfinite)), "No finite response values")

  lbl_new <- structure(
    list(
      labels = c("10", "10", "3"),
      data = data.frame(y = c(2, 3, 4)),
      source = "newdata",
      train_reference = list(
        labels = c("3", "10", "10"),
        data = data.frame(y = c(1, 2, 3))
      )
    ),
    class = c("dpmixgpd_cluster_labels", "list")
  )
  summary_dat <- cluster_summary_plot_data(lbl_new)
  expect_false(summary_dat$has_reference)
  expect_equal(summary_dat$levels, c("10", "3"))
  expect_equal(cluster_summary_plot_data(lbl_new, top_n = 1L, order_by = "size")$levels, "10")
  expect_equal(cluster_summary_plot_data(lbl_new, top_n = 1L, order_by = "label")$levels, "3")

  expect_null(cluster_profile_table(NULL, labels = c(1L, 2L)))
  expect_null(cluster_profile_table(data.frame(y = numeric(0)), labels = integer(0)))
  expect_error(cluster_profile_table(data.frame(y = 1:2), labels = 1:3), "same number of rows")
  expect_error(cluster_profile_table(data.frame(y = 1:2), labels = 1:2, vars = "missing"), "Unknown profiling variables")
  expect_error(cluster_profile_table(data.frame(y = 1:2), labels = 1:2, top_n = 0), "integer >= 1")

  prof <- cluster_profile_table(
    data = data.frame(y = c(1, 2), grp = c("a", "b")),
    labels = c("2", "10"),
    score_mat = matrix(c(0.9, 0.1, 0.2, 0.8), nrow = 2, byrow = TRUE),
    top_n = 1L,
    order_by = "label",
    vars = c("y", "grp")
  )
  expect_equal(nrow(prof), 1L)
  expect_true(all(c("certainty_mean", "certainty_sd") %in% names(prof)))
})

test_that("cluster newdata builders and density helpers cover edge branches", {
  train <- data.frame(
    y = c(1, 2, 3),
    x1 = c(0, 1, 0),
    g = factor(c("a", "b", "a"))
  )
  trm <- stats::terms(y ~ x1 + g, data = train)
  mf <- stats::model.frame(trm, data = train)
  mm <- stats::model.matrix(stats::delete.response(trm), data = mf)
  meta <- list(
    terms = trm,
    xlevels = stats::.getXlevels(trm, mf),
    contrasts = attr(mm, "contrasts"),
    X_cols = setdiff(colnames(mm), "(Intercept)"),
    response = "y"
  )

  meta_no_x <- list(
    terms = stats::terms(y ~ 1, data = train),
    xlevels = list(),
    contrasts = NULL,
    X_cols = character(0),
    response = "y"
  )

  out_no_x <- cluster_build_design(meta_no_x, data.frame(y = c(1, 2)))
  expect_null(out_no_x$X)
  expect_equal(out_no_x$y, c(1, 2))

  expect_error(cluster_build_design(meta, data.frame(x1 = 1, g = "a")), "response column")
  expect_error(cluster_build_design(meta_no_x, data.frame(y = c(1, NA_real_))), "response contains NA")
  expect_error(cluster_build_design(meta, data.frame(y = 1)), "Failed to build model frame")

  meta_missing <- utils::modifyList(meta, list(X_cols = c(meta$X_cols, "x_missing")))
  expect_error(
    cluster_build_design(
      meta_missing,
      data.frame(y = 1, x1 = 0, g = factor("a", levels = c("a", "b")))
    ),
    "missing required predictors"
  )

  expect_equal(cluster_softmax(c(Inf, Inf)), c(0.5, 0.5))

  testthat::local_mocked_bindings(
    .cluster_softmax = function(logits) 1,
    .package = "CausalMixGPD"
  )
  expect_null(cluster_gating_weights(list(eta = 0, B = matrix(1, nrow = 1)), x_row = 1))

  expect_error(cluster_resolve_density_fun(list(meta = list(kernel = "nope", GPD = FALSE))), "Kernel 'nope' not found")

  testthat::local_mocked_bindings(
    get_kernel_registry = function() {
      list(dummy = list(crp = list(d_base = NA_character_, d_gpd = NA_character_)))
    },
    .package = "CausalMixGPD"
  )
  expect_error(cluster_resolve_density_fun(list(meta = list(kernel = "dummy", GPD = FALSE))), "Could not resolve density function")

  testthat::local_mocked_bindings(
    get_kernel_registry = function() {
      list(dummy = list(crp = list(d_base = "definitely_not_a_density", d_gpd = "definitely_not_a_density")))
    },
    .package = "CausalMixGPD"
  )
  expect_error(cluster_resolve_density_fun(list(meta = list(kernel = "dummy", GPD = FALSE))), "is unavailable")

  spec_gpd <- list(
    meta = list(GPD = TRUE, P = 1L),
    plan = list(
      bulk = list(
        mean = list(mode = "link", link = "identity"),
        sd = list(mode = "fixed", value = 1)
      ),
      gpd = list(
        threshold = list(mode = "fixed", value = 0.5),
        tail_scale = list(mode = "link", link = "exp"),
        tail_shape = list(mode = "dist")
      )
    ),
    signatures = list(gpd = list(args = c("mean", "sd", "threshold", "tail_scale", "tail_shape")))
  )

  dens_fun <- function(x, mean, sd, threshold, tail_scale, tail_shape, log = FALSE) {
    out <- x + mean + sd + threshold + tail_scale + tail_shape
    if (isTRUE(log)) log(out) else out
  }

  dens_link <- cluster_component_density(
    spec = spec_gpd,
    draw_row = c(
      "beta_mean[1,1]" = 0.2,
      "sd[1]" = 1.1,
      "beta_tail_scale[1]" = 0,
      "tail_shape" = 0.3
    ),
    k = 1L,
    x_row = 2,
    y_val = 1,
    density_fun = dens_fun
  )
  expect_true(is.finite(dens_link))
  expect_gt(dens_link, 0)

  dens_indexed <- cluster_component_density(
    spec = spec_gpd,
    draw_row = c(
      "beta_mean[1,1]" = 0,
      "sd[1]" = 1,
      "beta_tail_scale[1]" = 0,
      "tail_shape[1]" = 0.25
    ),
    k = 1L,
    x_row = 1,
    y_val = 1,
    density_fun = function(...) -1
  )
  expect_equal(dens_indexed, 0)

  dens_missing <- cluster_component_density(
    spec = spec_gpd,
    draw_row = c(
      "beta_mean[1,1]" = 0,
      "sd[1]" = 1,
      "beta_tail_scale[1]" = 0
    ),
    k = 1L,
    x_row = 1,
    y_val = 1,
    density_fun = function(...) stop("boom")
  )
  expect_equal(dens_missing, 0)
})

test_that("cluster prediction helpers cover cache fallbacks and actual run assembly", {
  meta_no_x <- list(
    terms = stats::terms(y ~ 1, data = data.frame(y = 1)),
    xlevels = list(),
    contrasts = NULL,
    X_cols = character(0),
    response = "y"
  )

  cache_env <- new.env(parent = emptyenv())
  assign(
    "cache_1_0_1",
    list(
      dahl = list(labels = c(1L, 2L), K = 2L),
      probs_train = matrix(c(0.9, 0.1, 0.2, 0.8), nrow = 2, byrow = TRUE)
    ),
    envir = cache_env
  )

  fit_newdata <- structure(
    list(
      spec = list(
        meta = list(kernel = "normal", GPD = FALSE, components = 3L),
        cluster = list(type = "weights", formula_meta = meta_no_x)
      ),
      samples = "fake_samples",
      cache_env = cache_env
    ),
    class = c("dpmixgpd_cluster_fit", "list")
  )

  testthat::local_mocked_bindings(
    .cluster_samples_to_matrix = function(samples) {
      matrix(
        c(0, 0, 0, 0, 0),
        nrow = 1,
        dimnames = list(NULL, c("z[1]", "z[2]", "w[1]", "w[2]", "w[3]"))
      )
    },
    .cluster_draw_indices = function(n_draws, burnin = NULL, thin = NULL) 1L,
    .cluster_extract_z_from_matrix = function(draw_sub) matrix(c(0L, 0L), nrow = 1),
    .cluster_resolve_density_fun = function(spec) function(...) 1,
    .cluster_component_density = function(...) NA_real_,
    .package = "CausalMixGPD"
  )

  pred <- predict_labels_newdata(fit_newdata, newdata = data.frame(y = c(1, 2)))
  expect_equal(rowSums(pred$scores), rep(1, 2), tolerance = 1e-8)

  expect_equal(pred$cache$scores_train, pred$cache$probs_train)

  bundle <- structure(list(spec = list(meta = list(kernel = "normal"))), class = c("dpmixgpd_cluster_bundle", "list"))

  testthat::local_mocked_bindings(
    run_mcmc_bundle_manual = function(bundle, ...) {
      list(
        samples = NULL,
        mcmc = list(samples = "chain"),
        timing = list(build = 0.1, compile = 0.2, mcmc = 0.3)
      )
    },
    .package = "CausalMixGPD"
  )

  fit_run <- run_cluster_mcmc(bundle)
  expect_s3_class(fit_run, "dpmixgpd_cluster_fit")
  expect_equal(fit_run$samples, "chain")
  expect_equal(fit_run$timing$total, 0.6, tolerance = 1e-8)

  fit_cached <- structure(
    list(
      spec = list(meta = list()),
      samples = "fake_samples",
      cache_env = new.env(parent = emptyenv())
    ),
    class = c("dpmixgpd_cluster_fit", "list")
  )
  assign(
    "cache_1_0_1",
    list(
      psm = diag(2),
      dahl = list(labels = c(1L, 2L), K = 2L, draw_index = 1L),
      probs_train = matrix(c(0.9, 0.1, 0.2, 0.8), nrow = 2, byrow = TRUE)
    ),
    envir = fit_cached$cache_env
  )

  testthat::local_mocked_bindings(
    .cluster_samples_to_matrix = function(samples) {
      matrix(c(1, 2), nrow = 1, dimnames = list(NULL, c("z[1]", "z[2]")))
    },
    .cluster_draw_indices = function(n_draws, burnin = NULL, thin = NULL) 1L,
    .cluster_extract_z_from_matrix = function(draw_sub) matrix(c(1L, 2L), nrow = 1),
    .cluster_training_data_frame = function(object) data.frame(y = c(1, 2)),
    .package = "CausalMixGPD"
  )

  lbl <- predict(fit_cached, type = "label", return_scores = TRUE)
  expect_true(is.matrix(lbl$scores))
  expect_equal(lbl$scores, matrix(c(0.9, 0.1, 0.2, 0.8), nrow = 2, byrow = TRUE))

  fit_guard <- structure(
    list(
      spec = list(meta = list()),
      samples = "fake_samples",
      cache_env = new.env(parent = emptyenv())
    ),
    class = c("dpmixgpd_cluster_fit", "list")
  )

  testthat::local_mocked_bindings(
    .cluster_samples_to_matrix = function(samples) {
      matrix(c(1, 1, 2), nrow = 1, dimnames = list(NULL, c("z[1]", "z[2]", "z[3]")))
    },
    .cluster_draw_indices = function(n_draws, burnin = NULL, thin = NULL) 1L,
    .cluster_extract_z_from_matrix = function(draw_sub) matrix(c(1L, 1L, 2L), nrow = 1),
    .package = "CausalMixGPD"
  )

  expect_error(predict(fit_guard, type = "psm", psm_max_n = 2L), "PSM is O\\(n\\^2\\)")
})
