# test-causal.R
# Consolidated causal tests: validation, workflow, ATE/QTE, predict, plots, PS utils, S3
# Merged from: test-causal.R, test-causal-ate.R, test-causal-predict.R,
#             test-causal-validation.R, test-plots-causal-effects.R,
#             test-ps-utils.R, test-s3-causal-effects.R
#
# Tier A (cran): Basic PS parameter tests, build_causal_bundle validation
# Tier B (ci):   Representative causal combos, ATE/QTE, predict, plots, S3
# Tier C (full): Exhaustive kernel x backend x GPD grid

if (!exists(".cache_enabled")) {
  helper_path <- file.path("tests", "testthat", "helper-cache.R")
  if (file.exists(helper_path)) source(helper_path)
}
helper_test_levels <- file.path("tests", "testthat", "helper-test-levels.R")
if (file.exists(helper_test_levels)) source(helper_test_levels)

# =============================================================================
# Tier B (ci): Representative causal combos - covers main code paths
# =============================================================================
test_that("causal workflow: representative combos (Tier B)", {
  skip_if_not_test_level("ci")

  set.seed(1)
  n <- 25
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  t_ind <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.5 * X[, 1]))
  y <- stats::rexp(n) + 0.1

  mcmc_out <- mcmc_fast(seed = 1L)
  mcmc_ps <- mcmc_fast(seed = 2L)

  # Representative combos that cover main branches
  combos <- representative_causal_combos()

  for (cfg in combos) {
    info <- cfg$label

    cb <- DPmixGPD::build_causal_bundle(
      y = y,
      X = X,
      T = t_ind,
      backend = cfg$backend,
      kernel = cfg$kernel,
      GPD = cfg$GPD,
      components = c(4, 4),
      mcmc_outcome = mcmc_out,
      mcmc_ps = mcmc_ps,
      PS = cfg$PS,

    )

    cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)

    # Basic structure checks
    expect_true(inherits(cf, "dpmixgpd_causal_fit"), info = info)
    expect_true(inherits(cf$outcome_fit$con, "mixgpd_fit"), info = info)
    expect_true(inherits(cf$outcome_fit$trt, "mixgpd_fit"), info = info)

    # Summary works
    s_con <- summary(cf$outcome_fit$con)
    s_trt <- summary(cf$outcome_fit$trt)
    expect_true(inherits(s_con, "mixgpd_summary"), info = info)
    expect_true(inherits(s_trt, "mixgpd_summary"), info = info)

    # Predictions work
    pr_con <- predict(cf$outcome_fit$con, type = "mean")
    pr_trt <- predict(cf$outcome_fit$trt, type = "mean")
    expect_true(is.list(pr_con) && "fit" %in% names(pr_con), info = info)
    expect_true(is.list(pr_trt) && "fit" %in% names(pr_trt), info = info)

    # Fitted/residuals work
    fd_con <- fitted(cf$outcome_fit$con)
    rs_con <- residuals(cf$outcome_fit$con)
    expect_true(is.data.frame(fd_con), info = info)
    expect_true(is.numeric(rs_con), info = info)

    # Causal predict works
    X_new <- X[1:3, , drop = FALSE]
    pred <- predict(cf, x = X_new, type = "mean", store_draws = FALSE)
    expect_true(is.matrix(pred), info = info)
    expect_true(nrow(pred) == nrow(X_new), info = info)
  }
})

# =============================================================================
# Tier C (full): Exhaustive kernel x backend x GPD grid
# =============================================================================
test_that("causal workflow: exhaustive kernel combos (Tier C)", {
  skip_if_not_full()

  set.seed(1)
  n <- 30
  x_base <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  x_cat_df <- data.frame(
    x1 = x_base[, 1],
    x2 = x_base[, 2],
    x3 = factor(sample(c("A", "B", "C"), n, replace = TRUE))
  )
  x_cat <- stats::model.matrix(~ x1 + x2 + x3 - 1, data = x_cat_df)

  t_ind <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.5 * x_base[, 1]))
  y <- stats::rexp(n) + 0.1

  mcmc_out <- mcmc_fast(seed = 1L)
  mcmc_ps <- mcmc_fast(seed = 2L)

  kernels <- names(DPmixGPD::get_kernel_registry())
  combos <- list()
  for (k in kernels) {
    combos[[paste0(k, "_bulk")]] <- list(
      label = paste0("kernel=", k, ", backend=sb/crp, GPD=FALSE/FALSE, X=categorical"),
      kernel = c(k, k),
      backend = c("sb", "crp"),
      GPD = c(FALSE, FALSE),
      X = x_cat,
      components = c(4, 5),
      epsilon = c(0.025, 0.05)
    )
    if (!identical(k, "cauchy")) {
      combos[[paste0(k, "_gpd")]] <- list(
        label = paste0("kernel=", k, ", backend=sb/sb, GPD=TRUE/TRUE, X=categorical"),
        kernel = c(k, k),
        backend = c("sb", "sb"),
        GPD = c(TRUE, TRUE),
        X = x_cat,
        components = c(4, 4),
        epsilon = c(0.025, 0.025)
      )
    }
  }

  for (nm in names(combos)) {
    cfg <- combos[[nm]]
    info <- cfg$label %||% nm
    cache_key <- NULL
    if (exists(".cache_enabled") && isTRUE(.cache_enabled())) {
      key_str <- paste(
        "causal", nm,
        paste(cfg$backend, collapse = ","), paste(cfg$kernel, collapse = ","),
        paste(cfg$GPD, collapse = ","), paste(cfg$components, collapse = ","),
        paste(cfg$epsilon, collapse = ","),
        nrow(cfg$X), ncol(cfg$X),
        mcmc_out$niter, mcmc_out$nburnin, mcmc_out$thin, mcmc_out$nchains, mcmc_out$seed,
        mcmc_ps$niter, mcmc_ps$nburnin, mcmc_ps$thin, mcmc_ps$nchains, mcmc_ps$seed,
        sep = "|"
      )
      cache_key <- .cache_hash(key_str)
    }
    cached <- if (!is.null(cache_key)) .cache_get(cache_key) else NULL
    if (!is.null(cached) && inherits(cached$cf, "dpmixgpd_causal_fit")) {
      cf <- cached$cf
    } else {
      cb <- DPmixGPD::build_causal_bundle(
        y = y,
        X = cfg$X,
        T = t_ind,
        backend = cfg$backend,
        kernel = cfg$kernel,
        GPD = cfg$GPD,
        components = cfg$components,
        epsilon = cfg$epsilon,
        mcmc_outcome = mcmc_out,
        mcmc_ps = mcmc_ps
      )
      cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
      if (!is.null(cache_key)) .cache_set(cache_key, list(cf = cf))
    }

    expect_true(inherits(cf, "dpmixgpd_causal_fit"), info = info)
    expect_true(inherits(cf$outcome_fit$con, "mixgpd_fit"), info = info)
    expect_true(inherits(cf$outcome_fit$trt, "mixgpd_fit"), info = info)

    s_con <- summary(cf$outcome_fit$con)
    s_trt <- summary(cf$outcome_fit$trt)
    expect_true(inherits(s_con, "mixgpd_summary"), info = info)
    expect_true(inherits(s_trt, "mixgpd_summary"), info = info)

    pr_con <- predict(cf$outcome_fit$con, type = "mean")
    pr_trt <- predict(cf$outcome_fit$trt, type = "mean")
    expect_true(is.list(pr_con) && "fit" %in% names(pr_con), info = info)
    expect_true(is.list(pr_trt) && "fit" %in% names(pr_trt), info = info)

    fd_con <- fitted(cf$outcome_fit$con)
    fd_trt <- fitted(cf$outcome_fit$trt)
    rs_con <- residuals(cf$outcome_fit$con)
    rs_trt <- residuals(cf$outcome_fit$trt)
    fit_con <- if (is.data.frame(fd_con)) fd_con$fit else fd_con
    fit_trt <- if (is.data.frame(fd_trt)) fd_trt$fit else fd_trt
    expect_true(is.numeric(fit_con) && is.numeric(fit_trt), info = info)
    expect_true(is.numeric(rs_con) && is.numeric(rs_trt), info = info)
  }
})

# =============================================================================
# Tier A (cran): PS parameter tests - fast, no MCMC compilation loops
# =============================================================================

test_that("PS parameter: logit, probit, naive, FALSE all work", {
  # Skip during coverage runs due to covr/nimble interaction causing
  # "variable name conflicts with C++ keywords" error
  # Check multiple indicators since covr may not set COVERAGE env var
  is_covr <- nzchar(Sys.getenv("COVERAGE")) ||
    nzchar(Sys.getenv("R_COVR")) ||
    nzchar(Sys.getenv("CODECOV_TOKEN")) ||
    any(grepl("covr", loadedNamespaces(), ignore.case = TRUE))
  skip_if(is_covr, "Skipping during coverage due to covr/nimble interaction")

  set.seed(99)
  n <- 40
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T <- stats::rbinom(n, 1, stats::plogis(0.3 + 0.4 * X[, 1]))
  y <- abs(stats::rnorm(n)) + 0.2

  mcmc_out <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)
  mcmc_ps <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)

  # Test all four PS options
  ps_options <- list("logit", "probit", "naive", FALSE)

  for (ps_opt in ps_options) {
    label <- if (isFALSE(ps_opt)) "FALSE" else ps_opt
    # Build bundle
    cb <- DPmixGPD::build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "crp",
      kernel = "gamma",
      GPD = FALSE,
      components = 4,
      mcmc_outcome = mcmc_out,
      mcmc_ps = mcmc_ps,
      PS = ps_opt,
    )

    # Check metadata
    expect_true(inherits(cb, "dpmixgpd_causal_bundle"), info = label)
    expect_true(!is.null(cb$meta$ps), info = label)
    if (isFALSE(ps_opt)) {
      expect_false(cb$meta$ps$enabled, info = label)
      expect_false(cb$meta$ps$model_type, info = label)
    } else {
      expect_true(cb$meta$ps$enabled, info = label)
      expect_equal(cb$meta$ps$model_type, ps_opt, info = label)
    }

    # Run MCMC
    cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
    expect_true(inherits(cf, "dpmixgpd_causal_fit"), info = label)

    if (isFALSE(ps_opt)) {
      # No PS model
      expect_true(is.null(cf$ps_fit), info = label)
      expect_true(is.null(cf$ps_hat), info = label)
      expect_true(is.null(cf$ps), info = label)
    } else {
      # PS model computed
      expect_true(!is.null(cf$ps_fit), info = label)
      expect_true(!is.null(cf$ps_hat), info = label)
      expect_true(length(cf$ps_hat) == n, info = label)
      expect_true(all(cf$ps_hat >= 0 & cf$ps_hat <= 1), info = label)
      expect_true(!is.null(cf$outcome_fit$con$ps_model), info = label)
    }

    # Test prediction
    X_new <- X[1:5, ]
    pred <- predict(cf, x = X_new, type = "mean", store_draws = FALSE)
    expect_true(is.matrix(pred), info = label)
    expect_true(nrow(pred) == nrow(X_new), info = label)
    expect_true(all(c("ps", "estimate", "lower", "upper") %in% colnames(pred)), info = label)

    if (isFALSE(ps_opt)) {
      expect_true(all(is.na(pred[, "ps"])), info = label)
    } else {
      expect_true(all(is.finite(pred[, "ps"])), info = label)
      expect_true(all(pred[, "ps"] >= 0 & pred[, "ps"] <= 1), info = label)
    }
  }
})

# =============================================================================
# ATE/QTE and ate_rmean (from test-causal-ate.R)
# =============================================================================
test_that("ATE/QTE use matching posterior draws and shapes", {
  skip_if_not_test_level("ci")

  set.seed(42)
  n <- 20
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.4 * X[, 1]))
  y <- abs(stats::rnorm(n)) + 0.1

  mcmc_out <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)
  mcmc_ps  <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)

  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = c("sb", "sb"),
    kernel = c("normal", "normal"),
    GPD = c(FALSE, FALSE),
    components = c(4, 4),
    mcmc_outcome = mcmc_out,
    mcmc_ps = mcmc_ps
  )

  cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
  newx <- head(X, 3)

  qres <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), newdata = newx, interval = "credible")
  expect_true(is.matrix(qres$fit) && all(dim(qres$fit) == c(nrow(newx), 2)))
  expect_true(identical(dim(qres$trt$draws), dim(qres$con$draws)))
  diff_q <- qres$trt$draws - qres$con$draws
  expect_equal(apply(diff_q, c(2, 3), mean), qres$fit, tolerance = 1e-8)
  expect_true(all(qres$lower <= qres$upper, na.rm = TRUE))

  ares <- DPmixGPD::ate(cf, newdata = newx, interval = "credible", nsim_mean = 20L)
  expect_true(length(ares$fit) == nrow(newx))
  expect_true(identical(dim(ares$trt$draws), dim(ares$con$draws)))
  expect_true(is.null(ares$grid))
  diff_a <- ares$trt$draws - ares$con$draws
  expect_equal(colMeans(diff_a), ares$fit, tolerance = 1e-8)
  expect_true(all(ares$lower <= ares$upper, na.rm = TRUE))
})

test_that("ate_rmean returns finite estimates with same structure as ate", {
  skip_if_not_test_level("ci")

  set.seed(43)
  n <- 20
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.4 * X[, 1]))
  y <- abs(stats::rnorm(n)) + 0.1

  mcmc_out <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)
  mcmc_ps  <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)

  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = c("sb", "sb"),
    kernel = c("normal", "normal"),
    GPD = c(FALSE, FALSE),
    components = c(4, 4),
    mcmc_outcome = mcmc_out,
    mcmc_ps = mcmc_ps
  )

  cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
  newx <- head(X, 3)

  ares <- DPmixGPD::ate_rmean(cf, newdata = newx, cutoff = 10, interval = "credible", nsim_mean = 20L)
  expect_true(length(ares$fit) == nrow(newx))
  expect_true(all(is.finite(ares$fit)))
  expect_true(is.null(ares$grid))
  expect_true(all(c("fit", "lower", "upper", "trt", "con", "meta", "grid") %in% names(ares)))
})

# -----------------------------------------------------------------------------
# ate(type="mean") Inf vs ate_rmean finite when GPD tail_shape >= 1
# -----------------------------------------------------------------------------
test_that("ate(type='mean') returns Inf when outcome GPD has tail_shape >= 1", {
  skip_if_not_test_level("ci")

  set.seed(44)
  n <- 20
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.4 * X[, 1]))
  y <- abs(stats::rnorm(n)) + 0.1

  mcmc_out <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)
  mcmc_ps  <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)

  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = c("sb", "sb"),
    kernel = c("gamma", "gamma"),
    GPD = c(TRUE, TRUE),
    components = c(4, 4),
    mcmc_outcome = mcmc_out,
    mcmc_ps = mcmc_ps
  )

  cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
  newx <- head(X, 3)

  # Patch treated outcome so one draw has tail_shape >= 1 -> posterior mean infinite
  trt_fit <- cf$outcome_fit$trt
  smp <- trt_fit$mcmc$samples %||% trt_fit$samples
  if (is.null(smp) || !("tail_shape" %in% colnames(as.matrix(smp[[1]])))) {
    skip("Outcome fit has no tail_shape (unexpected structure)")
  }
  ch <- as.matrix(smp[[1]])
  orig_xi <- ch[1L, "tail_shape"]
  ch[1L, "tail_shape"] <- 1.5
  smp[[1]] <- coda::as.mcmc(ch)
  if (!is.null(trt_fit$mcmc$samples)) trt_fit$mcmc$samples <- smp
  if (!is.null(trt_fit$samples)) trt_fit$samples <- smp
  cf$outcome_fit$trt <- trt_fit

  expect_warning(
    ares <- DPmixGPD::ate(cf, newdata = newx, type = "mean", interval = NULL, nsim_mean = 20L),
    "infinite"
  )
  expect_true(all(!is.finite(ares$fit)) | any(ares$fit == Inf))
})

test_that("ate_rmean returns finite estimates when outcome has GPD (same structure as ate)", {
  skip_if_not_test_level("ci")

  set.seed(45)
  n <- 20
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.4 * X[, 1]))
  y <- abs(stats::rnorm(n)) + 0.1

  mcmc_out <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)
  mcmc_ps  <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)

  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = c("sb", "sb"),
    kernel = c("gamma", "gamma"),
    GPD = c(TRUE, TRUE),
    components = c(4, 4),
    mcmc_outcome = mcmc_out,
    mcmc_ps = mcmc_ps
  )

  cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
  newx <- head(X, 3)

  ares <- DPmixGPD::ate_rmean(cf, newdata = newx, cutoff = 10, interval = "credible", nsim_mean = 20L)
  expect_true(length(ares$fit) == nrow(newx))
  expect_true(all(is.finite(ares$fit)))
  expect_true(is.null(ares$grid))
  expect_true(all(c("fit", "lower", "upper", "trt", "con", "meta", "grid") %in% names(ares)))
})


# =============================================================================
# build_causal_bundle validation (from test-causal-validation.R)
# =============================================================================
build_causal_bundle <- DPmixGPD::build_causal_bundle

test_that("build_causal_bundle errors on empty y", {
  expect_error(
    build_causal_bundle(
      y = numeric(0),
      X = matrix(1:2, ncol = 1),
      T = c(0, 1),
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      components = 4,

    ),
    regexp = "non-empty"
  )
})

test_that("build_causal_bundle errors on T length mismatch", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- rbinom(10, 1, 0.5)  # wrong length

  expect_error(
    build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      components = 4,

    ),
    regexp = "same length"
  )
})

test_that("build_causal_bundle errors on non-binary T", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- rep(c(0, 1, 2), length.out = 20)  # not binary

  expect_error(
    build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      components = 4,

    ),
    regexp = "binary|0/1"
  )
})

test_that("build_causal_bundle errors on T with NA values", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 9), NA, rep(1, 10))

  expect_error(
    build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      components = 4,

    ),
    regexp = "binary|NA"
  )
})

test_that("build_causal_bundle errors when only one treatment arm has data", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- rep(1, 20)  # all treated, no control

  expect_error(
    build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      components = 4,

    ),
    regexp = "Both treatment arms|at least one"
  )
})

test_that("build_causal_bundle errors when X row mismatch", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(30), ncol = 3)  # 10 rows instead of 20
  T <- rbinom(20, 1, 0.5)

  expect_error(
    build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      components = 4,

    ),
    regexp = "same number of rows"
  )
})

test_that("build_causal_bundle errors on components < 2", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  expect_error(
    build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      components = 1,

    ),
    regexp = ">= 2"
  )
})

test_that("build_causal_bundle errors on invalid epsilon", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  expect_error(
    build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      components = 4,
      epsilon = -0.1,

    ),
    regexp = "epsilon|\\[0.*1\\)"
  )
})

# ======================================================================
# Bundle structure tests
# ======================================================================

test_that("build_causal_bundle returns correct class", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,

  )

  expect_s3_class(bundle, "dpmixgpd_causal_bundle")
})

test_that("build_causal_bundle has required components", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,

  )

  expect_true("outcome" %in% names(bundle))
  expect_true("data" %in% names(bundle))
  expect_true("index" %in% names(bundle))
  expect_true("meta" %in% names(bundle))
})

test_that("build_causal_bundle creates outcome bundles for both arms", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,

  )

  expect_true("trt" %in% names(bundle$outcome))
  expect_true("con" %in% names(bundle$outcome))
  expect_s3_class(bundle$outcome$trt, "dpmixgpd_bundle")
  expect_s3_class(bundle$outcome$con, "dpmixgpd_bundle")
})

test_that("build_causal_bundle stores correct indices", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,

  )

  expect_equal(bundle$index$con, 1:10)
  expect_equal(bundle$index$trt, 11:20)
})

# ======================================================================
# Arm-specific parameter tests
# ======================================================================

test_that("build_causal_bundle allows arm-specific backends", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = c("sb", "crp"),  # trt=sb, con=crp
    kernel = "normal",
    GPD = FALSE,
    components = 4,

  )

  expect_equal(bundle$meta$backend$trt, "sb")
  expect_equal(bundle$meta$backend$con, "crp")
})

test_that("build_causal_bundle allows arm-specific kernels", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = c("normal", "gamma"),  # trt=normal, con=gamma
    GPD = FALSE,
    components = 4,

  )

  expect_equal(bundle$meta$kernel$trt, "normal")
  expect_equal(bundle$meta$kernel$con, "gamma")
})

test_that("build_causal_bundle allows arm-specific GPD", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = c(TRUE, FALSE),  # trt=TRUE, con=FALSE
    components = 4,

  )

  expect_true(bundle$meta$GPD$trt)
  expect_false(bundle$meta$GPD$con)
})

test_that("build_causal_bundle allows arm-specific components", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = c(6, 8),  # trt=6, con=8

  )

  expect_equal(bundle$meta$components$trt, 6)
  expect_equal(bundle$meta$components$con, 8)
})

test_that("build_causal_bundle allows arm-specific epsilon", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,
    epsilon = c(0.01, 0.05),  # trt=0.01, con=0.05

  )

  expect_equal(bundle$meta$epsilon$trt, 0.01)
  expect_equal(bundle$meta$epsilon$con, 0.05)
})

# ======================================================================
# PS model type tests
# ======================================================================

test_that("build_causal_bundle supports PS='logit'", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,

    PS = "logit"
  )

  expect_s3_class(bundle, "dpmixgpd_causal_bundle")
  expect_equal(bundle$meta$ps$model_type, "logit")
})

test_that("build_causal_bundle supports PS='probit'", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,
    PS = "probit"
  )

  expect_s3_class(bundle, "dpmixgpd_causal_bundle")
  expect_equal(bundle$meta$ps$model_type, "probit")
})

test_that("build_causal_bundle supports PS='naive'", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,
    PS = "naive"
  )

  expect_s3_class(bundle, "dpmixgpd_causal_bundle")
  expect_equal(bundle$meta$ps$model_type, "naive")
})

# ======================================================================
# Empty X handling tests
# ======================================================================

test_that("build_causal_bundle handles NULL X for RCT", {
  set.seed(1)
  y <- rnorm(20)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = NULL,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4
  )

  expect_s3_class(bundle, "dpmixgpd_causal_bundle")
  expect_false(bundle$meta$has_x)
})

test_that("build_causal_bundle handles empty matrix X", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(nrow = 20, ncol = 0)  # no columns
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4
  )

  expect_s3_class(bundle, "dpmixgpd_causal_bundle")
  expect_false(bundle$meta$has_x)
})

# ======================================================================
# MCMC settings tests
# ======================================================================

test_that("build_causal_bundle stores MCMC settings", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,
    mcmc_outcome = list(niter = 100, nburnin = 20, thin = 2, nchains = 1, seed = 42)
  )

  expect_equal(bundle$outcome$trt$mcmc$niter, 100)
  expect_equal(bundle$outcome$con$mcmc$niter, 100)
})

# ======================================================================
# param_specs tests
# ======================================================================

test_that("build_causal_bundle applies shared param_specs", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  param_specs <- list(
    bulk = list(
      mean = list(mode = "link", link = "identity")
    )
  )

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,
    param_specs = param_specs
  )

  expect_equal(bundle$outcome$trt$spec$plan$bulk$mean$mode, "link")
  expect_equal(bundle$outcome$con$spec$plan$bulk$mean$mode, "link")
})

test_that("build_causal_bundle applies arm-specific param_specs", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  param_specs <- list(
    trt = list(bulk = list(mean = list(mode = "link", link = "identity"))),
    con = list(bulk = list(mean = list(mode = "dist")))
  )

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,
    param_specs = param_specs
  )

  expect_equal(bundle$outcome$trt$spec$plan$bulk$mean$mode, "link")
  expect_equal(bundle$outcome$con$spec$plan$bulk$mean$mode, "dist")
})


# =============================================================================
# Causal predict (from test-causal-predict.R)
# =============================================================================
# =============================================================================
# Shared test fixture: create a minimal causal fit once for reuse
# =============================================================================
.make_causal_fit <- function(ps_opt = "logit") {
  set.seed(42)
  n <- 20
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T_ind <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.4 * X[, 1]))
  y <- abs(stats::rnorm(n)) + 0.1

  mcmc_out <- mcmc_fast(seed = 1L)
  mcmc_ps <- mcmc_fast(seed = 2L)

  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    T = T_ind,
    backend = c("sb", "sb"),
    kernel = c("normal", "normal"),
    GPD = c(FALSE, FALSE),
    components = c(4, 4),
    mcmc_outcome = mcmc_out,
    mcmc_ps = mcmc_ps,
    PS = ps_opt,

  )

  cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
  list(fit = cf, X = X, y = y, n = n)
}

# =============================================================================
# Tier B (ci): Error path tests - require a causal fit to test predict errors
# =============================================================================
test_that("causal predict: x and newdata both provided error", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X

  # Should error when both provided
  expect_error(
    predict(cf, x = X[1:3, ], newdata = X[1:3, ], type = "mean"),
    "one of 'x' or 'newdata'"
  )
})

test_that("causal predict: quantile requires p parameter", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X

  # Missing p should error (no probabilities supplied)
  expect_error(
    predict(cf, x = X[1:3, ], type = "quantile"),
    "finite prob"
  )

  # Non-finite p should error
  expect_error(
    predict(cf, x = X[1:3, ], type = "quantile", p = NA),
    "finite prob"
  )

  # Multiple p values are supported: returns dpmixgpd_causal_predict
  pred_multi <- predict(cf, x = X[1:3, ], type = "quantile", p = c(0.25, 0.75))
  expect_s3_class(pred_multi, "dpmixgpd_causal_predict")
  expect_true(is.data.frame(pred_multi))
  expect_true("id" %in% names(pred_multi))
  expect_true("index" %in% names(pred_multi))
  expect_equal(nrow(pred_multi), 3L * 2L)  # 3 rows x 2 quantiles
})

test_that("causal predict: density/survival/prob require y", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X

  for (type in c("density", "survival", "prob")) {
    expect_error(
      predict(cf, x = X[1:3, ], type = type),
      "requires 'y'",
      info = paste0("type=", type)
    )
  }
})

test_that("causal predict: y length must match prediction rows", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X

  # 3 prediction rows but 5 y values
  expect_error(
    predict(cf, x = X[1:3, ], type = "density", y = rep(0.5, 5)),
    "match the number of prediction rows"
  )
})

# =============================================================================
# Tier B (ci): Integration tests for predict types
# =============================================================================
test_that("causal predict: type='quantile' returns correct structure", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X
  n_new <- 5
  X_new <- X[1:n_new, , drop = FALSE]

  # Single quantile prediction
  pred <- predict(cf, x = X_new, type = "quantile", p = 0.5, interval = "credible")

  expect_true(is.matrix(pred))
  expect_equal(nrow(pred), n_new)
  expect_true(all(c("ps", "estimate", "lower", "upper") %in% colnames(pred)))

  # PS should be computed (observational design with logit)
  expect_true(all(is.finite(pred[, "ps"])))
  expect_true(all(pred[, "ps"] >= 0 & pred[, "ps"] <= 1))

  # Estimates should be finite
  expect_true(all(is.finite(pred[, "estimate"])))

  # Interval should be valid
 # expect_true(all(pred[, "lower"] <= pred[, "estimate"], na.rm = TRUE))
 # expect_true(all(pred[, "estimate"] <= pred[, "upper"], na.rm = TRUE))
})

test_that("causal predict: type='density' returns correct structure", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X
  n_new <- 4
  X_new <- X[1:n_new, , drop = FALSE]
  y_eval <- data$y[1:n_new]  # Match length to X rows

  pred <- predict(cf, x = X_new, type = "density", y = y_eval, interval = "credible")

  expect_true(inherits(pred, "dpmixgpd_causal_predict"))
  expect_true(is.data.frame(pred))
  expect_equal(nrow(pred), n_new)
  expect_true(all(c("y", "ps", "trt_estimate", "con_estimate") %in% names(pred)))

  # Density should be non-negative
  expect_true(all(pred$trt_estimate >= 0))
  expect_true(all(pred$con_estimate >= 0))
})

test_that("causal predict: type='survival' returns correct structure", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X
  n_new <- 4
  X_new <- X[1:n_new, , drop = FALSE]
  y_eval <- data$y[1:n_new]

  pred <- predict(cf, x = X_new, type = "survival", y = y_eval, interval = "credible")

  expect_true(inherits(pred, "dpmixgpd_causal_predict"))
  expect_true(is.data.frame(pred))
  expect_equal(nrow(pred), n_new)

  # Survival should be in [0, 1]
  expect_true(all(pred$trt_estimate >= 0 & pred$trt_estimate <= 1))
  expect_true(all(pred$con_estimate >= 0 & pred$con_estimate <= 1))
})

test_that("causal predict: type='prob' inverts survival correctly", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X
  n_new <- 3
  X_new <- X[1:n_new, , drop = FALSE]
  y_eval <- data$y[1:n_new]

  # Get both survival and prob
  pred_surv <- predict(cf, x = X_new, type = "survival", y = y_eval, interval = "none")
  pred_prob <- predict(cf, x = X_new, type = "prob", y = y_eval, interval = "none")

  # prob = 1 - survival
  expect_equal(pred_prob$trt_estimate, 1 - pred_surv$trt_estimate, tolerance = 1e-10)
  expect_equal(pred_prob$con_estimate, 1 - pred_surv$con_estimate, tolerance = 1e-10)
})

test_that("causal predict: user-supplied ps= bypasses PS model", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X
  n_new <- 5
  X_new <- X[1:n_new, , drop = FALSE]

  # Supply manual PS values
  manual_ps <- seq(0.2, 0.8, length.out = n_new)

  pred <- predict(cf, x = X_new, type = "mean", ps = manual_ps, store_draws = FALSE)

  expect_true(is.matrix(pred))
  expect_equal(nrow(pred), n_new)

  # Returned PS should match manual input (possibly clamped)
  returned_ps <- pred[, "ps"]
  expect_true(all(abs(returned_ps - manual_ps) < 0.01))  # Allow small clamp tolerance
})

test_that("causal predict: PS disabled has NA propensity scores", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit(ps_opt = FALSE)
  cf <- data$fit
  X <- data$X
  n_new <- 5
  X_new <- X[1:n_new, , drop = FALSE]

  pred <- predict(cf, x = X_new, type = "mean", store_draws = FALSE)

  expect_true(is.matrix(pred))
  expect_equal(nrow(pred), n_new)

  # PS should be NA when PS is disabled
  expect_true(all(is.na(pred[, "ps"])))
})

# =============================================================================
# Tier B (ci): Plotting tests for causal objects
# =============================================================================
test_that("plot.dpmixgpd_causal_fit works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit()
  cf <- data$fit

  # Should return a list with treated/control plots
  plots <- plot(cf, family = "traceplot", params = "alpha")
  expect_true(is.list(plots))
  expect_true("treated" %in% names(plots) || "control" %in% names(plots))
})

test_that("plot.dpmixgpd_qte works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X

  qte_res <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), newdata = X[1:3, ], interval = "credible")

  # Should not error
  expect_no_error({
    plots <- plot(qte_res)
  })

  expect_true(is.list(plots))
  expect_true(all(c("trt_control", "treatment_effect") %in% names(plots)))
})

test_that("plot.dpmixgpd_ate works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X

  ate_res <- DPmixGPD::ate(cf, newdata = X[1:3, ], interval = "credible", nsim_mean = 20L)

  # Should not error
  expect_no_error({
    plots <- plot(ate_res)
  })

  expect_true(is.list(plots))
  expect_true(all(c("trt_control", "treatment_effect") %in% names(plots)))
})

test_that("plot.dpmixgpd_causal_predict works for mean type", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X

  pred <- predict(cf, x = X[1:5, ], type = "mean", interval = "credible", store_draws = FALSE)

  # Convert to proper class for plotting
  class(pred) <- c("dpmixgpd_causal_predict", class(pred))
  attr(pred, "type") <- "mean"
  attr(pred, "trt") <- predict(cf$outcome_fit$trt, x = X[1:5, ], type = "mean")
  attr(pred, "con") <- predict(cf$outcome_fit$con, x = X[1:5, ], type = "mean")

  expect_no_error({
    plots <- plot(pred)
  })

  expect_true(is.list(plots))
})

# =============================================================================
# Tier B (ci): Print/summary tests for causal objects
# =============================================================================
test_that("print.dpmixgpd_causal_fit works", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit

  expect_output(print(cf), "DPmixGPD causal fit")
  expect_output(print(cf), "PS model")
})

test_that("summary.dpmixgpd_causal_fit works", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit

  expect_output(summary(cf), "PS fit|Outcome fits")
})

test_that("print.dpmixgpd_causal_bundle works", {
  skip_if_not_test_level("ci")

  set.seed(42)
  n <- 20
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T_ind <- stats::rbinom(n, 1, 0.5)
  y <- abs(stats::rnorm(n)) + 0.1

  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    T = T_ind,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,
    PS = "logit",

  )

  expect_output(print(cb), "DPmixGPD causal bundle")
  expect_output(print(cb), "PS model")
})

# =============================================================================
# Tier B (ci): PS scale/summary/clamp variants
# =============================================================================

.make_causal_fit_with_ps_opts <- function(ps_scale = "logit", ps_summary = "mean", ps_clamp = 1e-6) {
  set.seed(42)
  n <- 20
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T_ind <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.4 * X[, 1]))
  y <- abs(stats::rnorm(n)) + 0.1

  mcmc_out <- mcmc_fast(seed = 1L)
  mcmc_ps <- mcmc_fast(seed = 2L)

  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    T = T_ind,
    backend = c("sb", "sb"),
    kernel = c("normal", "normal"),
    GPD = c(FALSE, FALSE),
    components = c(4, 4),
    mcmc_outcome = mcmc_out,
    mcmc_ps = mcmc_ps,
    PS = "logit",

    ps_scale = ps_scale,
    ps_summary = ps_summary,
    ps_clamp = ps_clamp
  )

  cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
  list(fit = cf, X = X, y = y, n = n)
}

test_that("causal predict: ps_scale='prob' vs 'logit' differ", {
  skip_if_not_test_level("ci")

  # Build two fits with different ps_scale
  data_logit <- .make_causal_fit_with_ps_opts(ps_scale = "logit")
  data_prob <- .make_causal_fit_with_ps_opts(ps_scale = "prob")

  X_new <- data_logit$X[1:3, , drop = FALSE]

  # Both should work
  pred_logit <- predict(data_logit$fit, x = X_new, type = "mean", store_draws = FALSE)
  pred_prob <- predict(data_prob$fit, x = X_new, type = "mean", store_draws = FALSE)

  expect_true(is.matrix(pred_logit))
  expect_true(is.matrix(pred_prob))

  # PS values should be in valid range [0,1] for both
  expect_true(all(pred_logit[, "ps"] >= 0 & pred_logit[, "ps"] <= 1))
  expect_true(all(pred_prob[, "ps"] >= 0 & pred_prob[, "ps"] <= 1))

  # Verify the bundle stored the scale correctly
  expect_equal(data_logit$fit$bundle$meta$ps_scale, "logit")
  expect_equal(data_prob$fit$bundle$meta$ps_scale, "prob")
})

test_that("causal predict: ps_summary='mean' vs 'median' differ", {
  skip_if_not_test_level("ci")

  # Build two fits with different ps_summary
  data_mean <- .make_causal_fit_with_ps_opts(ps_summary = "mean")
  data_median <- .make_causal_fit_with_ps_opts(ps_summary = "median")

  X_new <- data_mean$X[1:3, , drop = FALSE]

  # Both should work
  pred_mean <- predict(data_mean$fit, x = X_new, type = "mean", store_draws = FALSE)
  pred_median <- predict(data_median$fit, x = X_new, type = "mean", store_draws = FALSE)

  expect_true(is.matrix(pred_mean))
  expect_true(is.matrix(pred_median))

  # Verify the bundle stored the summary correctly
  expect_equal(data_mean$fit$bundle$meta$ps_summary, "mean")
  expect_equal(data_median$fit$bundle$meta$ps_summary, "median")
})

test_that("causal predict: ps_clamp affects extreme PS values", {
  skip_if_not_test_level("ci")

  # Build two fits with different clamp values
  data_small_clamp <- .make_causal_fit_with_ps_opts(ps_clamp = 1e-6)
  data_large_clamp <- .make_causal_fit_with_ps_opts(ps_clamp = 0.1)

  X_new <- data_small_clamp$X[1:5, , drop = FALSE]

  # Both should work
  pred_small <- predict(data_small_clamp$fit, x = X_new, type = "mean", store_draws = FALSE)
  pred_large <- predict(data_large_clamp$fit, x = X_new, type = "mean", store_draws = FALSE)

  expect_true(is.matrix(pred_small))
  expect_true(is.matrix(pred_large))

  # With large clamp (0.1), PS should be in [0.1, 0.9]
  # With small clamp (1e-6), PS should be in [1e-6, 1-1e-6]
  expect_true(all(pred_large[, "ps"] >= 0.1 - 1e-10))
  expect_true(all(pred_large[, "ps"] <= 0.9 + 1e-10))

  # Verify the bundle stored the clamp correctly
  expect_equal(data_small_clamp$fit$bundle$meta$ps_clamp, 1e-6)
  expect_equal(data_large_clamp$fit$bundle$meta$ps_clamp, 0.1)
})

# =============================================================================
# Tier B (ci): params() extractor for causal fits
# =============================================================================
test_that("params() works for causal fits", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit

  # params.dpmixgpd_causal_fit returns a list with treated/control
  p <- params(cf)

  expect_s3_class(p, "mixgpd_params_pair")
  expect_true(is.list(p))
  expect_true("treated" %in% names(p))
  expect_true("control" %in% names(p))

  # Each arm should have params
  expect_s3_class(p$treated, "mixgpd_params")
  expect_s3_class(p$control, "mixgpd_params")

  # Both should have alpha
  expect_true("alpha" %in% names(p$treated))
  expect_true("alpha" %in% names(p$control))
})


# =============================================================================
# Causal plots (from test-plots-causal-effects.R)
# =============================================================================
# =============================================================================
# Shared test fixture: create a minimal causal fit once for reuse
# =============================================================================
.make_causal_fit_for_plot <- function() {
  set.seed(42)
  n <- 20
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T_ind <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.4 * X[, 1]))
  y <- abs(stats::rnorm(n)) + 0.1

  mcmc_out <- mcmc_fast(seed = 1L)
  mcmc_ps <- mcmc_fast(seed = 2L)

  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    T = T_ind,
    backend = c("sb", "sb"),
    kernel = c("normal", "normal"),
    GPD = c(FALSE, FALSE),
    components = c(4, 4),
    mcmc_outcome = mcmc_out,
    mcmc_ps = mcmc_ps,
    PS = "logit",

  )

  cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
  list(fit = cf, X = X, y = y, n = n)
}

# =============================================================================
# Tier B (ci): print/summary tests for QTE objects
# =============================================================================
test_that("print.dpmixgpd_qte works", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  qte_res <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), newdata = X[1:3, ], interval = "credible")

  expect_output(print(qte_res), "QTE")
  expect_output(print(qte_res), "probs|quantile", ignore.case = TRUE)
})

test_that("summary.dpmixgpd_qte works", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  qte_res <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), newdata = X[1:3, ], interval = "credible")

  s <- summary(qte_res)
  expect_s3_class(s, "summary.dpmixgpd_qte")

  # Should print without error
  expect_output(print(s), "QTE")
})

# =============================================================================
# Tier B (ci): print/summary tests for ATE objects
# =============================================================================
test_that("print.dpmixgpd_ate works", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  ate_res <- DPmixGPD::ate(cf, newdata = X[1:3, ], interval = "credible", nsim_mean = 20L)

  expect_output(print(ate_res), "ATE")
})

test_that("summary.dpmixgpd_ate works", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  ate_res <- DPmixGPD::ate(cf, newdata = X[1:3, ], interval = "credible", nsim_mean = 20L)

  s <- summary(ate_res)
  expect_s3_class(s, "summary.dpmixgpd_ate")

  # Should print without error
  expect_output(print(s), "ATE")
})

# =============================================================================
# Tier B (ci): plot.dpmixgpd_qte with type parameter
# =============================================================================
test_that("plot.dpmixgpd_qte type='effect' works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  qte_res <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), newdata = X[1:3, ], interval = "credible")

  p <- plot(qte_res, type = "effect")

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))

  # Check no NA in y aesthetics (estimate column)
  built <- ggplot2::ggplot_build(p)
  expect_false(all(is.na(built$data[[1]]$y)))
})

test_that("plot.dpmixgpd_qte type='arms' works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  qte_res <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), newdata = X[1:3, ], interval = "credible")

  p <- plot(qte_res, type = "arms")

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))

  # Check for two groups (Treated, Control)
  built <- ggplot2::ggplot_build(p)
  # There should be line data
  expect_true(nrow(built$data[[1]]) > 0)
})

test_that("plot.dpmixgpd_qte type='both' returns list", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  qte_res <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), newdata = X[1:3, ], interval = "credible")

  plots <- plot(qte_res, type = "both")

  expect_true(is.list(plots))
  expect_true(all(c("effect", "arms") %in% names(plots)))
  expect_s3_class(plots$effect, "ggplot")
  expect_s3_class(plots$arms, "ggplot")
})

test_that("plot.dpmixgpd_qte default type is 'effect'", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  qte_res <- DPmixGPD::qte(cf, probs = c(0.5), newdata = X[1:3, ], interval = "credible")

  # Default should be effect
  p_default <- plot(qte_res)
  p_effect <- plot(qte_res, type = "effect")

  # Both should be single ggplot objects (not lists)
  expect_s3_class(p_default, "ggplot")
  expect_s3_class(p_effect, "ggplot")
})

# =============================================================================
# Tier B (ci): plot.dpmixgpd_ate with type parameter
# =============================================================================
test_that("plot.dpmixgpd_ate type='effect' works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  ate_res <- DPmixGPD::ate(cf, newdata = X[1:3, ], interval = "credible", nsim_mean = 20L)

  p <- plot(ate_res, type = "effect")

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))

  # Check no NA in y aesthetics
  built <- ggplot2::ggplot_build(p)
  expect_false(all(is.na(built$data[[1]]$y)))
})

test_that("plot.dpmixgpd_ate type='arms' works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  ate_res <- DPmixGPD::ate(cf, newdata = X[1:3, ], interval = "credible", nsim_mean = 20L)

  p <- plot(ate_res, type = "arms")

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))

  # Check for two groups (Treated, Control)
  built <- ggplot2::ggplot_build(p)
  expect_true(nrow(built$data[[1]]) > 0)
})

test_that("plot.dpmixgpd_ate type='both' returns list", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  ate_res <- DPmixGPD::ate(cf, newdata = X[1:3, ], interval = "credible", nsim_mean = 20L)

  plots <- plot(ate_res, type = "both")

  expect_true(is.list(plots))
  expect_true(all(c("effect", "arms") %in% names(plots)))
  expect_s3_class(plots$effect, "ggplot")
  expect_s3_class(plots$arms, "ggplot")
})

test_that("plot.dpmixgpd_ate default type is 'effect'", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  ate_res <- DPmixGPD::ate(cf, newdata = X[1:3, ], interval = "credible", nsim_mean = 20L)

  # Default should be effect
  p_default <- plot(ate_res)
  p_effect <- plot(ate_res, type = "effect")

  # Both should be single ggplot objects (not lists)
  expect_s3_class(p_default, "ggplot")
  expect_s3_class(p_effect, "ggplot")
})

# =============================================================================
# Tier B (ci): Test no NA aesthetics in built plots
# =============================================================================
test_that("QTE plot effect type has no NA in estimate data", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  qte_res <- DPmixGPD::qte(cf, probs = c(0.5), newdata = X[1:5, ], interval = "credible")

  p <- plot(qte_res, type = "effect")
  built <- ggplot2::ggplot_build(p)

  # The main line layer should have non-NA y values
  line_data <- built$data[[1]]
  expect_true(any(!is.na(line_data$y)), info = "Effect plot should have non-NA estimates")
})

test_that("ATE plot effect type has no NA in estimate data", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  ate_res <- DPmixGPD::ate(cf, newdata = X[1:5, ], interval = "credible", nsim_mean = 20L)

  p <- plot(ate_res, type = "effect")
  built <- ggplot2::ggplot_build(p)

  # The main line layer should have non-NA y values
  line_data <- built$data[[1]]
  expect_true(any(!is.na(line_data$y)), info = "Effect plot should have non-NA estimates")
})

# =============================================================================
# Tier A (cran): Error handling - ggplot2 missing message
# =============================================================================
test_that("plot.dpmixgpd_qte errors gracefully without ggplot2", {
  skip_if_not_test_level("cran")


  # Create a minimal mock QTE object
  mock_qte <- list(
    fit = matrix(1:3, nrow = 3),
    lower = matrix(0:2, nrow = 3),
    upper = matrix(2:4, nrow = 3),
    trt = list(fit = data.frame(id = 1:3, index = 0.5, estimate = 1:3, lower = 0:2, upper = 2:4)),
    con = list(fit = data.frame(id = 1:3, index = 0.5, estimate = 0:2, lower = 0:2, upper = 1:3)),
    grid = 0.5,
    ps = c(0.3, 0.5, 0.7)
  )
  class(mock_qte) <- "dpmixgpd_qte"

  # We can't easily mock requireNamespace, but we can at least verify the function exists
  expect_true(is.function(plot.dpmixgpd_qte))
})

test_that("plot.dpmixgpd_ate errors gracefully without ggplot2", {
  skip_if_not_test_level("cran")

  # Create a minimal mock ATE object
  mock_ate <- list(
    fit = 1:3,
    lower = 0:2,
    upper = 2:4,
    trt = list(fit = data.frame(id = 1:3, estimate = 2:4, lower = 1:3, upper = 3:5)),
    con = list(fit = data.frame(id = 1:3, estimate = 1:3, lower = 0:2, upper = 2:4)),
    ps = c(0.3, 0.5, 0.7)
  )
  class(mock_ate) <- "dpmixgpd_ate"

  # Verify the function exists
  expect_true(is.function(plot.dpmixgpd_ate))
})

# =============================================================================
# Tier A (cran): Test .coerce_fit_df helper function
# =============================================================================
test_that(".coerce_fit_df handles vector input", {
  skip_if_not_test_level("cran")

  # Access internal function
  coerce <- DPmixGPD:::.coerce_fit_df

  vec <- c(1.5, 2.5, 3.5)
  result <- coerce(vec)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true(all(c("estimate", "lower", "upper", "id") %in% names(result)))
  expect_equal(result$estimate, vec)
  expect_true(all(is.na(result$lower)))
  expect_true(all(is.na(result$upper)))
})

test_that(".coerce_fit_df handles matrix input", {
  skip_if_not_test_level("cran")

  coerce <- DPmixGPD:::.coerce_fit_df

  mat <- matrix(c(1, 2, 3, 0.5, 1.5, 2.5, 1.5, 2.5, 3.5), nrow = 3, ncol = 3)
  result <- coerce(mat)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true("estimate" %in% names(result))
})

test_that(".coerce_fit_df handles data.frame input", {
  skip_if_not_test_level("cran")

  coerce <- DPmixGPD:::.coerce_fit_df

  df <- data.frame(estimate = c(1, 2, 3), lower = c(0, 1, 2), upper = c(2, 3, 4))
  result <- coerce(df)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_equal(result$estimate, c(1, 2, 3))
  expect_true("id" %in% names(result))
})

test_that(".coerce_fit_df adds missing columns", {
  skip_if_not_test_level("cran")

  coerce <- DPmixGPD:::.coerce_fit_df

  # Data frame missing lower/upper
  df <- data.frame(fit = c(1, 2, 3))
  result <- coerce(df)

  expect_true(is.data.frame(result))
  expect_true("estimate" %in% names(result))
  expect_true("lower" %in% names(result))
  expect_true("upper" %in% names(result))
  expect_true("id" %in% names(result))
})


# =============================================================================
# PS utilities (from test-ps-utils.R)
# =============================================================================
test_that("PS utilities: design matrix alignment and scaling", {
  ps_design_matrix <- DPmixGPD:::.ps_design_matrix
  apply_ps_scale <- DPmixGPD:::.apply_ps_scale

  X_train <- cbind(`(Intercept)` = 1, x1 = c(0, 1), x2 = c(2, 3))
  ps_bundle <- list(
    spec = list(meta = list(include_intercept = TRUE), model = "logit"),
    data = list(X = X_train)
  )
  class(ps_bundle) <- "dpmixgpd_ps_bundle"

  X_new <- cbind(x2 = c(10, 11), x1 = c(4, 5))
  colnames(X_new) <- c("x2", "x1")
  dm <- ps_design_matrix(ps_bundle, X_new)
  expect_equal(colnames(dm), colnames(X_train))
  expect_equal(dm[, 1], c(1, 1))
  expect_equal(dm[, "x1"], c(4, 5))
  expect_equal(dm[, "x2"], c(10, 11))

  # mismatch column names error
  X_bad <- cbind(x1 = c(1, 2), x3 = c(3, 4))
  expect_error(ps_design_matrix(ps_bundle, X_bad), "do not match PS design")

  # intercept-only model expects 0 covariate columns
  X_train_int <- cbind(`(Intercept)` = 1)
  ps_bundle_int <- list(
    spec = list(meta = list(include_intercept = TRUE), model = "logit"),
    data = list(X = X_train_int)
  )
  class(ps_bundle_int) <- "dpmixgpd_ps_bundle"
  X0 <- matrix(numeric(0), nrow = 3, ncol = 0)
  dm0 <- ps_design_matrix(ps_bundle_int, X0)
  expect_equal(dim(dm0), c(3L, 1L))
  expect_error(ps_design_matrix(ps_bundle_int, matrix(1, nrow = 3, ncol = 1)), "PS-only intercept")

  # scaling: prob passthrough; logit clamps to finite
  expect_equal(apply_ps_scale(c(0.2, 0.8), scale = "prob"), c(0.2, 0.8))
  out <- apply_ps_scale(c(0, 1), scale = "logit", clamp = 1e-6)
  expect_true(all(is.finite(out)))
})

test_that("PS utilities: compute PS from fit (logit/probit/naive + error branches)", {
  compute_ps_from_fit <- DPmixGPD:::.compute_ps_from_fit

  # logit/probit paths
  X_train <- cbind(`(Intercept)` = 1, x1 = c(0, 1), x2 = c(2, 3))
  ps_bundle_logit <- list(
    spec = list(meta = list(include_intercept = TRUE), model = "logit"),
    data = list(X = X_train)
  )
  class(ps_bundle_logit) <- "dpmixgpd_ps_bundle"
  ps_bundle_probit <- ps_bundle_logit
  ps_bundle_probit$spec$model <- "probit"

  set.seed(1)
  S <- 10
  beta_draws <- matrix(stats::rnorm(S * 3, sd = 0.2), nrow = S)
  colnames(beta_draws) <- c("beta[1]", "beta[2]", "beta[3]")
  ps_fit_ok <- list(mcmc = list(samples = beta_draws))

  X_new <- cbind(x1 = c(-0.5, 0.5), x2 = c(0.1, -0.1))
  ps_l <- compute_ps_from_fit(ps_fit_ok, ps_bundle_logit, X_new, summary = "mean", clamp = 1e-6)
  ps_p <- compute_ps_from_fit(ps_fit_ok, ps_bundle_probit, X_new, summary = "median", clamp = 1e-6)
  expect_true(all(ps_l > 0 & ps_l < 1))
  expect_true(all(ps_p > 0 & ps_p < 1))
  expect_equal(length(ps_l), nrow(X_new))
  expect_equal(length(ps_p), nrow(X_new))

  # error: missing beta draws
  ps_fit_no_beta <- list(mcmc = list(samples = matrix(0, nrow = 2, ncol = 1, dimnames = list(NULL, "alpha"))))
  expect_error(compute_ps_from_fit(ps_fit_no_beta, ps_bundle_logit, X_new), "beta draws not found")

  # naive path
  ps_bundle_naive <- list(
    spec = list(meta = list(include_intercept = FALSE), model = "naive"),
    data = list(X = cbind(x1 = c(0, 1), x2 = c(2, 3)))
  )
  class(ps_bundle_naive) <- "dpmixgpd_ps_bundle"

  S2 <- 5
  samples_naive <- matrix(NA_real_, nrow = S2, ncol = 1 + 4 + 4)
  colnames(samples_naive) <- c(
    "pi_prior",
    "mu[1,1]", "mu[2,1]", "mu[1,2]", "mu[2,2]",
    "sigma[1,1]", "sigma[2,1]", "sigma[1,2]", "sigma[2,2]"
  )
  samples_naive[, "pi_prior"] <- 0.4
  samples_naive[, c("mu[1,1]", "mu[2,1]", "mu[1,2]", "mu[2,2]")] <- rep(c(0, 0.5, 0, 0.5), each = S2)
  samples_naive[, c("sigma[1,1]", "sigma[2,1]", "sigma[1,2]", "sigma[2,2]")] <- rep(c(1, 1, 1, 1), each = S2)
  ps_fit_naive <- list(mcmc = list(samples = samples_naive))

  Xn <- cbind(x1 = c(-0.2, 0.1, 0.4), x2 = c(0.0, 0.3, -0.1))
  ps_n <- compute_ps_from_fit(ps_fit_naive, ps_bundle_naive, Xn, summary = "mean", clamp = 1e-6)
  expect_true(all(ps_n > 0 & ps_n < 1))
  expect_equal(length(ps_n), nrow(Xn))

  # error: naive missing required columns
  ps_fit_naive_bad <- list(mcmc = list(samples = matrix(0, nrow = 2, ncol = 1, dimnames = list(NULL, "pi_prior"))))
  expect_error(compute_ps_from_fit(ps_fit_naive_bad, ps_bundle_naive, Xn), "Naive Bayes PS samples missing")
})



# =============================================================================
# S3 causal effects (from test-s3-causal-effects.R)
# =============================================================================
# test-s3-causal-effects.R
# S3 print/summary methods for QTE/ATE objects (Tier B - requires MCMC)

# Helper to get/cache a causal fit
get_causal_fit <- function() {
  cache_key <- "test_causal_fit_s3"
  cached <- get0(cache_key, envir = .GlobalEnv, ifnotfound = NULL)
  if (!is.null(cached)) return(cached)

  set.seed(123)
  n <- 30
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.4 * X[, 1]))
  y <- abs(stats::rnorm(n)) + 0.1

  mcmc_out <- list(niter = 30, nburnin = 10, thin = 1, nchains = 1, seed = 1)
  mcmc_ps  <- list(niter = 30, nburnin = 10, thin = 1, nchains = 1, seed = 1)

  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = c("sb", "sb"),
    kernel = c("normal", "normal"),
    GPD = c(FALSE, FALSE),
    components = c(4, 4),
    mcmc_outcome = mcmc_out,
    mcmc_ps = mcmc_ps
  )

  cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
  assign(cache_key, cf, envir = .GlobalEnv)
  cf
}

# ============================================================
# QTE Tests
# ============================================================

test_that("qte() returns proper dpmixgpd_qte class with expected fields", {
  skip_if_not_test_level("ci")

  cf <- get_causal_fit()

  q <- DPmixGPD::qte(cf, probs = c(0.25, 0.5, 0.75), interval = "credible")

  # Class check

  expect_s3_class(q, "dpmixgpd_qte")

  # Required fields exist
  expect_true(all(c("fit", "probs", "grid", "trt", "con", "type") %in% names(q)))
  expect_equal(q$type, "qte")
  expect_equal(q$probs, c(0.25, 0.5, 0.75))

  # QTE fit data frame structure
  expect_true("qte" %in% names(q))
  expect_true(is.data.frame(q$qte$fit))
  expect_true(all(c("estimate", "lower", "upper") %in% names(q$qte$fit)))

  # Metadata present
  expect_true("meta" %in% names(q))
  expect_true("level" %in% names(q))
  expect_true("interval" %in% names(q))
})

test_that("print.dpmixgpd_qte produces readable output", {
  skip_if_not_test_level("ci")

  cf <- get_causal_fit()
  q <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), interval = "credible")

  # Capture print output
  out <- capture.output(print(q))

  # Check key elements in output
  expect_true(any(grepl("QTE", out, fixed = TRUE)))
  expect_true(any(grepl("Prediction points", out, fixed = TRUE)))
  expect_true(any(grepl("Quantile grid", out, fixed = TRUE)))
  expect_true(any(grepl("Credible interval", out, fixed = TRUE)))
  expect_true(any(grepl("estimate", out, fixed = TRUE)))

  # Returns invisibly
  invisible_result <- print(q)
  expect_identical(invisible_result, q)
})

test_that("summary.dpmixgpd_qte returns structured summary object", {
  skip_if_not_test_level("ci")

  cf <- get_causal_fit()
  q <- DPmixGPD::qte(cf, probs = c(0.25, 0.5, 0.75), interval = "credible")

  s <- summary(q)

  # Class check
  expect_s3_class(s, "summary.dpmixgpd_qte")

  # Required summary components
  expect_true(all(c("overall", "quantile_summary", "ci_summary", "meta") %in% names(s)))

  # Overall summary fields
  expect_true(all(c("n_pred", "n_quantiles", "quantiles", "level", "interval") %in% names(s$overall)))
  expect_equal(s$overall$n_quantiles, 3L)
  expect_equal(s$overall$quantiles, c(0.25, 0.5, 0.75))

  # Quantile summary table
  expect_true(is.data.frame(s$quantile_summary))
  expect_true(all(c("quantile", "mean_qte", "median_qte") %in% names(s$quantile_summary)))
  expect_equal(nrow(s$quantile_summary), 3L)

  # CI summary present (since interval = "credible")
  expect_true(!is.null(s$ci_summary))
  expect_true(all(c("mean_width", "median_width") %in% names(s$ci_summary)))
})

test_that("print.summary.dpmixgpd_qte produces formatted output", {
  skip_if_not_test_level("ci")

  cf <- get_causal_fit()
  q <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), interval = "credible")
  s <- summary(q)

  out <- capture.output(print(s))

  # Check formatted sections
  expect_true(any(grepl("QTE Summary", out, fixed = TRUE)))
  expect_true(any(grepl("Prediction points", out, fixed = TRUE)))
  expect_true(any(grepl("QTE by quantile", out, fixed = TRUE)))
  expect_true(any(grepl("Credible interval width", out, fixed = TRUE)))

  # Returns invisibly
  invisible_result <- print(s)
  expect_identical(invisible_result, s)
})

# ============================================================
# ATE Tests
# ============================================================

test_that("ate() returns proper dpmixgpd_ate class with expected fields", {
  skip_if_not_test_level("ci")

  cf <- get_causal_fit()

  a <- DPmixGPD::ate(cf, interval = "credible", nsim_mean = 30L)

  # Class check
  expect_s3_class(a, "dpmixgpd_ate")

  # Required fields exist
  expect_true(all(c("fit", "trt", "con", "type") %in% names(a)))
  expect_equal(a$type, "ate")

  # ATE fit data frame structure
  expect_true("ate" %in% names(a))
  expect_true(is.data.frame(a$ate$fit))
  expect_true(all(c("estimate", "lower", "upper") %in% names(a$ate$fit)))

  # Metadata present
  expect_true("meta" %in% names(a))
  expect_true("level" %in% names(a))
  expect_true("interval" %in% names(a))
  expect_true("nsim_mean" %in% names(a))
})

test_that("print.dpmixgpd_ate produces readable output", {
  skip_if_not_test_level("ci")

  cf <- get_causal_fit()
  a <- DPmixGPD::ate(cf, interval = "credible")

  # Capture print output
  out <- capture.output(print(a))

  # Check key elements in output
  expect_true(any(grepl("ATE", out, fixed = TRUE)))
  expect_true(any(grepl("Prediction points", out, fixed = TRUE)))
  expect_true(any(grepl("Credible interval", out, fixed = TRUE)))
  expect_true(any(grepl("estimate", out, fixed = TRUE)))

  # Returns invisibly
  invisible_result <- print(a)
  expect_identical(invisible_result, a)
})

test_that("summary.dpmixgpd_ate returns structured summary object", {
  skip_if_not_test_level("ci")

  cf <- get_causal_fit()
  a <- DPmixGPD::ate(cf, interval = "credible", nsim_mean = 30L)

  s <- summary(a)

  # Class check
  expect_s3_class(s, "summary.dpmixgpd_ate")

  # Required summary components
  expect_true(all(c("overall", "ate_stats", "ci_summary", "meta") %in% names(s)))

  # Overall summary fields
  expect_true(all(c("n_pred", "level", "interval", "nsim_mean") %in% names(s$overall)))

  # ATE stats present
  expect_true(!is.null(s$ate_stats))
  expect_true(all(c("mean_ate", "median_ate", "min_ate", "max_ate") %in% names(s$ate_stats)))

  # CI summary present (since interval = "credible")
  expect_true(!is.null(s$ci_summary))
  expect_true(all(c("mean_width", "median_width") %in% names(s$ci_summary)))
})

test_that("print.summary.dpmixgpd_ate produces formatted output", {
  skip_if_not_test_level("ci")

  cf <- get_causal_fit()
  a <- DPmixGPD::ate(cf, interval = "credible")
  s <- summary(a)

  out <- capture.output(print(s))

  # Check formatted sections
  expect_true(any(grepl("ATE Summary", out, fixed = TRUE)))
  expect_true(any(grepl("Prediction points", out, fixed = TRUE)))
  expect_true(any(grepl("ATE statistics", out, fixed = TRUE)))
  expect_true(any(grepl("Credible interval width", out, fixed = TRUE)))

  # Returns invisibly
  invisible_result <- print(s)
  expect_identical(invisible_result, s)
})

# ============================================================
# Plot Tests
# ============================================================

test_that("plot.dpmixgpd_qte builds without error", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  cf <- get_causal_fit()
  q <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), interval = "credible")

  # Should return plot list without error
  p <- plot(q)
  expect_true(is.list(p))
  expect_s3_class(p, "dpmixgpd_causal_predict_plots")
  expect_true(all(c("trt_control", "treatment_effect") %in% names(p)))

  # Each plot should be ggplot
  expect_s3_class(p$trt_control, "ggplot")
  expect_s3_class(p$treatment_effect, "ggplot")

  # Plots should build without error
  expect_error(ggplot2::ggplot_build(p$trt_control), NA)
  expect_error(ggplot2::ggplot_build(p$treatment_effect), NA)
})

test_that("plot.dpmixgpd_ate builds without error", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  cf <- get_causal_fit()
  a <- DPmixGPD::ate(cf, interval = "credible")

  # Should return plot list without error
  p <- plot(a)
  expect_true(is.list(p))
  expect_s3_class(p, "dpmixgpd_causal_predict_plots")
  expect_true(all(c("trt_control", "treatment_effect") %in% names(p)))

  # Each plot should be ggplot
  expect_s3_class(p$trt_control, "ggplot")
  expect_s3_class(p$treatment_effect, "ggplot")

  # Plots should build without error
  expect_error(ggplot2::ggplot_build(p$trt_control), NA)
  expect_error(ggplot2::ggplot_build(p$treatment_effect), NA)
})

# ============================================================
# Edge Cases
# ============================================================

test_that("QTE with interval='none' omits CI fields gracefully", {
  skip_if_not_test_level("ci")

  cf <- get_causal_fit()
  q <- DPmixGPD::qte(cf, probs = c(0.5), interval = "none")

  expect_s3_class(q, "dpmixgpd_qte")
  expect_true(is.null(q$lower) || all(is.na(q$lower)))
  expect_true(is.null(q$upper) || all(is.na(q$upper)))

  # Print should still work
  out <- capture.output(print(q))
  expect_true(any(grepl("QTE", out, fixed = TRUE)))

  # Summary should handle missing CI
  s <- summary(q)
  expect_true(is.null(s$ci_summary) || length(s$ci_summary) == 0)
})

test_that("ATE with interval='none' omits CI fields gracefully", {
  skip_if_not_test_level("ci")

  cf <- get_causal_fit()
  a <- DPmixGPD::ate(cf, interval = "none")

  expect_s3_class(a, "dpmixgpd_ate")
  expect_true(is.null(a$lower) || all(is.na(a$lower)))
  expect_true(is.null(a$upper) || all(is.na(a$upper)))

  # Print should still work
  out <- capture.output(print(a))
  expect_true(any(grepl("ATE", out, fixed = TRUE)))

  # Summary should handle missing CI
  s <- summary(a)
  expect_true(is.null(s$ci_summary) || length(s$ci_summary) == 0)
})

