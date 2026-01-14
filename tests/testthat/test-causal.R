if (!exists(".cache_enabled")) {
  helper_path <- file.path("tests", "testthat", "helper-cache.R")
  if (file.exists(helper_path)) source(helper_path)
}

test_that("causal bundle and fit combos", {
  skip_if_not(requireNamespace("nimble", quietly = TRUE))

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

  mcmc_out <- list(niter = 15, nburnin = 5, thin = 1, nchains = 1, seed = 1)
  mcmc_ps <- list(niter = 15, nburnin = 5, thin = 1, nchains = 1, seed = 1)

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
    expect_true(is.numeric(fd_con) && is.numeric(fd_trt), info = info)
    expect_true(is.numeric(rs_con) && is.numeric(rs_trt), info = info)
  }
})

test_that("PS parameter: logit, probit, naive, FALSE all work", {
  skip_if_not(requireNamespace("nimble", quietly = TRUE))

  set.seed(99)
  n <- 40
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T <- stats::rbinom(n, 1, stats::plogis(0.3 + 0.4 * X[, 1]))
  y <- abs(stats::rnorm(n)) + 0.2

  mcmc_out <- list(niter = 20, nburnin = 10, thin = 1, nchains = 1, seed = 1)
  mcmc_ps <- list(niter = 20, nburnin = 10, thin = 1, nchains = 1, seed = 1)

  # Test all four PS options
  ps_options <- c("logit", "probit", "naive", FALSE)

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
      PS = ps_opt
    )

    # Check metadata
    expect_true(inherits(cb, "dpmixgpd_causal_bundle"), info = label)
    expect_true(!is.null(cb$meta$ps), info = label)
    if (isFALSE(ps_opt)) {
      expect_false(cb$meta$ps$enabled, info = label)
      expect_false(cb$meta$ps$model_type, info = label)
      expect_true(is.null(cb$design), info = label)
    } else {
      expect_true(cb$meta$ps$enabled, info = label)
      expect_equal(cb$meta$ps$model_type, ps_opt, info = label)
      expect_true(!is.null(cb$design), info = label)
    }

    # Run MCMC
    cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
    expect_true(inherits(cf, "dpmixgpd_causal_fit"), info = label)

    if (isFALSE(ps_opt)) {
      # No PS model
      expect_true(is.null(cf$ps_fit), info = label)
      expect_true(is.null(cf$ps_hat), info = label)
      expect_false(!is.null(cf$outcome_fit$con$ps_model), info = label)
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
    expect_true(is.list(pred), info = label)

    if (isFALSE(ps_opt)) {
      expect_true(is.null(pred$ps), info = label)
      expect_true(is.null(pred$ps_trt), info = label)
    } else {
      expect_true(!is.null(pred$ps), info = label)
      expect_true(length(pred$ps) == nrow(X_new), info = label)
      expect_true(all(pred$ps >= 0 & pred$ps <= 1), info = label)
    }
  }
})
