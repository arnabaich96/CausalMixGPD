`%||%` <- function(a, b) if (!is.null(a)) a else b

if (!exists(".cache_enabled")) {
  helper_path <- file.path("tests", "testthat", "helper-cache.R")
  if (file.exists(helper_path)) source(helper_path)
}

.supports_gpd <- function(kernel, backend) {
  kinfo <- get_kernel_registry()[[kernel]]
  sig <- kinfo$signatures[[backend]]$gpd %||% NULL
  !is.null(sig) && !isTRUE(is.na(sig$dist_name))
}

.run_predict_case <- function(label, kernel, backend, gpd, has_X) {
  ok <- TRUE
  msg <- label
  warn_msgs <- character()

  withCallingHandlers({
    set.seed(1)
    N <- 20
    y <- abs(stats::rnorm(N)) + 0.1
    X <- if (has_X) {
      data.frame(x1 = stats::rnorm(N), x2 = stats::runif(N))
    } else {
      NULL
    }

    if (isTRUE(gpd) && !.supports_gpd(kernel, backend)) {
      err <- tryCatch({
        build_nimble_bundle(
          y = y,
          X = X,
          backend = backend,
          kernel = kernel,
          GPD = TRUE,
          components = 6,
          mcmc = list(niter = 10, nburnin = 5, thin = 1, nchains = 1, seed = 1)
        )
        NULL
      }, error = function(e) e)
      if (is.null(err)) stop("Expected error for unsupported GPD backend.")
      msg <- paste0(msg, " | expected error for unsupported GPD backend")
      return(list(ok = TRUE, msg = msg))
    }

    mcmc_cfg <- list(niter = 30, nburnin = 10, thin = 1, nchains = 1, seed = 1)
    cache_key <- NULL
    if (exists(".cache_enabled") && isTRUE(.cache_enabled())) {
      key_str <- paste(
        "predict", kernel, backend, gpd, has_X, N,
        mcmc_cfg$niter, mcmc_cfg$nburnin, mcmc_cfg$thin, mcmc_cfg$nchains, mcmc_cfg$seed,
        sep = "|"
      )
      cache_key <- .cache_hash(key_str)
    }
    cached <- if (!is.null(cache_key)) .cache_get(cache_key) else NULL
    if (!is.null(cached) && inherits(cached$fit, "mixgpd_fit")) {
      fit <- cached$fit
    } else {
      bundle <- build_nimble_bundle(
        y = y,
        X = X,
        backend = backend,
        kernel = kernel,
        GPD = gpd,
        components = 6,
        mcmc = mcmc_cfg
      )
      fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
      if (!is.null(cache_key)) .cache_set(cache_key, list(fit = fit))
    }

    if (!inherits(fit, "mixgpd_fit")) stop("fit is not a mixgpd_fit.")
    print(fit)
    s <- summary(fit)
    print(s)

    y_grid <- sort(y)
    p_grid <- c(0.5, 0.9)
    nsim <- 5L

    if (has_X) {
      X_new <- X[1:3, , drop = FALSE]
      pr_den <- predict(fit, x = X_new, y = y_grid, type = "density", ncores = 1)
      pr_surv <- predict(fit, x = X_new, y = y_grid, type = "survival", ncores = 1)
      pr_q <- predict(fit, x = X_new, type = "quantile", index = p_grid, ncores = 1)
      pr_samp <- predict(fit, x = X_new, type = "sample", nsim = nsim)
      pr_mean <- predict(fit, x = X_new, type = "mean", nsim_mean = 50)
      n_pred <- nrow(X_new)
    } else {
      pr_den <- predict(fit, y = y_grid, type = "density", ncores = 1)
      pr_surv <- predict(fit, y = y_grid, type = "survival", ncores = 1)
      pr_q <- predict(fit, type = "quantile", index = p_grid, ncores = 1)
      pr_samp <- predict(fit, type = "sample", nsim = nsim)
      pr_mean <- predict(fit, type = "mean", nsim_mean = 50)
      n_pred <- 1L
    }

    if (!is.data.frame(pr_den$fit)) stop("density fit must be data.frame.")
    if (!is.data.frame(pr_surv$fit)) stop("survival fit must be data.frame.")
    if (!is.data.frame(pr_q$fit)) stop("quantile fit must be data.frame.")
    if (!is.data.frame(pr_mean$fit)) stop("mean fit must be data.frame.")

    den_rows <- if (has_X) n_pred * length(y_grid) else length(y_grid)
    if (nrow(pr_den$fit) != den_rows) stop("density rows mismatch.")
    if (nrow(pr_surv$fit) != den_rows) stop("survival rows mismatch.")

    q_rows <- if (has_X) n_pred * length(p_grid) else length(p_grid)
    if (nrow(pr_q$fit) != q_rows) stop("quantile rows mismatch.")

    mean_rows <- if (has_X) n_pred else 1L
    if (nrow(pr_mean$fit) != mean_rows) stop("mean rows mismatch.")

    if (has_X) {
      if (!all(c("id", "index", "estimate", "lower", "upper") %in% names(pr_q$fit))) {
        stop("quantile columns mismatch (conditional).")
      }
    } else {
      if (!all(c("index", "estimate", "lower", "upper") %in% names(pr_q$fit))) {
        stop("quantile columns mismatch (unconditional).")
      }
    }

    den_col <- if ("density" %in% names(pr_den$fit)) "density" else "estimate"
    surv_col <- if ("survival" %in% names(pr_surv$fit)) "survival" else "estimate"

    if (any(!is.finite(pr_den$fit[[den_col]]))) stop("density has non-finite values.")
    if (any(pr_den$fit[[den_col]] < 0)) stop("density has negative values.")
    if (any(!is.finite(pr_surv$fit[[surv_col]]))) stop("survival has non-finite values.")
    if (any(pr_surv$fit[[surv_col]] < 0 | pr_surv$fit[[surv_col]] > 1)) {
      stop("survival outside [0,1].")
    }

    if (has_X) {
      if (!identical(dim(pr_samp$fit), c(n_pred, nsim))) stop("sample dims mismatch.")
    } else {
      if (!is.numeric(pr_samp$fit) || length(pr_samp$fit) != nsim) {
        stop("sample length mismatch.")
      }
    }

    if (has_X) {
      res <- residuals(fit, type = "pit")
      if (!is.numeric(res) || length(res) != length(y)) stop("residuals length mismatch.")
      ftd <- fitted(fit)
      ftd_n <- if (is.data.frame(ftd)) nrow(ftd) else length(ftd)
      if (ftd_n != length(y)) stop("fitted length mismatch.")
    }

    if (requireNamespace("ggmcmc", quietly = TRUE) && requireNamespace("coda", quietly = TRUE)) {
      plots <- plot(fit, family = c("traceplot"), params = "alpha")
      if (!is.list(plots)) stop("plot() did not return a list.")
    }
  }, warning = function(w) {
    warn_msgs <<- c(warn_msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  }, error = function(e) {
    ok <<- FALSE
    msg <<- paste0(msg, " | error: ", conditionMessage(e))
  })

  if (length(warn_msgs)) {
    msg <- paste0(msg, " | warnings: ", length(warn_msgs))
  }

  list(ok = ok, msg = msg)
}
