`%||%` <- function(a, b) if (!is.null(a)) a else b

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
      return(invisible(NULL))
    }

    bundle <- build_nimble_bundle(
      y = y,
      X = X,
      backend = backend,
      kernel = kernel,
      GPD = gpd,
      components = 6,
      mcmc = list(niter = 30, nburnin = 10, thin = 1, nchains = 1, seed = 1)
    )
    fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)

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
      pr_q <- predict(fit, x = X_new, type = "quantile", p = p_grid, ncores = 1)
      pr_samp <- predict(fit, x = X_new, type = "sample", nsim = nsim)
      pr_mean <- predict(fit, x = X_new, type = "mean", nsim_mean = 50)
      n_pred <- nrow(X_new)
    } else {
      pr_den <- predict(fit, y = y_grid, type = "density", ncores = 1)
      pr_surv <- predict(fit, y = y_grid, type = "survival", ncores = 1)
      pr_q <- predict(fit, type = "quantile", p = p_grid, ncores = 1)
      pr_samp <- predict(fit, type = "sample", nsim = nsim)
      pr_mean <- predict(fit, type = "mean", nsim_mean = 50)
      n_pred <- 1L
    }

    if (!identical(dim(pr_den$fit), c(n_pred, length(y_grid)))) stop("density dims mismatch.")
    if (!identical(dim(pr_surv$fit), c(n_pred, length(y_grid)))) stop("survival dims mismatch.")
    if (!identical(dim(pr_q$fit), c(n_pred, length(p_grid)))) stop("quantile dims mismatch.")
    if (!identical(dim(pr_samp$fit), c(n_pred, nsim))) stop("sample dims mismatch.")
    if (!identical(length(pr_mean$fit), n_pred)) stop("mean length mismatch.")

    if (any(!is.finite(pr_den$fit))) stop("density has non-finite values.")
    if (any(pr_den$fit < 0)) stop("density has negative values.")
    if (any(!is.finite(pr_surv$fit))) stop("survival has non-finite values.")
    if (any(pr_surv$fit < 0 | pr_surv$fit > 1)) stop("survival outside [0,1].")

    res <- residuals(fit, type = "pit")
    if (length(res) != length(y)) stop("residuals length mismatch.")
    ftd <- fitted(fit)
    if (length(ftd) != length(y)) stop("fitted length mismatch.")

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
