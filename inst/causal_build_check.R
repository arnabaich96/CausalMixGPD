devtools::load_all()

set.seed(1)
N <- 40
X_base <- cbind(x1 = stats::rnorm(N), x2 = stats::runif(N, -1, 1))
X_cat_df <- data.frame(
  x1 = X_base[, 1],
  x2 = X_base[, 2],
  x3 = factor(sample(c("A", "B", "C"), N, replace = TRUE))
)
X_cat <- stats::model.matrix(~ x1 + x2 + x3 - 1, data = X_cat_df)

T <- stats::rbinom(N, 1, stats::plogis(0.2 + 0.5 * X_base[, 1]))
y <- stats::rexp(N) + 0.1

mcmc_out <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)
mcmc_ps <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)

run_case <- function(kernel, backend, GPD, X, components, epsilon) {
  cb <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = backend,
    kernel = kernel,
    GPD = GPD,
    components = components,
    epsilon = epsilon,
    mcmc_outcome = mcmc_out,
    mcmc_ps = mcmc_ps
  )
  cf <- run_mcmc_causal(cb, show_progress = FALSE)
  list(cb = cb, cf = cf)
}

kernel_list <- names(get_kernel_registry())
all_runs <- list()

for (k in kernel_list) {
  cat("\n==== kernel:", k, "(no GPD) ====\n")
  res <- run_case(
    kernel = c(k, k),
    backend = c("sb", "crp"),
    GPD = c(FALSE, FALSE),
    X = X_cat,
    components = c(6, 8),
    epsilon = c(0.025, 0.05)
  )
  all_runs[[paste0(k, "_bulk")]] <- res

  if (!identical(k, "cauchy")) {
    cat("\n==== kernel:", k, "(GPD, SB/SB) ====\n")
    res_gpd <- run_case(
      kernel = c(k, k),
      backend = c("sb", "sb"),
      GPD = c(TRUE, TRUE),
      X = X_cat,
      components = c(6, 6),
      epsilon = c(0.025, 0.025)
    )
    all_runs[[paste0(k, "_gpd")]] <- res_gpd
  }
}

cat("\nS3 checks\n")
plot_checked <- FALSE
for (nm in names(all_runs)) {
  cat("checking:", nm, "\n")
  cf <- all_runs[[nm]]$cf
  stopifnot(inherits(cf, "dpmixgpd_causal_fit"))

  if (!plot_checked) {
    tmp_pdf <- tempfile(fileext = ".pdf")
    grDevices::pdf(tmp_pdf)
    plot(cf, arm = 1, family = "traceplot")
    plot(cf, arm = "control", family = "traceplot")
    grDevices::dev.off()
    unlink(tmp_pdf)

    bad_arm <- try(plot(cf, arm = 2, family = "traceplot"), silent = TRUE)
    stopifnot(inherits(bad_arm, "try-error"))
    plot_checked <- TRUE
  }

  tmp_out <- tempfile(fileext = ".txt")
  con <- file(tmp_out, open = "wt")
  sink(con)
  print(cf)
  summary(cf)
  sink()
  close(con)
  unlink(tmp_out)

  fit_con <- cf$outcome_fit$con
  fit_trt <- cf$outcome_fit$trt
  stopifnot(inherits(fit_con, "mixgpd_fit"), inherits(fit_trt, "mixgpd_fit"))

  sc <- summary(fit_con)
  st <- summary(fit_trt)
  stopifnot(inherits(sc, "mixgpd_summary"), inherits(st, "mixgpd_summary"))

  pr_con <- predict(fit_con, type = "mean")
  pr_trt <- predict(fit_trt, type = "mean")
  stopifnot(is.list(pr_con), is.list(pr_trt), "fit" %in% names(pr_con), "fit" %in% names(pr_trt))

  fd_con <- fitted(fit_con)
  fd_trt <- fitted(fit_trt)
  rs_con <- residuals(fit_con)
  rs_trt <- residuals(fit_trt)
  stopifnot(is.numeric(fd_con), is.numeric(fd_trt), is.numeric(rs_con), is.numeric(rs_trt))
}
