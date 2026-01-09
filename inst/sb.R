devtools::load_all()

fast <- TRUE

set.seed(1)
y <- abs(stats::rnorm(80)) + 0.2
x <- seq(1, 80)

mcmc_args <- if (isTRUE(fast)) {
  list(niter = 50, nburnin = 10, thin = 1, nchains = 1, seed = 1)
} else {
  list(niter = 800, nburnin = 200, thin = 2, nchains = 2, seed = c(1, 2))
}

bundle_sb_amoroso <- build_nimble_bundle(
  y = y,
  X = data.frame(x = x),
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components  = 6,
  mcmc = mcmc_args
)

# bundle S3
print(summary(bundle_sb_amoroso))
print(bundle_sb_amoroso)
print(bundle_sb_amoroso, code = TRUE)

# fit (runner)
fit_sb_amoroso <- run_mcmc_bundle_manual(bundle_sb_amoroso)

# fit S3
print(fit_sb_amoroso)
s <- summary(fit_sb_amoroso)
print(s)

# MCMC diagnostic plots (ggmcmc)
nchains <- fit_sb_amoroso$mcmc$nchains
if (is.null(nchains) || !is.finite(nchains)) nchains <- 1L

plot_fams <- if (nchains > 1L) {
  c("traceplot", "density", "autocorrelation", "crosscorrelation", "Rhat",
    "grb", "effective", "geweke")
} else {
  c("traceplot", "density", "autocorrelation", "geweke")
}

plots_sb <- plot(
  fit_sb_amoroso,
  family = plot_fams
)

# Show all plots at once if a layout helper is available
if (requireNamespace("patchwork", quietly = TRUE)) {
  print(patchwork::wrap_plots(plots_sb))
} else if (requireNamespace("gridExtra", quietly = TRUE)) {
  gridExtra::grid.arrange(grobs = plots_sb)
} else {
  grDevices::devAskNewPage(TRUE)
  for (p in plots_sb) print(p)
}

# Prediction demos (density, survival, quantile, sample, mean)
fit <- fit_sb_amoroso
y_grid <- seq(min(y), stats::quantile(y, 0.99), length.out = 120)

X_new_pred <- data.frame(x = x)
pr_den <- predict(fit, x = X_new_pred, y = y_grid, type = "density")
pr_surv <- predict(fit, x = X_new_pred, y = y_grid, type = "survival")
pr_cdf <- list(fit = 1 - pr_surv$fit)

pr_q <- predict(fit, type = "quantile", p = c(0.5, 0.9, 0.99))
pr_samp <- predict(fit, type = "sample", nsim = 200)
pr_mean <- predict(fit, type = "mean")

str(pr_den)
str(pr_surv)
str(pr_cdf)
str(pr_q)
str(pr_samp)
str(pr_mean)











