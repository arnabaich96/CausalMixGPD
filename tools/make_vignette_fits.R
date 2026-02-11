# tools/make_vignette_artifacts.R
#
# Generate small, precomputed tables and figures for CRAN vignettes.
#
# Run this LOCALLY from the package root (do NOT run on CRAN):
#   source("tools/make_vignette_artifacts.R")
#
# The vignettes read these artifacts from inst/extdata/ so that vignette builds
# on CRAN remain fast and deterministic.

stopifnot(file.exists("DESCRIPTION"))

stopifnot(requireNamespace("DPmixGPD", quietly = TRUE))

suppressPackageStartupMessages(library(DPmixGPD))

dir.create(file.path("inst", "extdata"), recursive = TRUE, showWarnings = FALSE)

.save_png <- function(file, expr, width = 900, height = 550, res = 120) {
  grDevices::png(filename = file, width = width, height = height, res = res)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
}

.write_csv <- function(x, file) {
  utils::write.csv(x, file = file, row.names = FALSE)
}

message("[DPmixGPD] Precomputing vignette artifacts into inst/extdata/ ...")

# -----------------------------------------------------------------------------
# 1) Unconditional example (positive support + injected tail)
# -----------------------------------------------------------------------------
data("nc_pos_tail200_k4", package = "DPmixGPD")
y_u <- nc_pos_tail200_k4$y

kernel_u <- tryCatch(nc_pos_tail200_k4$truth$kernel, error = function(e) "lognormal")

bundle_u <- build_nimble_bundle(
  y = y_u,
  X = NULL,
  backend = "sb",
  kernel = kernel_u,
  GPD = TRUE,
  components = 4,
  mcmc = list(niter = 900, nburnin = 250, thin = 2, nchains = 1, seed = 101)
)

fit_u <- run_mcmc_bundle_manual(bundle_u, show_progress = TRUE, quiet = TRUE)

grid_u <- seq(min(y_u), max(y_u) * 1.5, length.out = 200)
dhat_u <- predict(fit_u, y = grid_u, type = "density", cred.level = 0.90)
qs_u   <- predict(fit_u, type = "quantile", p = c(0.5, 0.9, 0.95, 0.99), cred.level = 0.90)

.write_csv(qs_u$fit, file.path("inst", "extdata", "unconditional_quantiles.csv"))

.save_png(file.path("inst", "extdata", "unconditional_density.png"), {
  plot(dhat_u)
})

# -----------------------------------------------------------------------------
# 2) Conditional example (covariates)
# -----------------------------------------------------------------------------
data("nc_posX100_p4_k3", package = "DPmixGPD")
y_c  <- nc_posX100_p4_k3$y
Xdf  <- nc_posX100_p4_k3$X
X_c  <- stats::model.matrix(~ ., data = Xdf)

kernel_c <- tryCatch(nc_posX100_p4_k3$truth$kernel, error = function(e) "lognormal")

bundle_c <- build_nimble_bundle(
  y = y_c,
  X = X_c,
  backend = "sb",
  kernel = kernel_c,
  GPD = FALSE,
  components = 4,
  mcmc = list(niter = 1100, nburnin = 300, thin = 2, nchains = 1, seed = 202)
)

fit_c <- run_mcmc_bundle_manual(bundle_c, show_progress = TRUE, quiet = TRUE)

# Two covariate profiles built from the data:
# - baseline: column-wise medians
# - shifted: median + sd for continuous; 1 for binary (0/1)
is_binary <- vapply(Xdf, function(v) {
  u <- unique(stats::na.omit(v))
  length(u) > 0 && all(u %in% c(0, 1))
}, logical(1))

base <- lapply(Xdf, stats::median)
hi   <- base
for (nm in names(base)) {
  if (isTRUE(is_binary[[nm]])) {
    hi[[nm]] <- 1
  } else {
    hi[[nm]] <- base[[nm]] + stats::sd(Xdf[[nm]])
  }
}
newdf <- rbind(as.data.frame(base), as.data.frame(hi))
Xnew  <- stats::model.matrix(~ ., data = newdf)

grid_c <- seq(stats::quantile(y_c, 0.01), stats::quantile(y_c, 0.99), length.out = 160)
dens_c <- predict(fit_c, x = Xnew, y = grid_c, type = "density", cred.level = 0.90)
qs_c   <- predict(fit_c, x = Xnew, type = "quantile", p = c(0.5, 0.9, 0.95), cred.level = 0.90)

.write_csv(qs_c$fit, file.path("inst", "extdata", "conditional_quantiles.csv"))

.save_png(file.path("inst", "extdata", "conditional_density.png"), {
  plot(dens_c)
})

# -----------------------------------------------------------------------------
# 3) Causal example (two-arm model + PS augmentation)
# -----------------------------------------------------------------------------
data("causal_alt_pos500_p5_k4_tail", package = "DPmixGPD")
dat <- causal_alt_pos500_p5_k4_tail

# Use a subset to keep local precomputation fast while remaining data-driven.
N_use <- min(300L, length(dat$y))
y_a <- dat$y[1:N_use]
T_a <- dat$T[1:N_use]
Xdf <- dat$X[1:N_use, , drop = FALSE]
# For causal+PS workflows, pass raw covariates (no explicit intercept).
# The PS builder handles intercept augmentation internally when requested.
X_a <- as.matrix(Xdf)

kernel_t <- tryCatch(dat$truth$kernel1, error = function(e) "lognormal")
kernel_c0 <- tryCatch(dat$truth$kernel0, error = function(e) kernel_t)

cb <- build_causal_bundle(
  y = y_a,
  X = X_a,
  T = T_a,
  backend = "sb",
  kernel = c(kernel_t, kernel_c0),
  GPD = c(TRUE, TRUE),
  components = c(4, 4),
  PS = "logit",
  ps_scale = "logit",
  ps_summary = "mean",
  mcmc_outcome = list(niter = 1200, nburnin = 350, thin = 2, nchains = 1, seed = 303),
  mcmc_ps      = list(niter = 900,  nburnin = 300, thin = 2, nchains = 1, seed = 304)
)

fit_a <- run_mcmc_causal(cb, show_progress = TRUE)

q <- qte(fit_a, probs = c(0.5, 0.9, 0.95), interval = "credible", level = 0.90)
a <- ate(fit_a, interval = "credible", level = 0.90, nsim_mean = 100)

q_df <- data.frame(
  prob = q$grid,
  estimate = q$fit,
  lower = q$lower,
  upper = q$upper
)

a_df <- data.frame(
  estimand = "ATE",
  estimate = a$fit,
  lower = a$lower,
  upper = a$upper
)

.write_csv(q_df, file.path("inst", "extdata", "causal_qte.csv"))
.write_csv(a_df, file.path("inst", "extdata", "causal_ate.csv"))

.save_png(file.path("inst", "extdata", "causal_qte.png"), {
  op <- graphics::par(mar = c(4, 4, 1.5, 1))
  on.exit(graphics::par(op), add = TRUE)
  graphics::plot(q_df$prob, q_df$estimate,
    type = "b", pch = 19,
    xlab = "Quantile level (prob)", ylab = "QTE (treated - control)"
  )
  graphics::segments(q_df$prob, q_df$lower, q_df$prob, q_df$upper)
})

message(
  "[DPmixGPD] Done. Generated:\n",
  "  inst/extdata/unconditional_quantiles.csv\n",
  "  inst/extdata/unconditional_density.png\n",
  "  inst/extdata/conditional_quantiles.csv\n",
  "  inst/extdata/conditional_density.png\n",
  "  inst/extdata/causal_qte.csv\n",
  "  inst/extdata/causal_ate.csv\n",
  "  inst/extdata/causal_qte.png\n",
  "Now rebuild vignettes and run R CMD check."
)
