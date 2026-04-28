# CausalMixGPD replication entrypoint
#
# This script is the single package-level replication driver. It runs compact
# versions of the package workflows used in the manuscript and writes the
# resulting tables/figures to an output directory. Longer manuscript builds may
# source this file or reuse the same public API calls with larger MCMC settings.

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args)) args[[1]] else file.path(getwd(), "CausalMixGPD-replication")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

required <- c("ggplot2")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install required package(s): ", paste(missing, collapse = ", "), call. = FALSE)
}

if (file.exists("DESCRIPTION") &&
    any(grepl("^Package:\\s*CausalMixGPD\\s*$", readLines("DESCRIPTION", warn = FALSE)))) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Install 'devtools' to run replication from a source checkout.", call. = FALSE)
  }
  devtools::load_all(quiet = TRUE)
} else {
  if (!requireNamespace("CausalMixGPD", quietly = TRUE)) {
    stop("Install 'CausalMixGPD' or run this script from the package source root.", call. = FALSE)
  }
  library(CausalMixGPD)
}

set.seed(2026)
mcmc_rep <- list(
  niter = 80,
  nburnin = 40,
  thin = 1,
  nchains = 1,
  seed = 2026,
  show_progress = FALSE,
  quiet = TRUE
)

write_table <- function(x, name) {
  utils::write.csv(x, file.path(out_dir, name), row.names = FALSE)
}

save_plot <- function(p, name, width = 7, height = 5) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::ggsave(file.path(out_dir, name), plot = p, width = width, height = height)
  }
}

# One-arm workflow -----------------------------------------------------------
data("nc_pos200_k3", package = "CausalMixGPD")
idx_one <- seq_len(30)
fit_one <- dpmix(
  y = nc_pos200_k3$y[idx_one],
  kernel = "gamma",
  backend = "sb",
  components = 3,
  mcmc = mcmc_rep
)
write_table(summary(fit_one)$table, "one_arm_summary.csv")
pred_one <- predict(fit_one, type = "quantile", index = c(0.25, 0.5, 0.75), show_progress = FALSE)
write_table(pred_one$fit, "one_arm_quantiles.csv")
save_plot(plot(pred_one), "one_arm_quantiles.png")

# Clustering workflow --------------------------------------------------------
data("nc_realX100_p3_k2", package = "CausalMixGPD")
dat_cl <- data.frame(
  y = nc_realX100_p3_k2$y[1:30],
  nc_realX100_p3_k2$X[1:30, , drop = FALSE]
)
fit_cluster <- dpmix.cluster(
  y ~ x1 + x2 + x3,
  data = dat_cl,
  kernel = "normal",
  type = "param",
  components = 3,
  mcmc = mcmc_rep
)
labels_cluster <- predict(fit_cluster, type = "label", return_scores = TRUE)
write_table(cluster_profiles(labels_cluster), "cluster_profiles.csv")
save_plot(plot(labels_cluster, type = "sizes"), "cluster_sizes.png")

# Causal workflow ------------------------------------------------------------
data("causal_pos500_p3_k2", package = "CausalMixGPD")
idx_causal <- seq_len(40)
fit_causal <- dpmix.causal(
  y = causal_pos500_p3_k2$y[idx_causal],
  X = causal_pos500_p3_k2$X[idx_causal, , drop = FALSE],
  treat = causal_pos500_p3_k2$A[idx_causal],
  kernel = "gamma",
  backend = "sb",
  components = 3,
  mcmc = mcmc_rep
)
ate_rep <- ate(fit_causal, interval = "credible", show_progress = FALSE)
write_table(summary(ate_rep)$effect_table, "causal_ate.csv")
save_plot(plot(ate_rep, type = "effect"), "causal_ate.png")

cat("Replication outputs written to:", normalizePath(out_dir, winslash = "/"), "\n")
