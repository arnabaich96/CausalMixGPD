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
replication_seed <- 2026
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

write_text <- function(x, name) {
  writeLines(x, file.path(out_dir, name), useBytes = TRUE)
}

save_plot <- function(p, name, width = 7, height = 5) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::ggsave(file.path(out_dir, name), plot = p, width = width, height = height)
  }
}

pkg_version <- tryCatch(as.character(utils::packageVersion("CausalMixGPD")), error = function(e) NA_character_)
output_manifest <- data.frame(
  file = character(),
  workflow = character(),
  manuscript_use = character(),
  stringsAsFactors = FALSE
)
record_output <- function(file, workflow, manuscript_use) {
  output_manifest <<- rbind(
    output_manifest,
    data.frame(
      file = file,
      workflow = workflow,
      manuscript_use = manuscript_use,
      stringsAsFactors = FALSE
    )
  )
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
record_output("one_arm_summary.csv", "one-arm", "Package overview one-arm posterior summary")
pred_one <- predict(fit_one, type = "quantile", index = c(0.25, 0.5, 0.75), show_progress = FALSE)
write_table(pred_one$fit, "one_arm_quantiles.csv")
record_output("one_arm_quantiles.csv", "one-arm", "Package overview quantile prediction table")
save_plot(plot(pred_one), "one_arm_quantiles.png")
record_output("one_arm_quantiles.png", "one-arm", "Package overview quantile prediction figure")

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
record_output("cluster_profiles.csv", "clustering", "Cluster profile table via public accessor")
save_plot(plot(labels_cluster, type = "sizes"), "cluster_sizes.png")
record_output("cluster_sizes.png", "clustering", "Cluster assignment size figure")

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
record_output("causal_ate.csv", "causal", "Causal ATE summary table")
save_plot(plot(ate_rep, type = "effect"), "causal_ate.png")
record_output("causal_ate.png", "causal", "Causal ATE effect figure")

write_table(output_manifest, "manifest.csv")
write_text(
  c(
    "CausalMixGPD replication run",
    paste("Package version:", pkg_version),
    paste("Seed:", replication_seed),
    paste("Output directory:", normalizePath(out_dir, winslash = "/")),
    "",
    "Session info:",
    utils::capture.output(utils::sessionInfo())
  ),
  "session-info.txt"
)
record_output("manifest.csv", "metadata", "Output-to-manuscript mapping")
record_output("session-info.txt", "metadata", "R session, package version, seed, and platform")
write_table(output_manifest, "manifest.csv")

cat("Replication outputs written to:", normalizePath(out_dir, winslash = "/"), "\n")
