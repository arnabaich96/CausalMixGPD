## Regenerate lightweight precomputed vignette artifacts (inst/extdata/*).
## Run from package root:
##   Rscript data-raw/build_vignette_fits.R
## Requires: pkgload, and all package dependencies for MCMC (nimble, etc.).

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg)), ".."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
if (!file.exists(file.path(root, "DESCRIPTION"))) {
  stop("Run this script from the package root (directory containing DESCRIPTION).", call. = FALSE)
}
setwd(root)
message("Package root: ", root)

local({
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Install package 'pkgload' to run this script.", call. = FALSE)
  }
  pkgload::load_all(export_all = FALSE, quiet = TRUE)

  ext <- file.path(root, "inst", "extdata")
  if (!dir.exists(ext)) dir.create(ext, recursive = TRUE)
  vignette_assets <- file.path(root, "vignettes", "assets")
  if (!dir.exists(vignette_assets)) dir.create(vignette_assets, recursive = TRUE)

  # Helper: write printed output to a text file.
  write_print <- function(x, path) {
    txt <- capture.output(print(x))
    writeLines(txt, con = path, useBytes = TRUE)
    invisible(path)
  }

  # Helper: save a ggplot to PNG (base grDevices).
  save_png <- function(plot_obj, path, width = 7, height = 4.8, res = 144) {
    grDevices::png(filename = path, width = width, height = height, units = "in", res = res, type = "cairo-png")
    on.exit(grDevices::dev.off(), add = TRUE)
    print(plot_obj)
    invisible(path)
  }

  # ---- One-arm vignette ----
  mcmc_vig <- list(
    niter = 1200,
    nburnin = 300,
    thin = 2,
    nchains = 2,
    seed = 2026
  )
  data("nc_posX100_p3_k2", package = "CausalMixGPD")
  onearm_dat <- data.frame(
    y = nc_posX100_p3_k2$y,
    nc_posX100_p3_k2$X
  )
  fit_spliced <- dpmgpd(
    formula = y ~ x1 + x2 + x3,
    data = onearm_dat,
    backend = "sb",
    kernel = "lognormal",
    components = 5,
    mcmc = mcmc_vig
  )
  fit_bulk <- dpmix(
    formula = y ~ x1 + x2 + x3,
    data = onearm_dat,
    backend = "sb",
    kernel = "lognormal",
    components = 5,
    mcmc = mcmc_vig
  )

  # Diagnostics (static)
  write_print(summary(fit_spliced), file.path(ext, "one_arm_fit_summary.txt"))
  # Minimal alpha diagnostics without storing full fits: use coda samples.
  smp <- fit_spliced$mcmc$samples %||% fit_spliced$samples
  alpha_draws <- as.numeric(as.matrix(smp[[1]])[, "alpha"])
  df_alpha <- data.frame(iter = seq_along(alpha_draws), alpha = alpha_draws)
  p_trace <- ggplot2::ggplot(df_alpha, ggplot2::aes(x = iter, y = alpha)) +
    ggplot2::geom_line(linewidth = 0.35, alpha = 0.9) +
    ggplot2::labs(title = "Trace plot: alpha", x = "Iteration", y = "alpha") +
    ggplot2::theme_minimal(base_size = 12)
  p_dens <- ggplot2::ggplot(df_alpha, ggplot2::aes(x = alpha)) +
    ggplot2::geom_density(linewidth = 0.9, fill = "grey80", alpha = 0.6) +
    ggplot2::labs(title = "Posterior density: alpha", x = "alpha", y = "Density") +
    ggplot2::theme_minimal(base_size = 12)
  save_png(p_trace, file.path(ext, "one_arm_alpha_trace.png"), width = 7, height = 4.8)
  save_png(p_dens, file.path(ext, "one_arm_alpha_density.png"), width = 7, height = 4.8)
  save_png(p_trace, file.path(vignette_assets, "one_arm_alpha_trace.png"), width = 7, height = 4.8)
  save_png(p_dens, file.path(vignette_assets, "one_arm_alpha_density.png"), width = 7, height = 4.8)

  # Prediction outputs (store_draws = FALSE to keep size small)
  x_new <- as.data.frame(lapply(
    onearm_dat[, c("x1", "x2", "x3")],
    quantile,
    probs = c(0.25, 0.50, 0.75),
    na.rm = TRUE
  ))
  rownames(x_new) <- c("q25", "q50", "q75")
  y_grid <- seq(min(onearm_dat$y), quantile(onearm_dat$y, 0.99), length.out = 120)
  pred_dens <- predict(fit_spliced, newdata = x_new, y = y_grid, type = "density",
                       interval = "credible", level = 0.95, store_draws = FALSE, show_progress = FALSE)
  pred_surv <- predict(fit_spliced, newdata = x_new, y = y_grid, type = "survival",
                       interval = "credible", level = 0.95, store_draws = FALSE, show_progress = FALSE)
  pred_quant <- predict(fit_spliced, newdata = x_new, type = "quantile",
                        index = c(0.25, 0.50, 0.75, 0.90, 0.95),
                        interval = "credible", level = 0.95, store_draws = FALSE, show_progress = FALSE)
  pred_mean <- predict(fit_spliced, newdata = x_new, type = "mean",
                       interval = "hpd", level = 0.90, store_draws = FALSE, show_progress = FALSE)
  cutoff_val <- as.numeric(stats::quantile(onearm_dat$y, 0.95))
  pred_rmean <- predict(fit_spliced, newdata = x_new, type = "rmean",
                        cutoff = cutoff_val, interval = "hpd", level = 0.90, store_draws = FALSE, show_progress = FALSE)

  quant_bulk <- predict(fit_bulk, newdata = x_new, type = "quantile",
                        index = c(0.90, 0.95), interval = "credible", level = 0.95, store_draws = FALSE, show_progress = FALSE)
  quant_spliced <- predict(fit_spliced, newdata = x_new, type = "quantile",
                           index = c(0.90, 0.95), interval = "credible", level = 0.95, store_draws = FALSE, show_progress = FALSE)

  one_arm_outputs <- list(
    x_new = x_new,
    y_grid = y_grid,
    pred_dens = pred_dens,
    pred_surv = pred_surv,
    pred_quant = pred_quant,
    pred_mean = pred_mean,
    pred_rmean_fit_df = pred_rmean$fit_df,
    quant_bulk_fit_df = quant_bulk$fit_df,
    quant_spliced_fit_df = quant_spliced$fit_df,
    cutoff_val = cutoff_val
  )
  saveRDS(one_arm_outputs, file.path(ext, "one_arm_outputs.rds"), compress = "xz")
  message("Wrote one_arm_outputs.rds (+ one_arm_fit_summary.txt, one_arm_alpha_*.png)")

  # ---- Causal vignette ----
  mcmc_out <- list(
    niter = 1200,
    nburnin = 300,
    thin = 2,
    nchains = 2,
    seed = 2027
  )
  mcmc_ps <- list(
    niter = 1000,
    nburnin = 250,
    thin = 2,
    nchains = 2,
    seed = 2028
  )
  data("causal_pos500_p3_k2", package = "CausalMixGPD")
  causal_dat <- data.frame(
    y = causal_pos500_p3_k2$y,
    A = causal_pos500_p3_k2$A,
    causal_pos500_p3_k2$X
  )
  fit_causal <- dpmgpd.causal(
    formula = y ~ x1 + x2 + x3,
    data = causal_dat,
    treat = "A",
    backend = "crp",
    kernel = "gamma",
    components = 6,
    PS = "logit",
    ps_scale = "logit",
    ps_summary = "mean",
    mcmc_outcome = mcmc_out,
    mcmc_ps = mcmc_ps,
    parallel_arms = FALSE
  )

  write_print(summary(fit_causal), file.path(ext, "causal_fit_summary.txt"))

  ate_fit <- ate(fit_causal, interval = "hpd", level = 0.95, show_progress = FALSE)
  att_fit <- att(fit_causal, interval = "hpd", level = 0.95, show_progress = FALSE)
  qte_fit <- qte(fit_causal, probs = c(0.25, 0.50, 0.75, 0.90, 0.95),
                 interval = "credible", level = 0.95, show_progress = FALSE)
  qtt_fit <- qtt(fit_causal, probs = c(0.25, 0.50, 0.75, 0.90, 0.95),
                 interval = "credible", level = 0.95, show_progress = FALSE)

  x_grid <- as.data.frame(lapply(
    causal_dat[, c("x1", "x2", "x3")],
    quantile,
    probs = c(0.25, 0.50, 0.75),
    na.rm = TRUE
  ))
  rownames(x_grid) <- c("low", "mid", "high")

  cate_fit <- cate(fit_causal, newdata = x_grid, interval = "hpd", level = 0.95, show_progress = FALSE)
  cqte_fit <- cqte(fit_causal, newdata = x_grid, probs = c(0.25, 0.50, 0.75, 0.90, 0.95),
                   interval = "credible", level = 0.95, show_progress = FALSE)

  pred_q <- predict(fit_causal, newdata = x_grid, type = "quantile", p = c(0.50, 0.90),
                    store_draws = FALSE, show_progress = FALSE)
  pred_s <- predict(fit_causal, newdata = x_grid, type = "survival",
                    y = rep(as.numeric(stats::quantile(causal_dat$y, 0.75)), nrow(x_grid)),
                    store_draws = FALSE, show_progress = FALSE)

  causal_outputs <- list(
    x_grid = x_grid,
    ate_fit = ate_fit,
    att_fit = att_fit,
    qte_fit = qte_fit,
    qtt_fit = qtt_fit,
    cate_fit = cate_fit,
    cqte_fit = cqte_fit,
    pred_q = pred_q,
    pred_s = pred_s
  )
  saveRDS(causal_outputs, file.path(ext, "causal_outputs.rds"), compress = "xz")
  message("Wrote causal_outputs.rds (+ causal_fit_summary.txt)")

  # ---- Clustering vignette ----
  mcmc_clust <- list(
    niter = 1200,
    nburnin = 300,
    thin = 2,
    nchains = 2,
    seed = 2029
  )
  data("nc_realX100_p3_k2", package = "CausalMixGPD")
  clust_dat <- data.frame(
    y = nc_realX100_p3_k2$y,
    nc_realX100_p3_k2$X
  )
  set.seed(123)
  idx_train <- sample(seq_len(nrow(clust_dat)), size = 80)
  train_dat <- clust_dat[idx_train, ]
  test_dat <- clust_dat[-idx_train, ]

  fit_cluster <- dpmix.cluster(
    formula = y ~ x1 + x2 + x3,
    data = train_dat,
    kernel = "laplace",
    type = "both",
    components = 8,
    mcmc = mcmc_clust
  )
  fit_cluster_spliced <- dpmgpd.cluster(
    formula = y ~ x1 + x2 + x3,
    data = train_dat,
    kernel = "normal",
    type = "both",
    components = 8,
    mcmc = mcmc_clust
  )

  write_print(summary(fit_cluster), file.path(ext, "clustering_fit_summary.txt"))

  psm_obj <- predict(fit_cluster, type = "psm")
  train_lab <- predict(fit_cluster, type = "label", return_scores = TRUE)
  test_lab <- predict(fit_cluster, newdata = test_dat, type = "label", return_scores = TRUE)
  train_certainty <- apply(train_lab$scores, 1, max)
  test_certainty <- apply(test_lab$scores, 1, max)
  cluster_profiles <- summary(test_lab)$cluster_profiles

  clustering_outputs <- list(
    train_dat = train_dat,
    test_dat = test_dat,
    fit_cluster = fit_cluster,
    psm_obj = psm_obj,
    train_lab = train_lab,
    test_lab = test_lab,
    train_certainty_summary = summary(train_certainty),
    test_certainty_summary = summary(test_certainty),
    cluster_profiles = cluster_profiles
  )
  saveRDS(clustering_outputs, file.path(ext, "clustering_outputs.rds"), compress = "xz")
  message("Wrote clustering_outputs.rds (+ clustering_fit_summary.txt)")
  invisible(TRUE)
})
