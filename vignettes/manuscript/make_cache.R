#!/usr/bin/env Rscript

options(width = 70)

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required. Install with install.packages('here').", call. = FALSE)
}

pkg_root <- here::here()
manuscript_dir <- here::here("vignettes", "manuscript")
if (!dir.exists(manuscript_dir)) {
  stop("Could not locate 'vignettes/manuscript' from project root.", call. = FALSE)
}

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(pkg_root, quiet = TRUE, export_all = FALSE)
}

if (!"package:CausalMixGPD" %in% search()) {
  library(CausalMixGPD)
}

if (!"package:ggplot2" %in% search()) {
  library(ggplot2)
}

library(MASS)

CACHE_DIR <- file.path(manuscript_dir, "cache")
CACHE_SUBDIRS <- c("fits", "tables", "figs", "meta")
invisible(lapply(file.path(CACHE_DIR, CACHE_SUBDIRS), dir.create,
                 recursive = TRUE, showWarnings = FALSE))

cache_path <- function(...) file.path(CACHE_DIR, ...)

write_rds_atomic <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- sprintf("%s.tmp_%d_%d", path, Sys.getpid(), as.integer(Sys.time()))
  saveRDS(x, tmp)
  if (file.exists(path)) file.remove(path)
  ok <- file.rename(tmp, path)
  if (!ok) {
    file.copy(tmp, path, overwrite = TRUE)
    file.remove(tmp)
  }
  invisible(path)
}

write_text_atomic <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- sprintf("%s.tmp_%d_%d", path, Sys.getpid(), as.integer(Sys.time()))
  writeLines(lines, tmp)
  if (file.exists(path)) file.remove(path)
  ok <- file.rename(tmp, path)
  if (!ok) {
    file.copy(tmp, path, overwrite = TRUE)
    file.remove(tmp)
  }
  invisible(path)
}

write_png <- function(path, expr, width = 1400, height = 900, res = 150) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- sprintf("%s.tmp_%d_%d", path, Sys.getpid(), as.integer(Sys.time()))
  grDevices::png(filename = tmp, width = width, height = height, res = res)
  on.exit(grDevices::dev.off(), add = TRUE)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(
    cex = 1.1,
    cex.main = 1.45,
    cex.lab = 1.3,
    cex.axis = 1.18
  )
  eval.parent(substitute(expr))
  grDevices::dev.off()
  on.exit(NULL, add = FALSE)
  if (file.exists(path)) file.remove(path)
  ok <- file.rename(tmp, path)
  if (!ok) {
    file.copy(tmp, path, overwrite = TRUE)
    file.remove(tmp)
  }
  invisible(path)
}

sanitize_mixgpd_fit <- function(x) {
  if (!is.list(x)) return(x)
  if ("model" %in% names(x)) x$model <- NULL
  if ("mcmc_conf" %in% names(x)) x$mcmc_conf <- NULL
  if ("waic" %in% names(x)) x$waic <- NULL
  if ("cache" %in% names(x) && is.list(x$cache) && "predict_env" %in% names(x$cache)) {
    x$cache$predict_env <- NULL
  }
  if ("mcmc" %in% names(x) && is.list(x$mcmc)) {
    if ("engine" %in% names(x$mcmc)) x$mcmc$engine <- NULL
    if ("waic" %in% names(x$mcmc)) x$mcmc$waic <- NULL
  }
  x
}

sanitize_causal_fit <- function(x) {
  if (!is.list(x)) return(x)
  if ("outcome_fit" %in% names(x) && is.list(x$outcome_fit)) {
    if ("trt" %in% names(x$outcome_fit) && inherits(x$outcome_fit$trt, "mixgpd_fit")) {
      x$outcome_fit$trt <- sanitize_mixgpd_fit(x$outcome_fit$trt)
    }
    if ("con" %in% names(x$outcome_fit) && inherits(x$outcome_fit$con, "mixgpd_fit")) {
      x$outcome_fit$con <- sanitize_mixgpd_fit(x$outcome_fit$con)
    }
  }
  if ("ps_fit" %in% names(x) && is.list(x$ps_fit)) {
    x$ps_fit <- sanitize_mixgpd_fit(x$ps_fit)
  }
  x
}

prepare_for_cache <- function(x) {
  if (inherits(x, "mixgpd_fit")) {
    x <- sanitize_mixgpd_fit(x)
  }
  if (inherits(x, "causalmixgpd_causal_fit")) {
    x <- sanitize_causal_fit(x)
  }
  if (is.list(x)) {
    for (i in seq_along(x)) {
      x[i] <- list(prepare_for_cache(x[[i]]))
    }
  }
  x
}

save_cache_rds <- function(x, rel_path) {
  out <- cache_path(rel_path)
  write_rds_atomic(prepare_for_cache(x), out)
  out
}

mcmc_fixed <- list(
  niter = 200,
  nburnin = 50,
  thin = 1,
  nchains = 1,
  seed = 1
)

artifacts <- character()
track_artifact <- function(path) {
  artifacts <<- c(artifacts, path)
  invisible(path)
}

message("[1/6] Building overview cache artifacts")
data("nc_posX100_p3_k2", package = "CausalMixGPD")
dat <- data.frame(y = nc_posX100_p3_k2$y, nc_posX100_p3_k2$X)
track_artifact(save_cache_rds(list(dat = dat), file.path("tables", "overview_data.rds")))

fit <- dpmgpd(
  formula = y ~ x1 + x2 + x3,
  data = dat,
  backend = "sb",
  kernel = "lognormal",
  components = 5,
  mcmc = mcmc_fixed
)
track_artifact(save_cache_rds(fit, file.path("fits", "overview_fit.rds")))

overview_summary <- summary(fit)
p_hat <- params(fit)
track_artifact(save_cache_rds(
  list(overview_summary = overview_summary, p_hat = p_hat),
  file.path("tables", "overview_mcmc.rds")
))

q_grid <- c(0.50, 0.90, 0.95, 0.99)
x_eval <- dat[1:20, c("x1", "x2", "x3")]
est_quant <- predict(fit, newdata = x_eval, type = "quantile", index = q_grid,
                     interval = "hpd", level = 0.95)
track_artifact(save_cache_rds(
  list(q_grid = q_grid, x_eval = x_eval, est_quant = est_quant),
  file.path("tables", "overview_estimation_same_fit.rds")
))

q_levels <- c(0.25, 0.50, 0.75)
x_new <- as.data.frame(lapply(
  dat[, c("x1", "x2", "x3")],
  quantile,
  probs = q_levels,
  na.rm = TRUE
))
rownames(x_new) <- c("q25", "q50", "q75")
y_grid <- seq(0, 10, length.out = 200)
pdens <- predict(fit, newdata = x_new, y = y_grid, type = "density",
                 interval = "credible", level = 0.95)
psurv <- predict(fit, newdata = x_new, y = y_grid, type = "survival",
                 interval = "credible", level = 0.95)
p_grid <- c(0.50, 0.90, 0.95, 0.99)
pquant <- predict(fit, newdata = x_new, type = "quantile", index = p_grid,
                  interval = "hpd", level = 0.95)
track_artifact(save_cache_rds(
  list(
    q_levels = q_levels,
    x_new = x_new,
    y_grid = y_grid,
    pdens = pdens,
    psurv = psurv,
    p_grid = p_grid,
    pquant = pquant
  ),
  file.path("tables", "overview_predict.rds")
))

message("[2/6] Building theory figures")
track_artifact(write_png(cache_path("figs", "gpdDensityFigure.png"), {
  u <- 2.0
  y <- seq(u, u + 8, length.out = 500)
  sigma <- 1.2
  xi_vals <- c(-0.25, 0.0, 0.25)
  gpd_df <- do.call(
    rbind,
    lapply(xi_vals, function(xi) {
      data.frame(
        y = y,
        density = dgpd(y, threshold = u, scale = sigma, shape = xi),
        xi = factor(sprintf("ξ = %.2f", xi), levels = sprintf("ξ = %.2f", xi_vals))
      )
    })
  )
  gpd_df <- gpd_df[is.finite(gpd_df$density), , drop = FALSE]

  p <- ggplot(gpd_df, aes(x = y, y = density, color = xi)) +
    geom_line(linewidth = 0.9) +
    geom_vline(xintercept = u, linetype = "dashed", linewidth = 0.7, color = "gray35") +
    annotate("text", x = u + 0.05, y = max(gpd_df$density) * 0.95, label = "threshold u", hjust = 0) +
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    labs(
      title = "GPD Tail Density Above Threshold",
      x = "Outcome y",
      y = "Density",
      color = NULL
    ) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "top")
  print(p)
}))

track_artifact(write_png(cache_path("figs", "splicedDensityFigure.png"), {
  y <- seq(-3, 12, length.out = 800)
  u <- 2.0
  mu_bulk <- 0.0
  sd_bulk <- 1.1
  sigma_tail <- 1.0
  xi_tail <- 0.25

  f_bulk <- dnorm(y, mean = mu_bulk, sd = sd_bulk)
  p_u <- pnorm(u, mean = mu_bulk, sd = sd_bulk)

  excess <- y - u
  f_gpd <- rep(NA_real_, length(y))
  idx_tail <- which(y > u)
  f_gpd[idx_tail] <- dgpd(y[idx_tail], threshold = u, scale = sigma_tail, shape = xi_tail)

  f_spliced <- f_bulk
  f_spliced[idx_tail] <- (1 - p_u) * f_gpd[idx_tail]

  spliced_df <- rbind(
    data.frame(y = y, density = f_spliced, component = "Spliced density"),
    data.frame(y = y, density = f_bulk, component = "Bulk density"),
    data.frame(
      y = y[idx_tail],
      density = (1 - p_u) * f_gpd[idx_tail],
      component = sprintf("Tail density (ξ = %.2f)", xi_tail)
    )
  )
  p <- ggplot(spliced_df, aes(x = y, y = density, color = component, linetype = component)) +
    geom_line(linewidth = 0.9) +
    geom_vline(xintercept = u, linetype = "dashed", linewidth = 0.7, color = "gray35") +
    annotate("text", x = u + 0.05, y = max(spliced_df$density) * 0.95, label = "threshold u", hjust = 0) +
    scale_color_manual(values = c("#b2182b", "#2166ac", "#4d9221")) +
    scale_linetype_manual(values = c("solid", "dashed", "dotdash")) +
    labs(
      title = "Spliced Bulk-Tail Density",
      x = "Outcome y",
      y = "Density",
      color = NULL,
      linetype = NULL
    ) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "top")
  print(p)
}))

message("[3/6] Building overview causal-fit cache artifacts")
data("causal_pos500_p3_k2", package = "CausalMixGPD")
dat_causal <- causal_pos500_p3_k2
df <- data.frame(y = dat_causal$y, A = dat_causal$A, dat_causal$X)

cfit <- dpmgpd.causal(
  formula = y ~ x1 + x2 + x3,
  data = df,
  treat = "A",
  backend = "sb",
  kernel = "lognormal",
  components = 5,
  PS = "logit",
  ps_scale = "logit",
  ps_summary = "mean",
  mcmc_outcome = mcmc_fixed,
  mcmc_ps = mcmc_fixed
)
track_artifact(save_cache_rds(cfit, file.path("fits", "causal_fit.rds")))

ate_hat <- ate(cfit, level = 0.90, interval = "credible")
qte_hat <- qte(cfit, probs = c(0.50, 0.90, 0.95), level = 0.90, interval = "hpd")
Xnew <- df[1:5, c("x1", "x2", "x3")]
cate_hat <- cate(cfit, newdata = Xnew, type = "mean", level = 0.90)
cqte_hat <- cqte(cfit, newdata = Xnew, probs = c(0.50, 0.90), level = 0.90)
qs <- c(0.25, 0.50, 0.75)
Xgrid <- expand.grid(lapply(df[c("x1", "x2", "x3")], quantile, probs = qs))
pred_q <- predict(cfit, newdata = Xgrid, type = "quantile", p = c(0.50, 0.90))
pred_s <- predict(cfit, newdata = Xgrid, type = "survival", y = rep(4, nrow(Xgrid)))

track_artifact(save_cache_rds(
  list(
    ate_hat = ate_hat,
    qte_hat = qte_hat,
    Xnew = Xnew,
    cate_hat = cate_hat,
    cqte_hat = cqte_hat,
    qs = qs,
    Xgrid = Xgrid,
    pred_q = pred_q,
    pred_s = pred_s,
    pred_q_trt = attr(pred_q, "trt"),
    pred_q_con = attr(pred_q, "con")
  ),
  file.path("tables", "causal_fit_derived.rds")
))

message("[4/6] Building data analysis I cache artifacts")
data("Boston", package = "MASS")
dat <- Boston
stopifnot(all(dat$crim > 0))
set.seed(2026)
n_all <- nrow(dat)
n_train <- floor(0.8 * n_all)
train_idx <- sort(sample.int(n_all, size = n_train, replace = FALSE))
test_idx <- setdiff(seq_len(n_all), train_idx)
train_dat <- dat[train_idx, , drop = FALSE]
test_dat <- dat[test_idx, , drop = FALSE]

fit_clust <- dpmix(
  formula = crim ~ lstat + rm + nox,
  data = train_dat,
  backend = "crp",
  kernel = "normal",
  components = 5,
  mcmc = mcmc_fixed
)
track_artifact(save_cache_rds(
  list(
    dat = dat,
    train_idx = train_idx,
    test_idx = test_idx,
    train_dat = train_dat,
    test_dat = test_dat,
    fit_clust = fit_clust
  ),
  file.path("fits", "app_cluster_fit.rds")
))

cluster_alloc <- allocation(fit_clust)
train_dat$cluster <- factor(cluster_alloc$labels_train)
track_artifact(save_cache_rds(
  list(cluster_alloc = cluster_alloc, train_dat = train_dat),
  file.path("tables", "app_cluster_psm.rds")
))

newdata_cluster <- data.frame(
  y = test_dat$crim,
  lstat = test_dat$lstat,
  rm = test_dat$rm,
  nox = test_dat$nox
)
cluster_alloc_new <- allocation(fit_clust, newdata = newdata_cluster)
train_cluster_counts <- as.data.frame(table(cluster_alloc_new$labels_train))
new_cluster_counts <- as.data.frame(table(cluster_alloc_new$labels_new))
names(train_cluster_counts) <- c("cluster", "n_train")
names(new_cluster_counts) <- c("cluster", "n_new")
train_cluster_counts$cluster <- as.character(train_cluster_counts$cluster)
new_cluster_counts$cluster <- as.character(new_cluster_counts$cluster)

cluster_old_new_counts <- merge(
  train_cluster_counts,
  new_cluster_counts,
  by = "cluster",
  all = TRUE
)
cluster_old_new_counts[is.na(cluster_old_new_counts)] <- 0L
cluster_old_new_counts <- rbind(
  data.frame(sample = "old", cluster = cluster_old_new_counts$cluster, n = cluster_old_new_counts$n_train),
  data.frame(sample = "new", cluster = cluster_old_new_counts$cluster, n = cluster_old_new_counts$n_new)
)
cluster_old_new_counts$cluster <- factor(cluster_old_new_counts$cluster, levels = unique(cluster_old_new_counts$cluster))

train_cert <- apply(cluster_alloc_new$probs_train, 1, max)
new_cert <- apply(cluster_alloc_new$probs_new, 1, max)
cluster_certainty <- data.frame(
  sample = c("old", "new"),
  min = c(min(train_cert), min(new_cert)),
  q25 = c(as.numeric(quantile(train_cert, 0.25)), as.numeric(quantile(new_cert, 0.25))),
  median = c(median(train_cert), median(new_cert)),
  q75 = c(as.numeric(quantile(train_cert, 0.75)), as.numeric(quantile(new_cert, 0.75))),
  max = c(max(train_cert), max(new_cert))
)
track_artifact(save_cache_rds(
  list(
    newdata_cluster = newdata_cluster,
    cluster_alloc_new = cluster_alloc_new,
    train_cluster_counts = train_cluster_counts,
    new_cluster_counts = new_cluster_counts,
    cluster_old_new_counts = cluster_old_new_counts,
    cluster_certainty = cluster_certainty
  ),
  file.path("tables", "app_cluster_newdata_allocation.rds")
))

track_artifact(write_png(cache_path("figs", "app_cluster_old_new_barplot.png"), {
  p <- ggplot(cluster_old_new_counts, aes(x = cluster, y = n, fill = sample)) +
    geom_col(position = "dodge") +
    labs(
      title = "Cluster frequencies: old vs new",
      x = "Cluster",
      y = "Count",
      fill = "Sample"
    ) +
    theme_minimal(base_size = 16)
  print(p)
}))

message("[5/6] Building data analysis II cache artifacts")
data("causal_pos500_p3_k2", package = "CausalMixGPD")
dat_app2 <- causal_pos500_p3_k2
y <- dat_app2$y
A <- dat_app2$A
X <- dat_app2$X
covars <- colnames(X)
stopifnot(length(y) == nrow(X), length(A) == nrow(X))
track_artifact(save_cache_rds(
  list(y = y, A = A, X = X, covars = covars, dat_app2 = dat_app2),
  file.path("tables", "app2_setup.rds")
))

fit <- dpmgpd.causal(
  x = y,
  X = X,
  treat = A,
  backend = "sb",
  kernel = "normal",
  components = 4,
  mcmc = list(
    niter = 120,
    nburnin = 40,
    thin = 1,
    nchains = 1,
    seed = 101,
    show_progress = FALSE
  )
)
track_artifact(save_cache_rds(
  list(cache_version = 3L, fit = fit),
  file.path("fits", "app2_fit.rds")
))

ate_fit <- ate(fit)
taus <- c(0.25, 0.50, 0.75, 0.90, 0.95, 0.99)
qte_fit <- qte(fit, probs = taus)
ate_sum <- summary(ate_fit)
qte_sum <- summary(qte_fit)
track_artifact(save_cache_rds(
  list(
    cache_version = 3L,
    ate_fit = ate_fit,
    qte_fit = qte_fit,
    ate_sum = ate_sum,
    qte_sum = qte_sum,
    taus = taus
  ),
  file.path("tables", "app2_ate_qte.rds")
))

track_artifact(write_png(cache_path("figs", "app2_ate_effect.png"), {
  ate_effect_plot <- plot(ate_fit, type = "effect")
  print(ate_effect_plot)
}))
track_artifact(write_png(cache_path("figs", "app2_qte_effect.png"), {
  qte_effect_plot <- plot(qte_fit, type = "effect")
  print(qte_effect_plot)
}))

col_med <- apply(X, 2, median, na.rm = TRUE)
x_typical <- col_med
x_highprev <- col_med
profile_var <- if ("prevearn" %in% colnames(X)) "prevearn" else colnames(X)[1]
x_highprev[profile_var] <- as.numeric(quantile(X[, profile_var], 0.80, na.rm = TRUE))
xnew <- rbind(typical = x_typical, high_prev_earn = x_highprev)
track_artifact(save_cache_rds(
  list(
    col_med = col_med,
    x_typical = x_typical,
    x_highprev = x_highprev,
    profile_var = profile_var,
    xnew = xnew
  ),
  file.path("tables", "app2_xnew.rds")
))

cate_fit <- cate(fit, newdata = xnew)
cqte_fit <- cqte(fit, probs = taus, newdata = xnew)
cate_sum <- summary(cate_fit)
cqte_sum <- summary(cqte_fit)
track_artifact(save_cache_rds(
  list(
    cache_version = 4L,
    cate_fit = cate_fit,
    cqte_fit = cqte_fit,
    cate_sum = cate_sum,
    cqte_sum = cqte_sum
  ),
  file.path("tables", "app2_cate_cqte.rds")
))

track_artifact(write_png(cache_path("figs", "app2_cate_effect.png"), {
  cate_effect_plot <- plot(cate_fit, type = "effect")
  print(cate_effect_plot)
}))
track_artifact(write_png(cache_path("figs", "app2_cqte_effect.png"), {
  cqte_effect_plot <- plot(cqte_fit, type = "effect", facet_by = "id")
  print(cqte_effect_plot)
}))

trt_tail <- summary(fit$outcome_fit$trt, parameter = "tail_shape")$table
con_tail <- summary(fit$outcome_fit$con, parameter = "tail_shape")$table
xi_table <- rbind(
  data.frame(
    arm = "treated",
    trt_tail[trt_tail$parameter == "tail_shape", c("mean", "sd", "q0.025", "q0.500", "q0.975"), drop = FALSE]
  ),
  data.frame(
    arm = "control",
    con_tail[con_tail$parameter == "tail_shape", c("mean", "sd", "q0.025", "q0.500", "q0.975"), drop = FALSE]
  )
)
track_artifact(save_cache_rds(
  list(xi_table = xi_table),
  file.path("tables", "app2_tail.rds")
))

message("[6/6] Writing metadata")
track_artifact(write_text_atomic(capture.output(sessionInfo()), cache_path("meta", "sessionInfo.txt")))
manifest_paths <- unique(normalizePath(artifacts, winslash = "/", mustWork = FALSE))
manifest <- data.frame(
  path = sub(paste0("^", normalizePath(getwd(), winslash = "/", mustWork = TRUE), "/"), "", manifest_paths),
  bytes = as.numeric(file.info(manifest_paths)$size),
  modified = as.character(file.info(manifest_paths)$mtime),
  stringsAsFactors = FALSE
)
track_artifact(save_cache_rds(
  list(
    generated_at = as.character(Sys.time()),
    working_dir = normalizePath(getwd(), winslash = "/", mustWork = TRUE),
    files = manifest
  ),
  file.path("meta", "cache_manifest.rds")
))

message("Cache build complete")
message("Artifacts written under: ", normalizePath(CACHE_DIR, winslash = "/", mustWork = TRUE))
