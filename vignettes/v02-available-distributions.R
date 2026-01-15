## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  fig.width = 8,
  fig.height = 6,
  message = FALSE,
  warning = FALSE,
  eval = TRUE,
  tidy = FALSE,
  width = 80
)
library(DPmixGPD)
if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(quiet = TRUE)
library(ggplot2)
library(gridExtra)
set.seed(42)

## ----helpers------------------------------------------------------------------
# Helpers
q_vec <- function(fn, probs, ...) vapply(probs, function(p) fn(p, ...), numeric(1))
density_curve <- function(grid, fn, args) {
  vapply(grid, function(x) do.call(fn, c(list(x = x), args)), numeric(1))
}

draw_many <- function(fn, args, n_draws = 5) {
  if (!is.null(args$label)) args$label <- NULL
  vapply(seq_len(n_draws), function(i) do.call(fn, c(list(n = 1), args)), numeric(1))

}

## ----gpd-density--------------------------------------------------------------
dGpd(1.8, threshold = 1.5, scale = 0.5, shape = 0.2)
dGpd(1.8, threshold = 1.5, scale = 0.5, shape = 0.2, log = TRUE)

## ----gpd-cdf------------------------------------------------------------------
pGpd(1.8, threshold = 1.5, scale = 0.5, shape = 0.2)
pGpd(1.8, threshold = 1.5, scale = 0.5, shape = 0.2, lower.tail = FALSE)
pGpd(1.8, threshold = 1.5, scale = 0.5, shape = 0.2, log.p = TRUE)

## ----gpd-quantile-------------------------------------------------------------
q_vec(qGpd, c(0.25, 0.5, 0.75), threshold = 1.5, scale = 0.5, shape = 0.2)
q_vec(qGpd, c(0.25, 0.5, 0.75), threshold = 1.5, scale = 0.5, shape = 0.2,
      lower.tail = FALSE)
q_vec(qGpd, c(log(0.25), log(0.5), log(0.75)), threshold = 1.5, scale = 0.5,
      shape = 0.2, log.p = TRUE)

## ----gpd-random---------------------------------------------------------------
draw_many(rGpd, list(threshold = 1.5, scale = 0.5, shape = 0.2))

## ----gpd-plot-----------------------------------------------------------------
grid <- seq(-4, 15, length.out = 500)
gpd_sets <- list(
  list(label = "GPD A", threshold = 1.5, tail_scale = 0.5, tail_shape = 0.2),
  list(label = "GPD B", threshold = 2.0, tail_scale = 0.6, tail_shape = 0.15),
  list(label = "GPD C", threshold = 1.0, tail_scale = 0.4, tail_shape = 0.25)
)

df_gpd <- do.call(rbind, lapply(gpd_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dGpd, list(threshold = ps$threshold, scale = ps$tail_scale, shape = ps$tail_shape)), label = ps$label)
}))

ggplot(df_gpd, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Standalone GPD tails", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")

## ----normal-mix---------------------------------------------------------------
grid <- seq(-4, 5, length.out = 400)
normal_sets <- list(
  list(label = "Mix A", w = c(0.6, 0.3, 0.1), mean = c(-1, 0.5, 2), sd = c(2, 0.6, 1.1)),
  list(label = "Mix B", w = c(0.5, 0.3, 0.2), mean = c(-1.2, 0.3, 1.5), sd = c(0.9, 0.7, 1.0)),
  list(label = "Mix C", w = c(0.4, 0.35, 0.25), mean = c(-0.5, 2, 2.5), sd = c(0.7, 0.6, 1.2))
)

example <- normal_sets[[1]]

## ----normal-mix-d-------------------------------------------------------------
dNormMix(0, w = example$w, mean = example$mean, sd = example$sd)
dNormMix(0, w = example$w, mean = example$mean, sd = example$sd, log = TRUE)

## ----normal-mix-p-------------------------------------------------------------
pNormMix(0, w = example$w, mean = example$mean, sd = example$sd)
pNormMix(0, w = example$w, mean = example$mean, sd = example$sd, lower.tail = FALSE)
pNormMix(0, w = example$w, mean = example$mean, sd = example$sd, log.p = TRUE)

## ----normal-mix-q-------------------------------------------------------------
q_vec(qNormMix, c(0.25, 0.5, 0.75), w = example$w, mean = example$mean,
      sd = example$sd)
q_vec(qNormMix, c(0.25, 0.5, 0.75), w = example$w, mean = example$mean,
      sd = example$sd, lower.tail = FALSE)
q_vec(qNormMix, c(log(0.25), log(0.5), log(0.75)), w = example$w,
      mean = example$mean, sd = example$sd, log.p = TRUE)

## ----normal-mix-r-------------------------------------------------------------
draw_many(rNormMix, list(w = example$w, mean = example$mean, sd = example$sd))

## ----normal-mix-plot----------------------------------------------------------
df_norm <- do.call(rbind, lapply(normal_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dNormMix, list(w = ps$w, mean = ps$mean, sd = ps$sd)), label = ps$label)
}))

ggplot(df_norm, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Normal mixtures (bulk)", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")

## ----normal-gpd---------------------------------------------------------------
normal_gpd_sets <- list(
  list(label = "Mix A", w = c(0.6, 0.4), mean = c(-1, 2), sd = c(0.5, 0.8), threshold = 1.8, tail_scale = 0.4, tail_shape = 0.25),
  list(label = "Mix B", w = c(0.5, 0.5), mean = c(0, 1), sd = c(0.6, 0.6), threshold = 1.5, tail_scale = 0.3, tail_shape = 0.2),
  list(label = "Mix C", w = c(0.7, 0.3), mean = c(0.5, 2.5), sd = c(0.4, 1.0), threshold = 2.0, tail_scale = 0.5, tail_shape = 0.15)
)

example <- normal_gpd_sets[[1]]

## ----normal-gpd-d-------------------------------------------------------------
dNormMixGpd(2, w = example$w, mean = example$mean, sd = example$sd, threshold = example$threshold, tail_scale = example$tail_scale, tail_shape = example$tail_shape)

