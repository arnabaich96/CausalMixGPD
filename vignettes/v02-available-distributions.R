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

## ----normal-gpd-p-------------------------------------------------------------
pNormMixGpd(2, w = example$w, mean = example$mean, sd = example$sd, threshold = example$threshold, tail_scale = example$tail_scale, tail_shape = example$tail_shape)

## ----normal-gpd-q-------------------------------------------------------------
q_vec(qNormMixGpd, c(0.5, 0.9), w = example$w, mean = example$mean, sd = example$sd, threshold = example$threshold, tail_scale = example$tail_scale, tail_shape = example$tail_shape)

## ----normal-gpd-r-------------------------------------------------------------
draw_many(rNormMixGpd, example)

## ----normal-gpd-plot----------------------------------------------------------
df_norm_gpd <- do.call(rbind, lapply(normal_gpd_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dNormMixGpd, list(w = ps$w, mean = ps$mean, sd = ps$sd, threshold = ps$threshold, tail_scale = ps$tail_scale, tail_shape = ps$tail_shape)), label = ps$label)
}))

ggplot(df_norm_gpd, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Normal mixtures with GPD tail (different thresholds)", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")

## ----gamma-mix----------------------------------------------------------------
grid <- seq(0, 10, length.out = 400)
gamma_sets <- list(
  list(label = "Mix A", w = c(0.6, 0.3, 0.1), shape = c(2.0, 5.0, 9.0), scale = c(1.0, 0.6, 0.3)),
  list(label = "Mix B", w = c(0.5, 0.3, 0.2), shape = c(1.5, 4.0, 7.0), scale = c(1.2, 0.7, 0.35)),
  list(label = "Mix C", w = c(0.4, 0.3, 0.3), shape = c(1.2, 3.5, 6.0), scale = c(1.4, 0.75, 0.4))
)
example <- gamma_sets[[1]]

dens <- do.call(rbind, lapply(gamma_sets, function(s) {
  data.frame(
    x = grid,
    density = dGammaMix(grid, w = s$w, shape = s$shape, scale = s$scale),
    label = s$label
  )
}))

## ----gamma-mix-d--------------------------------------------------------------
dGammaMix(2, w = example$w, shape = example$shape, scale = example$scale)
dGammaMix(2, w = example$w, shape = example$shape, scale = example$scale, log = TRUE)

## ----gamma-mix-p--------------------------------------------------------------
pGammaMix(2, w = example$w, shape = example$shape, scale = example$scale)
pGammaMix(2, w = example$w, shape = example$shape, scale = example$scale, lower.tail = FALSE)
pGammaMix(2, w = example$w, shape = example$shape, scale = example$scale, log.p = TRUE)

## ----gamma-mix-q--------------------------------------------------------------
qGammaMix(0.95, w = example$w, shape = example$shape, scale = example$scale)
qGammaMix(0.95, w = example$w, shape = example$shape, scale = example$scale, lower.tail = FALSE)

## ----gamma-mix-r--------------------------------------------------------------
df_gamma <- do.call(rbind, lapply(gamma_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dGammaMix, list(w = ps$w, shape = ps$shape, scale = ps$scale)), label = ps$label)
}))

ggplot(df_norm, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Gamma mixtures (bulk)", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")

## ----gamma-gpd----------------------------------------------------------------
u <- 6
tail_scale <- 1.0
tail_shape <- 0.2

## ----gamma-gpd-d--------------------------------------------------------------
dGammaMixGpd(6.5, w = example$w, shape = example$shape, scale = example$scale,
             threshold = u, tail_scale = tail_scale, tail_shape = tail_shape)
dGammaMixGpd(6.5, w = example$w, shape = example$shape, scale = example$scale,
             threshold = u, tail_scale = tail_scale, tail_shape = tail_shape, log = TRUE)

## ----gamma-gpd-p--------------------------------------------------------------
pGammaMixGpd(6.5, w = example$w, shape = example$shape, scale = example$scale,
             threshold = u, tail_scale = tail_scale, tail_shape = tail_shape)
pGammaMixGpd(6.5, w = example$w, shape = example$shape, scale = example$scale,
             threshold = u, tail_scale = tail_scale, tail_shape = tail_shape, lower.tail = FALSE)

## ----gamma-gpd-q--------------------------------------------------------------
qGammaMixGpd(0.95, w = example$w, shape = example$shape, scale = example$scale,
             threshold = u, tail_scale = tail_scale, tail_shape = tail_shape)

## ----gamma-gpd-r--------------------------------------------------------------
gamma_gpd_sets <- list(
  list(label = "Mix A", w = c(0.6, 0.4), shape = c(2.0, 5.0), scale = c(1.0, 0.6), threshold = 6.0, tail_scale = 1.0, tail_shape = 0.2),
  list(label = "Mix B", w = c(0.5, 0.5), shape = c(1.5, 4.0), scale = c(1.2, 0.7), threshold = 5.5, tail_scale = 0.8, tail_shape = 0.15),
  list(label = "Mix C", w = c(0.7, 0.3), shape = c(1.2, 3.5), scale = c(1.4, 0.75), threshold = 6.5, tail_scale = 1.2, tail_shape = 0.25)
)



df_gamma_gpd <- do.call(rbind, lapply(gamma_gpd_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dGammaMixGpd, list(w = ps$w, shape = ps$shape, scale = ps$scale, threshold = ps$threshold, tail_scale = ps$tail_scale, tail_shape = ps$tail_shape)), label = ps$label)
}))

ggplot(df_norm_gpd, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Gamma mixtures with GPD tail (different thresholds)", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")

## ----lognormal-mix------------------------------------------------------------
grid <- seq(0, 8, length.out = 400)
logn_sets <- list(
  list(label = "Mix A", w = c(0.6, 0.3, 0.1), meanlog = c(0.0, 0.3, 0.6), sdlog = c(0.4, 0.5, 0.6)),
  list(label = "Mix B", w = c(0.5, 0.3, 0.2), meanlog = c(0.1, 0.4, 0.7), sdlog = c(0.35, 0.45, 0.55)),
  list(label = "Mix C", w = c(0.4, 0.35, 0.25), meanlog = c(0.2, 0.5, 2), sdlog = c(0.3, 0.4, 0.5))
)

example <- logn_sets[[1]]

## ----lognormal-mix-d----------------------------------------------------------
dLognormalMix(1, w = example$w, meanlog = example$meanlog, sdlog = example$sdlog)
dLognormalMix(1, w = example$w, meanlog = example$meanlog, sdlog = example$sdlog, log = TRUE)

## ----lognormal-mix-p----------------------------------------------------------
pLognormalMix(1, w = example$w, meanlog = example$meanlog, sdlog = example$sdlog)
pLognormalMix(1, w = example$w, meanlog = example$meanlog, sdlog = example$sdlog, lower.tail = FALSE)
pLognormalMix(1, w = example$w, meanlog = example$meanlog, sdlog = example$sdlog, log.p = TRUE)

## ----lognormal-mix-q----------------------------------------------------------
q_vec(qLognormalMix, c(0.25, 0.5, 0.75), w = example$w,
      meanlog = example$meanlog, sdlog = example$sdlog)
q_vec(qLognormalMix, c(0.25, 0.5, 0.75), w = example$w,
      meanlog = example$meanlog, sdlog = example$sdlog, lower.tail = FALSE)
q_vec(qLognormalMix, c(log(0.25), log(0.5), log(0.75)), w = example$w,
      meanlog = example$meanlog, sdlog = example$sdlog, log.p = TRUE)

## ----lognormal-mix-r----------------------------------------------------------
draw_many(rLognormalMix, list(w = example$w, meanlog = example$meanlog, sdlog = example$sdlog))

## ----lognormal-mix-plot-------------------------------------------------------
df_logn <- do.call(rbind, lapply(logn_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dLognormalMix, list(w = ps$w, meanlog = ps$meanlog, sdlog = ps$sdlog)), label = ps$label)
}))

ggplot(df_logn, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Lognormal mixtures (bulk)", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")

## ----lognormal-gpd------------------------------------------------------------
logn_gpd_sets <- list(
  list(label = "Mix A", w = c(0.6, 0.4), meanlog = c(0, 1), sdlog = c(0.3, 0.5), threshold = 2.5, tail_scale = 0.5, tail_shape = 0.2),
  list(label = "Mix B", w = c(0.5, 0.5), meanlog = c(0.5, 1.2), sdlog = c(0.4, 0.4), threshold = 2.0, tail_scale = 0.4, tail_shape = 0.15),
  list(label = "Mix C", w = c(0.7, 0.3), meanlog = c(0.8, 1.5), sdlog = c(0.25, 0.6), threshold = 3.0, tail_scale = 0.6, tail_shape = 0.18)
)

example <- logn_gpd_sets[[1]]

## ----lognormal-gpd-d----------------------------------------------------------
dLognormalMixGpd(2.5, w = example$w, meanlog = example$meanlog, sdlog = example$sdlog, threshold = example$threshold, tail_scale = example$tail_scale, tail_shape = example$tail_shape)

## ----lognormal-gpd-p----------------------------------------------------------
pLognormalMixGpd(2.5, w = example$w, meanlog = example$meanlog, sdlog = example$sdlog, threshold = example$threshold, tail_scale = example$tail_scale, tail_shape = example$tail_shape)

## ----lognormal-gpd-q----------------------------------------------------------
q_vec(qLognormalMixGpd, c(0.5, 0.9), w = example$w, meanlog = example$meanlog, sdlog = example$sdlog, threshold = example$threshold, tail_scale = example$tail_scale, tail_shape = example$tail_shape)

## ----lognormal-gpd-r----------------------------------------------------------
draw_many(rLognormalMixGpd, example)

## ----lognormal-gpd-plot-------------------------------------------------------
df_logn_gpd <- do.call(rbind, lapply(logn_gpd_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dLognormalMixGpd, list(w = ps$w, meanlog = ps$meanlog, sdlog = ps$sdlog, threshold = ps$threshold, tail_scale = ps$tail_scale, tail_shape = ps$tail_shape)), label = ps$label)
}))

ggplot(df_logn_gpd, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Lognormal mixtures with GPD tail (different thresholds)", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")

## ----laplace-mix--------------------------------------------------------------
grid <- seq(-4, 4, length.out = 400)
lap_sets <- list(
  list(label = "Mix A", w = c(0.6, 0.3, 0.1), location = c(0.0, 1.0, -2), scale = c(1.0, 0.9, 1.1)),
  list(label = "Mix B", w = c(0.5, 0.3, 0.2), location = c(0.2, 1.2, -0.5), scale = c(0.9, 2, 1.0)),
  list(label = "Mix C", w = c(0.4, 0.35, 0.25), location = c(-0.2, 2, -1.0), scale = c(1.1, 0.95, 1.05))
)

example <- lap_sets[[1]]

## ----laplace-mix-d------------------------------------------------------------
dLaplaceMix(0, w = example$w, location = example$location, scale = example$scale)
dLaplaceMix(0, w = example$w, location = example$location, scale = example$scale, log = TRUE)

## ----laplace-mix-p------------------------------------------------------------
pLaplaceMix(0, w = example$w, location = example$location, scale = example$scale)
pLaplaceMix(0, w = example$w, location = example$location, scale = example$scale, lower.tail = FALSE)
pLaplaceMix(0, w = example$w, location = example$location, scale = example$scale, log.p = TRUE)

## ----laplace-mix-q------------------------------------------------------------
q_vec(qLaplaceMix, c(0.25, 0.5, 0.75), w = example$w,
      location = example$location, scale = example$scale)
q_vec(qLaplaceMix, c(0.25, 0.5, 0.75), w = example$w,
      location = example$location, scale = example$scale, lower.tail = FALSE)
q_vec(qLaplaceMix, c(log(0.25), log(0.5), log(0.75)), w = example$w,
      location = example$location, scale = example$scale, log.p = TRUE)

## ----laplace-mix-r------------------------------------------------------------
draw_many(rLaplaceMix, list(w = example$w, location = example$location, scale = example$scale))

## ----laplace-mix-plot---------------------------------------------------------
df_lap <- do.call(rbind, lapply(lap_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dLaplaceMix, list(w = ps$w, location = ps$location, scale = ps$scale)), label = ps$label)
}))

ggplot(df_lap, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Laplace mixtures (bulk)", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")

## ----laplace-gpd--------------------------------------------------------------
lap_gpd_sets <- list(
  list(label = "Mix A", w = c(0.6, 0.4), location = c(-0.5, 1), scale = c(0.4, 0.6), threshold = 1.5, tail_scale = 0.35, tail_shape = 0.3),
  list(label = "Mix B", w = c(0.5, 0.5), location = c(0, 0.8), scale = c(0.5, 0.5), threshold = 1.2, tail_scale = 0.3, tail_shape = 0.25),
  list(label = "Mix C", w = c(0.7, 0.3), location = c(0.2, 1.5), scale = c(0.35, 0.7), threshold = 1.8, tail_scale = 0.4, tail_shape = 0.22)
)

example <- lap_gpd_sets[[1]]

## ----laplace-gpd-d------------------------------------------------------------
dLaplaceMixGpd(1, w = example$w, location = example$location, scale = example$scale, threshold = example$threshold, tail_scale = example$tail_scale, tail_shape = example$tail_shape)

## ----laplace-gpd-p------------------------------------------------------------
pLaplaceMixGpd(1, w = example$w, location = example$location, scale = example$scale, threshold = example$threshold, tail_scale = example$tail_scale, tail_shape = example$tail_shape)

## ----laplace-gpd-q------------------------------------------------------------
q_vec(qLaplaceMixGpd, c(0.5, 0.9), w = example$w, location = example$location, scale = example$scale, threshold = example$threshold, tail_scale = example$tail_scale, tail_shape = example$tail_shape)

## ----laplace-gpd-r------------------------------------------------------------
draw_many(rLaplaceMixGpd, example)

## ----laplace-gpd-plot---------------------------------------------------------
df_lap_gpd <- do.call(rbind, lapply(lap_gpd_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dLaplaceMixGpd, list(w = ps$w, location = ps$location, scale = ps$scale, threshold = ps$threshold, tail_scale = ps$tail_scale, tail_shape = ps$tail_shape)), label = ps$label)
}))

ggplot(df_lap_gpd, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Laplace mixtures with GPD tail (different thresholds)", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")

## ----invgauss-mix-------------------------------------------------------------
grid <- seq(0.1, 6, length.out = 400)
ig_sets <- list(
  list(label = "Mix A", w = c(0.6, 0.3, 0.1), mean = c(1.0, 1.5, 2.0), shape = c(2.0, 3.0, 4.0)),
  list(label = "Mix B", w = c(0.5, 0.3, 0.2), mean = c(1.1, 1.6, 2.2), shape = c(2.2, 3.2, 4.2)),
  list(label = "Mix C", w = c(0.4, 0.35, 0.25), mean = c(0.9, 1.4, 2.1), shape = c(1.8, 2.8, 3.8))
)

example <- ig_sets[[1]]

## ----invgauss-mix-d-----------------------------------------------------------
dInvGaussMix(1, w = example$w, mean = example$mean, shape = example$shape)
dInvGaussMix(1.5, w = example$w, mean = example$mean, shape = example$shape, log = TRUE)

## ----invgauss-mix-p-----------------------------------------------------------
pInvGaussMix(1.5, w = example$w, mean = example$mean, shape = example$shape)
pInvGaussMix(1.5, w = example$w, mean = example$mean, shape = example$shape, lower.tail = FALSE)
pInvGaussMix(1.5, w = example$w, mean = example$mean, shape = example$shape, log.p = TRUE)

## ----invgauss-mix-q-----------------------------------------------------------
q_vec(qInvGaussMix, c(0.25, 0.5, 0.75), w = example$w, mean = example$mean,
      shape = example$shape)
q_vec(qInvGaussMix, c(0.25, 0.5, 0.75), w = example$w, mean = example$mean,
      shape = example$shape, lower.tail = FALSE)
q_vec(qInvGaussMix, c(log(0.25), log(0.5), log(0.75)), w = example$w,
      mean = example$mean, shape = example$shape, log.p = TRUE)

## ----invgauss-mix-r-----------------------------------------------------------
draw_many(rInvGaussMix, list(w = example$w, mean = example$mean, shape = example$shape))

## ----invgauss-mix-plot--------------------------------------------------------
df_ig <- do.call(rbind, lapply(ig_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dInvGaussMix, list(w = ps$w, mean = ps$mean, shape = ps$shape)), label = ps$label)
}))

ggplot(df_ig, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Inverse Gaussian mixtures (bulk)", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")

## ----invgauss-gpd-------------------------------------------------------------
ig_gpd_sets <- list(
  list(label = "Mix A", w = c(0.6, 0.4), mean = c(1, 2.5), shape = c(2, 3), threshold = 2.5, tail_scale = 0.5, tail_shape = 0.25),
  list(label = "Mix B", w = c(0.5, 0.5), mean = c(1.5, 2), shape = c(2.5, 2.5), threshold = 2.0, tail_scale = 0.4, tail_shape = 0.2),
  list(label = "Mix C", w = c(0.7, 0.3), mean = c(1.2, 3), shape = c(3, 2), threshold = 3.0, tail_scale = 0.6, tail_shape = 0.18)
)

example <- ig_gpd_sets[[1]]

## ----invgauss-gpd-d-----------------------------------------------------------
dInvGaussMixGpd(2.5, w = example$w, mean = example$mean, shape = example$shape, threshold = example$threshold, tail_scale = example$tail_scale, tail_shape = example$tail_shape)

## ----invgauss-gpd-p-----------------------------------------------------------
pInvGaussMixGpd(2.5, w = example$w, mean = example$mean, shape = example$shape, threshold = example$threshold, tail_scale = example$tail_scale, tail_shape = example$tail_shape)

## ----invgauss-gpd-q-----------------------------------------------------------
q_vec(qInvGaussMixGpd, c(0.5, 0.9), w = example$w, mean = example$mean, shape = example$shape, threshold = example$threshold, tail_scale = example$tail_scale, tail_shape = example$tail_shape)

## ----invgauss-gpd-r-----------------------------------------------------------
draw_many(rInvGaussMixGpd, example)

## ----invgauss-gpd-plot--------------------------------------------------------
df_ig_gpd <- do.call(rbind, lapply(ig_gpd_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dInvGaussMixGpd, list(w = ps$w, mean = ps$mean, shape = ps$shape, threshold = ps$threshold, tail_scale = ps$tail_scale, tail_shape = ps$tail_shape)), label = ps$label)
}))

ggplot(df_ig_gpd, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Inverse Gaussian mixtures with GPD tail", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")

## ----amoroso-mix--------------------------------------------------------------
grid <- seq(0.01, 6, length.out = 400)
amor_sets <- list(
  list(label = "Mix A", w = c(0.6, 0.3, 0.1), loc = c(0.5, 0.5, 0.5), scale = c(1.0, 1.3, 1.6), shape1 = c(2.5, 3.0, 4.0), shape2 = c(1.2, 1.2, 1.2)),
  list(label = "Mix B", w = c(0.5, 0.3, 0.2), loc = c(0.4, 0.6, 0.6), scale = c(1.1, 1.2, 1.5), shape1 = c(2.2, 2.8, 3.8), shape2 = c(1.1, 1.2, 1.3)),
  list(label = "Mix C", w = c(0.4, 0.35, 0.25), loc = c(0.6, 0.6, 0.6), scale = c(0.9, 1.1, 1.4), shape1 = c(2.0, 2.6, 3.5), shape2 = c(1.0, 1.1, 1.2))
)

example <- amor_sets[[1]]

## ----amoroso-mix-d------------------------------------------------------------
dAmorosoMix(2, w = example$w, loc = example$loc, scale = example$scale,
            shape1 = example$shape1, shape2 = example$shape2)
dAmorosoMix(2, w = example$w, loc = example$loc, scale = example$scale,
            shape1 = example$shape1, shape2 = example$shape2, log = TRUE)

## ----amoroso-mix-p------------------------------------------------------------
pAmorosoMix(2, w = example$w, loc = example$loc, scale = example$scale,
            shape1 = example$shape1, shape2 = example$shape2)
pAmorosoMix(2, w = example$w, loc = example$loc, scale = example$scale,
            shape1 = example$shape1, shape2 = example$shape2, lower.tail = FALSE)
pAmorosoMix(2, w = example$w, loc = example$loc, scale = example$scale,
            shape1 = example$shape1, shape2 = example$shape2, log.p = TRUE)

## ----amoroso-mix-q------------------------------------------------------------
q_vec(qAmorosoMix, c(0.25, 0.5, 0.75), w = example$w, loc = example$loc,
      scale = example$scale, shape1 = example$shape1, shape2 = example$shape2)
q_vec(qAmorosoMix, c(0.25, 0.5, 0.75), w = example$w, loc = example$loc,
      scale = example$scale, shape1 = example$shape1, shape2 = example$shape2,
      lower.tail = FALSE)
q_vec(qAmorosoMix, c(log(0.25), log(0.5), log(0.75)), w = example$w,
      loc = example$loc, scale = example$scale, shape1 = example$shape1,
      shape2 = example$shape2, log.p = TRUE)

## ----amoroso-mix-r------------------------------------------------------------
draw_many(rAmorosoMix, list(w = example$w, loc = example$loc,
                            scale = example$scale, shape1 = example$shape1,
                            shape2 = example$shape2))

## ----amoroso-mix-plot---------------------------------------------------------
df_amor <- do.call(rbind, lapply(amor_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dAmorosoMix, list(w = ps$w, loc = ps$loc, scale = ps$scale, shape1 = ps$shape1, shape2 = ps$shape2)), label = ps$label)
}))

ggplot(df_amor, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Amoroso mixtures (bulk)", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")

## ----amoroso-gpd--------------------------------------------------------------
amor_gpd_sets <- list(
  list(label = "Mix A", w = c(0.6, 0.3, 0.1), loc = c(0.5, 0.5, 0.5), scale = c(1.0, 1.3, 1.6), shape1 = c(2.5, 3.0, 4.0), shape2 = c(1.2, 1.2, 1.2), threshold = 2.8, tail_scale = 0.4, tail_shape = 0.2),
  list(label = "Mix B", w = c(0.5, 0.3, 0.2), loc = c(0.4, 0.6, 0.6), scale = c(1.1, 1.2, 1.5), shape1 = c(2.2, 2.8, 3.8), shape2 = c(1.1, 1.2, 1.3), threshold = 3.0, tail_scale = 0.35, tail_shape = 0.18),
  list(label = "Mix C", w = c(0.4, 0.35, 0.25), loc = c(0.6, 0.6, 0.6), scale = c(0.9, 1.1, 1.4), shape1 = c(2.0, 2.6, 3.5), shape2 = c(1.0, 1.1, 1.2), threshold = 2.5, tail_scale = 0.45, tail_shape = 0.22)
)
example <- amor_gpd_sets[[1]]

## ----amoroso-gpd-d------------------------------------------------------------
dAmorosoMixGpd(3, w = example$w, loc = example$loc, scale = example$scale,
               shape1 = example$shape1, shape2 = example$shape2,
               threshold = example$threshold, tail_scale = example$tail_scale,
               tail_shape = example$tail_shape)
dAmorosoMixGpd(3, w = example$w, loc = example$loc, scale = example$scale,
               shape1 = example$shape1, shape2 = example$shape2,
               threshold = example$threshold, tail_scale = example$tail_scale,
               tail_shape = example$tail_shape, log = TRUE)

## ----amoroso-gpd-p------------------------------------------------------------
pAmorosoMixGpd(3, w = example$w, loc = example$loc, scale = example$scale,
               shape1 = example$shape1, shape2 = example$shape2,
               threshold = example$threshold, tail_scale = example$tail_scale,
               tail_shape = example$tail_shape)
pAmorosoMixGpd(3, w = example$w, loc = example$loc, scale = example$scale,
               shape1 = example$shape1, shape2 = example$shape2,
               threshold = example$threshold, tail_scale = example$tail_scale,
               tail_shape = example$tail_shape, lower.tail = FALSE)
pAmorosoMixGpd(3, w = example$w, loc = example$loc, scale = example$scale,
               shape1 = example$shape1, shape2 = example$shape2,
               threshold = example$threshold, tail_scale = example$tail_scale,
               tail_shape = example$tail_shape, log.p = TRUE)

## ----amoroso-gpd-q------------------------------------------------------------
q_vec(qAmorosoMixGpd, c(0.25, 0.5, 0.75), w = example$w, loc = example$loc,
      scale = example$scale, shape1 = example$shape1, shape2 = example$shape2,
      threshold = example$threshold, tail_scale = example$tail_scale,
      tail_shape = example$tail_shape)
q_vec(qAmorosoMixGpd, c(0.25, 0.5, 0.75), w = example$w, loc = example$loc,
      scale = example$scale, shape1 = example$shape1, shape2 = example$shape2,
      threshold = example$threshold, tail_scale = example$tail_scale,
      tail_shape = example$tail_shape, lower.tail = FALSE)
q_vec(qAmorosoMixGpd, c(log(0.25), log(0.5), log(0.75)), w = example$w,
      loc = example$loc, scale = example$scale, shape1 = example$shape1,
      shape2 = example$shape2, threshold = example$threshold,
      tail_scale = example$tail_scale, tail_shape = example$tail_shape,
      log.p = TRUE)

## ----amoroso-gpd-r------------------------------------------------------------
draw_many(rAmorosoMixGpd, example)

## ----amoroso-gpd-plot---------------------------------------------------------
df_amor_gpd <- do.call(rbind, lapply(amor_gpd_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dAmorosoMixGpd, list(w = ps$w, loc = ps$loc, scale = ps$scale, shape1 = ps$shape1, shape2 = ps$shape2, threshold = ps$threshold, tail_scale = ps$tail_scale, tail_shape = ps$tail_shape)), label = ps$label)
}))

ggplot(df_amor_gpd, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Amoroso mixtures with GPD tail", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")

## ----invgauss-base------------------------------------------------------------
grid <- seq(0.1, 6, length.out = 400)
ig_base_sets <- list(
  list(label = "Base A", mean = 1.2, shape = 3.0),
  list(label = "Base B", mean = 1.5, shape = 4.0),
  list(label = "Base C", mean = 1.0, shape = 2.5)
)

example <- ig_base_sets[[1]]

## ----invgauss-base-d----------------------------------------------------------
dInvGauss(1, mean = example$mean, shape = example$shape)
dInvGauss(1, mean = example$mean, shape = example$shape, log = TRUE)

## ----invgauss-base-p----------------------------------------------------------
pInvGauss(1, mean = example$mean, shape = example$shape)
pInvGauss(1, mean = example$mean, shape = example$shape, lower.tail = FALSE)
pInvGauss(1, mean = example$mean, shape = example$shape, log.p = TRUE)

## ----invgauss-base-q----------------------------------------------------------
q_vec(qInvGauss, c(0.25, 0.5, 0.75), mean = example$mean,
      shape = example$shape)
q_vec(qInvGauss, c(0.25, 0.5, 0.75), mean = example$mean,
      shape = example$shape, lower.tail = FALSE)
q_vec(qInvGauss, c(log(0.25), log(0.5), log(0.75)), mean = example$mean,
      shape = example$shape, log.p = TRUE)

## ----invgauss-base-r----------------------------------------------------------
draw_many(rInvGauss, list(mean = example$mean, shape = example$shape))

## ----invgauss-base-plot-------------------------------------------------------
df_ig_base <- do.call(rbind, lapply(ig_base_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dInvGauss, list(mean = ps$mean, shape = ps$shape)), label = ps$label)
}))

ggplot(df_ig_base, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Inverse Gaussian base kernels", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")

## ----invgauss-base-gpd--------------------------------------------------------
ig_base_gpd_sets <- list(
  list(label = "Base A", mean = 1.5, shape = 2, threshold = 2.0, tail_scale = 0.4, tail_shape = 0.2),
  list(label = "Base B", mean = 1.2, shape = 2.5, threshold = 1.8, tail_scale = 0.35, tail_shape = 0.18),
  list(label = "Base C", mean = 1.8, shape = 3, threshold = 2.5, tail_scale = 0.5, tail_shape = 0.22)
)
example <- ig_base_gpd_sets[[1]]

## ----invgauss-base-gpd-d------------------------------------------------------
dInvGaussGpd(2, mean = example$mean, shape = example$shape,
             threshold = example$threshold, tail_scale = example$tail_scale,
             tail_shape = example$tail_shape)
dInvGaussGpd(2, mean = example$mean, shape = example$shape,
             threshold = example$threshold, tail_scale = example$tail_scale,
             tail_shape = example$tail_shape, log = TRUE)

## ----invgauss-base-gpd-p------------------------------------------------------
pInvGaussGpd(2, mean = example$mean, shape = example$shape,
             threshold = example$threshold, tail_scale = example$tail_scale,
             tail_shape = example$tail_shape)
pInvGaussGpd(2, mean = example$mean, shape = example$shape,
             threshold = example$threshold, tail_scale = example$tail_scale,
             tail_shape = example$tail_shape, lower.tail = FALSE)
pInvGaussGpd(2, mean = example$mean, shape = example$shape,
             threshold = example$threshold, tail_scale = example$tail_scale,
             tail_shape = example$tail_shape, log.p = TRUE)

## ----invgauss-base-gpd-q------------------------------------------------------
q_vec(qInvGaussGpd, c(0.25, 0.5, 0.75), mean = example$mean,
      shape = example$shape, threshold = example$threshold,
      tail_scale = example$tail_scale, tail_shape = example$tail_shape)
q_vec(qInvGaussGpd, c(0.25, 0.5, 0.75), mean = example$mean,
      shape = example$shape, threshold = example$threshold,
      tail_scale = example$tail_scale, tail_shape = example$tail_shape,
      lower.tail = FALSE)
q_vec(qInvGaussGpd, c(log(0.25), log(0.5), log(0.75)), mean = example$mean,
      shape = example$shape, threshold = example$threshold,
      tail_scale = example$tail_scale, tail_shape = example$tail_shape,
      log.p = TRUE)

## ----invgauss-base-gpd-r------------------------------------------------------
draw_many(rInvGaussGpd, example)

## ----invgauss-base-gpd-plot---------------------------------------------------
df_ig_base_gpd <- do.call(rbind, lapply(ig_base_gpd_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dInvGaussGpd, list(mean = ps$mean, shape = ps$shape, threshold = ps$threshold, tail_scale = ps$tail_scale, tail_shape = ps$tail_shape)), label = ps$label)
}))

ggplot(df_ig_base_gpd, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Inverse Gaussian base with GPD tail", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")

## ----cauchy-base--------------------------------------------------------------
grid <- seq(-8, 8, length.out = 400)
cauchy_sets <- list(
  list(label = "Base", location = 0, scale = 1),
  list(label = "Mix A", w = c(0.6, 0.3, 0.1), location = c(-1, 0, 1), scale = c(1.0, 1.2, 2)),
  list(label = "Mix B", w = c(0.5, 0.3, 0.2), location = c(-1.5, 0.5, 1.5), scale = c(1.1, 1.0, 0.9))
)

base_par <- cauchy_sets[[1]]
mix_par1 <- cauchy_sets[[2]]

## ----cauchy-d-----------------------------------------------------------------
dCauchy(0, location = base_par$location, scale = base_par$scale)
dCauchy(0, location = base_par$location, scale = base_par$scale, log = TRUE)

## ----cauchy-p-----------------------------------------------------------------
pCauchy(0, location = base_par$location, scale = base_par$scale)
pCauchy(0, location = base_par$location, scale = base_par$scale, lower.tail = FALSE)
pCauchy(0, location = base_par$location, scale = base_par$scale, log.p = TRUE)

## ----cauchy-q-----------------------------------------------------------------
q_vec(qCauchy, c(0.25, 0.5, 0.75), location = base_par$location,
      scale = base_par$scale)
q_vec(qCauchy, c(0.25, 0.5, 0.75), location = base_par$location,
      scale = base_par$scale, lower.tail = FALSE)
q_vec(qCauchy, c(log(0.25), log(0.5), log(0.75)), location = base_par$location,
      scale = base_par$scale, log.p = TRUE)

## ----cauchy-r-----------------------------------------------------------------
draw_many(rCauchy, list(location = base_par$location, scale = base_par$scale))

## ----cauchy-mix-d-------------------------------------------------------------
dCauchyMix(0, w = mix_par1$w, location = mix_par1$location, scale = mix_par1$scale)
dCauchyMix(0, w = mix_par1$w, location = mix_par1$location, scale = mix_par1$scale, log = TRUE)

## ----cauchy-mix-r-------------------------------------------------------------
draw_many(rCauchyMix, list(w = mix_par1$w, location = mix_par1$location, scale = mix_par1$scale))

## ----cauchy-plot--------------------------------------------------------------
df_cauchy <- rbind(
  data.frame(x = grid, density = density_curve(grid, dCauchy, list(location = base_par$location, scale = base_par$scale)), label = "Base"),
  data.frame(x = grid, density = density_curve(grid, dCauchyMix, list(w = mix_par1$w, location = mix_par1$location, scale = mix_par1$scale)), label = "Mix A"),
  data.frame(x = grid, density = density_curve(grid, dCauchyMix, list(w = cauchy_sets[[3]]$w, location = cauchy_sets[[3]]$location, scale = cauchy_sets[[3]]$scale)), label = "Mix B")
)

ggplot(df_cauchy, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Cauchy base and mixtures", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")

## ----dist-backend-summary, echo=FALSE, message=FALSE, warning=FALSE-----------
df <- data.frame(
  Distribution = c(
    "Normal","Gamma","Lognormal","Laplace","Inverse Gaussian","Amoroso","Cauchy",
    "GPD",
    "Normal + GPD","Gamma + GPD","Lognormal + GPD","Laplace + GPD","InvGauss + GPD","Amoroso + GPD"
  ),
  Parameters = c(
    "$\\mu,\\,\\sigma$",
    "$\\alpha,\\,\\beta$",
    "$\\mu,\\,\\sigma$",
    "$\\ell,\\,b$",
    "$\\mu,\\,\\lambda$",
    "$a,\\,\\theta,\\,\\alpha,\\,\\beta$",
    "$x_0,\\,\\gamma$",
    "$u,\\,\\sigma,\\,\\xi$",
    "$\\mu,\\,\\sigma,\\,u,\\,\\sigma,\\,\\xi$",
    "$\\alpha,\\,\\beta,\\,u,\\,\\sigma,\\,\\xi$",
    "$\\mu,\\,\\sigma,\\,u,\\,\\sigma,\\,\\xi$",
    "$\\ell,\\,b,\\,u,\\,\\sigma,\\,\\xi$",
    "$\\mu,\\,\\lambda,\\,u,\\,\\sigma,\\,\\xi$",
    "$a,\\,\\theta,\\,\\alpha,\\,\\beta,\\,u,\\,\\sigma,\\,\\xi$"
  ),
  `Arguments` = c(
    "`mean`, `sd`",
    "`shape`, `scale`",
    "`meanlog`, `sdlog`",
    "`location`, `scale`",
    "`mean`, `shape`",
    "`loc`, `scale`, `shape1`, `shape2`",
    "`location`, `scale`",
    "`threshold`, `scale`, `shape`",
    "`mean`, `sd`, `threshold`, `tail_scale`, `tail_shape`",
    "`shape`, `scale`, `threshold`, `tail_scale`, `tail_shape`",
    "`meanlog`, `sdlog`, `threshold`, `tail_scale`, `tail_shape`",
    "`location`, `scale`, `threshold`, `tail_scale`, `tail_shape`",
    "`mean`, `shape`, `threshold`, `tail_scale`, `tail_shape`",
    "`loc`, `scale`, `shape1`, `shape2`, `threshold`, `tail_scale`, `tail_shape`"
  ),
  `Type` = c(
    "scalars","scalars","scalars","scalars","scalars","scalars","scalars",
    "scalars",
    "scalars","scalars","scalars","scalars","scalars","scalars"
  ),
  `Function` = c(
    "`dnorm()`, `pnorm()`, `qnorm()`, `rnorm()`",
    "`dgamma()`, `pgamma()`, `qgamma()`, `rgamma()`",
    "`dlnorm()`, `plnorm()`, `qlnorm()`, `rlnorm()`",
    "`ddexp()`, `pdexp()`, `qdexp()`, `rdexp()`",
    "`dinvgauss()`, `pinvgauss()`, `qinvgauss()`, `rinvgauss()`",
    "`damoroso()`, `pamoroso()`, `qamoroso()`, `ramoroso()`",
    "`dcauchy()`, `pcauchy()`, `qcauchy()`, `rcauchy()`",
    "`dGpd()`, `pGpd()`, `qGpd()`, `rGpd()`",
    "`dNormGpd()`, `pNormGpd()`, `qNormGpd()`, `rNormGpd()`",
    "`dGammaGpd()`, `pGammaGpd()`, `qGammaGpd()`, `rGammaGpd()`",
    "`dLognormalGpd()`, `pLognormalGpd()`, `qLognormalGpd()`, `rLognormalGpd()`",
    "`dLaplaceGpd()`, `pLaplaceGpd()`, `qLaplaceGpd()`, `rLaplaceGpd()`",
    "`dInvGaussGpd()`, `pInvGaussGpd()`, `qInvGaussGpd()`, `rInvGaussGpd()`",
    "`dAmorosoGpd()`, `pAmorosoGpd()`, `qAmorosoGpd()`, `rAmorosoGpd()`"
  ),
  `Type.` = c(
    "vectors","vectors","vectors","vectors","vectors","vectors","vectors",
    "NA",
    "vectors","vectors + scalars","vectors + scalars","vectors + scalars","vectors + scalars","vectors + scalars"
  ),
  `Function.` = c(
    "`dNormMix()`, `pNormMix()`, `qNormMix()`, `rNormMix()`",
    "`dGammaMix()`, `pGammaMix()`, `qGammaMix()`, `rGammaMix()`",
    "`dLognormalMix()`, `pLognormalMix()`, `qLognormalMix()`, `rLognormalMix()`",
    "`dLaplaceMix()`, `pLaplaceMix()`, `qLaplaceMix()`, `rLaplaceMix()`",
    "`dInvGaussMix()`, `pInvGaussMix()`, `qInvGaussMix()`, `rInvGaussMix()`",
    "`dAmorosoMix()`, `pAmorosoMix()`, `qAmorosoMix()`, `rAmorosoMix()`",
    "`dCauchyMix()`, `pCauchyMix()`, `qCauchyMix()`, `rCauchyMix()`",
    "NA",
    "`dNormMixGpd()`, `pNormMixGpd()`, `qNormMixGpd()`, `rNormMixGpd()`",
    "`dGammaMixGpd()`, `pGammaMixGpd()`, `qGammaMixGpd()`, `rGammaMixGpd()`",
    "`dLognormalMixGpd()`, `pLognormalMixGpd()`, `qLognormalMixGpd()`, `rLognormalMixGpd()`",
    "`dLaplaceMixGpd()`, `pLaplaceMixGpd()`, `qLaplaceMixGpd()`, `rLaplaceMixGpd()`",
    "`dInvGaussMixGpd()`, `pInvGaussMixGpd()`, `qInvGaussMixGpd()`, `rInvGaussMixGpd()`",
    "`dAmorosoMixGpd()`, `pAmorosoMixGpd()`, `qAmorosoMixGpd()`, `rAmorosoMixGpd()`"
  ),
  check.names = FALSE
)

# Rename SB columns to match header labels cleanly
names(df)[4:7] <- c("Type", "Function", "Type", "Function")

knitr::kable(
  df,
  format = "html",
  escape = FALSE,
  align = rep("c", 7)
) |>
  kableExtra::kable_styling(full_width = FALSE, position = "center") |>
  kableExtra::add_header_above(c(" " = 3, "CRP" = 2, "SB" = 2)) |>
  kableExtra::row_spec(0, bold = TRUE)

