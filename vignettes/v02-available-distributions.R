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
  width = 80,
  cache = TRUE,
  cache.path = "../.cache/vignettes/"
)
library(DPmixGPD)
if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(quiet = TRUE)
library(ggplot2)
library(gridExtra)
set.seed(42)

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

