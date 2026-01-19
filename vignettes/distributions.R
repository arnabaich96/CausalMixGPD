## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
<<<<<<< HEAD
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 7,
  fig.height = 4.5
)
set.seed(1)

## -----------------------------------------------------------------------------
knitr::kable(
  data.frame(
    kernel = c("normal", "gamma", "lognormal", "cauchy", "laplace", "invgauss", "amoroso"),
    typical_parameters = c(
      "mean, sd",
      "shape, rate",
      "meanlog, sdlog",
      "location, scale",
      "location, scale",
      "mean, shape",
      "location/scale/shape (family-specific)"
    )
  ),
  caption = "Common bulk kernels (names may vary by version)"
)

## ----eval=FALSE---------------------------------------------------------------
# bundle <- build_nimble_bundle(
#   y = y,
#   backend = "sb",      # or "crp"
#   kernel  = "lognormal",
#   GPD     = TRUE,
#   components = 6,
#   mcmc = list(niter = 500, nburnin = 100, thin = 2, nchains = 1, seed = 1)
# )

## -----------------------------------------------------------------------------
x <- seq(-4, 4, length.out = 400)
plot(x, dnorm(x, mean = 0, sd = 1), type = "l", xlab = "x", ylab = "density", main = "Normal(0,1) density")

## -----------------------------------------------------------------------------
x <- seq(0, 12, length.out = 400)
plot(x, dgamma(x, shape = 2, rate = 1), type = "l", xlab = "x", ylab = "density", main = "Gamma(shape=2, rate=1) density")

## -----------------------------------------------------------------------------
x <- seq(0, 12, length.out = 400)
plot(x, dlnorm(x, meanlog = 0, sdlog = 0.6), type = "l", xlab = "x", ylab = "density", main = "Lognormal(meanlog=0, sdlog=0.6) density")

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()
=======
  comment = NA,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)
library(DPmixGPD)

## ----kernel-help--------------------------------------------------------------
# ?dGammaMix
# ?dLognormalMix
# ?dLaplaceMix
# ?dInvGaussMix
# ?dAmorosoMix
# ?dCauchyMix
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090

