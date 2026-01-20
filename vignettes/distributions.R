## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 7,
  fig.height = 4.5,
  fig.align = "center"
)
library(ggplot2)
library(kableExtra)
set.seed(1)

## -----------------------------------------------------------------------------
kernel_df <- data.frame(
  Kernel = c("normal", "gamma", "lognormal", "cauchy", "laplace", "invgauss", "amoroso"),
  Parameters = c(
    "mean, sd",
    "shape, rate",
    "meanlog, sdlog",
    "location, scale",
    "location, scale",
    "mean, shape",
    "location, scale, shape"
  )
)

kable(kernel_df, align = "c", caption = "Available Bulk Kernels") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE, position = "center")

## ----eval=FALSE---------------------------------------------------------------
# bundle <- build_nimble_bundle(
#   y = y,
#   backend = "sb",
#   kernel  = "lognormal",
#   GPD     = TRUE,
#   components = 6,
#   mcmc = list(niter = 500, nburnin = 100, thin = 2, nchains = 1, seed = 1)
# )

## -----------------------------------------------------------------------------
x <- seq(-4, 4, length.out = 400)
ggplot(data.frame(x = x, y = dnorm(x, mean = 0, sd = 1)), aes(x, y)) +
  geom_line(linewidth = 1, color = "steelblue") +
  labs(x = "x", y = "Density", title = "Normal(0, 1)") +
  theme_minimal()

## -----------------------------------------------------------------------------
x <- seq(0, 12, length.out = 400)
ggplot(data.frame(x = x, y = dgamma(x, shape = 2, rate = 1)), aes(x, y)) +
  geom_line(linewidth = 1, color = "steelblue") +
  labs(x = "x", y = "Density", title = "Gamma(shape = 2, rate = 1)") +
  theme_minimal()

## -----------------------------------------------------------------------------
x <- seq(0, 12, length.out = 400)
ggplot(data.frame(x = x, y = dlnorm(x, meanlog = 0, sdlog = 0.6)), aes(x, y)) +
  geom_line(linewidth = 1, color = "steelblue") +
  labs(x = "x", y = "Density", title = "Lognormal(meanlog = 0, sdlog = 0.6)") +
  theme_minimal()

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()

