## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 7,
  fig.height = 4.5
)
set.seed(1)
FAST <- TRUE
mcmc <- if (FAST) {
  list(niter = 400, nburnin = 100, thin = 2, nchains = 1, seed = 1)
} else {
  list(niter = 8000, nburnin = 2000, thin = 5, nchains = 2, seed = c(1, 2))
}

## -----------------------------------------------------------------------------
library(DPmixGPD)

n <- 100
y <- abs(rnorm(n)) + 0.2

bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)

fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
fit

## -----------------------------------------------------------------------------
# For unconditional models, fitted values replicate a population-level location
f_mean <- fitted(fit, type = "mean", level = 0.90)
head(f_mean)

f_med <- fitted(fit, type = "median", level = 0.90)
head(f_med)

## -----------------------------------------------------------------------------
pred_mean <- predict(fit, type = "mean", cred.level = 0.90, interval = "credible")
pred_q95  <- predict(fit, type = "quantile", index = 0.95, cred.level = 0.90, interval = "credible")

pred_mean$fit
pred_q95$fit

## -----------------------------------------------------------------------------
res <- f_mean$residuals
summary(res)

## -----------------------------------------------------------------------------
try(plot(fit, family = "trace"), silent = TRUE)

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()

