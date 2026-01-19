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
FAST <- TRUE
mcmc <- if (FAST) {
  list(niter = 300, nburnin = 80, thin = 2, nchains = 1, seed = 1)
} else {
  list(niter = 5000, nburnin = 1500, thin = 5, nchains = 2, seed = c(1, 2))
}

## -----------------------------------------------------------------------------
library(DPmixGPD)

y <- abs(rnorm(50)) + 0.1
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)
fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)

## -----------------------------------------------------------------------------
print(fit)

## -----------------------------------------------------------------------------
summary(fit)

## -----------------------------------------------------------------------------
try(plot(fit, family = "trace"), silent = TRUE)

## -----------------------------------------------------------------------------
# Mean / median
predict(fit, type = "mean", cred.level = 0.90, interval = "credible")$fit
predict(fit, type = "median", cred.level = 0.90, interval = "credible")$fit

# Quantile
predict(fit, type = "quantile", index = 0.90, cred.level = 0.90, interval = "credible")$fit

## -----------------------------------------------------------------------------
# Returns a data.frame with fit, interval, residuals
f <- fitted(fit, type = "mean", level = 0.90)
head(f)

## -----------------------------------------------------------------------------
str(fit, max.level = 2)

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()
=======
  comment = NA,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)
library(DPmixGPD)

## ----s3-example---------------------------------------------------------------
# fit <- run_mcmc_bundle_manual(bundle)
# print(fit)
# summary(fit)
# plot(fit, family = c("traceplot", "running"))
# q_pred <- predict(fit, type = "quantile", index = c(0.1, 0.5, 0.9))
# plot(q_pred)
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090

