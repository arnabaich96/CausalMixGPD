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
  list(niter = 400, nburnin = 100, thin = 2, nchains = 1, seed = 1)
} else {
  list(niter = 4000, nburnin = 1000, thin = 5, nchains = 2, seed = c(1, 2))
}

## ----minimal-run--------------------------------------------------------------
library(DPmixGPD)

# A toy heavy-ish tail sample
n <- 80
y <- abs(rnorm(n)) + 0.15

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

## ----fitted-------------------------------------------------------------------
f <- fitted(fit, type = "mean", level = 0.90)
head(f)

# quick residual sanity check
summary(f$residuals)

## ----predict------------------------------------------------------------------
# For unconditional models, predict() returns population-level summaries
pred_mean <- predict(fit, type = "mean", cred.level = 0.90, interval = "credible")
pred_q90  <- predict(fit, type = "quantile", index = 0.90, cred.level = 0.90, interval = "credible")

pred_mean$fit
pred_q90$fit

## ----plot-fit-----------------------------------------------------------------
# Plot methods may vary by version; keep this simple.
# If your plot() method supports a family argument, trace plots are the safest.
try(plot(fit, family = "trace"), silent = TRUE)

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()

=======
  comment = NA,
  fig.width = 7,
  fig.height = 4.5,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)
library(DPmixGPD)

>>>>>>> 50289162bd36853addda01bb01ee507dfa332090
