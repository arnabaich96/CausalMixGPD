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
  list(niter = 9000, nburnin = 2500, thin = 5, nchains = 2, seed = c(1, 2))
}

## -----------------------------------------------------------------------------
library(DPmixGPD)

n <- 90
y <- abs(rnorm(n)) + 0.2

## -----------------------------------------------------------------------------
bundle_sb <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)

fit_sb <- run_mcmc_bundle_manual(bundle_sb, show_progress = FALSE)

## -----------------------------------------------------------------------------
bundle_crp <- build_nimble_bundle(
  y = y,
  backend = "crp",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)

fit_crp <- run_mcmc_bundle_manual(bundle_crp, show_progress = FALSE)

## -----------------------------------------------------------------------------
mean_sb <- predict(fit_sb, type = "mean", cred.level = 0.90, interval = "credible")$fit
mean_crp <- predict(fit_crp, type = "mean", cred.level = 0.90, interval = "credible")$fit

rbind(
  SB  = unlist(mean_sb[1, c("estimate","lower","upper")]),
  CRP = unlist(mean_crp[1, c("estimate","lower","upper")])
)

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()

