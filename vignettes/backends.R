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

comparison_df <- data.frame(
  Backend = c("SB", "CRP"),
  Estimate = c(mean_sb$estimate[1], mean_crp$estimate[1]),
  Lower = c(mean_sb$lower[1], mean_crp$lower[1]),
  Upper = c(mean_sb$upper[1], mean_crp$upper[1])
)

kable(comparison_df, digits = 3, align = "c",
      caption = "Posterior Mean Comparison: SB vs CRP") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE, position = "center")

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()

