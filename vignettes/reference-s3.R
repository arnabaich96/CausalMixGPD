## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
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
  list(niter = 300, nburnin = 80, thin = 2, nchains = 1, seed = 1)
} else {
  list(niter = 5000, nburnin = 1500, thin = 5, nchains = 2, seed = c(1, 2))
}

## ----results='hide'-----------------------------------------------------------
library(DPmixGPD)

data("faithful", package = "datasets")
y <- faithful$eruptions
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
if (interactive()) plot(fit, family = "traceplot")

## -----------------------------------------------------------------------------
predict(fit, type = "mean", cred.level = 0.90, interval = "credible")$fit
predict(fit, type = "median", cred.level = 0.90, interval = "credible")$fit
predict(fit, type = "quantile", index = 0.90, cred.level = 0.90, interval = "hpd")$fit
predict(fit, type = "quantile", index = 0.90, interval = NULL)$fit

## fitted() is for conditional (covariate) models only; this example is unconditional, so use predict() above.

## -----------------------------------------------------------------------------
str(fit, max.level = 2)

## -----------------------------------------------------------------------------
data("mtcars", package = "datasets")
df <- mtcars
X <- df[, c("wt", "hp", "qsec", "cyl")]
X <- as.data.frame(X)
T_ind <- df$am
y <- df$mpg

causal_bundle <- build_causal_bundle(
  y = y,
  X = X,
  T = T_ind,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  PS = "logit",
  mcmc_outcome = mcmc,
  mcmc_ps = mcmc
)

## ----results='hide'-----------------------------------------------------------
causal_fit <- run_mcmc_causal(causal_bundle, show_progress = FALSE)

## -----------------------------------------------------------------------------
ate_result <- ate(causal_fit, interval = "hpd", nsim_mean = 50)
print(ate_result)

## -----------------------------------------------------------------------------
summary(ate_result)

## ----fig.width=7, fig.height=5------------------------------------------------
ate_plots <- if (interactive()) plot(ate_result)
ate_plots$treatment_effect

## ----fig.width=7, fig.height=5------------------------------------------------
if (interactive()) plot(ate_result, type = "effect")

## -----------------------------------------------------------------------------
qte_result <- qte(causal_fit, probs = c(0.1, 0.5, 0.9), interval = "hpd")
print(qte_result)

## -----------------------------------------------------------------------------
summary(qte_result)

## ----fig.width=7, fig.height=5------------------------------------------------
qte_plots <- if (interactive()) plot(qte_result)
qte_plots$treatment_effect

## ----fig.width=7, fig.height=5------------------------------------------------
if (interactive()) plot(qte_result, type = "effect")

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()

