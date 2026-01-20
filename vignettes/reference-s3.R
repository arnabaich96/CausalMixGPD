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
  list(niter = 300, nburnin = 80, thin = 2, nchains = 1, seed = 1)
} else {
  list(niter = 5000, nburnin = 1500, thin = 5, nchains = 2, seed = c(1, 2))
}

## -----------------------------------------------------------------------------
n <- 60
X <- data.frame(x = rnorm(n))
T_ind <- rbinom(n, 1, plogis(0.2 + 0.5 * X$x))
y0 <- 0.5 + 0.7 * X$x + abs(rnorm(n)) + 0.1
te <- 0.4 + 0.6 * (X$x > 0)
y1 <- y0 + te
y <- ifelse(T_ind == 1, y1, y0)

causal_bundle <- build_causal_bundle(
  y = y,
  X = X,
  T = T_ind,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  PS = "logit",
  design = "observational",
  mcmc_outcome = mcmc,
  mcmc_ps = mcmc
)
causal_fit <- run_mcmc_causal(causal_bundle, show_progress = FALSE)

## -----------------------------------------------------------------------------
ate_result <- ate(causal_fit, interval = "hpd", nsim_mean = 50)
print(ate_result)

## -----------------------------------------------------------------------------
summary(ate_result)

## ----fig.width=7, fig.height=5------------------------------------------------
ate_plots <- plot(ate_result)
ate_plots$treatment_effect

## ----fig.width=7, fig.height=5------------------------------------------------
plot(ate_result, type = "effect")

## -----------------------------------------------------------------------------
qte_result <- qte(causal_fit, probs = c(0.1, 0.5, 0.9), interval = "hpd")
print(qte_result)

## -----------------------------------------------------------------------------
summary(qte_result)

## ----fig.width=7, fig.height=5------------------------------------------------
qte_plots <- plot(qte_result)
qte_plots$treatment_effect

## ----fig.width=7, fig.height=5------------------------------------------------
plot(qte_result, type = "effect")

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()

