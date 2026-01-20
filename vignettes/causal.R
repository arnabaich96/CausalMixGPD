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
  list(niter = 12000, nburnin = 3000, thin = 5, nchains = 2, seed = c(1, 2))
}

## -----------------------------------------------------------------------------
library(DPmixGPD)

n <- 80
X <- data.frame(x = rnorm(n))
T_ind <- rbinom(n, 1, plogis(0.2 + 0.5 * X$x))
y0 <- 0.5 + 0.7 * X$x + abs(rnorm(n)) + 0.1
te <- 0.4 + 0.6 * (X$x > 0)
y1 <- y0 + te
y <- ifelse(T_ind == 1, y1, y0)

## -----------------------------------------------------------------------------
bundle <- build_causal_bundle(
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

bundle

## -----------------------------------------------------------------------------
fit <- run_mcmc_causal(bundle, show_progress = FALSE)
fit

## -----------------------------------------------------------------------------
ate_result <- ate(fit, interval = "hpd", nsim_mean = 100)
print(ate_result)

X_new <- data.frame(x = seq(min(X$x), max(X$x), length.out = 20))
ate_grid <- ate(fit, newdata = X_new, interval = "hpd", nsim_mean = 100, level = 0.90)

print(ate_grid)
summary(ate_grid)

## ----fig.width=7, fig.height=5------------------------------------------------
ate_plots <- plot(ate_grid)
ate_plots$treatment_effect

## ----fig.width=7, fig.height=5------------------------------------------------
ate_plots$trt_control

## -----------------------------------------------------------------------------
probs <- c(0.1, 0.5, 0.9)
qte_result <- qte(fit, probs = probs, interval = "hpd")
print(qte_result)

qte_grid <- qte(fit, probs = probs, newdata = X_new, interval = "hpd")
print(qte_grid)
summary(qte_grid)

## ----fig.width=7, fig.height=5------------------------------------------------
qte_plots <- plot(qte_grid)
qte_plots$treatment_effect

## ----fig.width=7, fig.height=5------------------------------------------------
qte_plots$trt_control

## -----------------------------------------------------------------------------
summary(fit)

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()

