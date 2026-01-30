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
  list(niter = 400, nburnin = 100, thin = 2, nchains = 1, seed = 1)
} else {
  list(niter = 12000, nburnin = 3000, thin = 5, nchains = 2, seed = c(1, 2))
}

## -----------------------------------------------------------------------------
library(DPmixGPD)

data("mtcars", package = "datasets")
df <- mtcars
X <- df[, c("wt", "hp", "qsec", "cyl")]
X <- as.data.frame(X)
T_ind <- df$am
y <- df$mpg

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
  mcmc_outcome = mcmc,
  mcmc_ps = mcmc
)

bundle

## ----results='hide'-----------------------------------------------------------
fit <- run_mcmc_causal(bundle, show_progress = FALSE)

## -----------------------------------------------------------------------------
ate_result <- ate(fit, interval = "hpd", nsim_mean = 100)
print(ate_result)

X_new <- data.frame(
  wt = seq(min(X$wt), max(X$wt), length.out = 20),
  hp = stats::median(X$hp),
  qsec = stats::median(X$qsec),
  cyl = stats::median(X$cyl)
)
ate_grid <- ate(fit, newdata = X_new, interval = "hpd", nsim_mean = 100, level = 0.90)

print(ate_grid)
summary(ate_grid)

## ----fig.width=7, fig.height=5------------------------------------------------
ate_plots <- if (interactive()) plot(ate_grid)
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
qte_plots <- if (interactive()) plot(qte_grid)
qte_plots$treatment_effect

## ----fig.width=7, fig.height=5------------------------------------------------
qte_plots$trt_control

## -----------------------------------------------------------------------------
summary(fit)

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()

