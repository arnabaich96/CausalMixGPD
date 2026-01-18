## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = NA,
  fig.width = 8,
  fig.height = 6,
  message = FALSE,
  warning = FALSE,
  eval = TRUE,
  cache = TRUE,
  cache.path = "../.cache/vignettes/"
)
library(DPmixGPD)
if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(quiet = TRUE)
library(ggplot2)
library(dplyr)
library(tibble)
set.seed(123)

quiet_mcmc <- function(expr) {
  nullfile <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  out <- NULL
  utils::capture.output(
    out <- eval(substitute(expr), envir = parent.frame()),
    file = nullfile
  )
  out
}

## ----data-setup---------------------------------------------------------------
data("causal_alt_real500_p4_k2")
y <- abs(causal_alt_real500_p4_k2$y) + 0.01
T <- causal_alt_real500_p4_k2$T
X <- as.matrix(causal_alt_real500_p4_k2$X)

summary_tbl <- tibble(
  statistic = c("N", "Mean", "SD", "Min", "Max"),
  value = c(length(y), mean(y), sd(y), min(y), max(y))
)

summary_tbl %>%
  mutate(value = signif(value, 4)) %>%
  print()

x_eval <- X[1:40, , drop = FALSE]
y_eval <- y[1:40]
u_threshold <- as.numeric(stats::quantile(y, 0.8, names = FALSE))

## ----bundle-crp-gpd-----------------------------------------------------------
param_specs_gpd <- list(
  gpd = list(
    threshold = list(
      mode = "dist",
      dist = "lognormal",
      args = list(meanlog = log(max(u_threshold, .Machine$double.eps)), sdlog = 0.25)
    )
  )
)

bundle_crp_gpd <- build_causal_bundle(
  y = y,
  T = T,
  X = X,
  kernel = "invgauss",
  backend = "crp",
  PS = FALSE,
  GPD = TRUE,
  components = 6,
  param_specs = param_specs_gpd,
  mcmc_outcome = list(niter = 300, nburnin = 80, nchains = 1, thin = 1, seed = 3)
)

bundle_crp_gpd

## ----fit-crp-gpd--------------------------------------------------------------
fit_crp_gpd <- quiet_mcmc(run_mcmc_causal(bundle_crp_gpd))
summary(fit_crp_gpd)

## ----predict-mean-crp-gpd-----------------------------------------------------
pred_mean_gpd <- predict(fit_crp_gpd, x = x_eval, type = "mean", interval = "credible", nsim_mean = 150)
head(pred_mean_gpd)
plot(pred_mean_gpd)

## ----predict-quantile-crp-gpd-------------------------------------------------
pred_q_gpd <- predict(fit_crp_gpd, x = x_eval, type = "quantile", p = 0.5, interval = "credible")
head(pred_q_gpd)
plot(pred_q_gpd)

## ----predict-density-crp-gpd--------------------------------------------------
pred_d_gpd <- predict(fit_crp_gpd, x = x_eval, y = y_eval, type = "density", interval = "credible")
head(pred_d_gpd)
plot(pred_d_gpd)

## ----predict-survival-crp-gpd-------------------------------------------------
pred_surv_gpd <- predict(fit_crp_gpd, x = x_eval, y = y_eval, type = "survival", interval = "credible")
head(pred_surv_gpd)
plot(pred_surv_gpd)

## ----ate-crp-gpd--------------------------------------------------------------
ate_gpd <- ate(fit_crp_gpd, newdata = x_eval, interval = "credible", nsim_mean = 150)
head(ate_gpd)
plot(ate_gpd)

## ----qte-crp-gpd--------------------------------------------------------------
qte_gpd <- qte(fit_crp_gpd, probs = c(0.25, 0.5, 0.75), newdata = x_eval, interval = "credible")
head(qte_gpd)
plot(qte_gpd)

## ----bundle-sb-bulk-----------------------------------------------------------
bundle_sb_bulk <- build_causal_bundle(
  y = y,
  T = T,
  X = X,
  kernel = "amoroso",
  backend = "sb",
  PS = FALSE,
  GPD = FALSE,
  components = 6,
  mcmc_outcome = list(niter = 300, nburnin = 80, nchains = 1, thin = 1, seed = 4)
)

bundle_sb_bulk

