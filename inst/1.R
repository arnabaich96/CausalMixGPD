## =========================================================
## FULL USAGE SCRIPT (end-to-end): fit.TE + ate + qte + plot
## =========================================================

# clean session-ish (optional)
# rm(list = ls())

# devtools::load_all(".")  # if you're developing locally
library(DPmixGPD)

set.seed(1)

## -------------------------
## 1) Make toy data
## -------------------------
dat <- data.frame(
  y  = rgamma(200, shape = 2, rate = 1),
  A  = rbinom(200, 1, 0.5),
  x1 = rnorm(200),
  x2 = rnorm(200)
)

table(dat$A)

## -------------------------
## 2) Unconditional TE fit (y ~ 0)
## -------------------------
fit_te0 <- fit.TE(
  y ~ 0,
  data   = dat,
  A      = "A",
  kernel = "gamma",
  tail   = "none",
  dp_rep = "stick_breaking",
  dp_ctrl = list(K = 5),
  mcmc   = list(n_iter = 1000, burn_in = 500, chains = 1),
  alpha  = 0.05
)

str(fit_te0, max.level = 1)

## ATE (posterior mean/sd/CI)
ate0 <- ate(fit_te0, level = 0.95)
print(ate0)

## QTE at chosen taus (posterior mean/sd/CI)
qte0 <- qte(fit_te0, probs = c(0.1, 0.5, 0.9), level = 0.95)
print(qte0)

## ggplot-based plot methods (should call plot.mixgpd_te_fit internally)
# default should plot ATE
p_ate <- plot(fit_te0)
print(p_ate)

# quantile effect curve
p_qte <- plot(fit_te0, effect = "quantile", tau = seq(0.1, 0.9, by = 0.05))
print(p_qte)

## -------------------------
## 3) Conditional TE fit (y ~ x1 + x2)
## -------------------------
fit_te1 <- fit.TE(
  y ~ x1 + x2,
  data   = dat,
  A      = "A",
  kernel = "gamma",
  tail   = "none",
  dp_rep = "stick_breaking",
  dp_ctrl = list(K = 5),
  mcmc   = list(n_iter = 1000, burn_in = 500, chains = 1),
  alpha  = 0.05
)

## If your current ate/qte are implemented only for unconditional Gamma DP,
## these calls should either work (if you extended them) or error cleanly.
try(print(ate(fit_te1)))
try(print(qte(fit_te1, probs = c(0.25, 0.5, 0.75))))

## Plot (should be ggplot)
try(print(plot(fit_te1)))
try(print(plot(fit_te1, effect="quantile", tau=seq(0.1, 0.9, by=0.1))))

## -------------------------
## 4) Different kernels by arm (treat/control)
## -------------------------
fit_te2 <- fit.TE(
  y ~ 0,
  data   = dat,
  A      = "A",
  kernel = c("gamma", "lognormal"),  # treat, control
  tail   = "none",
  dp_rep = "stick_breaking",
  dp_ctrl = list(K = 5),
  mcmc   = list(n_iter = 800, burn_in = 400, chains = 1),
  alpha  = 0.05
)

fit_te2$spec_trt$kernel
fit_te2$spec_con$kernel

## If ate/qte only implemented for gamma currently, this should error cleanly
try(print(ate(fit_te2)))
try(print(qte(fit_te2, probs=c(0.1,0.5,0.9))))

## -------------------------
## 5) Sanity checks that should always pass
## -------------------------
stopifnot(inherits(fit_te0, "mixgpd_te_fit"))
stopifnot(all(c("fit_trt","fit_con","spec_trt","spec_con") %in% names(fit_te0)))
stopifnot(is.matrix(as.matrix(fit_te0$fit_trt$mcmc_draws)))
stopifnot(is.matrix(as.matrix(fit_te0$fit_con$mcmc_draws)))

cat("\nDONE: fit.TE -> ate/qte -> ggplot plots\n")
