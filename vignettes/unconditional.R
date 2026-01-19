## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
<<<<<<< HEAD
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
  list(niter = 8000, nburnin = 2000, thin = 5, nchains = 2, seed = c(1, 2))
}

## -----------------------------------------------------------------------------
library(DPmixGPD)

n <- 100
y <- abs(rnorm(n)) + 0.2

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

## -----------------------------------------------------------------------------
# For unconditional models, fitted values replicate a population-level location
f_mean <- fitted(fit, type = "mean", level = 0.90)
head(f_mean)

f_med <- fitted(fit, type = "median", level = 0.90)
head(f_med)

## -----------------------------------------------------------------------------
pred_mean <- predict(fit, type = "mean", cred.level = 0.90, interval = "credible")
pred_q95  <- predict(fit, type = "quantile", index = 0.95, cred.level = 0.90, interval = "credible")

pred_mean$fit
pred_q95$fit

## -----------------------------------------------------------------------------
res <- f_mean$residuals
summary(res)

## -----------------------------------------------------------------------------
try(plot(fit, family = "trace"), silent = TRUE)

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()
=======
=======
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090
  comment = NA,
  fig.width = 7,
  fig.height = 4.5,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)
library(DPmixGPD)

## ----data---------------------------------------------------------------------
# data("nc_pos200_k3")
# y <- nc_pos200_k3$y

## ----bulk-only----------------------------------------------------------------
# bundle_bulk <- build_nimble_bundle(
#   y = y,
#   kernel = "gamma",
#   backend = "crp",
#   GPD = FALSE,
#   J = 4,
#   mcmc = list(niter = 200, nburnin = 50, nchains = 1)
# )
# fit_bulk <- run_mcmc_bundle_manual(bundle_bulk, show_progress = FALSE)

## ----bulk-gpd-----------------------------------------------------------------
# bundle_tail <- build_nimble_bundle(
#   y = y,
#   kernel = "gamma",
#   backend = "crp",
#   GPD = TRUE,
#   J = 4,
#   mcmc = list(niter = 200, nburnin = 50, nchains = 1)
# )
# fit_tail <- run_mcmc_bundle_manual(bundle_tail, show_progress = FALSE)

## ----money-plot, eval=TRUE----------------------------------------------------
grid <- seq(0, 8, length.out = 200)
bulk <- dGammaMix(grid, w = c(0.7, 0.3), shape = c(2, 6), scale = c(1, 0.4))
tail <- vapply(
  grid,
  function(x) dGammaMixGpd(
    x,
    w = c(0.7, 0.3),
    shape = c(2, 6),
    scale = c(1, 0.4),
    threshold = 4,
    tail_scale = 1.0,
    tail_shape = 0.2
  ),
  numeric(1)
)
plot(grid, bulk, type = "l", lwd = 2, col = "steelblue", ylab = "density", xlab = "y")
lines(grid, tail, lwd = 2, col = "firebrick")
legend("topright", legend = c("Bulk only", "Bulk + GPD"), col = c("steelblue", "firebrick"), lwd = 2, bty = "n")
<<<<<<< HEAD
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090
=======
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090

