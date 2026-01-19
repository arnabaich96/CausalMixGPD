## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
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

