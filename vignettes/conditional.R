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
# data("nc_posX100_p3_k2")
# X <- as.matrix(nc_posX100_p3_k2$X)
# y <- nc_posX100_p3_k2$y

## ----cond---------------------------------------------------------------------
# bundle_cond <- build_nimble_bundle(
#   y = y,
#   X = X,
#   kernel = "lognormal",
#   backend = "sb",
#   GPD = TRUE,
#   J = 4,
#   mcmc = list(niter = 250, nburnin = 50, nchains = 1)
# )
# fit_cond <- run_mcmc_bundle_manual(bundle_cond, show_progress = FALSE)

## ----cond-pred----------------------------------------------------------------
# x_new <- X[1:10, , drop = FALSE]
# q_pred <- predict(fit_cond, x = x_new, type = "quantile", index = c(0.1, 0.5, 0.9))
# plot(q_pred)

