## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
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
  list(niter = 6000, nburnin = 2000, thin = 5, nchains = 2, seed = c(1, 2))
}

## -----------------------------------------------------------------------------
library(DPmixGPD)

y <- abs(rnorm(60)) + 0.15
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
print(fit)
summary(fit)

## -----------------------------------------------------------------------------
try(plot(fit, family = "trace"), silent = TRUE)

## -----------------------------------------------------------------------------
# Common patterns
if (!is.null(fit$mcmc$samples)) {
  s <- fit$mcmc$samples
  mat <- try(as.matrix(s), silent = TRUE)
  if (!inherits(mat, "try-error")) {
    dim(mat)
    colnames(mat)[1:min(20, ncol(mat))]
  }
}

## ----eval=FALSE---------------------------------------------------------------
# # Example (pseudo):
# # bundle2 <- update_mcmc(bundle, niter = 8000, nburnin = 2000)
# # fit2 <- run_mcmc_bundle_manual(bundle2)

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()
=======
  comment = NA,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)
library(DPmixGPD)

## ----mcmc-example-------------------------------------------------------------
# bundle <- build_nimble_bundle(
#   y = y,
#   kernel = "gamma",
#   backend = "crp",
#   GPD = TRUE,
#   J = 5,
#   mcmc = list(niter = 1000, nburnin = 250, nchains = 2)
# )
# fit <- run_mcmc_bundle_manual(bundle)
# plot(fit, family = c("traceplot", "running"))
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090

