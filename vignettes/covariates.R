## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(DPmixGPD)
library(nimble)

# Helper functions
print_dataset_header <- function(data, name = deparse(substitute(data))) {
  cat(sprintf("Dataset: %s (n=%d", name, length(data$y)))
  if (!is.null(data$X)) cat(sprintf(", p=%d", ncol(data$X)))
  cat(")\n")
  invisible(NULL)
}

default_mcmc <- function() {
  list(niter = 1000, nburnin = 500, nchains = 2, thin = 1)
}

## ----data-pos-----------------------------------------------------------------
data("nc_posX100_p3_k2")
data("nc_posX100_p4_k3")
data("nc_posX100_p5_k4")

## ----data-real----------------------------------------------------------------
data("nc_realX100_p3_k2")
data("nc_realX100_p5_k3")

## ----covariate-pos, eval=FALSE------------------------------------------------
# pos_sets <- list(
#   nc_posX100_p3_k2,
#   nc_posX100_p4_k3,
#   nc_posX100_p5_k4
# )
# pos_kernels <- c("gamma", "lognormal", "invgauss", "amoroso")
# backends <- c("crp", "sb")
# mcmc <- default_mcmc()
# 
# for (obj in pos_sets) {
#   print_dataset_header(obj)
#   for (ker in pos_kernels) {
#     for (backend in backends) {
#       bundle <- build_nimble_bundle(
#         y = obj$y,
#         X = obj$X,
#         backend = backend,
#         kernel = ker,
#         GPD = FALSE,
#         components = obj$meta$K_true,
#         mcmc = mcmc
#       )
#       summary(bundle)
#       fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
#       summary(fit)
#     }
#   }
# }

## ----covariate-real, eval=FALSE-----------------------------------------------
# real_sets <- list(
#   nc_realX100_p3_k2,
#   nc_realX100_p5_k3
# )
# real_kernels <- c("normal", "laplace", "cauchy")
# backends <- c("crp", "sb")
# mcmc <- default_mcmc()
# 
# for (obj in real_sets) {
#   print_dataset_header(obj)
#   for (ker in real_kernels) {
#     for (backend in backends) {
#       bundle <- build_nimble_bundle(
#         y = obj$y,
#         X = obj$X,
#         backend = backend,
#         kernel = ker,
#         GPD = FALSE,
#         components = obj$meta$K_true,
#         mcmc = mcmc
#       )
#       summary(bundle)
#       fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
#       summary(fit)
#     }
#   }
# }

