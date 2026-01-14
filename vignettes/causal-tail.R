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

## ----data, eval=FALSE---------------------------------------------------------
# data("causal_alt_pos500_p5_k4_tail")
# print_dataset_header(causal_alt_pos500_p5_k4_tail, "causal_alt_pos500_p5_k4_tail")

## ----causal-tail, eval=FALSE--------------------------------------------------
# mcmc <- default_mcmc()
# cb <- build_causal_bundle(
#   y = causal_alt_pos500_p5_k4_tail$y,
#   X = causal_alt_pos500_p5_k4_tail$X,
#   T = causal_alt_pos500_p5_k4_tail$T,
#   backend = c("sb", "sb"),
#   kernel = c("invgauss", "amoroso"),
#   GPD = c(TRUE, TRUE),
#   J = c(causal_alt_pos500_p5_k4_tail$meta$K0, causal_alt_pos500_p5_k4_tail$meta$K1),
#   mcmc_outcome = mcmc,
#   mcmc_ps = mcmc
# )
# cf <- run_mcmc_causal(cb, show_progress = FALSE)
# 
# newdata <- causal_alt_pos500_p5_k4_tail$X[1:100, ]
# qte(cf, probs = c(0.1, 0.25, 0.5, 0.9), newdata = newdata)
# 
# # Posterior mean ATE
# ate(cf, newdata = newdata)

