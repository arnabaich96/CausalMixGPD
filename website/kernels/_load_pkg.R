knitr::opts_chunk$set(
  collapse = FALSE,
  comment = NA,
  fig.width = 8,
  fig.height = 5,
  message = FALSE,
  warning = FALSE,
  cache = TRUE,
  cache.path = "legacy-cache/"
)

# Reproducibility helpers
set.seed(1)
FAST <- if (nzchar(Sys.getenv("LEGACY_FAST"))) as.logical(Sys.getenv("LEGACY_FAST")) else TRUE
mcmc_fast <- list(niter = 400, nburnin = 100, thin = 2, nchains = 1, seed = 1)
mcmc_heavy <- list(niter = 4000, nburnin = 1000, thin = 5, nchains = 2, seed = c(1, 2))
mcmc <- if (isTRUE(FAST)) mcmc_fast else mcmc_heavy

library(DPmixGPD)
library(ggplot2)
library(kableExtra)

# Reuse the legacy helper that manages cached fits and GPD data.
source("_legacy-setup.R")

q_vec <- function(fn, probs, ...) vapply(probs, function(p) fn(p, ...), numeric(1))
density_curve <- function(grid, fn, args) {
  vapply(grid, function(x) do.call(fn, c(list(x = x), args)), numeric(1))
}

draw_many <- function(fn, args, n_draws = 5) {
  if (!is.null(args$label)) args$label <- NULL
  vapply(seq_len(n_draws), function(i) do.call(fn, c(list(n = 1), args)), numeric(1))
}
