options(error = function() { traceback(10); q(status = 1) })
setwd('D:/Git-Repos/DPMGPD_package/DPmixGPD/website/workflows')
knitr::opts_chunk$set(cache = TRUE, cache.path='legacy-cache-v2/', message = FALSE, warning = FALSE)
set.seed(1)
FAST <- if (nzchar(Sys.getenv('LEGACY_FAST'))) as.logical(Sys.getenv('LEGACY_FAST')) else TRUE
mcmc_fast  <- list(niter = 400, nburnin = 100, thin = 2, nchains = 1, seed = 1)
mcmc_heavy <- list(niter = 4000, nburnin = 1000, thin = 5, nchains = 2, seed = c(1, 2))
mcmc <- if (isTRUE(FAST)) mcmc_fast else mcmc_heavy
library(DPmixGPD)
if (!require('devtools', quietly = TRUE)) library(devtools)
pkg_root <- rprojroot::find_root(rprojroot::has_file('DESCRIPTION'))
devtools::load_all(path = pkg_root, quiet = TRUE)
source('_legacy-setup.R')
data('nc_pos200_k3', package = 'DPmixGPD')
y <- nc_pos200_k3$y
bundle_uncond <- DPmixGPD::build_nimble_bundle(
  y = y,
  backend = 'crp',
  kernel = 'gamma',
  GPD = FALSE,
  components = 5,
  mcmc = mcmc
)
cat('about to fit\n')
fit_uncond <- load_or_fit('v01-start-here-fit_uncond', quiet_mcmc(run_mcmc_bundle_manual(bundle_uncond, show_progress = FALSE)))
print(summary(fit_uncond))
