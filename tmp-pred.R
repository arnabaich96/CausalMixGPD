library(devtools)
load_all('.')
Sys.setenv(LEGACY_FAST='FALSE')
FAST <- FALSE
mcmc <- list(niter=4000, nburnin=1000, thin=5, nchains=2, seed=c(1,2))
data('nc_pos200_k3')
y_mixed <- nc_pos200_k3
source('vignettes/workflows/_legacy-setup.R')
bundle_sb_gamma <- build_nimble_bundle(
  y = y_mixed,
  kernel = 'gamma',
  backend = 'sb',
  components = 5,
  GPD = FALSE,
  mcmc = mcmc
)
bundle_sb_cauchy <- build_nimble_bundle(
  y = y_mixed,
  kernel = 'cauchy',
  backend = 'sb',
  components = 5,
  GPD = FALSE,
  mcmc = mcmc
)
fit_sb_gamma <- run_mcmc_bundle_manual(bundle_sb_gamma, show_progress=FALSE, quiet=TRUE)
fit_sb_cauchy <- run_mcmc_bundle_manual(bundle_sb_cauchy, show_progress=FALSE, quiet=TRUE)
y_grid <- seq(min(y_mixed), max(y_mixed) * 1.3, length.out = 250)
pred_density_gamma <- predict(fit_sb_gamma, y = y_grid, type = 'density')
print(class(pred_density_gamma))
print(names(pred_density_gamma))
pred_density_cauchy <- predict(fit_sb_cauchy, y = y_grid, type = 'density')
print('done density predicted')
