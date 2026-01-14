devtools::load_all(quiet=TRUE)
load('data/nc_posX100_p3_k2.rda')
mcmc <- list(niter=50, nburnin=20, thin=1, nchains=1)
cat('K_true:', nc_posX100_p3_k2$meta$K_true, '\n')
for (ker in c('gamma','lognormal','invgauss','amoroso')) {
  for (backend in c('crp','sb')) {
    cat('Trying', ker, backend, '\n')
    bundle <- build_nimble_bundle(
      y = nc_posX100_p3_k2$y,
      X = nc_posX100_p3_k2$X,
      backend = backend,
      kernel = ker,
      GPD = FALSE,
      components = nc_posX100_p3_k2$meta$K_true,
      mcmc = mcmc
    )
    print(bundle$spec$meta)
    fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
    print(summary(fit))
  }
}
