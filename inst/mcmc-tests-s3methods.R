devtools::load_all()

set.seed(1)
y <- abs(rnorm(80)) + 0.2

bundle_sb_amoroso <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "lognormal",
  GPD = FALSE,
  Kmax = 6,
  mcmc = list(niter = 800, nburnin = 200, thin = 2, nchains = 1, seed = 1)
)

bundle_sb_amoroso$code
# fit_sb_amoroso <- run_mcmc_bundle_manual(bundle_sb_amoroso)
# fit_sb_amoroso$model$monitors_requested
# fit_sb_amoroso$model$monitors_used
# fit_sb_amoroso$model$monitors_dropped
#
# class(fit_sb_amoroso$mcmc$samples)
# colnames(as.matrix(fit_sb_amoroso$mcmc$samples))[1:30]
# print(fit_sb_amoroso)
# summary(fit_sb_amoroso)
# plot(fit_sb_amoroso, family = "trace", params=c("weight[1]", "location[1]", "scale[1]", "shape[1]"))

set.seed(1)
N <- 100
X <- cbind(x1=rnorm(N), x2=rbinom(N,1,0.5), x3=runif(N,-1,1))
y <- rexp(N) + 0.1

bundle_crp_invgauss <- build_nimble_bundle(
  y = y,
  X = X,
  backend = "crp",
  kernel = "invgauss",
  GPD = TRUE,
  Kmax = 30,
  mcmc = list(niter = 800, nburnin = 200, thin = 2, nchains = 1, seed = 1)
)
bundle_crp_invgauss$code

# fit_crp_invgauss_gpd <- run_mcmc_bundle_manual(bundle_crp_invgauss)
#
#
#
#
# fit_crp_invgauss_gpd$spec$meta  # should show backend=crp, has_X=TRUE, GPD=TRUE, p=3, Kmax=30
# fit_crp_invgauss_gpd$model$monitors_requested
# fit_crp_invgauss_gpd$model$monitors_used
# fit_crp_invgauss_gpd$model$monitors_dropped
#
#
# class(fit_crp_invgauss_gpd$mcmc$samples)
# cn <- colnames(as.matrix(fit_crp_invgauss_gpd$mcmc$samples))
# head(cn, 40)
#
# # Quick grep checks
# grep("^beta_", cn, value = TRUE)[1:20]
# grep("tail|threshold|shape|scale", cn, value = TRUE)[1:30]
#
# print(fit_crp_invgauss_gpd)
# summary(fit_crp_invgauss_gpd)
#
#
