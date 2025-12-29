devtools::load_all()
set.seed(1)
N <- 100
X <- cbind(x1=rnorm(N), x2=rbinom(N,1,0.5), x3=runif(N,-1,1))
y <- rexp(N) + 0.1

bundle_crp <- build_nimble_bundle(
  y = y,
  X = X,
  backend = "crp",
  kernel = "invgauss",
  GPD = TRUE,
  Kmax = N,
  mcmc = list(niter = 800, nburnin = 200, thin = 2, nchains = 3, seed = c(1,3,32))
)
# bundle S3
summary(bundle_crp)
print(bundle_crp)
print(bundle_crp, code = TRUE)



# fit (runner)
fit_crp_invgauss_gpd <- run_mcmc_bundle_manual(bundle_crp)
fit_crp_invgauss_gpd$spec
fit_crp_invgauss_gpd$spec$meta

class(fit_crp_invgauss_gpd$mcmc$samples)
cn <- colnames(as.matrix(fit_crp_invgauss_gpd$mcmc$samples))
head(cn, 40)

# Quick grep checks
grep("^beta_", cn, value = TRUE)
grep("tail|threshold|shape|scale", cn, value = TRUE)[1:30]

print(fit_crp_invgauss_gpd)
summary(fit_crp_invgauss_gpd)
plot(fit_crp_invgauss_gpd,
     family = c("histogram","density","traceplot","running","compare_partial",
                "autocorrelation","crosscorrelation","Rhat","grb","effective",
                "geweke","caterpillar","pairs"),
     params = "beta_|threshold|tail_|weights\\[")


