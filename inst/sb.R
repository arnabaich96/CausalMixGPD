devtools::load_all()

set.seed(1)
y <- abs(rnorm(80)) + 0.2

bundle_sb_amoroso <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "amoroso",
  GPD = TRUE,
  Kmax = 6,
  mcmc = list(niter = 800, nburnin = 200, thin = 2, nchains = 2, seed = c(1,2))
)

# bundle S3
summary(bundle_sb_amoroso)
print(bundle_sb_amoroso)
print(bundle_sb_amoroso, code = TRUE)

# fit (runner)
fit_sb_amoroso <- run_mcmc_bundle_manual(bundle_sb_amoroso)

# fit S3
print(fit_sb_amoroso)
s <- summary(fit_sb_amoroso)
print(s)

# confirm multi-chain
length(fit_sb_amoroso$mcmc$samples)
table(ggmcmc::ggs(fit_sb_amoroso$mcmc$samples)$Chain)

# parameter names present:
cn <- colnames(as.matrix(fit_sb_amoroso$mcmc$samples[[1]]))

# All ggmcmc plots that are meaningful for your chain count
plot(fit_sb_amoroso,
     family = c("histogram","density","traceplot","running","compare_partial",
                "autocorrelation","crosscorrelation","Rhat","grb","effective",
                "geweke","caterpillar","pairs"),
     params = "alpha|threshold|tail_|weights\\[")










