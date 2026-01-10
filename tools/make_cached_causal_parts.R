devtools::load_all(".")
library(nimble)
set.seed(123)
dat <- sim_causal_cqte(n = 120, seed = 123)
X <- dat$X
bundle <- build_causal_bundle(
  y = dat$y,
  X = X,
  T = dat$t,
  backend = "sb",
  kernel = "normal",
  GPD = FALSE,
  J = 4,
  mcmc_outcome = list(niter = 120, nburnin = 20, thin = 2, nchains = 1, seed = 1),
  mcmc_ps = list(niter = 100, nburnin = 20, thin = 2, nchains = 1, seed = 2)
)
fit <- run_mcmc_causal(bundle, show_progress = FALSE)
strip_fit <- function(fit) {
  if (is.list(fit)) {
    if (!is.null(fit$mcmc_conf)) fit$mcmc_conf <- NULL
    if (!is.null(fit$waic)) fit$waic <- NULL
    if (!is.null(fit$model)) fit$model <- NULL
    if (!is.null(fit$code)) fit$code <- NULL
    if (!is.null(fit$mcmc) && is.list(fit$mcmc)) {
      fit$mcmc$engine <- "cached"
      if (!is.null(fit$mcmc$waic)) fit$mcmc$waic <- NULL
    }
  }
  fit
}
fit_con <- strip_fit(fit$outcome_fit$con)
fit_trt <- strip_fit(fit$outcome_fit$trt)
meta <- list(meta = fit$bundle$meta)

saveRDS(fit_con, "inst/extdata/fit_causal_con.rds")
saveRDS(fit_trt, "inst/extdata/fit_causal_trt.rds")
saveRDS(meta, "inst/extdata/fit_causal_meta.rds")
