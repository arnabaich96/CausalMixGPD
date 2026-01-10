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

fit_small <- readRDS("inst/extdata/fit_small.rds")
fit_small <- strip_fit(fit_small)
saveRDS(fit_small, "inst/extdata/fit_small.rds")

fit_small_crp <- readRDS("inst/extdata/fit_small_crp.rds")
fit_small_crp <- strip_fit(fit_small_crp)
saveRDS(fit_small_crp, "inst/extdata/fit_small_crp.rds")

fit_causal_con <- readRDS("inst/extdata/fit_causal_con.rds")
fit_causal_trt <- readRDS("inst/extdata/fit_causal_trt.rds")
fit_causal_con <- strip_fit(fit_causal_con)
fit_causal_trt <- strip_fit(fit_causal_trt)
saveRDS(fit_causal_con, "inst/extdata/fit_causal_con.rds")
saveRDS(fit_causal_trt, "inst/extdata/fit_causal_trt.rds")
