## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  cache.path = "cache/causal-",
  warning = FALSE,
  message = FALSE
)

## ----libraries, eval=FALSE----------------------------------------------------
# if (requireNamespace("devtools", quietly = TRUE)) {
#   devtools::load_all(quiet = TRUE)
# } else {
#   library(DPmixGPD)
# }
# library(nimble)
# use_cached_fit <- FALSE
# .fit_path <- function(name) {
#   path <- system.file("extdata", name, package = "DPmixGPD")
#   if (path == "") path <- file.path("inst", "extdata", name)
#   path
# }
# fit_causal_small <- NULL
# if (use_cached_fit) {
#   fit_causal_con <- readRDS(.fit_path("fit_causal_con.rds"))
#   fit_causal_trt <- readRDS(.fit_path("fit_causal_trt.rds"))
#   fit_causal_meta <- readRDS(.fit_path("fit_causal_meta.rds"))
#   fit_causal_small <- list(
#     ps_fit = NULL,
#     outcome_fit = list(con = fit_causal_con, trt = fit_causal_trt),
#     bundle = fit_causal_meta,
#     call = NULL
#   )
#   class(fit_causal_small) <- "dpmixgpd_causal_fit"
# }

## ----data, eval=FALSE---------------------------------------------------------
# set.seed(1)
# dat <- sim_causal_qte(n = 120)
# X <- dat$X
# T <- dat$t
# y <- dat$y
# 
# cb <- build_causal_bundle(
#   y = y,
#   X = X,
#   T = T,
#   backend = c("sb", "sb"),
#   kernel = c("normal", "normal"),
#   GPD = c(FALSE, FALSE),
#   J = c(8, 8),
#   mcmc_outcome = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1),
#   mcmc_ps = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
# )

## ----fit, eval=FALSE----------------------------------------------------------
# if (use_cached_fit) {
#   cf <- fit_causal_small
# } else {
#   cf <- run_mcmc_causal(cb, show_progress = FALSE)
# }
# summary(cf$outcome_fit$trt)
# newdata <- X

## ----qte, eval=FALSE----------------------------------------------------------
# qt <- qte(cf, probs = c(0.5, 0.9), newdata = newdata)
# qt$fit
# 
# qs <- c(0.1, 0.5, 0.9)
# qt2 <- qte(cf, probs = qs, newdata = newdata)
# plot(qs, as.numeric(qt2$fit[1, ]), type = "b",
#      xlab = "quantile", ylab = "QTE",
#      main = "Quantile treatment effect")
# 
# # Posterior mean (ATE)
# ate_res <- ate(cf, newdata = newdata)
# head(ate_res$fit)

## ----session------------------------------------------------------------------
sessionInfo()

