## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = NA,
  fig.width = 8,
  fig.height = 5,
  message = FALSE,
  warning = FALSE,
  eval = TRUE,
  cache = TRUE,
  cache.path = "../.cache/vignettes/"
)
library(DPmixGPD)
if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(quiet = TRUE)
library(ggplot2)
set.seed(1)

quiet_mcmc <- function(expr) {
  nullfile <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  out <- NULL
  utils::capture.output(
    out <- eval(substitute(expr), envir = parent.frame()),
    file = nullfile
  )
  out
}

## ----start-cond-fit-----------------------------------------------------------
fit_cond <- quiet_mcmc(run_mcmc_bundle_manual(bundle_cond, show_progress = FALSE))
summary(fit_cond)

## ----start-cond-predict-------------------------------------------------------
x_new <- X[1:20, , drop = FALSE]
pred_mean <- predict(fit_cond, x = x_new, type = "mean", interval = "credible", nsim_mean = 200)
head(pred_mean$fit)
plot(pred_mean)

## ----start-s3-----------------------------------------------------------------
params(fit_uncond)
plot(fit_uncond, family = c("traceplot", "running"))

