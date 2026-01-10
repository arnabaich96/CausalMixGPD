## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  cache.path = "cache/quickstart-",
  warning = FALSE,
  message = FALSE
)

## -----------------------------------------------------------------------------
library(DPmixGPD)
library(nimble)
use_cached_fit <- TRUE
.fit_path <- function(name) {
  path <- system.file("extdata", name, package = "DPmixGPD")
  if (path == "") path <- file.path("inst", "extdata", name)
  path
}
fit_small <- readRDS(.fit_path("fit_small.rds"))
fit_small_crp <- readRDS(.fit_path("fit_small_crp.rds"))
set.seed(1)
N <- 60
y <- abs(rnorm(N)) + 0.1

## -----------------------------------------------------------------------------
bundle_sb <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = FALSE,
  J = 8,
  mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
)

if (use_cached_fit) {
  fit_sb <- fit_small
} else {
  fit_sb <- run_mcmc_bundle_manual(bundle_sb, show_progress = FALSE)
}
summary(fit_sb)

## -----------------------------------------------------------------------------
plots <- plot(fit_sb, family = "density", params = "alpha")
plots[[1]]

## -----------------------------------------------------------------------------
y_grid <- seq(min(y), max(y), length.out = 50)

pr_den <- predict(fit_sb, type = "density", y = y_grid)
pr_surv <- predict(fit_sb, type = "survival", y = y_grid)
pr_q <- predict(fit_sb, type = "quantile", p = c(0.5, 0.9))

head(pr_den$fit)
pr_q$fit

## -----------------------------------------------------------------------------
bundle_crp <- build_nimble_bundle(
  y = y,
  backend = "crp",
  kernel = "normal",
  GPD = FALSE,
  J = 8,
  mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
)

if (use_cached_fit) {
  fit_crp <- fit_small_crp
} else {
  fit_crp <- run_mcmc_bundle_manual(bundle_crp, show_progress = FALSE)
}
summary(fit_crp)

## -----------------------------------------------------------------------------
q_sb <- predict(fit_sb, type = "quantile", p = 0.5)$fit[1, 1]
q_crp <- predict(fit_crp, type = "quantile", p = 0.5)$fit[1, 1]

data.frame(backend = c("sb", "crp"), q50 = c(q_sb, q_crp))

## -----------------------------------------------------------------------------
bundle_gpd <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  J = 8,
  mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 2)
)

if (use_cached_fit) {
  fit_gpd <- fit_small
} else {
  fit_gpd <- run_mcmc_bundle_manual(bundle_gpd, show_progress = FALSE)
}
q_off <- predict(fit_sb, type = "quantile", p = 0.99)$fit[1, 1]
q_on <- predict(fit_gpd, type = "quantile", p = 0.99)$fit[1, 1]

data.frame(model = c("GPD=FALSE", "GPD=TRUE"), q99 = c(q_off, q_on))

