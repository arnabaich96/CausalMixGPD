## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  cache.path = "cache/v00-start-here-",
  fig.width = 6,
  warning = FALSE,
  message = FALSE
)
library(DPmixGPD)
library(nimble)
use_cached_fit <- TRUE
.fit_path <- function(name) {
  path <- system.file("extdata", name, package = "DPmixGPD")
  if (path == "") path <- file.path("inst", "extdata", name)
  path
}
fit_small <- readRDS(.fit_path("fit_small.rds"))
library(ggplot2)

## ----diagram, fig.cap="DPmixGPD concept: flexible bulk + GPD tail stitched at a threshold."----
bulk <- data.frame(x = seq(0, 10, length.out = 200))
bulk$y <- dlnorm(bulk$x, meanlog = 0.5, sdlog = 0.4) * 0.8
tail <- data.frame(x = seq(8, 20, length.out = 3))
tail$y <- vapply(
  tail$x,
  DPmixGPD::dGpd,
  numeric(1),
  threshold = 10,
  scale = 2,
  shape = 0.3
)
ggplot() +
  geom_area(data = bulk, aes(x = x, y = y), fill = "#66c2a5", alpha = 0.7) +
  geom_line(data = tail, aes(x = x, y = y), color = "#fc8d62", size = 1.2) +
  geom_vline(xintercept = 10, linetype = "dashed") +
  annotate("text", x = 12, y = 0.15, label = "GPD tail", color = "#fc8d62") +
  annotate("text", x = 4, y = 0.25, label = "DP mixture bulk", color = "#66c2a5") +
  labs(x = "Outcome", y = "Density")

## ----quick-demo---------------------------------------------------------------
set.seed(7)
y <- sim_bulk_tail(90, tail_prob = 0.2)
J <- 6
if (use_cached_fit) {
  fit <- fit_small
} else {
  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "lognormal",
    GPD = TRUE,
    J = J,
    mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 2, seed = c(1, 2))
  )
  fit <- run_mcmc_bundle_manual(bundle)
}
print(fit)
summary(fit, component = "tail")

## ----predictions, fig.cap="Density prediction on held-out grid (posterior mean)."----
pred <- predict(fit, type = "survival", y = seq(0, max(y) + 5, length.out = 100))
surv_df <- data.frame(y = pred$grid, survival = as.numeric(pred$fit[1, ]))
ggplot(surv_df, aes(x = y, y = survival)) +
  geom_line(color = "#7b3294") +
  labs(title = "Posterior survival curve", y = "Survival", x = "y")

## ----quantiles----------------------------------------------------------------
predict(fit, type = "quantile", p = c(.5, .8, .95))

## ----session------------------------------------------------------------------
sessionInfo()

