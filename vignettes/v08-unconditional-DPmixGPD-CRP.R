## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  fig.width = 8,
  fig.height = 6,
  message = FALSE,
  warning = FALSE,
  eval = TRUE,
  cache = TRUE,
  cache.path = "../.cache/vignettes/"
)
library(DPmixGPD)
if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(quiet = TRUE)
library(nimble)
library(ggplot2)
library(gridExtra)
library(kableExtra)
library(dplyr)
library(patchwork)
set.seed(123)

## ----data-setup---------------------------------------------------------------
# Load data with an obvious tail
data("nc_pos_tail200_k4")
y_tail <- nc_pos_tail200_k4$y
y_tail <- y_tail[is.finite(y_tail) & y_tail > 0]

# Summaries and table
summary_tbl <- tibble(
  statistic = c("N", "Mean", "SD", "Min", "Max"),
  value = c(length(y_tail), mean(y_tail), sd(y_tail), min(y_tail), max(y_tail))
)

df_data <- data.frame(y = y_tail)

ggplot(df_data, aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "steelblue", color = "black", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Raw Data: Mix of Bulk and Tail", x = "y", y = "Density") +
  theme_minimal()

## ----data-table, echo=FALSE---------------------------------------------------
summary_tbl %>%
  mutate(value = signif(value, 4)) %>%
  kable(caption = "Summary of the Tail Dataset", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "center")

## ----threshold-selection------------------------------------------------------
thresholds <- quantile(y_tail, c(0.70, 0.75, 0.80, 0.85, 0.90))
u_threshold <- thresholds["80%"]

df_tail <- tibble(
  y = y_tail,
  region = ifelse(y_tail > u_threshold, "Tail", "Bulk")
)

ggplot(df_tail, aes(x = y, fill = region)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.7, color = "black") +
  geom_vline(xintercept = u_threshold, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("Bulk" = "steelblue", "Tail" = "red")) +
  labs(title = "Threshold Partition", x = "y", y = "Density") +
  theme_minimal()

## ----bundle-mcmc-crp-gpd------------------------------------------------------
bundle_gpd <- build_nimble_bundle(
  y = y_tail,
  kernel = "invgauss",
  backend = "crp",
  GPD = TRUE,
  components = 5,
  alpha_random = TRUE,
  param_specs = list(
    gpd = list(
      threshold = list(
        mode = "dist",
        dist = "lognormal",
        args = list(meanlog = log(max(u_threshold, .Machine$double.eps)), sdlog = 0.25)
      )
    )
  ),
  mcmc = list(
    niter = 60,
    nburnin = 10,
    nchains = 2,
    thin = 1
  )
)

## ----mcmc-run-----------------------------------------------------------------
fit_gpd <- run_mcmc_bundle_manual(bundle_gpd)

## ----fit-summary--------------------------------------------------------------
summary(fit_gpd)

## ----params-gpd-crp-----------------------------------------------------------
params_gpd <- params(fit_gpd)
params_gpd

## ----fit-plots----------------------------------------------------------------
# S3 plot methods highlight trace + density diagnostics
plot(fit_gpd, family = c("traceplot", "density", "running"))
plot(fit_gpd, params = "alpha", family = c("traceplot", "autocorrelation", "geweke"))

## ----density-predict----------------------------------------------------------
y_min <- max(min(y_tail), .Machine$double.eps)
y_grid <- seq(y_min, max(y_tail) * 1.3, length.out = 300)
pred_density <- predict(fit_gpd, y = y_grid, type = "density")
plot(pred_density)

## ----survival-predict---------------------------------------------------------
y_surv <- seq(max(u_threshold, y_min), max(y_tail) * 1.1, length.out = 60)
pred_surv <- predict(fit_gpd, y = y_surv, type = "survival")
plot(pred_surv)

## ----quantile-predict---------------------------------------------------------
quantile_probs <- c(0.90, 0.95, 0.99)
pred_quantiles <- predict(fit_gpd, type = "quantile", index = quantile_probs, interval = "credible")
plot(pred_quantiles)

## ----bulk-only-fit------------------------------------------------------------
bundle_bulk_only <- build_nimble_bundle(
  y = y_tail,
  kernel = "lognormal",
  backend = "crp",
  GPD = FALSE,
  components = 5,
  mcmc = list(niter = 2500, nburnin = 500, nchains = 1)
)
fit_bulk_only <- run_mcmc_bundle_manual(bundle_bulk_only)

## ----compare-quantiles--------------------------------------------------------
bulk_quantiles <- predict(fit_bulk_only, type = "quantile", index = quantile_probs)
tail_quantiles <- predict(fit_gpd, type = "quantile", index = quantile_probs)

compare_tbl <- bind_rows(
  bulk_quantiles$fit %>% mutate(model = "Bulk only"),
  tail_quantiles$fit %>% mutate(model = "Bulk + GPD")
) %>%
  select(any_of(c("model", "index", "estimate", "lwr", "upr", "lower", "upper")))

compare_tbl %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  kable(caption = "Posterior-Mean Quantiles for Bulk-Only vs GPD Models", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "center")

## ----quantile-plot------------------------------------------------------------
plot(bulk_quantiles)
plot(tail_quantiles)

## ----return-levels------------------------------------------------------------
probs <- c(0.995, 0.99, 0.975)
return_levels <- predict(fit_gpd, type = "quantile", index = probs)

return_levels$fit %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  kable(caption = "Extreme Quantile Estimates (Posterior Mean and Credible Intervals)", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "center")

## ----residuals----------------------------------------------------------------
fit_vals <- fitted(fit_gpd)
plot(fit_vals)

