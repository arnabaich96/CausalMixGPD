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
library(ggplot2)
library(gridExtra)
library(kableExtra)
library(dplyr)
library(patchwork)
library(purrr)
set.seed(123)

## ----data-setup, fig.width=8, fig.height=6------------------------------------
# Load benchmark dataset
data("nc_pos200_k3")
y_mixed <- nc_pos200_k3$y

df_data <- data.frame(y = y_mixed)

summary_tbl <- tibble(
  statistic = c("N", "Mean", "SD", "Min", "Max"),
  value = c(length(y_mixed), mean(y_mixed), sd(y_mixed), min(y_mixed), max(y_mixed))
)

p_raw <- ggplot(df_data, aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "darkorange", alpha = 0.7, color = "black") +
  geom_density(color = "darkred", linewidth = 1.2) +
  labs(title = "Raw Data: Mixed Gamma Distribution", x = "y", y = "Density") +
  theme_minimal()

grid.arrange(p_raw, ncol = 1)

## ----data-table, echo=FALSE---------------------------------------------------
summary_tbl %>%
  mutate(value = signif(value, 4)) %>%
  kable(caption = "Summary of the SB Dataset", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "center")

## ----bundle-mcmc-sb-----------------------------------------------------------
# --- Gamma kernel ---
bundle_sb_gamma <- build_nimble_bundle(
  y = y_mixed,
  kernel = "gamma",
  backend = "sb",
  components = 5,            # fixed truncation level for SB
  GPD = FALSE,               # bulk-only scenario
  mcmc = list(
    niter = 60,
    nburnin = 10,
    nchains = 2,
    thin = 1
  )
)

# --- Cauchy kernel ---
bundle_sb_cauchy <- build_nimble_bundle(
  y = y_mixed,
  kernel = "cauchy",
  backend = "sb",
  components = 5,
  GPD = FALSE,
  mcmc = list(
    niter = 60,
    nburnin = 10,
    nchains = 1,
    thin = 1
  )
)

## ----mcmc-run-----------------------------------------------------------------
fit_sb_gamma <- run_mcmc_bundle_manual(bundle_sb_gamma)
fit_sb_cauchy <- run_mcmc_bundle_manual(bundle_sb_cauchy)

## ----fit-summary--------------------------------------------------------------
summary(fit_sb_gamma)
summary(fit_sb_cauchy)

## ----params-sb----------------------------------------------------------------
params_gamma <- params(fit_sb_gamma)
params_gamma

## ----diag-plots---------------------------------------------------------------
# Default diagnostics for each fit
plot(fit_sb_gamma, family = c("traceplot", "autocorrelation", "running"))
plot(fit_sb_cauchy, family = c("density", "geweke", "caterpillar"))

## ----density-predict----------------------------------------------------------
y_grid <- seq(min(y_mixed), max(y_mixed) * 1.3, length.out = 250)

pred_density_gamma <- predict(fit_sb_gamma, y = y_grid, type = "density")
pred_density_cauchy <- predict(fit_sb_cauchy, y = y_grid, type = "density")

plot(pred_density_gamma)
plot(pred_density_cauchy)

## ----quantile-predict---------------------------------------------------------
quant_probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

pred_q_gamma <- predict(fit_sb_gamma, type = "quantile", index = quant_probs, interval = "credible")
pred_q_cauchy <- predict(fit_sb_cauchy, type = "quantile", index = quant_probs, interval = "credible")

plot(pred_q_gamma)
plot(pred_q_cauchy)

## ----mean-predict-------------------------------------------------------------
pred_mean_gamma <- predict(fit_sb_gamma, type = "mean")
pred_mean_cauchy <- predict(fit_sb_cauchy, type = "mean")

plot(pred_mean_gamma)
plot(pred_mean_cauchy)

## ----compare-backend----------------------------------------------------------
bundle_crp_small <- build_nimble_bundle(
  y = y_mixed,
  kernel = "gamma",
  backend = "crp",
  components = 5,
  GPD = FALSE,
  mcmc = list(niter = 2500, nburnin = 500, nchains = 1)
)
fit_crp_small <- run_mcmc_bundle_manual(bundle_crp_small)

crp_quant <- predict(fit_crp_small, type = "quantile", index = quant_probs, interval = "credible")
sb_quant <- predict(fit_sb_gamma, type = "quantile", index = quant_probs, interval = "credible")

bind_rows(
  crp_quant$fit %>% mutate(model = "CRP"),
  sb_quant$fit %>% mutate(model = "SB")
) %>%
  select(any_of(c("model", "index", "estimate", "lower", "upper"))) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  kable(caption = "Quantile Comparison: CRP vs SB", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "center")

## ----compare-plots------------------------------------------------------------
plot(crp_quant)
plot(sb_quant)

## ----residuals----------------------------------------------------------------
fit_vals <- fitted(fit_sb_gamma)
plot(fit_vals)

