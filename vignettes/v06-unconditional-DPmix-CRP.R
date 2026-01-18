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
library(nimble)  
library(DPmixGPD)
if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(quiet = TRUE)
library(ggplot2)
library(gridExtra)
library(kableExtra)
library(patchwork)
set.seed(123)


## ----data-setup---------------------------------------------------------------
# Load pre-generated dataset: 200 observations from mixture of 3 gamma components
data(nc_pos200_k3)
y_mixed <- nc_pos200_k3$y

paste("Sample size:", length(y_mixed))
paste("Mean:", mean(y_mixed))
paste("SD:", sd(y_mixed))
paste("Range:", paste(range(y_mixed), collapse = " to "))

# Visualization
df_data <- data.frame(y = y_mixed)
p_raw <- ggplot(df_data, aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.6, fill = "steelblue",color = "black") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Raw Data: Mixed Gamma Distribution", x = "y", y = "Density") +
  theme_minimal()

print(p_raw)

## ----bundle-mcmc-crp----------------------------------------------------------
bundle_crp <- build_nimble_bundle(
  y = y_mixed,
  kernel = "laplace",         # Use laplace kernel
  backend = "crp",            # CRP backend
  GPD = FALSE,                # No tail augmentation
  components = 3,             # Minimal for testing
  alpha_random = TRUE,        # Random DP concentration
  mcmc = list(
    niter = 50,             # Minimal for testing
    nburnin = 10,           # Minimal burnin
    nchains = 2,            # Two chains for diagnostics
    thin = 1                # No thinning
  )
)


## ----mcmc-crp-----------------------------------------------------------------
bundle_crp <- build_nimble_bundle(
  y_mixed,
  kernel = "laplace",
  backend = "crp",
  GPD = FALSE,
  components = 3,
  alpha_random = TRUE,
  mcmc = list(niter = 1500, nburnin = 250, nchains = 2, thin = 1)
)

## -----------------------------------------------------------------------------
summary(bundle_crp)

## ----mcmc-crp-fit, results = "hide"-------------------------------------------
fit_crp <- run_mcmc_bundle_manual(bundle_crp)

## -----------------------------------------------------------------------------
summary(fit_crp)

## ----params-crp---------------------------------------------------------------
params_crp <- params(fit_crp)
params_crp

## ----diag-trace---------------------------------------------------------------
# Trace plots for key parameters
plot(fit_crp, params = "alpha", family = c("traceplot", "density", "geweke"))

## ----pred-density-------------------------------------------------------------
# Generate prediction grid
y_grid <- seq(0, max(y_mixed) * 1.2, length.out = 200)

# Posterior predictive density
pred_density <- predict(fit_crp, y = y_grid, type = "density")

# Use S3 plot method
plot(pred_density)

## ----pred-quantiles-----------------------------------------------------------
# Posterior predictive quantiles with credible intervals
quantiles_pred <- predict(fit_crp, type = "quantile", 
                          index = c(0.05, 0.25, 0.5, 0.75, 0.95),
                          interval = "credible")

# Display table
quantiles_pred$fit %>%
 kbl(caption = "Posterior Predictive Quantiles with Credible Intervals",
   align = "c",
                  digits = 3) %>%
 kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")

# Use S3 plot method
plot(quantiles_pred)

## ----components-sensitivity---------------------------------------------------
# Demonstrate with one value
bundle_components <- build_nimble_bundle(
  y = y_mixed,
  kernel = "laplace",
  backend = "crp",
  components = 5,
  mcmc = list(niter = 2500, nburnin = 500, nchains = 1)
)
fit_components <- run_mcmc_bundle_manual(bundle_components)

## -----------------------------------------------------------------------------
summary(fit_components)

## ----residuals-analysis-------------------------------------------------------
# Extract fitted values with diagnostics
Fit <- fitted(fit_components)

# Display table
kableExtra::kbl(head(Fit), caption = "Fitted Values, Residuals and Credible Interval", digits = 3, align = "c") %>%
 kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")

# Use S3 plot method for diagnostic plots
fit.plots <- plot(Fit)
fit.plots$residual_plot

## ----comp-laplace-------------------------------------------------------------
bundle_laplace <- build_nimble_bundle(
  y = y_mixed,
  kernel = "laplace",
  backend = "crp",
  components = 5,
  mcmc = list(niter = 2500, nburnin = 500, nchains = 1)
)
fit_laplace <- run_mcmc_bundle_manual(bundle_laplace)

