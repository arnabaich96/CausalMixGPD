## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  fig.width = 8,
  fig.height = 6,
  message = FALSE,
  warning = FALSE,
  eval = TRUE,
  cache = FALSE
)
library(nimble)  
library(DPmixGPD)
library(ggplot2)
library(gridExtra)
library(kableExtra)
set.seed(123)

## ----data-setup---------------------------------------------------------------
# Generate from mixture of gamma distributions
set.seed(42)
component1 <- rgamma(60, shape = 2, rate = 1.5)
component2 <- rgamma(40, shape = 1, rate = 0.5)
y_mixed <- c(component1, component2)

paste("Sample size:", length(y_mixed))
paste("Mean:", mean(y_mixed))
paste("SD:", sd(y_mixed))
paste("Range:", paste(range(y_mixed), collapse = " to "))

# Visualization
df_data <- data.frame(y = y_mixed)
p_raw <- ggplot(df_data, aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.6, fill = "steelblue") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Raw Data: Mixed Gamma Distribution", x = "y", y = "Density") +
  theme_minimal()

print(p_raw)

## ----bundle-mcmc-crp----------------------------------------------------------
bundle_crp <- build_nimble_bundle(
  y = y_mixed,
  kernel = "gamma",           # Use gamma kernel
  backend = "crp",            # CRP backend
  GPD = FALSE,                # No tail augmentation
  components = 3,             # Minimal for testing
  alpha_random = TRUE,        # Random DP concentration
  mcmc = list(
    niter = 50,             # Minimal for testing
    nburnin = 10,           # Minimal burnin
    nchains = 1,            # Single chain for speed
    thin = 1                # No thinning
  )
)

message("Bundle compiled with CRP sampler.")
message("Ready for MCMC execution.")

## ----mcmc-crp-----------------------------------------------------------------
bundle_crp <- build_nimble_bundle(
  y_mixed,
  kernel = "gamma",
  backend = "crp",
  GPD = FALSE,
  components = 3,
  alpha_random = TRUE,
  mcmc = list(niter = 50, nburnin = 10, nchains = 1, thin = 1)
)

fit_crp <- run_mcmc_bundle_manual(bundle_crp)
summary(fit_crp)

## ----diag-trace---------------------------------------------------------------
# Trace plots for key parameters
plot(fit_crp, params = "alpha|beta")

## ----diag-ess-----------------------------------------------------------------
# ESS and R-hat from summary
cat("Check ESS > 1000 and Rhat < 1.1 for convergence.\n")

