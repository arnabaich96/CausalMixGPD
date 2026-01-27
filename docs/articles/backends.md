# Backends: SB vs CRP

## Overview

This vignette compares the two mixture backends:

- **SB**: Stick-breaking (finite truncation), controlled by `components`
- **CRP**: Chinese Restaurant Process (cluster allocations)

## Theory (brief)

Both backends represent a DP mixture, but they differ in how the random
measure $`G`$ is constructed. The stick-breaking backend uses a
truncated stick-breaking representation, while the CRP backend uses an
allocation-based partitioning scheme. In practice, SB offers explicit
control over truncation, while CRP adapts the number of clusters through
the allocation process.

## Data

``` r
library(DPmixGPD)

data("faithful", package = "datasets")
y <- faithful$eruptions
```

## Stick-Breaking Backend

``` r
bundle_sb <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)

fit_sb <- run_mcmc_bundle_manual(bundle_sb, show_progress = FALSE)
```

## CRP Backend

``` r
bundle_crp <- build_nimble_bundle(
  y = y,
  backend = "crp",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)

fit_crp <- run_mcmc_bundle_manual(bundle_crp, show_progress = FALSE)
```

## Comparison of Fitted Summaries

``` r
mean_sb <- predict(fit_sb, type = "mean", cred.level = 0.90, interval = "credible")$fit
mean_crp <- predict(fit_crp, type = "mean", cred.level = 0.90, interval = "credible")$fit

comparison_df <- data.frame(
  Backend = c("SB", "CRP"),
  Estimate = c(mean_sb$estimate[1], mean_crp$estimate[1]),
  Lower = c(mean_sb$lower[1], mean_crp$lower[1]),
  Upper = c(mean_sb$upper[1], mean_crp$upper[1])
)

kable(comparison_df, digits = 3, align = "c",
      caption = "Posterior Mean Comparison: SB vs CRP") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE, position = "center")
```

| Backend | Estimate | Lower | Upper |
|:-------:|:--------:|:-----:|:-----:|
|   SB    |   3.20   | 3.07  | 3.33  |
|   CRP   |   3.21   | 3.09  | 3.34  |

Posterior Mean Comparison: SB vs CRP

## Backend Selection Guidelines

| Backend | Use Case                                                   |
|---------|------------------------------------------------------------|
| **SB**  | Explicit control over truncation level; stable computation |
| **CRP** | Adaptive cluster number; more label/cluster dynamics       |
