# Plot MCMC diagnostics for a MixGPD fit (ggmcmc backend)

Uses ggmcmc to produce standard MCMC diagnostic plots. Works with 1+
chains.

## Usage

``` r
# S3 method for class 'mixgpd_fit'
plot(x, family = "auto", params = NULL, nLags = 50, ...)
```

## Arguments

- x:

  A fitted object of class `"mixgpd_fit"`.

- family:

  Character vector of plot names (ggmcmc plot types) or a single one.
  Use `"auto"` (or `"all"`) to include all plots supported for the
  available number of chains/parameters. Supported types:

  - `"histogram"`: posterior histograms

  - `"density"`: posterior density curves

  - `"traceplot"`: MCMC trace plots

  - `"running"`: running mean plots

  - `"compare_partial"`: partial chain comparisons

  - `"autocorrelation"`: autocorrelation plots

  - `"crosscorrelation"`: cross-correlation matrix

  - `"Rhat"`: Gelman–Rubin R-hat (2+ chains)

  - `"grb"`: Gelman–Rubin–Brooks (2+ chains)

  - `"effective"`: effective sample size

  - `"geweke"`: Geweke diagnostic

  - `"caterpillar"`: caterpillar/forest plots

  - `"pairs"`: pairwise scatter plots (2+ params)

- params:

  Optional parameter selector:

  - character vector of parameter patterns (exact names or partial
    matches)

  - a single regex string (e.g. `"alpha|threshold|tail_"`)

  - `NULL` (default): plots all monitored parameters

- nLags:

  Number of lags for autocorrelation (ggmcmc).

- ...:

  Passed through to the underlying ggmcmc plotting functions when
  applicable.

## Value

Invisibly returns a named list of ggplot objects.

## Details

The supported plots diagnose posterior simulation quality rather than
data fit. Depending on the selected `family`, they show chain traces,
marginal posterior densities, autocorrelation, cross-correlation,
running means, or Gelman-style convergence summaries for the monitored
parameters.

These graphics should be read before interpreting posterior summaries or
treatment-effect results. Poor mixing or strong autocorrelation in the
MCMC output can invalidate downstream summaries even when the fitted
model itself is correctly specified.

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = TRUE, components = 6,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)
plot(fit, family = c("traceplot", "density"))
} # }
```
