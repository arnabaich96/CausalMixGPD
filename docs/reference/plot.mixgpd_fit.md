# Plot MCMC diagnostics for a MixGPD fit (ggmcmc backend)

Uses ggmcmc to produce standard MCMC diagnostic plots. Works with 1+
chains.

## Usage

``` r
# S3 method for class 'mixgpd_fit'
plot(x, family = c("traceplot", "density"), params = NULL, nLags = 50, ...)
```

## Arguments

- x:

  A fitted object of class `"mixgpd_fit"`.

- family:

  Character vector of plot names (ggmcmc plot types) or a single one.
  Supported:
  `histogram, density, traceplot, running, compare_partial, autocorrelation, crosscorrelation, Rhat, grb, effective, geweke, caterpillar, pairs`.

- params:

  Optional parameter selector. Either: (i) character vector of exact
  parameter names (e.g. `c("alpha","threshold")`), or (ii) a single
  regex string (e.g. `"alpha|threshold|tail_"`). If `NULL`, plots a
  reasonable default.

- nLags:

  Number of lags for autocorrelation (ggmcmc).

- ...:

  Passed through to the underlying ggmcmc plotting functions when
  applicable.

## Value

Invisibly returns a named list of ggplot objects.

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
