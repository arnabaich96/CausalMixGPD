# Print a MixGPD summary object

Print a MixGPD summary object

## Usage

``` r
# S3 method for class 'mixgpd_summary'
print(x, digits = 3, max_rows = 60, show_ess = FALSE, ...)
```

## Arguments

- x:

  A `"mixgpd_summary"` object.

- digits:

  Number of digits to print.

- max_rows:

  Maximum rows to print.

- show_ess:

  Logical; if `TRUE`, include the `ess` column when present.

- ...:

  Unused.

## Value

`x` invisibly.

## Details

This method formats the output of
[`summary.mixgpd_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.mixgpd_fit.md).
It prints the model metadata, any stored WAIC value, the effective
truncation information induced by `epsilon`, and the parameter-level
posterior summary table.

The printed rows correspond to monitored posterior parameters. They are
not predictions of densities, quantiles, or means, which should instead
be obtained from
[`predict.mixgpd_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md).

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = TRUE, components = 6,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)
summary(fit)
} # }
```
