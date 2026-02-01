# Summarize an ATE object

Returns a structured summary of ATE results for further analysis or
display. Includes overall statistics and metadata.

## Usage

``` r
# S3 method for class 'dpmixgpd_ate'
summary(object, ...)
```

## Arguments

- object:

  A `"dpmixgpd_ate"` object from
  [`ate()`](https://arnabaich96.github.io/DPmixGPD/reference/ate.md).

- ...:

  Unused.

## Value

An object of class `"summary.dpmixgpd_ate"` containing summary
statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", components = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
a <- ate(fit, interval = "credible")
summary(a)
} # }
```
