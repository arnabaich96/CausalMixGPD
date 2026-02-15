# Summarize a QTE object

Returns a structured summary of QTE results for further analysis or
display. Includes overall statistics, per-quantile summaries, and
metadata.

## Usage

``` r
# S3 method for class 'causalmixgpd_qte'
summary(object, ...)
```

## Arguments

- object:

  A `"causalmixgpd_qte"` object from
  [`qte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md).

- ...:

  Unused.

## Value

An object of class `"summary.causalmixgpd_qte"` containing summary
statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
q <- qte(fit, probs = c(0.25, 0.5, 0.75), interval = "credible")
summary(q)
} # }
```
