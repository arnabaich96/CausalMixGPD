# Summarize a QTE-style effect object

`summary.causalmixgpd_qte()` converts QTE, QTT, or CQTE output into a
tabular summary suitable for reporting.

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

An object of class `"summary.causalmixgpd_qte"` with `overall`,
`quantile_summary`, `effect_table`, `ci_summary`, `meta`, and the
original `object`.

## Details

The summary reorganizes the posterior effect object into reporting
tables. The target estimand remains a quantile contrast,
\$\$\Delta(\tau) = Q\_{Y^1}(\tau) - Q\_{Y^0}(\tau),\$\$ with the
appropriate marginal, treated-standardized, or conditional
interpretation depending on whether the source object came from
[`qte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`qtt()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qtt.md),
or
[`cqte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md).

Besides the effect table itself, the summary records the quantile grid,
the interval settings, and per-quantile distributional summaries when
posterior draws are available. This makes the object convenient for
reporting and downstream printing without recomputing the estimand.

## See also

[`print.causalmixgpd_qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.causalmixgpd_qte.md),
[`plot.causalmixgpd_qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.causalmixgpd_qte.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`cqte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
q <- qte(fit, probs = c(0.25, 0.5, 0.75), interval = "credible")
summary(q)
} # }
```
