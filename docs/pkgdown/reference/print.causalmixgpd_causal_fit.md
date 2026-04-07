# Print a fitted causal model

`print.causalmixgpd_causal_fit()` provides a compact overview of the
fitted treated/control outcome blocks and the PS component when present.

## Usage

``` r
# S3 method for class 'causalmixgpd_causal_fit'
print(x, ...)
```

## Arguments

- x:

  A `"causalmixgpd_causal_fit"` object.

- ...:

  Unused.

## Value

The input object (invisibly).

## Details

A fitted causal object combines posterior draws for the treated outcome
model, the control outcome model, and optionally the propensity-score
model. Those fitted blocks are the ingredients used later to evaluate
causal estimands such as \\\mu_1(x) - \mu_0(x)\\ or \\Q\_{Y^1}(\tau \mid
x) - Q\_{Y^0}(\tau \mid x)\\.

The print method is deliberately high level. It identifies which models
were fitted and whether GPD tails are active, but it does not report
posterior summaries or treatment-effect estimates. Use
[`summary()`](https://rdrr.io/r/base/summary.html),
[`predict()`](https://rdrr.io/r/stats/predict.html), or the dedicated
causal estimand helpers for inferential output.

## See also

[`summary.causalmixgpd_causal_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.causalmixgpd_causal_fit.md),
[`predict.causalmixgpd_causal_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.causalmixgpd_causal_fit.md),
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md).
