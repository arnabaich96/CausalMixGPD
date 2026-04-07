# Summarize a fitted causal model

`summary.causalmixgpd_causal_fit()` returns posterior summaries for the
fitted PS block (when present) and both arm-specific outcome models.

## Usage

``` r
# S3 method for class 'causalmixgpd_causal_fit'
summary(object, pars = NULL, ps_pars = NULL, probs = c(0.025, 0.5, 0.975), ...)
```

## Arguments

- object:

  A `"causalmixgpd_causal_fit"` object.

- pars:

  Optional character vector of outcome-model parameters to summarize in
  both treatment arms. Passed to
  [`summary.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.mixgpd_fit.md).

- ps_pars:

  Optional character vector of PS-model parameters to summarize. If
  `NULL`, all monitored PS parameters are summarized.

- probs:

  Numeric vector of posterior quantiles to report.

- ...:

  Unused.

## Value

An object of class `"summary.causalmixgpd_causal_fit"` with elements
`ps`, `outcome`, and `probs`.

## Details

This summary stays at the model-parameter level. It aggregates posterior
summaries for the nuisance model \\e(x)\\ and for the arm-specific
outcome models \\f_0(y \mid x)\\ and \\f_1(y \mid x)\\, but it does not
yet collapse those pieces into treatment-effect functionals.

That separation is intentional. Parameters and treatment effects answer
different questions: `summary.causalmixgpd_causal_fit()` summarizes
posterior draws of the fitted model, whereas
[`ate()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`att()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/att.md),
[`cate()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md),
[`qte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`qtt()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qtt.md),
and
[`cqte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md)
transform those draws into causal contrasts.

## See also

[`print.causalmixgpd_causal_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.causalmixgpd_causal_fit.md),
[`predict.causalmixgpd_causal_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.causalmixgpd_causal_fit.md),
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md),
[`cqte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md).
