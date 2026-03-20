# Quantile treatment effects, marginal over the empirical covariate distribution

`qte()` returns the marginal quantile treatment effect implied by the
causal fit.

## Usage

``` r
qte(
  fit,
  probs = c(0.1, 0.5, 0.9),
  newdata = NULL,
  y = NULL,
  interval = "credible",
  level = 0.95,
  show_progress = TRUE
)
```

## Arguments

- fit:

  A `"causalmixgpd_causal_fit"` object from
  [`run_mcmc_causal()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_causal.md).

- probs:

  Numeric vector of probabilities in (0, 1) specifying the quantile
  levels of the outcome distribution to estimate treatment effects at.

- newdata:

  Ignored for marginal estimands. If supplied, a warning is issued and
  training data are used.

- y:

  Ignored for marginal estimands. If supplied, a warning is issued and
  training data are used.

- interval:

  Character or NULL; type of credible interval: `NULL` for no interval,
  `"credible"` for equal-tailed quantile intervals (default), or `"hpd"`
  for highest posterior density intervals.

- level:

  Numeric credible level for intervals (default 0.95 for 95 percent CI).

- show_progress:

  Logical; if TRUE, print step messages and render progress where
  supported.

## Value

An object of class `"causalmixgpd_qte"` containing the marginal QTE
summary, the probability grid, and the arm-specific predictive objects
used in the aggregation.

## Details

The package computes \$\$\mathrm{QTE}(\tau) = Q_1^{m}(\tau) -
Q_0^{m}(\tau),\$\$ where \\Q_a^{m}(\tau)\\ is the arm-\\a\\ posterior
predictive marginal quantile obtained by averaging over the empirical
training covariate distribution.

For unconditional causal models (`X = NULL`), this reduces to a direct
contrast of the arm-level unconditional predictive distributions.

## See also

[`qtt`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qtt.md),
[`cqte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md),
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`predict.causalmixgpd_causal_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.causalmixgpd_causal_fit.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
qte(fit, probs = c(0.5, 0.9))
} # }
```
