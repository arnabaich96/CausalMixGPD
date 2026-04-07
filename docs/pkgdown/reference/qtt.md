# Quantile treatment effects standardized to treated covariates

`qtt()` computes the quantile treatment effect on the treated.

## Usage

``` r
qtt(
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

  Character or NULL; type of credible interval:

  - `NULL`: no interval

  - `"credible"` (default): equal-tailed quantile intervals

  - `"hpd"`: highest posterior density intervals

- level:

  Numeric credible level for intervals (default 0.95 for 95 percent CI).

- show_progress:

  Logical; if TRUE, print step messages and render progress where
  supported.

## Value

An object of class `"causalmixgpd_qte"` containing the QTT summary, the
probability grid, and the arm-specific predictive objects used in the
aggregation. The returned object includes a top-level `$fit_df` data
frame for direct extraction.

## Details

The estimand is \$\$\mathrm{QTT}(\tau) = Q_1^{t}(\tau) -
Q_0^{t}(\tau),\$\$ where marginalization is over the empirical covariate
distribution of the treated units only.

## See also

[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`cqte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md),
[`att`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/att.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
qtt(fit, probs = c(0.5, 0.9))
} # }
```
