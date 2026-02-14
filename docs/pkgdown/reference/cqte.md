# Conditional quantile treatment effects (CQTE)

Computes treated-minus-control quantiles from a causal fit.

## Usage

``` r
cqte(
  fit,
  probs = c(0.1, 0.5, 0.9),
  newdata = NULL,
  interval = "credible",
  level = 0.95
)
```

## Arguments

- fit:

  A `"dpmixgpd_causal_fit"` object from
  [`run_mcmc_causal()`](https://arnabaich96.github.io/DPmixGPD/pkgdown/reference/run_mcmc_causal.md).

- probs:

  Numeric vector of probabilities in (0, 1) specifying the quantile
  levels of the outcome distribution to estimate treatment effects at.

- newdata:

  Optional data.frame or matrix of covariates for prediction. If `NULL`,
  uses the training covariates stored in `fit`.

- interval:

  Character or NULL; type of credible interval: `NULL` for no interval,
  `"credible"` for equal-tailed quantile intervals (default), or `"hpd"`
  for highest posterior density intervals.

- level:

  Numeric credible level for intervals (default 0.95 for 95 percent CI).

## Value

A list with elements `fit` (CQTE), `grid` (probabilities), and the
treated/control prediction objects.

## Details

This estimand is available only for **conditional** causal models with
covariates (`X` not `NULL`). For unconditional causal models
(`X = NULL`), use
[`qte()`](https://arnabaich96.github.io/DPmixGPD/pkgdown/reference/qte.md)
or
[`qtt()`](https://arnabaich96.github.io/DPmixGPD/pkgdown/reference/qtt.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
cqte(fit, probs = c(0.5, 0.9), newdata = X[1:5, ])
cqte(fit, probs = c(0.5, 0.9), interval = "credible", level = 0.90)  # 90% CI
cqte(fit, probs = c(0.5, 0.9), interval = "hpd")  # HPD intervals
cqte(fit, probs = c(0.5, 0.9), interval = NULL)   # No intervals
} # }
```
