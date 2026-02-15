# Conditional average treatment effects (CATE)

Computes treated-minus-control posterior means from a causal fit.

## Usage

``` r
cate(
  fit,
  newdata = NULL,
  type = c("mean", "rmean"),
  cutoff = NULL,
  interval = "credible",
  level = 0.95,
  nsim_mean = 200L
)
```

## Arguments

- fit:

  A `"causalmixgpd_causal_fit"` object from
  [`run_mcmc_causal()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_causal.md).

- newdata:

  Optional data.frame or matrix of covariates for prediction. If `NULL`,
  uses the training covariates stored in `fit`.

- type:

  Character; `"mean"` (default) for ordinary mean CATE or `"rmean"` for
  restricted-mean CATE.

- cutoff:

  Finite numeric cutoff for restricted mean; required for
  `type = "rmean"`, ignored otherwise.

- interval:

  Character or NULL; type of credible interval: `NULL` for no interval,
  `"credible"` for equal-tailed quantile intervals (default), or `"hpd"`
  for highest posterior density intervals.

- level:

  Numeric credible level for intervals (default 0.95 for 95 percent CI).

- nsim_mean:

  Number of posterior predictive draws to approximate the mean.

## Value

A list with elements `fit` (CATE), optional `lower`/`upper`, and the
treated/control prediction objects.

## Details

This estimand is available only for **conditional** causal models with
covariates (`X` not `NULL`). For unconditional causal models
(`X = NULL`), use
[`ate()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md)
or
[`att()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/att.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
cate(fit, newdata = X[1:5, ])
cate(fit, interval = "credible", level = 0.90)  # 90% CI
cate(fit, interval = "hpd")  # HPD intervals
cate(fit, interval = NULL)   # No intervals
} # }
```
