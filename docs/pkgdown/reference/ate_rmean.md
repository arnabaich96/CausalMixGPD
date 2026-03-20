# Restricted-mean ATE helper

`ate_rmean()` is a convenience wrapper for restricted-mean treatment
effects when the ordinary mean is unstable or undefined.

## Usage

``` r
ate_rmean(
  fit,
  newdata = NULL,
  cutoff,
  interval = "credible",
  level = 0.95,
  nsim_mean = 200L,
  show_progress = TRUE
)
```

## Arguments

- fit:

  A `"causalmixgpd_causal_fit"` object from
  [`run_mcmc_causal()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_causal.md).

- newdata:

  Optional data.frame or matrix of covariates for prediction. If `NULL`,
  uses the training covariates stored in `fit`.

- cutoff:

  Finite numeric cutoff for the restricted mean.

- interval:

  Character or NULL; type of credible interval: `NULL` for no interval,
  `"credible"` for equal-tailed quantile intervals (default), or `"hpd"`
  for highest posterior density intervals.

- level:

  Numeric credible level for intervals (default 0.95 for 95 percent CI).

- nsim_mean:

  Number of posterior predictive draws to approximate the mean.

- show_progress:

  Logical; if TRUE, print step messages and render progress where
  supported.

## Value

A `"causalmixgpd_ate"` object computed via
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md)
for unconditional fits or
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md)
for conditional fits.

## Details

The restricted-mean estimand replaces \\Y(a)\\ by \\\min\\Y(a), c\\\\,
so the contrast remains finite even when the fitted GPD tail implies
\\\xi \ge 1\\.

## See also

[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md),
[`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal",
                         GPD = TRUE, components = 6)
fit <- run_mcmc_causal(cb)
ate_rm <- ate_rmean(fit, cutoff = 10, interval = "credible")
} # }
```
