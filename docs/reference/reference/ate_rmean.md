# Restricted-mean ATE (finite under heavy tails)

Computes an average treatment effect on the restricted mean scale,
\\E\[min(Y, cutoff)\]\\, which is finite even when the ordinary mean
does not exist under heavy-tailed GPD regimes.

## Usage

``` r
ate_rmean(
  fit,
  newdata = NULL,
  cutoff,
  interval = "credible",
  level = 0.95,
  nsim_mean = 200L
)
```

## Arguments

- fit:

  A `"dpmixgpd_causal_fit"` object from
  [`run_mcmc_causal()`](https://arnabaich96.github.io/DPmixGPD/reference/run_mcmc_causal.md).

- newdata:

  Optional data.frame or matrix of covariates for prediction.

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

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal",
                         GPD = TRUE, components = 6)
fit <- run_mcmc_causal(cb)
ate_rm <- ate_rmean(fit, cutoff = 10, interval = "credible")
} # }
```
