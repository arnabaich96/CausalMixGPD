# Predict from a causal fit

Provides a unified interface to the treated and control outcome models
while guaranteeing that the same propensity scores are used for both
arms. For new covariates, the PS model stored in `object$ps_fit` is used
to estimate the required scores unless the user supplies their own via
`ps`. If the bundle was built with `PS=FALSE`, the PS model does not
exist and outcome predictions use only the original covariates `X`.

## Usage

``` r
# S3 method for class 'dpmixgpd_causal_fit'
predict(
  object,
  x = NULL,
  y = NULL,
  ps = NULL,
  newdata = NULL,
  type = c("mean", "quantile", "density", "survival", "prob", "location"),
  p = NULL,
  nsim = NULL,
  interval = "credible",
  probs = c(0.025, 0.5, 0.975),
  store_draws = TRUE,
  nsim_mean = 200L,
  ncores = 1L,
  ...
)
```

## Arguments

- object:

  A `"dpmixgpd_causal_fit"` object returned by
  [`run_mcmc_causal()`](https://arnabaich96.github.io/DPmixGPD/pkgdown/reference/run_mcmc_causal.md).

- x:

  Optional new data. Alias for `newdata`.

- y:

  Numeric vector of evaluation points (required for `type="density"` or
  `"survival"`).

- ps:

  Optional numeric vector of propensity scores aligned with `x` /
  `newdata`. When provided, the supplied scores are used instead of
  recomputing them from the stored PS model (needed only for custom
  inputs).

- newdata:

  Optional new data. If `NULL`, uses training design (if stored).

- type:

  Prediction type. Supported: `"mean"`, `"quantile"`, `"density"`,
  `"survival"`, `"prob"`.

- p:

  Numeric vector of probabilities for quantiles (required for
  `type="quantile"`).

- nsim:

  Number of posterior predictive samples (for `type="sample"`).

- interval:

  Character or NULL; type of credible interval: `NULL` for no interval,
  `"credible"` for equal-tailed quantile intervals (default), or `"hpd"`
  for highest posterior density intervals.

- probs:

  Quantiles for credible interval bands.

- store_draws:

  Logical; whether to store all posterior draws (for `type="sample"`).

- nsim_mean:

  Number of posterior predictive samples to use for posterior mean
  estimation (for `type="mean"`).

- ncores:

  Number of CPU cores to use for parallel prediction (if supported).

- ...:

  Unused.

## Value

For `"mean"` or `"quantile"`, a numeric matrix with columns `ps`,
`estimate`, `lower`, `upper`, representing treated-minus-control
posterior summaries. When PS is disabled or X is absent, `ps` is `NA`
and no PS is used. For `"density"`, `"survival"`, and `"prob"`, a data
frame with columns `y`, `ps`, `trt_estimate`, `trt_lower`, `trt_upper`,
`con_estimate`, `con_lower`, `con_upper`.

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal")
fit <- run_mcmc_causal(cb)
predict(fit, x = X[1:10, ], type = "quantile", p = c(0.25, 0.5, 0.75))
predict(fit, x = X[1:10, ], type = "mean", interval = "hpd")  # HPD intervals
predict(fit, x = X[1:10, ], type = "mean", interval = NULL)   # No intervals
} # }
```
