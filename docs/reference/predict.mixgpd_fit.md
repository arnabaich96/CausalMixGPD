# Predict from a MixGPD fit

This provides a stable interface for distributional predictions. The
default implementation supports:

- `type="density"` using `y`

- `type="survival"` using `y`

- `type="quantile"` using `p`

- `type="sample"` (posterior predictive draws)

- `type="mean"` (posterior predictive mean)

## Usage

``` r
# S3 method for class 'mixgpd_fit'
predict(
  object,
  x = NULL,
  y = NULL,
  ps = NULL,
  newdata = NULL,
  type = c("density", "survival", "quantile", "sample", "mean", "median", "location", "fit"),
  p = NULL,
  index = NULL,
  nsim = NULL,
  cred.level = 0.95,
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

  A fitted object of class `"mixgpd_fit"`.

- x:

  Optional new data. Alias for `newdata`.

- y:

  Numeric vector of evaluation points (required for `type="density"` or
  `"survival"`).

- ps:

  Optional numeric vector of propensity scores for conditional
  prediction. Used when the model was fit with propensity score
  augmentation.

- newdata:

  Optional new data. If `NULL`, uses training design (if stored).

- type:

  Prediction type: `"density"`, `"survival"`, `"quantile"`, `"sample"`,
  `"mean"`, `"median"`, `"location"`.

- p:

  Numeric vector of probabilities for quantiles (required for
  `type="quantile"`).

- index:

  Alias for `p`; numeric vector of quantile levels.

- nsim:

  Number of posterior predictive samples (for `type="sample"`).

- cred.level:

  Credible level for credible intervals (default 0.95 for 95 percent
  intervals).

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

A list with elements:

- `fit`: data frame with `estimate`/`lower`/`upper` columns (posterior
  means over draws) plus any index columns (e.g. `id`, `y`, `index`).

- `lower`, `upper`: reserved for backward compatibility (typically
  `NULL`).

- `type`, `grid`: metadata.

## Details

If your object stores dedicated predictive machinery, you can keep this
signature and swap the internals without breaking user code.

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = TRUE, components = 6,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)
pr <- predict(fit, type = "quantile", p = c(0.5, 0.9))
pr_surv <- predict(fit, y = sort(y), type = "survival")
pr_cdf <- list(fit = 1 - pr_surv$fit)
# HPD intervals
pr_hpd <- predict(fit, type = "quantile", p = c(0.5, 0.9), interval = "hpd")
# No intervals
pr_none <- predict(fit, type = "quantile", p = c(0.5, 0.9), interval = NULL)
} # }
```
