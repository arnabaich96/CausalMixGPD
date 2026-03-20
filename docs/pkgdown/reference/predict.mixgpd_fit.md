# Posterior predictive summaries from a fitted one-arm model

`predict.mixgpd_fit()` is the central distributional prediction method
for fitted one-arm models.

## Usage

``` r
# S3 method for class 'mixgpd_fit'
predict(
  object,
  x = NULL,
  y = NULL,
  ps = NULL,
  id = NULL,
  newdata = NULL,
  type = c("density", "survival", "quantile", "sample", "mean", "rmean", "median",
    "location", "fit"),
  p = NULL,
  index = NULL,
  nsim = NULL,
  level = 0.95,
  interval = "credible",
  probs = c(0.025, 0.5, 0.975),
  store_draws = TRUE,
  nsim_mean = 200L,
  cutoff = NULL,
  ncores = 1L,
  show_progress = TRUE,
  ndraws_pred = NULL,
  chunk_size = NULL,
  parallel = FALSE,
  workers = NULL,
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

- id:

  Optional identifier for prediction rows. Provide either a column name
  in `x`/`newdata` or a vector of length `nrow(x)`. The id column is
  excluded from analysis.

- newdata:

  Optional new data. If `NULL`, uses training design (if stored).

- type:

  Prediction type:

  - `"density"`: Posterior predictive density f(y \| x, data)

  - `"survival"`: Posterior predictive survival S(y \| x, data) = 1 -
    F(y \| x, data)

  - `"quantile"`: Posterior predictive quantiles Q(p \| x, data)

  - `"sample"`: Posterior predictive samples Y^rep ~ f(y \| x, data)

  - `"mean"`: Posterior predictive mean E(Y \| x, data) (averaged over
    posterior parameter uncertainty)

  - `"rmean"`: Posterior predictive restricted mean \\E\[\min(Y, cutoff)
    \mid x, data\]\\

  - `"median"`: Posterior predictive median (quantile at p=0.5)

  - `"location"`: Alias for "mean"

  - `"fit"`: Per-observation posterior predictive draws

  Note: `type="mean"` returns the posterior predictive mean, which
  integrates over parameter uncertainty. This differs from the mean of a
  single model distribution.

- p:

  Numeric vector of probabilities for quantiles (required for
  `type="quantile"`).

- index:

  Alias for `p`; numeric vector of quantile levels.

- nsim:

  Number of posterior predictive samples (for `type="sample"`).

- level:

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

- cutoff:

  Finite numeric cutoff for `type="rmean"` (restricted mean).

- ncores:

  Number of CPU cores to use for parallel prediction (if supported).

- show_progress:

  Logical; if TRUE, print step messages and render progress where
  supported.

- ndraws_pred:

  Optional integer subsample of posterior draws for prediction speed. If
  NULL and `nrow(newdata) > 20000`, defaults to 200.

- chunk_size:

  Optional row chunk size for large `newdata` prediction. If NULL and
  `nrow(newdata) > 20000`, defaults to 10000.

- parallel:

  Logical; if TRUE, enable parallel prediction (alias for setting
  `ncores > 1`).

- workers:

  Optional integer worker count (alias for `ncores`).

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

The method works with posterior predictive functionals rather than raw
model parameters. Supported output types include:

- `"density"` for \\f(y \mid x, \mathcal{D})\\,

- `"survival"` for \\S(y \mid x, \mathcal{D}) = 1 - F(y \mid x,
  \mathcal{D})\\,

- `"quantile"` for \\Q(\tau \mid x, \mathcal{D})\\,

- `"mean"` for \\E(Y \mid x, \mathcal{D})\\,

- `"rmean"` for \\E\\\min(Y, c) \mid x, \mathcal{D}\\\\,

- `"sample"` and `"fit"` for draw-level predictive output.

For spliced models these predictions integrate over both the DPM bulk
and the GPD tail. When `type = "mean"`, the function averages
conditional means over posterior draws, so the result is a Bayesian
predictive mean.

The `type="mean"` option computes the posterior predictive mean by:

1.  For each posterior draw s, computing E(Y \| x, theta_s) via Monte
    Carlo simulation

2.  Averaging these conditional means over the posterior: E(Y \| x,
    data) = mean_s(E(Y \| x, theta_s))

This accounts for parameter uncertainty and is the Bayesian predictive
mean. When the tail shape parameter xi \>= 1 (heavy tail), the mean is
undefined and the function returns Inf with a warning suggesting
alternatives like median or restricted mean.

## See also

[`summary.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.mixgpd_fit.md),
[`fitted.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/fitted.mixgpd_fit.md),
[`residuals.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/residuals.mixgpd_fit.md),
[`predict.causalmixgpd_causal_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.causalmixgpd_causal_fit.md).

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
# Restricted mean (finite under heavy tails)
pr_rmean <- predict(fit, type = "rmean", cutoff = 10, interval = "credible")
} # }
```
