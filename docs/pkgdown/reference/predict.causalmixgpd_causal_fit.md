# Predict arm-specific and contrast-scale quantities from a causal fit

`predict.causalmixgpd_causal_fit()` is the causal counterpart to
[`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md).
It coordinates the treated and control arm predictions so that both
sides use the same covariate rows and the same PS adjustment.

## Usage

``` r
# S3 method for class 'causalmixgpd_causal_fit'
predict(
  object,
  x = NULL,
  y = NULL,
  ps = NULL,
  id = NULL,
  newdata = NULL,
  type = c("mean", "quantile", "density", "survival", "prob", "location"),
  p = NULL,
  index = NULL,
  nsim = NULL,
  interval = "credible",
  probs = c(0.025, 0.5, 0.975),
  store_draws = TRUE,
  nsim_mean = 200L,
  ncores = 1L,
  show_progress = TRUE,
  ...
)
```

## Arguments

- object:

  A `"causalmixgpd_causal_fit"` object returned by
  [`run_mcmc_causal()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_causal.md).

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

- id:

  Optional identifier for prediction rows. Provide either a column name
  in `x`/`newdata` or a vector of length `nrow(x)`. The id column is
  excluded from analysis.

- newdata:

  Optional new data. If `NULL`, uses training design (if stored).

- type:

  Prediction type. Supported: `"mean"`, `"quantile"`, `"density"`,
  `"survival"`, `"prob"`, `"location"`.

- p:

  Numeric vector of probabilities for quantiles (required for
  `type="quantile"`).

- index:

  Alias for `p`; numeric vector of quantile levels.

- nsim:

  Number of posterior predictive samples. For
  `predict.causalmixgpd_causal_fit()`, posterior sampling output is not
  available because `type="sample"` is not supported by this causal
  method; this argument is effectively unused.

- interval:

  Character or NULL; type of credible interval: `NULL` for no interval,
  `"credible"` for equal-tailed quantile intervals (default), or `"hpd"`
  for highest posterior density intervals.

- probs:

  Quantiles for credible interval bands.

- store_draws:

  Logical; whether to store posterior draws. For
  `predict.causalmixgpd_causal_fit()`, stored draws are not available
  because `type="sample"` is not supported by this causal method; this
  argument is effectively unused.

- nsim_mean:

  Number of posterior predictive samples to use for posterior mean
  estimation (for `type="mean"`).

- ncores:

  Number of CPU cores to use for parallel prediction (if supported).

- show_progress:

  Logical; if TRUE, print step messages and render progress where
  supported.

- ...:

  Additional arguments forwarded to per-arm
  [`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md)
  calls.

## Value

For `"mean"` and `"quantile"`, a causal prediction object whose `$fit`
component reports treated-minus-control posterior summaries. For
`"density"`, `"survival"`, and `"prob"`, the `$fit` component contains
side-by-side treated and control summaries evaluated on the supplied `y`
grid.

## Details

For each prediction row \\x\\, the function evaluates arm-specific
posterior predictive quantities based on \\F_1(y \mid x, \mathcal{D})\\
and \\F_0(y \mid x, \mathcal{D})\\. Mean and quantile outputs are
returned on the treatment-effect scale, while density, survival, and
probability outputs retain both arm-specific curves.

If a PS model is stored in the fit, the same estimated score is supplied
to both arms unless the user overrides it with `ps`. This is the main
prediction entry point used internally by
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md),
and
[`cqte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md).

## See also

[`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md),
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md),
[`cqte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal")
fit <- run_mcmc_causal(cb)
predict(fit, x = X[1:10, ], type = "quantile", index = c(0.25, 0.5, 0.75))
predict(fit, x = X[1:10, ], type = "mean", interval = "hpd")  # HPD intervals
predict(fit, x = X[1:10, ], type = "mean", interval = NULL)   # No intervals
} # }
```
