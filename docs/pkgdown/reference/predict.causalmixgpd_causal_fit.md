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
  newdata = NULL,
  y = NULL,
  ps = NULL,
  id = NULL,
  type = c("mean", "quantile", "density", "survival", "prob", "sample"),
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

- newdata:

  Optional new data. If `NULL`, uses training design (if stored).

- y:

  Numeric vector of evaluation points (required for `type="density"` or
  `"survival"`).

- ps:

  Optional numeric vector of propensity scores aligned with `newdata`.
  When provided, the supplied scores are used instead of recomputing
  them from the stored PS model (needed only for custom inputs).

- id:

  Optional identifier for prediction rows. Provide either a column name
  in `newdata` or a vector of length `nrow(newdata)`. The id column is
  excluded from analysis.

- type:

  Prediction type:

  - `"mean"`: posterior predictive mean treatment effect

  - `"quantile"`: posterior predictive quantile treatment effect

  - `"density"`: arm-specific posterior predictive densities

  - `"survival"`: arm-specific posterior predictive survival functions

  - `"prob"`: arm-specific posterior predictive probabilities

  - `"sample"`: paired posterior predictive samples

- p:

  Numeric vector of probabilities for quantiles (required for
  `type="quantile"`).

- index:

  Alias for `p`; numeric vector of quantile levels.

- nsim:

  Number of posterior predictive samples when `type = "sample"`.

- interval:

  Character or NULL; type of credible interval:

  - `NULL`: no interval

  - `"credible"` (default): equal-tailed quantile intervals

  - `"hpd"`: highest posterior density intervals

- probs:

  Quantiles for credible interval bands.

- store_draws:

  Logical; whether to store treatment-effect sample draws in the
  returned object when `type = "sample"`.

- nsim_mean:

  Number of posterior predictive samples used by simulation-based mean
  targets. Ignored for analytical `type = "mean"`; still used for
  `type = "rmean"`.

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
grid. For `"sample"`, the returned object contains paired treated,
control, and treatment-effect posterior predictive samples. Sample
outputs also include long-form data frames `$fit_df`, `$trt_fit_df`, and
`$con_fit_df` for direct extraction.

## Details

For each prediction row \\x\\, the function evaluates arm-specific
posterior predictive quantities based on \\F_1(y \mid x)\\ and \\F_0(y
\mid x)\\. Mean and quantile outputs are returned on the
treatment-effect scale, while density, survival, and probability outputs
retain both arm-specific curves. For outcome kernels with a finite
analytical mean, the mean path uses analytical per-draw means;
restricted means remain simulation-based.

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
predict(fit, newdata = X[1:10, ], type = "quantile", index = c(0.25, 0.5, 0.75))
predict(fit, newdata = X[1:10, ], type = "mean", interval = "hpd")  # HPD intervals
predict(fit, newdata = X[1:10, ], type = "mean", interval = NULL)   # No intervals
} # }
```
