# Conditional average treatment effects

`cate()` evaluates treated-minus-control predictive means, or restricted
means, at user-supplied covariate rows.

## Usage

``` r
cate(
  fit,
  newdata = NULL,
  type = c("mean", "rmean"),
  cutoff = NULL,
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

- type:

  Character; type of mean treatment effect:

  - `"mean"` (default): ordinary mean CATE

  - `"rmean"`: restricted-mean CATE (requires `cutoff`)

- cutoff:

  Finite numeric cutoff for restricted mean; required for
  `type = "rmean"`, ignored otherwise.

- interval:

  Character or NULL; type of credible interval:

  - `NULL`: no interval

  - `"credible"` (default): equal-tailed quantile intervals

  - `"hpd"`: highest posterior density intervals

- level:

  Numeric credible level for intervals (default 0.95 for 95 percent CI).

- nsim_mean:

  Number of posterior predictive draws used by simulation-based mean
  targets. Ignored for analytical ordinary means.

- show_progress:

  Logical; if TRUE, print step messages and render progress where
  supported.

## Value

An object of class `"causalmixgpd_ate"` containing the CATE summary,
optional intervals, and the treated/control prediction objects used to
construct the effect. The returned object includes a top-level `$fit_df`
data frame for direct extraction.

## Details

For each prediction row \\x\\, the conditional average treatment effect
is \$\$\mathrm{CATE}(x) = E\\Y(1) \mid x\\ - E\\Y(0) \mid x\\.\$\$

With `type = "rmean"`, the estimand becomes the conditional restricted
mean contrast \$\$E\\\min(Y(1), c) \mid x\\ - E\\\min(Y(0), c) \mid
x\\,\$\$ which remains finite even when the ordinary mean is unstable
under a heavy GPD tail. For outcome kernels with a finite analytical
mean, the ordinary mean path is analytical within each posterior draw;
`rmean` remains a separate simulation-based estimand.

This estimand is available only for conditional causal models with
covariates. For marginal mean contrasts, use
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md)
or
[`att`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/att.md).

## See also

[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`att`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/att.md),
[`cqte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md),
[`ate_rmean`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate_rmean.md),
[`predict.causalmixgpd_causal_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.causalmixgpd_causal_fit.md).

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
