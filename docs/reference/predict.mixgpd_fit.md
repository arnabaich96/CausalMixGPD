# Predict from a MixGPD fit

This provides a stable interface for distributional predictions. The
default implementation supports: - `type="density"` using `y` -
`type="survival"` using `y` - `type="quantile"` using `p` -
`type="sample"` (posterior predictive draws) - `type="mean"` (posterior
predictive mean)

## Usage

``` r
# S3 method for class 'mixgpd_fit'
predict(
  object,
  x = NULL,
  y = NULL,
  ps = NULL,
  newdata = NULL,
  type = c("density", "survival", "quantile", "sample", "mean"),
  p = NULL,
  index = NULL,
  nsim = NULL,
  cred.level = 0.95,
  interval = c("none", "credible"),
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

  Ignored. Propensity scores are always computed internally from the
  fitted PS model or stored training PS. For new covariates, PS are
  derived from the attached PS posterior draws when available. For
  causal workflows, use
  [`qte()`](https://example.com/DPmixGPD/reference/qte.md) /
  [`ate()`](https://example.com/DPmixGPD/reference/ate.md) which
  orchestrate PS estimation and outcome prediction jointly.

- newdata:

  Optional new data. If `NULL`, uses training design (if stored).

- type:

  Prediction type: `"density"`, `"survival"`, `"quantile"`, `"sample"`,
  `"mean"`.

- p:

  Numeric vector of probabilities for quantiles (required for
  `type="quantile"`).

- nsim:

  Number of posterior predictive samples (for `type="sample"`).

- interval:

  `"none"` or `"credible"` for posterior credible bands.

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

- `fit`: matrix (nrow = n_newdata, ncol = length(p or y)) of posterior
  medians.

- `lower`, `upper`: matrices for credible interval if requested (else
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
} # }
```
