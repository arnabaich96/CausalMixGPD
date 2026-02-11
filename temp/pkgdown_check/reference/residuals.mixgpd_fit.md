# Residuals for a MixGPD fit

Returns residuals aligned with the training data for **conditional
(covariate) models only**. Not supported for unconditional models (no
covariates); use [`predict()`](https://rdrr.io/r/stats/predict.html) for
predictions in that case. For `type = "raw"`, this uses fitted means.
For `type = "pit"`, this returns approximate PIT values via the
predictive survival function. The plug-in PIT uses the posterior mean
CDF, while the Bayesian PIT modes use draw-wise CDFs (averaged or
sampled).

## Usage

``` r
# S3 method for class 'mixgpd_fit'
residuals(
  object,
  type = c("raw", "pit"),
  fitted_type = c("mean", "median"),
  pit = c("plugin", "bayes_mean", "bayes_draw"),
  pit_seed = NULL,
  ...
)
```

## Arguments

- object:

  A fitted object of class `"mixgpd_fit"` (must have covariates).

- type:

  Residual type: `"raw"` or `"pit"`.

- fitted_type:

  For `type = "raw"`, use fitted means or medians.

- pit:

  PIT mode for `type = "pit"`:

  - `"plugin"`: plug-in PIT using the posterior mean CDF.

  - `"bayes_mean"`: Bayesian PIT using draw-wise CDFs averaged over
    draws.

  - `"bayes_draw"`: Bayesian PIT using a single draw-wise CDF per
    observation.

  Bayesian PIT modes drop invalid posterior draws using the same
  validation rules as prediction and attach diagnostics via
  `attr(res, "pit_diagnostics")`.

- pit_seed:

  Optional integer seed for reproducible `bayes_draw` sampling.

- ...:

  Unused.

## Value

Numeric vector of residuals with length equal to the training sample
size.

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
X <- data.frame(x1 = stats::rnorm(50), x2 = stats::runif(50))
bundle <- build_nimble_bundle(y = y, X = X, backend = "sb", kernel = "lognormal",
                             GPD = FALSE, components = 4,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)
pit_plugin <- residuals(fit, type = "pit", pit = "plugin")
pit_bayes_mean <- residuals(fit, type = "pit", pit = "bayes_mean", pit_seed = 1L)
pit_bayes_draw <- residuals(fit, type = "pit", pit = "bayes_draw", pit_seed = 1L)
attr(pit_bayes_draw, "pit_diagnostics")
} # }
```
