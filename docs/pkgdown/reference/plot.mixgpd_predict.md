# Plot prediction results

Generates type-specific visualizations for prediction objects returned
by
[`predict.mixgpd_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md).
Each prediction type produces a tailored plot:

- `quantile`: Quantile indices vs estimates with credible intervals

- `sample`: Histogram of samples with density overlay

- `mean`: Histogram density with posterior mean vertical line and CI
  bounds

- `density`: Density values vs evaluation points

- `survival`: Survival function (decreasing y values)

## Usage

``` r
# S3 method for class 'mixgpd_predict'
plot(x, y = NULL, ...)
```

## Arguments

- x:

  A prediction object returned by
  [`predict.mixgpd_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md).

- y:

  Ignored; included for S3 compatibility.

- ...:

  Additional arguments passed to ggplot2 functions.

## Value

Invisibly returns the ggplot object.

## Details

The plotting method is tied to the predictive functional stored in the
input object. Quantile and mean outputs display posterior point
summaries and intervals, density and survival outputs show evaluated
functions on the supplied grid, and posterior samples are visualized as
empirical predictive draws.

In every case the plot reflects the quantity requested from
[`predict.mixgpd_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md)
after integrating over the retained posterior draws. It is therefore
distinct from parameter-level summaries and from chain diagnostics.

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = TRUE, components = 6,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)

# Quantile prediction with plot
pred_q <- predict(fit, type = "quantile", index = c(0.25, 0.5, 0.75))
plot(pred_q)

# Sample prediction with plot
pred_s <- predict(fit, type = "sample", nsim = 500)
plot(pred_s)

# Mean prediction with plot
pred_m <- predict(fit, type = "mean", nsim_mean = 300)
plot(pred_m)
} # }
```
