# Conditional Modeling

## Goal

Conditional modeling lets **distributional features** (not just means)
vary with covariates. This is **not** mean regression; it is full
distributional regression.

## Example data

``` r
data("nc_posX100_p3_k2")
X <- as.matrix(nc_posX100_p3_k2$X)
y <- nc_posX100_p3_k2$y
```

## Conditional mixture

``` r
bundle_cond <- build_nimble_bundle(
  y = y,
  X = X,
  kernel = "lognormal",
  backend = "sb",
  GPD = TRUE,
  J = 4,
  mcmc = list(niter = 250, nburnin = 50, nchains = 1)
)
fit_cond <- run_mcmc_bundle_manual(bundle_cond, show_progress = FALSE)
```

## Interpretation across quantiles

``` r
x_new <- X[1:10, , drop = FALSE]
q_pred <- predict(fit_cond, x = x_new, type = "quantile", index = c(0.1, 0.5, 0.9))
plot(q_pred)
```

## Covariates and tails

By allowing tail parameters to depend on $`X`$, DPmixGPD can capture
**heterogeneous tail risk** (e.g., extreme outcomes becoming more likely
in certain covariate regimes).

## Connection to unconditional models

The unconditional case is the special case with **no covariates**. See
[unconditional](https://arnabaich96.github.io/DPmixGPD/articles/unconditional.Rmd).

## What this is NOT

- It is **not** treatment effect modeling.
- It is **not** identification or causal inference.

For causal contrasts, see
[causal](https://arnabaich96.github.io/DPmixGPD/articles/causal.Rmd).
