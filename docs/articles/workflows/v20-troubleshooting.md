# 20. Troubleshooting

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

## Troubleshooting

This vignette collects common errors and fixes encountered when building
or running DPmixGPD models.

------------------------------------------------------------------------

### 1. “Number of columns in ‘x’ does not match training design matrix”

**Cause**: New data has different columns than the training `X`.

**Fix**: Use the same column names and order as the training matrix.

``` r

X_train <- fit$data$X
X_new <- X_train[1:5, , drop = FALSE]
X_new <- X_new[, colnames(X_train), drop = FALSE]

predict(fit, x = X_new, type = "mean")
```

------------------------------------------------------------------------

### 2. “Cauchy kernels are never paired with GPD tails”

**Cause**: The Cauchy kernel is not allowed with GPD tails.

**Fix**: Use a different kernel or disable GPD.

``` r

bundle <- build_nimble_bundle(
  y = y,
  kernel = "cauchy",
  backend = "sb",
  GPD = FALSE
)
```

------------------------------------------------------------------------

### 3. “GPD tail not moving” (threshold/shape stuck)

**Cause**: Very short chains or overly restrictive priors.

**Fix**: Increase iterations or relax tail priors.

``` r

bundle <- build_nimble_bundle(
  y = y,
  kernel = "gamma",
  backend = "crp",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)
```

------------------------------------------------------------------------

### 4. “future.globals.maxSize exceeded”

**Cause**: Parallel predictions export large functions/objects.

**Fix**: Set a higher limit or use `ncores = 1`.

``` r

options(future.globals.maxSize = 4 * 1024^3)

pred <- predict(fit, x = X_new, y = y_grid, type = "density", ncores = 2)
```

------------------------------------------------------------------------

### 5. MCMC is slow

**Cause**: Large datasets, many components, or heavy kernels.

**Fix**: Use smaller `components`/`J`, shorter `niter`, or start with
CRP.

``` r

bundle <- build_nimble_bundle(
  y = y,
  backend = "crp",
  kernel = "normal",
  components = 4,
  mcmc = mcmc
)
```

------------------------------------------------------------------------

### 6. Prediction returns constant fitted values in unconditional models

**Cause**: Unconditional fitted values are model-based population
summaries.

**Fix**: This is expected. Use conditional models if you need
observation-specific fitted values.

``` r

fit_vals <- fitted(fit)
head(fit_vals)
```

------------------------------------------------------------------------

### 7. PS not used in causal model

**Cause**: `PS=FALSE` or `X` missing.

**Fix**: Provide `X` and enable PS.

``` r

cb <- build_causal_bundle(
  y = y,
  T = T,
  X = X,
  PS = "logit"
)
fit <- run_mcmc_causal(cb)
```

------------------------------------------------------------------------

### 8. Kernel mismatch when mixing arms

**Cause**: Using kernels with different supports across arms.

**Fix**: Ensure both arms use kernels on the same support (e.g., both
positive-support or both real-line).

``` r

cb <- build_causal_bundle(
  y = y,
  T = T,
  X = X,
  kernel = c("gamma", "lognormal"),
  backend = c("sb", "crp")
)
```

------------------------------------------------------------------------

### 9. “Cannot resolve owner of file” / “No mapping between account names and security IDs” (Windows)

**Cause**: The project or package lives in OneDrive (or another
synced/network path). Windows cannot map file-owner SIDs to account
names when R uses [`file.info()`](https://rdrr.io/r/base/file.info.html)
on those paths.

**Fix**: The warnings are harmless; DPmixGPD and nimble load and run
normally. If you **keep the project in OneDrive** (e.g. as a backup to
GitHub), the project `.Rprofile` suppresses these warnings automatically
when using R ≥ 4.0. Otherwise, move the project to a local non-synced
path or take ownership of the OneDrive folder (right‑click → Properties
→ Security → Advanced → Take ownership).
