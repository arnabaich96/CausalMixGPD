# 5. Basic Workflow: Model Specification, Bundle, & MCMC

## The Three-Phase Workflow

DPmixGPD uses a **three-phase model-building paradigm** designed for
clarity and control:

1.  **Specification** (`compile_model_spec`): Validate inputs, NO NIMBLE
    code generation
2.  **Bundle** (`build_nimble_bundle`): Generate NIMBLE code, compile
    sampler
3.  **MCMC** (`run_mcmc_bundle_manual`): Execute posterior sampling

------------------------------------------------------------------------

### Phase 1: Specification (Input Validation)

**Purpose**: Validate data, kernels, backends, and parameters WITHOUT
generating NIMBLE code.

``` r
# Load packaged data
data("nc_pos200_k3")
y <- nc_pos200_k3$y

# Create specification
spec <- compile_model_spec(
  y = y,
  kernel = "gamma",
  backend = "crp",
  GPD = FALSE
)

# Inspect specification
print("Specification object class:", class(spec), "\n")
print("Kernel chosen:", spec$kernel, "\n")
print("Backend chosen:", spec$backend, "\n")
print("GPD enabled:", spec$GPD, "\n")
print("Sample size n:", length(spec$y), "\n")
```

#### Specification for Different Model Variants

``` r
# 1. Unconditional DPmix (bulk only, no tail)
spec_unconditional <- compile_model_spec(y, kernel = "gamma", backend = "crp", GPD = FALSE)

# 2. Unconditional DPmixGPD (bulk + tail)
spec_gpd <- compile_model_spec(y, kernel = "gamma", backend = "crp", GPD = TRUE)

# 3. Conditional (with covariates)
data("nc_posX100_p3_k2")
y_cond <- nc_posX100_p3_k2$y
X <- as.matrix(nc_posX100_p3_k2$X)
spec_conditional <- compile_model_spec(
  y = y_cond,
  X = X,
  kernel = "lognormal",
  backend = "crp",
  GPD = FALSE
)

# 4. With custom MCMC settings
spec_custom <- compile_model_spec(
  y = y,
  kernel = "gamma",
  backend = "crp",
  GPD = FALSE,
  mcmc = list(niter = 1500, nburnin = 250, nchains = 2, thin = 2)
)

print("Created 4 specification objects successfully.\n")
```

------------------------------------------------------------------------

### Phase 2: Bundle (NIMBLE Code Generation & Compilation)

**Purpose**: Generate NIMBLE model code, compile sampler, prepare for
MCMC execution.

#### Building from Specification

``` r
# Method 1: From specification object
bundle <- build_nimble_bundle(spec)

print("Bundle object class:", class(bundle), "\n")
print("Bundle contains compiled sampler ready for MCMC.\n")
```

#### Building Directly (Alternative)

``` r
# Method 2: Direct call (recommended for quick workflows)
bundle_direct <- build_nimble_bundle(
  y = y,
  kernel = "gamma",
  backend = "crp",
  GPD = FALSE,
  mcmc = list(niter = 1500, nburnin = 250, nchains = 2)
)

print("Direct bundle creation successful.\n")
```

#### Inspecting Bundle Contents

``` r
# Bundle is an S3 object with structure
print("Bundle class:", class(bundle_direct), "\n")
print("Bundle contains:\n")
print(names(bundle_direct))

# Access key components
print("\nMCMC settings:\n")
print(bundle_direct$mcmc_settings)
```

------------------------------------------------------------------------

### Phase 3: MCMC Execution

**Purpose**: Run posterior sampling from the compiled bundle.

#### Basic MCMC Run

``` r
# Run MCMC
fit <- run_mcmc_bundle_manual(bundle_direct)

print("Fit object class:", class(fit), "\n")
print("MCMC execution complete. Posterior samples collected.\n")
```

#### Accessing Posterior Samples

``` r
# Posterior summary
print("\n--- POSTERIOR SUMMARY ---\n")
summary(fit)

# Posterior mean parameters in original form
params_fit <- params(fit)
params_fit
```

#### Diagnostic Plots

``` r
# Trace plots
plot(fit, params = "alpha|beta", family = c("traceplot", "running", "autocorrelation"))
```

------------------------------------------------------------------------

### Complete Workflow: End-to-End Example

``` r
# Load packaged data
data("nc_pos200_k3")
y_data <- nc_pos200_k3$y

# PHASE 1: Specification
spec_final <- compile_model_spec(
  y = y_data,
  kernel = "gamma",
  backend = "crp",
  GPD = FALSE,
  Kmax = 5
)

# PHASE 2: Bundle
bundle_final <- build_nimble_bundle(
  spec_final,
  mcmc = list(
    niter = 60,
    nburnin = 10,
    nchains = 2,
    thin = 1
  )
)

# PHASE 3: MCMC
fit_final <- run_mcmc_bundle_manual(bundle_final)

print("\n=== THREE-PHASE WORKFLOW COMPLETE ===\n")
summary(fit_final)
```

------------------------------------------------------------------------

### Backend Comparison: CRP vs Stick-Breaking

#### CRP Backend

``` r
# Chinese Restaurant Process
bundle_crp <- build_nimble_bundle(
  y = y_data,
  kernel = "gamma",
  backend = "crp",
  Kmax = 5,
  mcmc = list(niter = 1500, nburnin = 250, nchains = 2)
)

fit_crp <- run_mcmc_bundle_manual(bundle_crp)
print("CRP execution complete.\n")
```

#### Stick-Breaking Backend

``` r
# Stick-Breaking Process
bundle_sb <- build_nimble_bundle(
  y = y_data,
  kernel = "gamma",
  backend = "sb",
  J = 5,
  mcmc = list(niter = 100, nburnin = 20, nchains = 3)
)

fit_sb <- run_mcmc_bundle_manual(bundle_sb)
print("SB execution complete.\n")
```

------------------------------------------------------------------------

### Kernel Selection Guide

``` r
kernels_available <- c("gamma", "lognormal", "normal", "laplace", "invgauss", "amoroso", "cauchy")

print("Available kernels:\n")
for (k in kernels_available) {
  print("  -", k, "\n")
}

print("\nChoose kernel based on:\n")
print("  gamma:     Right-skewed, positive support\n")
print("  lognormal: Log-transformed normality\n")
print("  normal:    Symmetric, unbounded\n")
print("  laplace:   Sharp peak, exponential tails\n")
print("  invgauss:  Positive, near-normal shape\n")
print("  amoroso:   Generalized, maximum flexibility\n")
print("  cauchy:    Heavy-tailed, rare cases\n")
```

------------------------------------------------------------------------

### GPD Tail Augmentation

#### Unconditional with GPD

``` r
# Data with tail behavior
data("nc_pos_tail200_k4")
y_tail <- nc_pos_tail200_k4$y

# Build with GPD
bundle_gpd <- build_nimble_bundle(
  y = y_tail,
  kernel = "gamma",
  backend = "crp",
  GPD = TRUE,
  mcmc = list(niter = 2500, nburnin = 500, nchains = 1)
)

fit_gpd <- run_mcmc_bundle_manual(bundle_gpd)
print("\nGPD augmentation applied to tail region.\n")
summary(fit_gpd)
```

------------------------------------------------------------------------

### Summary of Key Functions

| Phase | Function | Input | Output |
|----|----|----|----|
| **1. Spec** | `compile_model_spec()` | y, X (optional), kernel, backend, GPD | `dpmixgpd_spec` |
| **2. Bundle** | [`build_nimble_bundle()`](https://arnabaich96.github.io/DPmixGPD/reference/build_nimble_bundle.md) | spec or y, kernel, backend, GPD | `dpmixgpd_bundle` |
| **3. MCMC** | [`run_mcmc_bundle_manual()`](https://arnabaich96.github.io/DPmixGPD/reference/run_mcmc_bundle_manual.md) | bundle | `mixgpd_fit` |

------------------------------------------------------------------------

### Common Parameter Settings

``` r
print("=== Recommended MCMC Parameters ===\n")
print("Quick test:     niter=500,  nburnin=100, nchains=1\n")
print("Standard:       niter=1000, nburnin=250, nchains=2\n")
print("Production:     niter=1000, nburnin=250, nchains=3\n")
print("\n=== Backend Parameters ===\n")
print("CRP: Kmax=3-5 (truncation for # components)\n")
print("SB:  J=3-5    (fixed # components)\n")
print("\n=== Kernel Selection ===\n")
print("Positive data:     gamma, lognormal, invgauss\n")
print("Any real data:     normal\n")
print("Symmetric tails:   laplace\n")
print("Extreme outliers:  cauchy\n")
```

------------------------------------------------------------------------

### Next Steps

- Move to **vignette 6-7** for unconditional models (CRP vs SB backends)
- Move to **vignette 8-9** for tail modeling with GPD
- Move to **vignette 10-13** for conditional models with covariates
- Move to **vignette 14-19** for causal inference workflows
