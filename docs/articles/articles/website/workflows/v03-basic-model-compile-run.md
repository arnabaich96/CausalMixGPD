# 3. Basic Workflow: Model Specification, Bundle, & MCMC

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

## Workflow Overview

DPmixGPD uses a direct two-step workflow:

1.  **Bundle** (`build_nimble_bundle`): Generate NIMBLE code and compile
    the sampler.
2.  **MCMC** (`run_mcmc_bundle_manual`): Execute posterior sampling.

------------------------------------------------------------------------

### Phase 1: Bundle (NIMBLE Code Generation & Compilation)

**Purpose**: Generate NIMBLE model code, compile sampler, prepare for
MCMC execution.

#### Building Directly

``` r

# Load packaged data
data("nc_pos200_k3")
y <- nc_pos200_k3$y

# Direct call
bundle_direct <- build_nimble_bundle(
  y = y,
  kernel = "gamma",
  backend = "crp",
  GPD = FALSE,
  components = 5,
  mcmc = mcmc
)

print("Direct bundle creation successful.\n")
```

    [1] "Direct bundle creation successful.\n"

#### Inspecting Bundle Contents

``` r

# Bundle is an S3 object with structure
cat("Bundle class:", class(bundle_direct), "\n")
```

    Bundle class: dpmixgpd_bundle 

``` r

cat("Bundle contains:\n")
```

    Bundle contains:

``` r

print(names(bundle_direct))
```

    [1] "spec"       "code"       "constants"  "dimensions" "data"      
    [6] "inits"      "monitors"   "mcmc"       "epsilon"   

``` r

# Access key components
cat("\nMCMC settings:\n")
```


    MCMC settings:

``` r

print(bundle_direct$mcmc_settings)
```

    NULL

------------------------------------------------------------------------

### Phase 3: MCMC Execution

**Purpose**: Run posterior sampling from the compiled bundle.

#### Basic MCMC Run

``` r

# Run MCMC
fit <- load_or_fit("v03-basic-model-compile-run-fit", run_mcmc_bundle_manual(bundle_direct, show_progress = FALSE))

cat("Fit object class:", class(fit), "\n")
```

    Fit object class: mixgpd_fit list 

``` r

cat("MCMC execution complete. Posterior samples collected.\n")
```

    MCMC execution complete. Posterior samples collected.

#### Accessing Posterior Samples

``` r

# Posterior summary
print("\n--- POSTERIOR SUMMARY ---\n")
```

    [1] "\n--- POSTERIOR SUMMARY ---\n"

``` r

summary(fit)
```

    MixGPD summary | backend: Chinese Restaurant Process | kernel: Gamma Distribution | GPD tail: FALSE | epsilon: 0.025
    n = 200 | components = 5
    Summary
    Initial components: 5 | Components after truncation: 1

    WAIC: 929.279
    lppd: -399.887 | pWAIC: 64.752

    Summary table
    <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
     <thead>
      <tr>
       <th style="text-align:center;"> parameter </th>
       <th style="text-align:center;"> mean </th>
       <th style="text-align:center;"> sd </th>
       <th style="text-align:center;"> q0.025 </th>
       <th style="text-align:center;"> q0.500 </th>
       <th style="text-align:center;"> q0.975 </th>
       <th style="text-align:center;"> ess </th>
      </tr>
     </thead>
    <tbody>
      <tr>
       <td style="text-align:center;"> weights[1] </td>
       <td style="text-align:center;"> 0.684 </td>
       <td style="text-align:center;"> 0.216 </td>
       <td style="text-align:center;"> 0.36 </td>
       <td style="text-align:center;"> 0.63 </td>
       <td style="text-align:center;"> 1 </td>
       <td style="text-align:center;"> 25.041 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> alpha </td>
       <td style="text-align:center;"> 0.559 </td>
       <td style="text-align:center;"> 0.388 </td>
       <td style="text-align:center;"> 0.028 </td>
       <td style="text-align:center;"> 0.498 </td>
       <td style="text-align:center;"> 1.526 </td>
       <td style="text-align:center;"> 204.872 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> shape[1] </td>
       <td style="text-align:center;"> 1.784 </td>
       <td style="text-align:center;"> 0.847 </td>
       <td style="text-align:center;"> 0.943 </td>
       <td style="text-align:center;"> 1.483 </td>
       <td style="text-align:center;"> 3.912 </td>
       <td style="text-align:center;"> 27.633 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> scale[1] </td>
       <td style="text-align:center;"> 0.494 </td>
       <td style="text-align:center;"> 0.343 </td>
       <td style="text-align:center;"> 0.212 </td>
       <td style="text-align:center;"> 0.35 </td>
       <td style="text-align:center;"> 1.402 </td>
       <td style="text-align:center;"> 59.82 </td>
      </tr>
    </tbody>
    </table>

``` r

# Posterior mean parameters in original form
params_fit <- params(fit)
params_fit
```

    Posterior mean parameters

    $alpha
    [1] "0.559"

    $w
    [1] "0.684"

    $shape
    [1] "1.784"

    $scale
    [1] "0.494"

#### Diagnostic Plots

``` r

# Trace plots
if (interactive()) plot(fit, params = "alpha|beta", family = c("traceplot", "running", "autocorrelation"))
```

------------------------------------------------------------------------

### Complete Workflow: End-to-End Example

``` r

# Load packaged data
data("nc_pos200_k3")
y_data <- nc_pos200_k3$y

# PHASE 1: Bundle
bundle_final <- build_nimble_bundle(
  y = y_data,
  kernel = "gamma",
  backend = "crp",
  GPD = FALSE,
  components = 5,
  mcmc = mcmc
)

# PHASE 2: MCMC
fit_final <- load_or_fit("v03-basic-model-compile-run-fit_final", run_mcmc_bundle_manual(bundle_final, show_progress = FALSE))

print("\n=== THREE-PHASE WORKFLOW COMPLETE ===\n")
```

    [1] "\n=== THREE-PHASE WORKFLOW COMPLETE ===\n"

``` r

summary(fit_final)
```

    MixGPD summary | backend: Chinese Restaurant Process | kernel: Gamma Distribution | GPD tail: FALSE | epsilon: 0.025
    n = 200 | components = 5
    Summary
    Initial components: 5 | Components after truncation: 1

    WAIC: 943.55
    lppd: -417.985 | pWAIC: 53.791

    Summary table
    <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
     <thead>
      <tr>
       <th style="text-align:center;"> parameter </th>
       <th style="text-align:center;"> mean </th>
       <th style="text-align:center;"> sd </th>
       <th style="text-align:center;"> q0.025 </th>
       <th style="text-align:center;"> q0.500 </th>
       <th style="text-align:center;"> q0.975 </th>
       <th style="text-align:center;"> ess </th>
      </tr>
     </thead>
    <tbody>
      <tr>
       <td style="text-align:center;"> weights[1] </td>
       <td style="text-align:center;"> 0.701 </td>
       <td style="text-align:center;"> 0.192 </td>
       <td style="text-align:center;"> 0.39 </td>
       <td style="text-align:center;"> 0.665 </td>
       <td style="text-align:center;"> 1 </td>
       <td style="text-align:center;"> 27.636 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> alpha </td>
       <td style="text-align:center;"> 0.543 </td>
       <td style="text-align:center;"> 0.388 </td>
       <td style="text-align:center;"> 0.037 </td>
       <td style="text-align:center;"> 0.464 </td>
       <td style="text-align:center;"> 1.511 </td>
       <td style="text-align:center;"> 183.632 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> shape[1] </td>
       <td style="text-align:center;"> 1.599 </td>
       <td style="text-align:center;"> 0.622 </td>
       <td style="text-align:center;"> 0.926 </td>
       <td style="text-align:center;"> 1.422 </td>
       <td style="text-align:center;"> 3.312 </td>
       <td style="text-align:center;"> 35.47 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> scale[1] </td>
       <td style="text-align:center;"> 0.409 </td>
       <td style="text-align:center;"> 0.266 </td>
       <td style="text-align:center;"> 0.198 </td>
       <td style="text-align:center;"> 0.299 </td>
       <td style="text-align:center;"> 1.132 </td>
       <td style="text-align:center;"> 40.43 </td>
      </tr>
    </tbody>
    </table>

------------------------------------------------------------------------

### Backend Comparison: CRP vs Stick-Breaking

#### CRP Backend

``` r

# Chinese Restaurant Process
bundle_crp <- build_nimble_bundle(
  y = y_data,
  kernel = "gamma",
  backend = "crp",
  components = 5,
  mcmc = mcmc
)

fit_crp <- load_or_fit("v03-basic-model-compile-run-fit_crp", run_mcmc_bundle_manual(bundle_crp, show_progress = FALSE))
print("CRP execution complete.\n")
```

    [1] "CRP execution complete.\n"

#### Stick-Breaking Backend

``` r

# Stick-Breaking Process
bundle_sb <- build_nimble_bundle(
  y = y_data,
  kernel = "gamma",
  backend = "sb",
  components = 5,
  mcmc = mcmc
)

fit_sb <- load_or_fit("v03-basic-model-compile-run-fit_sb", run_mcmc_bundle_manual(bundle_sb, show_progress = FALSE))
print("SB execution complete.\n")
```

    [1] "SB execution complete.\n"

------------------------------------------------------------------------

### Kernel Selection Guide

``` r

kernels_available <- c("gamma", "lognormal", "normal", "laplace", "invgauss", "amoroso", "cauchy")

cat("Available kernels:\n")
```

    Available kernels:

``` r

for (k in kernels_available) {
  cat("  -", k, "\n")
}
```

      - gamma 
      - lognormal 
      - normal 
      - laplace 
      - invgauss 
      - amoroso 
      - cauchy 

``` r

print("\nChoose kernel based on:\n")
```

    [1] "\nChoose kernel based on:\n"

``` r

print("  gamma:     Right-skewed, positive support\n")
```

    [1] "  gamma:     Right-skewed, positive support\n"

``` r

print("  lognormal: Log-transformed normality\n")
```

    [1] "  lognormal: Log-transformed normality\n"

``` r

print("  normal:    Symmetric, unbounded\n")
```

    [1] "  normal:    Symmetric, unbounded\n"

``` r

print("  laplace:   Sharp peak, exponential tails\n")
```

    [1] "  laplace:   Sharp peak, exponential tails\n"

``` r

print("  invgauss:  Positive, near-normal shape\n")
```

    [1] "  invgauss:  Positive, near-normal shape\n"

``` r

print("  amoroso:   Generalized, maximum flexibility\n")
```

    [1] "  amoroso:   Generalized, maximum flexibility\n"

``` r

print("  cauchy:    Heavy-tailed, rare cases\n")
```

    [1] "  cauchy:    Heavy-tailed, rare cases\n"

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
  backend = "sb",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)

fit_gpd <- load_or_fit("v03-basic-model-compile-run-fit_gpd", run_mcmc_bundle_manual(bundle_gpd, show_progress = FALSE))
print("\nGPD augmentation applied to tail region.\n")
summary(fit_gpd)
```

------------------------------------------------------------------------

### Summary of Key Functions

| Phase | Function | Input | Output |
|----|----|----|----|
| **1. Bundle** | [`build_nimble_bundle()`](https://arnabaich96.github.io/DPmixGPD/reference/build_nimble_bundle.md) | y, X (optional), kernel, backend, GPD, components | `dpmixgpd_bundle` |
| **2. MCMC** | [`run_mcmc_bundle_manual()`](https://arnabaich96.github.io/DPmixGPD/reference/run_mcmc_bundle_manual.md) | bundle | `mixgpd_fit` |

------------------------------------------------------------------------

### Common Parameter Settings

``` r

print("=== Recommended MCMC Parameters ===\n")
```

    [1] "=== Recommended MCMC Parameters ===\n"

``` r

print("Quick test:     niter=500,  nburnin=100, nchains=1\n")
```

    [1] "Quick test:     niter=500,  nburnin=100, nchains=1\n"

``` r

print("Standard:       niter=1000, nburnin=250, nchains=2\n")
```

    [1] "Standard:       niter=1000, nburnin=250, nchains=2\n"

``` r

print("Production:     niter=1000, nburnin=250, nchains=3\n")
```

    [1] "Production:     niter=1000, nburnin=250, nchains=3\n"

``` r

print("\n=== Backend Parameters ===\n")
```

    [1] "\n=== Backend Parameters ===\n"

``` r

print("Use components=3-5 for both backends in this implementation.\n")
```

    [1] "Use components=3-5 for both backends in this implementation.\n"

``` r

print("\n=== Kernel Selection ===\n")
```

    [1] "\n=== Kernel Selection ===\n"

``` r

print("Positive data:     gamma, lognormal, invgauss\n")
```

    [1] "Positive data:     gamma, lognormal, invgauss\n"

``` r

print("Any real data:     normal\n")
```

    [1] "Any real data:     normal\n"

``` r

print("Symmetric tails:   laplace\n")
```

    [1] "Symmetric tails:   laplace\n"

``` r

print("Extreme outliers:  cauchy\n")
```

    [1] "Extreme outliers:  cauchy\n"

------------------------------------------------------------------------

### Next Steps

- Move to **vignette 6-7** for unconditional models (CRP vs SB backends)
- Move to **vignette 8-9** for tail modeling with GPD
- Move to **vignette 10-13** for conditional models with covariates
- Move to **vignette 14-19** for causal inference workflows
