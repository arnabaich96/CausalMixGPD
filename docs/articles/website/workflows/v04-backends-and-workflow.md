# 4. Backends, Kernels, and Workflow Map

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

## Big picture

DPmixGPD has two orthogonal dials you turn when building models:

- **Backend** (how the mixture weights / clustering are represented)
  - **CRP**: Chinese Restaurant Process representation.
  - **SB**: stick-breaking truncation with a fixed number of components.
- **Kernel / family** (what distribution models the bulk of the data)
  - Examples: normal, lognormal, gamma, inverse-Gaussian, Laplace,
    Cauchy, Amoroso, etc.

Optionally, you can also turn on:

- **GPD = TRUE/FALSE** to splice a Generalized Pareto tail beyond a
  threshold.

## What changes between CRP and SB?

Both backends target the same posterior over densities. The difference
is representation:

- **CRP** learns a random number of occupied clusters within a finite
  `components` cap.
- **SB** uses the same finite `components` cap and learns stick-breaking
  weights.

Practical rule of thumb:

- **CRP** is convenient when you want adaptive complexity while still
  using a finite `components` cap.
- **SB** is convenient when you want predictable memory/time and easy
  vectorization.

## What does the workflow look like?

DPmixGPD uses a consistent build -\> run -\> summarize loop:

1.  **Build a bundle** using
    [`build_nimble_bundle()`](https://arnabaich96.github.io/DPmixGPD/reference/build_nimble_bundle.md)
    (or causal builders if you are doing TE work).
2.  **Run MCMC** using
    [`run_mcmc_bundle_manual()`](https://arnabaich96.github.io/DPmixGPD/reference/run_mcmc_bundle_manual.md).
3.  **Inspect and summarize** using
    [`print()`](https://rdrr.io/r/base/print.html),
    [`summary()`](https://rdrr.io/r/base/summary.html),
    `if (interactive()) plot()`.
4.  **Predict** using
    [`predict()`](https://rdrr.io/r/stats/predict.html). For conditional
    (covariate) models,
    [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) is also
    available.

``` r

# Build
bundle <- build_nimble_bundle(
  y = rnorm(50),
  backend = "crp",
  kernel = "normal",
  GPD = FALSE,
  components = 5,
  mcmc = mcmc
)

# Run
fit <- load_or_fit("v04-backends-and-workflow-fit", run_mcmc_bundle_manual(bundle))

# Summarize
print(fit)
```

    MixGPD fit | backend: Chinese Restaurant Process | kernel: Normal Distribution | GPD tail: FALSE
    n = 50 | components = 5 | epsilon = 0.025
    MCMC: niter=4000, nburnin=1000, thin=5, nchains=2 
    Fit
    Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.

``` r

summary(fit)
```

    MixGPD summary | backend: Chinese Restaurant Process | kernel: Normal Distribution | GPD tail: FALSE | epsilon: 0.025
    n = 50 | components = 5
    Summary
    Initial components: 5 | Components after truncation: 1

    WAIC: 126.256
    lppd: -55.18 | pWAIC: 7.948

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
       <td style="text-align:center;"> 0.933 </td>
       <td style="text-align:center;"> 0.122 </td>
       <td style="text-align:center;"> 0.56 </td>
       <td style="text-align:center;"> 1 </td>
       <td style="text-align:center;"> 1 </td>
       <td style="text-align:center;"> 224.34 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> alpha </td>
       <td style="text-align:center;"> 0.337 </td>
       <td style="text-align:center;"> 0.349 </td>
       <td style="text-align:center;"> 0.006 </td>
       <td style="text-align:center;"> 0.233 </td>
       <td style="text-align:center;"> 1.291 </td>
       <td style="text-align:center;"> 801.989 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> mean[1] </td>
       <td style="text-align:center;"> 0.152 </td>
       <td style="text-align:center;"> 0.156 </td>
       <td style="text-align:center;"> -0.13 </td>
       <td style="text-align:center;"> 0.146 </td>
       <td style="text-align:center;"> 0.489 </td>
       <td style="text-align:center;"> 595.413 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> sd[1] </td>
       <td style="text-align:center;"> 1.71 </td>
       <td style="text-align:center;"> 0.577 </td>
       <td style="text-align:center;"> 0.989 </td>
       <td style="text-align:center;"> 1.582 </td>
       <td style="text-align:center;"> 3.286 </td>
       <td style="text-align:center;"> 376.985 </td>
      </tr>
    </tbody>
    </table>

``` r

if (interactive()) plot(fit)
```

## Kernel support quick check

Use
[`kernel_support_table()`](https://arnabaich96.github.io/DPmixGPD/reference/kernel_support_table.md)
and the kernel registry helpers to confirm what is available.

``` r

kernel_support_table()
```

                 kernel gpd covariates sb crp
    normal       normal   ✔          ✔  ✔   ✔
    lognormal lognormal   ✔          ✔  ✔   ✔
    invgauss   invgauss   ✔          ✔  ✔   ✔
    gamma         gamma   ✔          ✔  ✔   ✔
    laplace     laplace   ✔          ✔  ✔   ✔
    amoroso     amoroso   ✔          ✔  ✔   ✔
    cauchy       cauchy  ❌          ✔  ✔   ✔

## Where to go next

- **Available distributions**: see **v02** for the `d/p/q/r` functions
  and examples.
- **Basic build/compile/run**: see **v03**.
- **Unconditional / Conditional / Causal**: continue through v06+.
