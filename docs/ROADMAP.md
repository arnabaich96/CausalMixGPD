# DPmixGPD Package Roadmap

This document describes the levels of a complete analysis process in
DPmixGPD and what each key function does. It is a high-level guide only;
for step-by-step tutorials and full API details, use the vignettes and
the function reference linked at the end.

------------------------------------------------------------------------

## Main analysis flow

The standard workflow moves from setup through model building, MCMC,
diagnostics, and extraction. The diagram below shows the five levels in
order.

        ┌─────────────────┐      ┌──────────────────┐      ┌────────────┐      ┌─────────────────────┐      ┌─────────────────────┐
        │ Setup and data  │ ───► │ Specify and build │ ───► │ Run MCMC   │ ───► │ Inspect and diagnose│ ───► │ Extract and predict │
        └─────────────────┘      └──────────────────┘      └────────────┘      └─────────────────────┘      └─────────────────────┘

------------------------------------------------------------------------

## Causal workflow

For treatment-effect analyses, you use a causal bundle and causal MCMC
instead of a single bundle and standard MCMC. Setup is the same; then
the flow branches as below.

        ┌─────────────────┐      ┌─────────────────┐      ┌──────────────────┐      ┌─────────────────────┐      ┌──────────────────┐
        │ Setup and data  │ ───► │ Causal bundle   │ ───► │ Run causal MCMC │ ───► │ Inspect and diagnose│ ───► │ QTE ATE predict   │
        └─────────────────┘      └─────────────────┘      └──────────────────┘      └─────────────────────┘      └──────────────────┘

------------------------------------------------------------------------

## Level overview

Each level has a clear purpose. The pipeline view below summarizes all
levels in one place.

        Level 0  ┌─────────────────┐
                 │ Setup and data   │
                 └────────┬─────────┘
                          │
        Level 1           ▼
                 ┌──────────────────┐
                 │ Specify and build│
                 └────────┬──────────┘
                          │
        Level 2           ▼
                 ┌────────────┐         ┌──────────────────────────────┐
                 │ Run MCMC   │ - - - -►│ Level 5  Causal               │
                 └────────┬────┘         │ (QTE, ATE, causal predict)   │
                          │             └──────────────────────────────┘
                          ▼
        Level 3  ┌─────────────────────┐
                 │ Inspect and diagnose│
                 └────────┬────────────┘
                          │
        Level 4           ▼
                 ┌─────────────────────┐
                 │ Extract and predict │
                 └─────────────────────┘

------------------------------------------------------------------------

## Function-by-level map

The following diagram groups key functions by the level where they are
used. Build and run functions produce objects that inspection and
extraction functions consume.

        Level 0  ┌──────────────────────────────────────────────────────────────────┐
                 │ init_kernel_registry   get_kernel_registry   get_tail_registry    │
                 │ kernel_support_table   sim_bulk_tail   sim_causal_qte   sim_survival_tail │
                 └──────────────────────────────────────────────┬───────────────────┘
                                                                │
        Level 1                                                 ▼
                 ┌──────────────────────────────────────────────────────────────────┐
                 │ build_nimble_bundle              build_causal_bundle             │
                 └──────────────────────────────────────────────┬───────────────────┘
                                                                │
        Level 2                                                 ▼
                 ┌──────────────────────────────────┐    ┌─────────────────────────────┐
                 │ run_mcmc_bundle_manual            │ - -►│ Level 5  qte  ate  ate_rmean │
                 │ run_mcmc_causal                   │    │ (causal effects)            │
                 └──────────────────────┬─────────────┘    └─────────────────────────────┘
                                        │
        Level 3                         ▼
                 ┌──────────────────────────────────────────────────────────────────┐
                 │ summary   print   plot   params   check_glue_validity             │
                 └──────────────────────────────────────────────┬───────────────────┘
                                                                │
        Level 4                                                 ▼
                 ┌──────────────────────────────────────────────────────────────────┐
                 │ predict   fitted   residuals                                     │
                 └──────────────────────────────────────────────────────────────────┘

------------------------------------------------------------------------

## Levels of the analysis process

### Level 0 — Setup and data

**Goal:** Prepare the environment and data before building a model. The
kernel registry must be initialized so the package knows which mixture
kernels and tail options are available. You can use built-in simulated
data generators for examples or tests.

**Key functions:**

- **init_kernel_registry** — Creates or refreshes the internal registry
  of mixture kernels (normal, gamma, lognormal, etc.) and their GPD-tail
  support. Call this if you need to ensure the registry is loaded.
- **get_kernel_registry** — Returns the current kernel registry so you
  can inspect available kernels and their parameters.
- **get_tail_registry** — Returns the tail (GPD) registry used for tail
  parameterization.
- **kernel_support_table** — Produces a summary table of which kernels
  support which features (e.g. GPD tail, regression).
- **sim_bulk_tail** — Generates simulated outcomes from a bulk-plus-tail
  setup for demonstration or testing.
- **sim_causal_qte** — Generates simulated outcome, treatment, and
  covariate data for causal QTE examples.
- **sim_survival_tail** — Generates simulated survival-style data (time,
  status, covariates) for tail-focused examples.

------------------------------------------------------------------------

### Level 1 — Specify and build

**Goal:** Choose the model (kernel, GPD on/off, backend, number of
components) and build a bundle that contains the NIMBLE model
specification, data, inits, monitors, and MCMC settings. For causal
analyses, build a causal bundle that includes outcome bundles per
treatment arm and an optional propensity-score design.

**Key functions:**

- **build_nimble_bundle** — One-stop builder for standard (unconditional
  or conditional) models. It compiles the model spec, generates NIMBLE
  code, and assembles constants, dimensions, data, inits, monitors, and
  MCMC options into a single bundle object. You pass outcome vector,
  optional covariate matrix, optional propensity scores, backend
  (stick-breaking or CRP), kernel name, GPD flag, and number of
  components.
- **build_causal_bundle** — Builds the structure for causal inference:
  separate outcome bundles for treated and control arms, optional
  propensity-score model bundle, and metadata. You supply outcome,
  covariates, treatment indicator, and per-arm or shared options for
  backend, kernel, GPD, and components.

------------------------------------------------------------------------

### Level 2 — Run MCMC

**Goal:** Execute posterior sampling. For a standard bundle, run MCMC
once; for a causal bundle, run outcome MCMC for each arm and optionally
run propensity-score MCMC.

**Key functions:**

- **run_mcmc_bundle_manual** — Takes a bundle produced by
  build_nimble_bundle, builds and compiles the NIMBLE model and MCMC,
  and runs the sampler. Returns a fit object that can be summarized,
  plotted, and used for prediction.
- **run_mcmc_causal** — Takes a causal bundle, runs MCMC for the
  propensity model (if present) and for each outcome arm, and returns a
  causal fit object. That object is then used for QTE, ATE, and causal
  prediction.

------------------------------------------------------------------------

### Level 3 — Inspect and diagnose

**Goal:** Check that the run succeeded and that the posterior is
sensible. Summarize parameters, view traceplots and densities, extract
parameter draws, and optionally verify threshold/continuity (glue) at
the bulk–tail boundary.

**Key functions:**

- **summary** — For a fit object, returns posterior summaries
  (e.g. mean, median, credible intervals) for selected parameters. For
  bundles, returns a structural summary of the model and MCMC setup.
- **print** — Displays a concise view of the object (fit or bundle),
  including key dimensions and options.
- **plot** — For a fit, produces diagnostic plots (e.g. traceplots,
  density plots) to assess mixing and convergence. Optional arguments
  control which plot families are shown.
- **params** — Extracts posterior parameter draws (weights and kernel
  parameters) from a fit in a structured form for further use.
- **check_glue_validity** — Checks that the fitted model satisfies
  continuity and threshold constraints at the bulk–GPD boundary. Useful
  for diagnosing implausible tail fits.

------------------------------------------------------------------------

### Level 4 — Extract and predict

**Goal:** Use the fitted model to obtain predictions (mean, quantiles,
density, survival, location) and, for conditional models, fitted values
and residuals. Predictions can include credible or HPD intervals.

**Key functions:**

- **predict** — Produces posterior predictions from a fit. You choose
  type (e.g. mean, quantile, density, survival, location) and optionally
  request credible or HPD intervals. For conditional models, pass
  covariate (and optionally propensity) values; for unconditional
  models, predictions are over the outcome distribution.
- **fitted** — For conditional (covariate-dependent) fits only, returns
  fitted location, mean, median, or quantile at the observed covariate
  values. Useful for in-sample comparison.
- **residuals** — For conditional fits only, returns residuals based on
  the chosen fitted type (e.g. observed minus fitted location or
  quantile).

------------------------------------------------------------------------

### Level 5 — Causal (optional)

**Goal:** Estimate treatment effects and make arm-specific predictions
when you have a causal fit. Quantile treatment effects (QTE), average
treatment effect (ATE), and restricted-mean ATE are computed from the
treated and control outcome fits; prediction can be done per arm with
optional propensity adjustment.

**Key functions:**

- **qte** — Computes quantile treatment effects at specified quantiles,
  optionally at new covariate values. Uses the treated and control
  outcome fits and, when propensity scores are in use, propensity values
  at the prediction points. Can return point estimates and credible or
  HPD intervals.
- **ate** — Computes the average treatment effect (difference in means
  between treated and control) from the causal fit, with optional
  intervals.
- **ate_rmean** — Computes the restricted mean treatment effect up to a
  finite cutoff, i.e. the difference in restricted means between arms.
- **predict** (on causal fit) — When applied to a causal fit object,
  produces arm-specific predictions (e.g. mean or quantile for treated
  and control), with optional propensity input where the design includes
  propensity scores.

------------------------------------------------------------------------

## Quick reference table

| Level | Function               | Role                                         |
|-------|------------------------|----------------------------------------------|
| 0     | init_kernel_registry   | Initialize kernel and tail registries        |
| 0     | get_kernel_registry    | Return kernel registry                       |
| 0     | get_tail_registry      | Return tail registry                         |
| 0     | kernel_support_table   | Summary table of kernel support              |
| 0     | sim_bulk_tail          | Simulate bulk-plus-tail data                 |
| 0     | sim_causal_qte         | Simulate causal QTE data                     |
| 0     | sim_survival_tail      | Simulate survival-style data                 |
| 1     | build_nimble_bundle    | Build standard model bundle                  |
| 1     | build_causal_bundle    | Build causal bundle (arms and optional PS)   |
| 2     | run_mcmc_bundle_manual | Run MCMC on a bundle                         |
| 2     | run_mcmc_causal        | Run causal MCMC (outcome and optional PS)    |
| 3     | summary                | Posterior or structural summary              |
| 3     | print                  | Concise print of fit or bundle               |
| 3     | plot                   | Diagnostic plots for fit                     |
| 3     | params                 | Extract parameter draws from fit             |
| 3     | check_glue_validity    | Check bulk–tail continuity                   |
| 4     | predict                | Posterior predictions (mean, quantile, etc.) |
| 4     | fitted                 | Fitted values (conditional fits)             |
| 4     | residuals              | Residuals (conditional fits)                 |
| 5     | qte                    | Quantile treatment effect                    |
| 5     | ate                    | Average treatment effect                     |
| 5     | ate_rmean              | Restricted-mean treatment effect             |

The package also exports distribution functions (density, CDF, quantile,
random number generation) for each mixture kernel and GPD tail, and
coverage utilities for development; see the function reference for the
full list.

------------------------------------------------------------------------

## Where to go next

- [**Introduction**](https://arnabaich96.github.io/DPmixGPD/articles/introduction.html)
  — Quick start and minimal example
- [**Model
  spec**](https://arnabaich96.github.io/DPmixGPD/articles/model-spec.html)
  — Specifying models and priors
- [**MCMC
  workflow**](https://arnabaich96.github.io/DPmixGPD/articles/mcmc-workflow.html)
  — Running and diagnosing MCMC
- [**Unconditional /
  Conditional**](https://arnabaich96.github.io/DPmixGPD/articles/unconditional.html)
  — Density estimation with or without covariates
- [**Causal**](https://arnabaich96.github.io/DPmixGPD/articles/causal.html)
  — Treatment effects with propensity scores
- [**Backends**](https://arnabaich96.github.io/DPmixGPD/articles/backends.html)
  — CRP vs stick-breaking
- [**S3
  reference**](https://arnabaich96.github.io/DPmixGPD/articles/reference-s3.html)
  — print, summary, plot, predict, fitted, residuals
- [**Function
  reference**](https://arnabaich96.github.io/DPmixGPD/reference/index.html)
  — Full API documentation
