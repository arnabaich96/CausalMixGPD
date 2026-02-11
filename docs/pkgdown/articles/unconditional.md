# DPmixGPD: Unconditional workflow (bulk + optional GPD tail)

This vignette shows the end-to-end workflow for an unconditional model:
build a bundle, run posterior sampling, and compute distributional
predictions. The mathematical specification of the DPM bulk model and
the spliced GPD tail is defined once in **“Model specification and
posterior computation”** (the `basic` vignette). Here we focus on usage.

DPmixGPD ships small example datasets; we use `nc_pos_tail200_k4`, which
is a positive-support dataset with an injected upper tail (so `GPD=TRUE`
is meaningful).

Model fit (code shown for reproducibility, but not executed during CRAN
checks)

``` r

bundle <- build_nimble_bundle(
  y = y,
  X = NULL,
  backend = "sb",         # "sb" or "crp"
  kernel = "lognormal",  # choose a positive-support kernel
  GPD = TRUE,
  components = 4,
  mcmc = list(niter = 2000, nburnin = 500, thin = 5, nchains = 1, seed = 101)
)

fit <- run_mcmc_bundle_manual(bundle, show_progress = TRUE)
```

Predictions

``` r

grid <- seq(min(y), max(y) * 1.5, length.out = 200)

dhat <- predict(fit, y = grid, type = "density")
shat <- predict(fit, y = grid, type = "survival")
qs   <- predict(fit, type = "quantile", p = c(0.5, 0.9, 0.95, 0.99), cred.level = 0.90)
```

Results (precomputed)

The table and figure below are precomputed from the shipped dataset to
keep CRAN checks fast and deterministic. To reproduce them locally, run
`tools/make_vignette_artifacts.R` and rebuild the vignettes.

| estimate | index | lower | upper |
|---------:|------:|------:|------:|
|     1.45 |  0.50 |  1.20 |  1.66 |
|     4.63 |  0.90 |  3.56 |  5.56 |
|     6.34 |  0.95 |  5.04 |  7.64 |
|    10.99 |  0.99 |  8.71 | 13.98 |

![Posterior predictive density for the unconditional fit
(precomputed).](../inst/extdata/unconditional_density.png)

Posterior predictive density for the unconditional fit (precomputed).

Notes on customization

1.  Kernel choice: use
    [`kernel_support_table()`](https://arnabaich96.github.io/DPmixGPD/pkgdown/reference/kernel_support_table.md)
    to see which kernels match your support (real line vs positive
    support) and which parameters are available.

2.  Tail control: setting `GPD=TRUE` activates the spliced tail module
    (see the `basic` vignette for the exact spliced density). Tail
    priors and threshold control are provided via `param_specs`.

3.  Backend choice: stick-breaking (`backend="sb"`) is often the most
    stable default; the CRP backend is available when you prefer
    partition-based updates.
