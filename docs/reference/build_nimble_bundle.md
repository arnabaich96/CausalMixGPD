# Build a NIMBLE bundle

Creates a runnable "bundle" containing:

- compiled model `spec`

- `nimbleCode` model code

- `constants`, `data`, explicit `dimensions`

- initialization function `inits` (stored as a function)

- monitor specification

- MCMC settings list (stored but not used for code generation)

## Usage

``` r
build_nimble_bundle(
  y,
  X = NULL,
  backend = c("sb", "crp"),
  kernel,
  GPD = FALSE,
  components = NULL,
  J = NULL,
  param_specs = NULL,
  mcmc = list(niter = 2000, nburnin = 500, thin = 1, nchains = 1, seed = 1),
  epsilon = 0.025,
  alpha_random = TRUE
)
```

## Arguments

- y:

  Numeric outcome vector.

- X:

  Optional design matrix/data.frame (N x p) for conditional variants.

- backend:

  Character; `"sb"` (stick-breaking) or `"crp"` (Chinese Restaurant
  Process).

- kernel:

  Character kernel name (must exist in
  [`get_kernel_registry()`](https://example.com/DPmixGPD/reference/get_kernel_registry.md)).

- GPD:

  Logical; whether a GPD tail is requested.

- components:

  Deprecated alias for `J`. Only one of `J` or `components` should be
  supplied.

- J:

  Integer \>= 2. Single user-facing truncation parameter:

  - SB: number of mixture components used in stick-breaking truncation

  - CRP: maximum number of clusters represented in the finite NIMBLE
    model

- param_specs:

  Optional list with entries `bulk` and `tail` to override defaults.

- mcmc:

  Named list of MCMC settings (niter, nburnin, thin, nchains, seed).
  Stored in bundle.

- epsilon:

  Numeric in \[0,1). For downstream summaries/plots/prediction we keep
  the smaller k defined by either (i) cumulative mass \>= 1 - epsilon
  or (ii) per-component weights \>= epsilon, then renormalize.

- alpha_random:

  Logical; whether concentration `alpha` is stochastic.

## Value

A named list (bundle) of class `"dpmixgpd_bundle"`.

## Details

This function intentionally stops at the "pre-run" stage
(spec/code/constants/data/dimensions/inits/monitors). Use
[`run_mcmc_bundle_manual()`](https://example.com/DPmixGPD/reference/run_mcmc_bundle_manual.md)
to execute MCMC with the stored settings.

## Examples

``` r
y <- abs(rnorm(60)) + 0.1
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = FALSE,
  components = 4,
  mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
)
bundle
#> DPmixGPD bundle
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel    Normal Distribution
#>  Components                      4
#>           N                     60
#>           X                     NO
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#>   contains  : code, constants, data, dimensions, inits, monitors
```
