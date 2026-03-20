# Build the explicit one-arm NIMBLE bundle

`build_nimble_bundle()` is the detailed constructor behind
[`bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md)
for one-arm models. It compiles the modeling plan into a self-contained
object holding code-generation inputs, initialization rules, monitor
policy, and stored MCMC defaults.

## Usage

``` r
build_nimble_bundle(
  y,
  X = NULL,
  ps = NULL,
  backend = c("sb", "crp", "spliced"),
  kernel,
  GPD = FALSE,
  components = NULL,
  param_specs = NULL,
  mcmc = list(niter = 2000, nburnin = 500, thin = 1, nchains = 1, seed = 1),
  epsilon = 0.025,
  alpha_random = TRUE,
  monitor = c("core", "full"),
  monitor_latent = FALSE,
  monitor_v = FALSE
)
```

## Arguments

- y:

  Numeric outcome vector.

- X:

  Optional design matrix/data.frame (N x p) for conditional variants.

- ps:

  Optional numeric vector (length N) of propensity scores. When
  provided, augments the design matrix for PS-adjusted outcome modeling.

- backend:

  Character; `"sb"` (stick-breaking) or `"crp"` (Chinese Restaurant
  Process).

- kernel:

  Character kernel name (must exist in
  [`get_kernel_registry()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/get_kernel_registry.md)).

- GPD:

  Logical; whether a GPD tail is requested.

- components:

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

  Logical; whether the DP concentration parameter \\\kappa\\ is
  stochastic.

- monitor:

  Character monitor profile: `"core"` (default) or `"full"`.

- monitor_latent:

  Logical; if TRUE, include latent cluster labels (`z`) in monitors.

- monitor_v:

  Logical; if TRUE and backend is SB, include stick breaks (`v`) in
  monitors.

## Value

A named list of class `"causalmixgpd_bundle"`. Its primary components
are `spec`, `code`, `constants`, `dimensions`, `data`, `inits`,
`monitors`, and stored `mcmc` settings.

## Details

The returned bundle encodes a finite approximation to a Dirichlet
process mixture using either a stick-breaking (`"sb"`) or Chinese
restaurant process / spliced (`"crp"` / `"spliced"`) representation.

For the bulk-only model, the target likelihood is the DPM predictive law
\$\$f(y \mid x) = \sum\_{k=1}^{K} w_k(x) f_k(y \mid x, \theta_k).\$\$
When `GPD = TRUE`, the bundle augments the bulk model with a threshold
\\u(x)\\ and generalized Pareto tail above that threshold, producing the
spliced predictive distribution described in the manuscript vignette.

This function intentionally stops before model compilation and sampling.
Use
[`run_mcmc_bundle_manual`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_bundle_manual.md)
or
[`mcmc`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/mcmc.md)
to execute the stored model definition.

The object contains:

- compiled model `spec`

- `nimbleCode` model code

- `constants`, `data`, explicit `dimensions`

- initialization function `inits` (stored as a function)

- monitor specification

- MCMC settings list (stored but not used for code generation)

## See also

[`bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md),
[`run_mcmc_bundle_manual`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_bundle_manual.md),
[`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md),
[`kernel_support_table`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/kernel_support_table.md),
[`get_kernel_registry`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/get_kernel_registry.md).

## Examples

``` r
if (FALSE) { # \dontrun{
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
} # }
```
