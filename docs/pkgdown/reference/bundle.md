# Build the workflow bundle used by the package fitters

`bundle()` is the main workflow constructor. It converts raw inputs, a
formula/data pair, or an already prepared bundle into the canonical
object consumed by
[`mcmc`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/mcmc.md),
[`dpmix`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.md),
[`dpmgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md),
[`dpmix.causal`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.causal.md),
and
[`dpmgpd.causal`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.causal.md).

## Usage

``` r
bundle(
  x = NULL,
  data = NULL,
  X = NULL,
  treat = NULL,
  formula = NULL,
  ...,
  GPD = FALSE
)
```

## Arguments

- x:

  Either a response vector or an existing bundle.

- data:

  Optional data.frame used with `formula`.

- X:

  Optional design matrix/data.frame.

- treat:

  Optional binary treatment indicator.

- formula:

  Optional formula.

- ...:

  Additional arguments passed to
  [`build_nimble_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md)
  or
  [`build_causal_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_causal_bundle.md).

- GPD:

  Logical; include GPD tail in build mode.

## Value

A `"causalmixgpd_bundle"` for one-arm models or a
`"causalmixgpd_causal_bundle"` for causal models. The bundle stores
code-generation inputs, monitor policy, and default MCMC settings, but
it does not run MCMC.

## Details

For one-arm models the returned object represents a bulk Dirichlet
process mixture, optionally augmented with a spliced generalized Pareto
tail. For causal models the returned object contains two arm-specific
outcome bundles plus an optional propensity score block.

The workflow is:

1.  prepare a bundle with `bundle()`,

2.  run posterior sampling with
    [`mcmc`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/mcmc.md)
    or one of the `dpmix*`/`dpmgpd*` wrappers,

3.  inspect the fitted object with
    [`summary.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.mixgpd_fit.md),
    [`params`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/params.md),
    [`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md),
    or the causal estimand helpers.

Setting `GPD = TRUE` requests the spliced bulk-tail model with
conditional distribution \$\$F(y \mid x) = F\_{\mathrm{bulk}}(y \mid
x)\mathbf{1}\\y \le u(x)\\ + \left\[p_u(x) + \\1 -
p_u(x)\\F\_{\mathrm{GPD}}(y \mid x)\right\]\mathbf{1}\\y \> u(x)\\,\$\$
where \\p_u(x)\\ is the bulk probability below the threshold \\u(x)\\.

See the manuscript vignette for the DPM hierarchy, SB/CRP
representations, and the spliced bulk-tail construction used throughout
the package.

## See also

[`build_nimble_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md),
[`build_causal_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_causal_bundle.md),
[`mcmc`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/mcmc.md),
[`dpmix`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.md),
[`dpmgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md).
