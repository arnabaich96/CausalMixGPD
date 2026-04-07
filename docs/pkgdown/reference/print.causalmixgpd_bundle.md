# Print a one-arm workflow bundle

`print.causalmixgpd_bundle()` gives a compact structural summary of the
pre-run bundle created by
[`build_nimble_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md).

## Usage

``` r
# S3 method for class 'causalmixgpd_bundle'
print(x, code = FALSE, max_code_lines = 200L, ...)
```

## Arguments

- x:

  A `"causalmixgpd_bundle"` object.

- code:

  Logical; if TRUE, print the generated NIMBLE model code.

- max_code_lines:

  Integer; maximum number of code lines to print when `code=TRUE`.

- ...:

  Unused.

## Value

The object `x`, invisibly.

## Details

The bundle is the compiled representation of the predictive model before
MCMC. For a bulk-only fit, the underlying target law is \$\$f(y \mid x)
= \sum\_{k=1}^{K} w_k(x) f_k(y \mid x, \theta_k).\$\$ When a GPD tail is
enabled, the same bulk mixture is spliced to a generalized Pareto tail
above the threshold recorded in the bundle specification.

[`print()`](https://rdrr.io/r/base/print.html) is intentionally brief.
It is meant to confirm that the stored backend, kernel, truncation size,
covariate structure, and code-generation artifacts match the intended
model before you compile and sample with
[`run_mcmc_bundle_manual`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_bundle_manual.md).

## See also

[`summary.causalmixgpd_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.causalmixgpd_bundle.md),
[`mcmc`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/mcmc.md),
[`run_mcmc_bundle_manual`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_bundle_manual.md).

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = FALSE, components = 6)
print(bundle)
print(bundle, code = TRUE, max_code_lines = 30)
} # }
```
