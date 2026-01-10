# Summarize a dpmixgpd bundle

User-facing summary for pre-run bundles produced by
[`build_nimble_bundle()`](https://example.com/DPmixGPD/reference/build_nimble_bundle.md).
This prints:

- meta information (backend, kernel, components, N, covariates, GPD
  flag)

- a readable prior/parameter table derived from `spec$plan`

- monitor set overview

## Usage

``` r
# S3 method for class 'dpmixgpd_bundle'
summary(object, ...)
```

## Arguments

- object:

  A `"dpmixgpd_bundle"` object.

- ...:

  Unused.

## Value

An invisible list with elements `meta`, `priors`, `monitors`.

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = FALSE, components = 6)
summary(bundle)
} # }
```
