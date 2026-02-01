# Print a dpmixgpd bundle

User-facing print method for pre-run bundles produced by
[`build_nimble_bundle()`](https://arnabaich96.github.io/DPmixGPD/pkgdown/reference/build_nimble_bundle.md).
This prints a compact description of the model structure
(backend/kernel/components), whether covariates are used, and whether a
GPD tail is enabled.

## Usage

``` r
# S3 method for class 'dpmixgpd_bundle'
print(x, code = FALSE, max_code_lines = 200L, ...)
```

## Arguments

- x:

  A `"dpmixgpd_bundle"` object.

- code:

  Logical; if TRUE, print the generated NIMBLE model code.

- max_code_lines:

  Integer; maximum number of code lines to print when `code=TRUE`.

- ...:

  Unused.

## Value

The object `x`, invisibly.

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
