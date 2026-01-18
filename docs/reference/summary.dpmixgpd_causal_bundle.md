# Summarize a causal bundle

User-facing summary for causal bundles produced by
[`build_causal_bundle()`](https://arnabaich96.github.io/DPmixGPD/reference/build_causal_bundle.md).

## Usage

``` r
# S3 method for class 'dpmixgpd_causal_bundle'
summary(object, code = FALSE, max_code_lines = 200L, ...)
```

## Arguments

- object:

  A `"dpmixgpd_causal_bundle"` object.

- code:

  Logical; if TRUE, print generated NIMBLE code for each block.

- max_code_lines:

  Integer; maximum number of code lines to print when `code=TRUE`.

- ...:

  Unused.

## Value

The input object (invisibly).

## Examples

``` r
# \dontrun{
cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal")
#> Error: object 'y' not found
summary(cb)
#> Error: object 'cb' not found
# }
```
