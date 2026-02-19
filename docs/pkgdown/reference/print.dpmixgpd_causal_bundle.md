# Print a causal bundle

User-facing print method for causal bundles produced by
[`build_causal_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_causal_bundle.md).

## Usage

``` r
# S3 method for class 'dpmixgpd_causal_bundle'
print(x, code = FALSE, max_code_lines = 200L, ...)
```

## Arguments

- x:

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
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal")
print(cb)
} # }
```
