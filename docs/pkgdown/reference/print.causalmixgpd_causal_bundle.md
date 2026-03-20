# Print a causal workflow bundle

`print.causalmixgpd_causal_bundle()` gives a compact structural summary
of the pre-run causal bundle created by
[`build_causal_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_causal_bundle.md).

## Usage

``` r
# S3 method for class 'causalmixgpd_causal_bundle'
print(x, code = FALSE, max_code_lines = 200L, ...)
```

## Arguments

- x:

  A `"causalmixgpd_causal_bundle"` object.

- code:

  Logical; if TRUE, print generated NIMBLE code for each block.

- max_code_lines:

  Integer; maximum number of code lines to print when `code=TRUE`.

- ...:

  Unused.

## Value

The input object (invisibly).

## See also

[`summary.causalmixgpd_causal_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.causalmixgpd_causal_bundle.md),
[`run_mcmc_causal`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_causal.md),
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal")
print(cb)
} # }
```
