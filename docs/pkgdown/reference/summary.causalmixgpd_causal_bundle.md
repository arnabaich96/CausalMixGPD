# Summarize a causal workflow bundle

`summary.causalmixgpd_causal_bundle()` is the bundle-level validation
checkpoint for the causal workflow.

## Usage

``` r
# S3 method for class 'causalmixgpd_causal_bundle'
summary(object, code = FALSE, max_code_lines = 200L, ...)
```

## Arguments

- object:

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

[`print.causalmixgpd_causal_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.causalmixgpd_causal_bundle.md),
[`run_mcmc_causal`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_causal.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal")
summary(cb)
} # }
```
