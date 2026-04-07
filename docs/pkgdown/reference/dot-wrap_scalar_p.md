# Wrap scalar CDF to handle q/x naming and vector inputs.

Wrap scalar CDF to handle q/x naming and vector inputs.

## Usage

``` r
.wrap_scalar_p(fun)
```

## Details

Different scalar CDF helpers use either `q` or `x` for their evaluation
argument. This wrapper normalizes those naming differences and then
applies the same vector-lifting strategy used elsewhere so prediction
code can call the resulting function consistently.
