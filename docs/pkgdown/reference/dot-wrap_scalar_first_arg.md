# Wrap scalar first-argument functions to handle vector inputs.

Wrap scalar first-argument functions to handle vector inputs.

## Usage

``` r
.wrap_scalar_first_arg(fun, first_arg_name)
```

## Details

Many low-level distribution helpers are scalar in their first argument.
This wrapper lifts such functions to vector inputs by evaluating the
scalar function repeatedly and combining the results into either a
numeric vector or a matrix, depending on the length of the original
return value.
