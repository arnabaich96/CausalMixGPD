# Wrap scalar RNG to handle n \> 1.

Wrap scalar RNG to handle n \> 1.

## Usage

``` r
.wrap_scalar_r(fun)
```

## Details

Random-generation helpers in the package are scalar-at-a-time. This
wrapper promotes them to the standard `n` interface by repeating the
scalar generator and returning either a numeric vector or a matrix of
generated values, depending on the length of one draw.
