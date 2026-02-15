# Plot a causal fit

Plot a causal fit

## Usage

``` r
# S3 method for class 'causalmixgpd_causal_fit'
plot(x, arm = "both", ...)
```

## Arguments

- x:

  A `"causalmixgpd_causal_fit"` object.

- arm:

  Integer or character; `1` or `"treated"` for treatment, `0` or
  `"control"` for control.

- ...:

  Additional arguments forwarded to the underlying outcome plot method.

## Value

The result of the underlying plot call (invisibly).
