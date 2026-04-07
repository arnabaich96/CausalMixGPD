# Get number of observations used in fitting

Get number of observations used in fitting

## Usage

``` r
.get_nobs(object)
```

## Arguments

- object:

  A mixgpd_fit.

## Value

Integer n.

## Details

The fitted object may carry the response in slightly different storage
slots depending on how it was built. This helper centralizes the lookup
and returns the effective training sample size used by summaries and
print methods.
