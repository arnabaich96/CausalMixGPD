# Validate a fitted object

Validate a fitted object

## Usage

``` r
.validate_fit(object)
```

## Arguments

- object:

  A fitted object.

## Value

Invisibly TRUE, otherwise errors.

## Details

This is a lightweight structural check used by multiple internal
helpers. It verifies that the object inherits from `mixgpd_fit` and that
posterior draws are available in one of the expected storage locations
before later code tries to summarize or predict from the fit.
