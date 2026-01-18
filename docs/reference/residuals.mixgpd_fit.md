# Residuals for a MixGPD fit

Returns residuals aligned with the training data. For `type = "raw"`,
this uses fitted means. For `type = "pit"`, this returns approximate PIT
values via the predictive survival function.

## Usage

``` r
# S3 method for class 'mixgpd_fit'
residuals(object, type = c("raw", "pit"), ...)
```

## Arguments

- object:

  A fitted object of class `"mixgpd_fit"`.

- type:

  Residual type: `"raw"` or `"pit"`.

- ...:

  Unused.

## Value

Numeric vector of residuals with length equal to the training sample
size.
