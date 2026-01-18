# Residuals for a MixGPD fit

Returns residuals aligned with the training data. For `type = "raw"`,
this uses fitted means. For `type = "pit"`, this returns approximate PIT
values via the predictive survival function.

## Usage

``` r
# S3 method for class 'mixgpd_fit'
residuals(
  object,
  type = c("raw", "pit"),
  fitted_type = c("mean", "median"),
  ...
)
```

## Arguments

- object:

  A fitted object of class `"mixgpd_fit"`.

- type:

  Residual type: `"raw"` or `"pit"`.

- fitted_type:

  For `type = "raw"`, use fitted means or medians.

- ...:

  Unused.

## Value

Numeric vector of residuals with length equal to the training sample
size.
