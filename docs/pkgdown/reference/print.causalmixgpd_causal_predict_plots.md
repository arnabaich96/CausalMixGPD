# Print method for causal prediction plots

Print method for causal prediction plots

## Usage

``` r
# S3 method for class 'causalmixgpd_causal_predict_plots'
print(x, ...)
```

## Arguments

- x:

  Object of class `causalmixgpd_causal_predict_plots`.

- ...:

  Additional arguments (ignored).

## Value

Invisibly returns the input object.

## Details

The causal prediction plotting methods can return either a single plot
or a named list of plots. This print method renders those stored plot
objects in sequence so both arm-level and contrast-level graphics appear
in console or notebook workflows without manual extraction.

It is a display helper only and does not modify the underlying
prediction summaries.
