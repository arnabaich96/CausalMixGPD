# Get epsilon value from object spec/meta or argument

Get epsilon value from object spec/meta or argument

## Usage

``` r
.get_epsilon(object, epsilon = NULL)
```

## Arguments

- object:

  A mixgpd_fit object.

- epsilon:

  Numeric; if provided, overrides object spec/meta.

## Details

Many downstream summaries truncate mixture components according to the
bundle or fit-level `epsilon` setting. This helper centralizes that
lookup so an explicit function argument overrides the stored fit
metadata, and the package fallback is used only when neither source is
present.
