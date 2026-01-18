# Safely coerce MCMC samples to a numeric matrix

Safely coerce MCMC samples to a numeric matrix

## Usage

``` r
.extract_draws(
  object,
  pars = NULL,
  chains = c("stack", "first"),
  epsilon = NULL
)
```

## Arguments

- object:

  A mixgpd_fit.

- pars:

  Optional character vector of parameter names to keep (exact match).

## Value

Numeric matrix of draws (iter x parameters).
