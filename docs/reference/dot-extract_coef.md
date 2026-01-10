# Extract coefficient-like parameters from posterior draws

This is pattern-based and works immediately with common naming
conventions: - bulk: beta_mu, beta, mu regression blocks, etc. - tail:
beta_u, beta_sigma, beta_xi, threshold regression blocks, etc.

## Usage

``` r
.extract_coef(
  object,
  component = c("bulk", "tail", "both"),
  format = c("vector", "list", "tidy"),
  probs = c(0.025, 0.5, 0.975)
)
```

## Arguments

- object:

  mixgpd_fit.

- component:

  bulk/tail/both.

- format:

  vector/list/tidy.

- probs:

  intervals for tidy format.

## Value

coefficients.

## Details

For a perfect experience, keep your engine naming consistent; this will
then be stable.
