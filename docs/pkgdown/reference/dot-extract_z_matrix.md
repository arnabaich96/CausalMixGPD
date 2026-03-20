# Extract cluster assignment matrix from MCMC samples

Extracts the posterior draws of cluster assignments `z[1:N]` from a
fitted mixgpd_fit object and returns them as an integer matrix
(iterations x N).

## Usage

``` r
.extract_z_matrix(object)
```

## Arguments

- object:

  A `mixgpd_fit` object.

## Value

Integer matrix with rows = posterior draws, cols = observations.
