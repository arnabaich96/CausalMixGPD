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

## Details

Cluster samplers store latent labels as separate monitored nodes `z[i]`.
This helper locates those nodes in every retained chain, orders them by
observation index, stacks the chains, and returns the result as one
integer matrix that is ready for PSM and representative-partition
calculations.
