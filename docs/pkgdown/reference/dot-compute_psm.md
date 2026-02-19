# Compute posterior similarity matrix

Computes the posterior similarity matrix (PSM) from a matrix of cluster
assignments. `PSM[i,j]` = probability that observations i and j are in
the same cluster.

## Usage

``` r
.compute_psm(z_matrix)
```

## Arguments

- z_matrix:

  Integer matrix (iterations x N) of cluster assignments.

## Value

Symmetric N x N matrix of co-clustering probabilities.
