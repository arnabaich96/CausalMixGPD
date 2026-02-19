# Compute cluster membership probabilities from PSM

For each observation, computes the probability of membership in each
cluster defined by the representative clustering, derived from the
posterior similarity matrix.

## Usage

``` r
.compute_cluster_probs(z_matrix, labels_representative, PSM)
```

## Arguments

- z_matrix:

  Integer matrix (iterations x N) of cluster assignments.

- labels_representative:

  Integer vector of representative cluster labels.

- PSM:

  Posterior similarity matrix (N x N).

## Value

N x K matrix of cluster membership probabilities.
