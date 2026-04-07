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

## Details

The representative labels define a reference partition with clusters
\\C_1, \dots, C_K\\. For each observation \\i\\, this helper averages
the posterior similarity scores \\\mathrm{PSM}\_{ij}\\ over members \\j
\in C_k\\ to obtain a cluster-membership score for cluster \\k\\, and
then normalizes those scores to sum to one across clusters.
