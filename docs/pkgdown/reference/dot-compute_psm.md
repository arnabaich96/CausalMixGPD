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

## Details

If \\z_i^{(s)}\\ denotes the cluster label of observation \\i\\ at draw
\\s\\, then this helper computes \$\$\mathrm{PSM}\_{ij} \approx
\frac{1}{S} \sum\_{s=1}^S I(z_i^{(s)} = z_j^{(s)}).\$\$ The resulting
matrix is the basic posterior co-clustering summary used by the Dahl
representative partition and several cluster diagnostics.
