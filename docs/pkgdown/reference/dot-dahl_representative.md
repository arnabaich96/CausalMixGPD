# Find Dahl representative clustering

Identifies the posterior draw that minimizes squared distance to the
posterior similarity matrix, following Dahl (2006). Returns relabeled
cluster assignments as consecutive integers 1, 2, ..., K.

## Usage

``` r
.dahl_representative(z_matrix, PSM)
```

## Arguments

- z_matrix:

  Integer matrix (iterations x N) of cluster assignments.

- PSM:

  Posterior similarity matrix (N x N).

## Value

List with components: draw_index (integer), labels (integer vector), K
(number of clusters).

## References

Dahl, D. B. (2006). Model-based clustering for expression data via a
Dirichlet process mixture model. In M. Vannucci, et al. (Eds.), Bayesian
Inference for Gene Expression and Proteomics (pp. 201-218). Cambridge
University Press.
