# Extract posterior cluster allocation from mixgpd_fit

Extract posterior cluster allocation from mixgpd_fit

## Usage

``` r
# S3 method for class 'mixgpd_fit'
allocation(object, method = c("dahl"), ...)
```

## Arguments

- object:

  A `mixgpd_fit` object from
  [`dpmgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md)
  or related functions.

- method:

  Character; clustering summarization method. Currently only `"dahl"` is
  supported (Dahl's method for finding representative partition).

- ...:

  Additional arguments. If `newdata` is provided, cluster allocations
  will be predicted for new observations (requires `y` column in
  `newdata`).

## Value

An object of class `"mixgpd_allocation"` containing:

- labels_train:

  Integer vector of cluster labels for training data (1, 2, ..., K)

- probs_train:

  N x K matrix of cluster membership probabilities for training data

- PSM:

  N x N posterior similarity matrix

- K:

  Number of clusters in representative partition

- method:

  Clustering method used

- backend:

  Model backend (crp or spliced)

- GPD:

  Logical; whether GPD tail is included

- kernel:

  Kernel family used

- n_train:

  Number of training observations

- labels_new:

  (if newdata) Integer vector of predicted cluster labels

- probs_new:

  (if newdata) N_new x K matrix of predicted membership probabilities

- n_new:

  (if newdata) Number of new observations

- y_new:

  (if newdata) Response values for new observations

## References

Dahl, D. B. (2006). Model-based clustering for expression data via a
Dirichlet process mixture model. In M. Vannucci, et al. (Eds.), Bayesian
Inference for Gene Expression and Proteomics (pp. 201-218). Cambridge
University Press.

## Examples

``` r
if (FALSE) { # \dontrun{
# Fit a spliced CRP model
data("nc_posX100_p3_k2", package = "CausalMixGPD")
fit <- dpmgpd(y ~ x1 + x2 + x3, data = nc_posX100_p3_k2,
              backend = "spliced", kernel = "lognormal",
              components = 4, GPD = TRUE)

# Extract allocation
alloc <- allocation(fit)
print(alloc)
summary(alloc)
plot(alloc)

# Predict for new data
X_new <- data.frame(x1 = rnorm(20), x2 = rnorm(20), x3 = rnorm(20))
y_new <- rlnorm(20, meanlog = 1, sdlog = 0.5)
newdata <- cbind(y = y_new, X_new)
alloc_new <- allocation(fit, newdata = newdata)
plot(alloc_new, overlay = TRUE)
} # }
```
