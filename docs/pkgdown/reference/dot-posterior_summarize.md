# Summarize posterior draws (mean + quantiles)

Summarize posterior draws (mean + quantiles)

## Usage

``` r
.posterior_summarize(
  draws,
  probs = c(0.025, 0.5, 0.975),
  interval = "credible"
)
```

## Arguments

- draws:

  Numeric vector, matrix, or array with draws in last dimension.

- probs:

  Numeric quantile probs.

- interval:

  Character or NULL; interval type:

  - `NULL`: no interval

  - `"credible"` (default): equal-tailed quantile intervals

  - `"hpd"`: highest posterior density intervals

## Value

List with estimate, lower, upper, and q.

## Details

The last dimension of `draws` is interpreted as the posterior-draw
dimension. This helper collapses that dimension to posterior means and
interval summaries, while preserving the leading dimensions of the input
object. It is used throughout prediction and treatment-effect code to
turn per-draw evaluations into reported posterior summaries.
