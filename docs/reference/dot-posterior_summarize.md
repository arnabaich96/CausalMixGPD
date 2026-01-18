# Summarize posterior draws (mean + quantiles)

Summarize posterior draws (mean + quantiles)

## Usage

``` r
.posterior_summarize(draws, probs = c(0.025, 0.5, 0.975))
```

## Arguments

- draws:

  Numeric vector, matrix, or array with draws in last dimension.

- probs:

  Numeric quantile probs.

## Value

List with estimate, lower, upper, and q.
