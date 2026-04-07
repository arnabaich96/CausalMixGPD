# Print a propensity score fit

Print a propensity score fit

## Usage

``` r
# S3 method for class 'causalmixgpd_ps_fit'
print(x, ...)
```

## Arguments

- x:

  A `"causalmixgpd_ps_fit"` object.

- ...:

  Unused.

## Value

The input object (invisibly).

## Details

A propensity-score fit models the treatment assignment probability
\\e(x) = \Pr(A = 1 \mid X = x)\\. The printed header identifies which PS
family was fitted, but it intentionally omits coefficient-level
summaries.

Use [`summary()`](https://rdrr.io/r/base/summary.html) on the same
object when you need posterior means, spread, and intervals for the
monitored PS parameters. The compact print method is mainly an identity
check inside larger causal workflows.
