# Print an ATE-style effect object

`print.causalmixgpd_ate()` prints a compact summary for objects produced
by
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`att`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/att.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md),
or
[`ate_rmean`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate_rmean.md).

## Usage

``` r
# S3 method for class 'causalmixgpd_ate'
print(x, digits = 3, max_rows = 6, ...)
```

## Arguments

- x:

  A `"causalmixgpd_ate"` object from
  [`ate()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md).

- digits:

  Number of digits to display.

- max_rows:

  Maximum number of estimate rows to display.

- ...:

  Unused.

## Value

The object `x`, invisibly.

## Details

These objects summarize posterior treatment contrasts on the mean scale.
For the marginal average treatment effect, \$\$\Delta = E(Y^1) -
E(Y^0).\$\$
[`att()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/att.md)
changes the standardization target to the treated population,
[`cate()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md)
conditions on supplied covariate profiles, and
[`ate_rmean()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate_rmean.md)
replaces the ordinary mean by a restricted mean \\\int_0^c S_a(t)\\dt\\
up to the chosen truncation point.

The print method shows the main effect table and setup metadata, but it
is not a full diagnostic report. Use
[`summary()`](https://rdrr.io/r/base/summary.html) for tabular summaries
and [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
graphical inspection of the same treatment-effect object.

## See also

[`summary.causalmixgpd_ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.causalmixgpd_ate.md),
[`plot.causalmixgpd_ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.causalmixgpd_ate.md),
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
a <- ate(fit, interval = "credible")
print(a)
} # }
```
