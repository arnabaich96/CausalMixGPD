# Print a MixGPD summary object

Print a MixGPD summary object

## Usage

``` r
# S3 method for class 'mixgpd_summary'
print(x, digits = 3, max_rows = 60, ...)
```

## Arguments

- x:

  A `"mixgpd_summary"` object.

- digits:

  Number of digits to print.

- max_rows:

  Maximum rows to print.

- ...:

  Unused.

## Value

`x` invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = TRUE, components = 6,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)
summary(fit)
} # }
```
