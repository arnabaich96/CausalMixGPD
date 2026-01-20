# Extract posterior mean parameters in original form

Returns posterior means reshaped to their natural dimensions (scalars,
vectors, or matrices). Intended as a lightweight extractor for model
parameters.

## Usage

``` r
params(object, ...)
```

## Arguments

- object:

  A fitted object of class `"mixgpd_fit"`.

- ...:

  Unused.

## Value

An object of class `"mixgpd_params"` (a named list).

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = TRUE, components = 6,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)
params(fit)
p <- params(fit)
} # }
```
