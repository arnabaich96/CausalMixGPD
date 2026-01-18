# Print a dpmixgpd bundle

User-facing print method for pre-run bundles produced by
[`build_nimble_bundle()`](https://arnabaich96.github.io/DPmixGPD/reference/build_nimble_bundle.md).
This prints a compact description of the model structure
(backend/kernel/components), whether covariates are used, and whether a
GPD tail is enabled.

## Usage

``` r
# S3 method for class 'dpmixgpd_bundle'
print(x, code = FALSE, max_code_lines = 200L, ...)
```

## Arguments

- x:

  A `"dpmixgpd_bundle"` object.

- code:

  Logical; if TRUE, print the generated NIMBLE model code.

- max_code_lines:

  Integer; maximum number of code lines to print when `code=TRUE`.

- ...:

  Unused.

## Value

The object `x`, invisibly.

## Examples

``` r
# \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = FALSE, components = 6)
print(bundle)
#> DPmixGPD bundle
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel    Normal Distribution
#>  Components                      6
#>           N                     50
#>           X                     NO
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#>   contains  : code, constants, data, dimensions, inits, monitors
print(bundle, code = TRUE, max_code_lines = 30)
#> DPmixGPD bundle
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel    Normal Distribution
#>  Components                      6
#>           N                     50
#>           X                     NO
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#>   contains  : code, constants, data, dimensions, inits, monitors
#> 
#> Model code
#> {
#>     alpha ~ dgamma(1, 1)
#>     for (j in 1:(components - 1)) {
#>         v[j] ~ dbeta(1, alpha)
#>     }
#>     stick_mass[1] <- 1
#>     for (j in 2:components) {
#>         stick_mass[j] <- stick_mass[j - 1] * (1 - v[j - 1])
#>     }
#>     for (j in 1:(components - 1)) {
#>         w[j] <- v[j] * stick_mass[j]
#>     }
#>     w[components] <- stick_mass[components]
#>     for (j in 1:components) {
#>         mean[j] ~ dnorm(0, sd = 5)
#>         sd[j] ~ dgamma(2, 1)
#>     }
#>     for (i in 1:N) {
#>         z[i] ~ dcat(prob = w[1:components])
#>         y[i] ~ dnorm(mean[z[i]], sd[z[i]])
#>     }
#> } 
# }
```
