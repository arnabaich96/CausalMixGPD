# Lognormal mixture distribution

Finite mixture of lognormal components for positive-support bulk
modeling. The scalar functions in this topic are the NIMBLE-compatible
building blocks for the lognormal bulk kernel family.

## Usage

``` r
dLognormalMix(x, w, meanlog, sdlog, log = 0)

pLognormalMix(q, w, meanlog, sdlog, lower.tail = 1, log.p = 0)

rLognormalMix(n, w, meanlog, sdlog)

qLognormalMix(
  p,
  w,
  meanlog,
  sdlog,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)
```

## Arguments

- x:

  Numeric scalar giving the point at which the density is evaluated.

- w:

  Numeric vector of mixture weights of length \\K\\. The functions
  normalize `w` internally when needed.

- meanlog, sdlog:

  Numeric vectors of length \\K\\ giving component log-means and
  log-standard deviations.

- log:

  Integer flag `0/1`; if `1`, return the log-density.

- q:

  Numeric scalar giving the point at which the distribution function is
  evaluated.

- lower.tail:

  Integer flag `0/1`; if `1` (default), probabilities are \\P(X \le
  q)\\.

- log.p:

  Integer flag `0/1`; if `1`, probabilities are returned on the log
  scale.

- n:

  Integer giving the number of draws. The RNG implementation supports
  `n = 1`.

- p:

  Numeric scalar probability in \\(0,1)\\ for the quantile function.

- tol:

  Numeric scalar tolerance passed to
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

- maxiter:

  Integer maximum number of iterations for
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

## Value

Density/CDF/RNG functions return numeric scalars. `qLognormalMix()`
returns a numeric vector with the same length as `p`.

## Details

The mixture density is \$\$ f(x) = \sum\_{k = 1}^K \tilde{w}\_k
f\_{LN}(x \mid \mu_k, \sigma_k), \qquad x \> 0, \$\$ with normalized
weights \\\tilde{w}\_k\\. For vectorized R usage, use
[`lognormal_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_lowercase.md).

Each component satisfies \\\log X_k \sim N(\mu_k,\sigma_k^2)\\, so the
mixture CDF is \$\$ F(x) = \sum\_{k=1}^K \tilde{w}\_k
\Phi\left(\frac{\log x-\mu_k}{\sigma_k}\right), \qquad x\>0. \$\$ Random
generation proceeds by drawing a component index with probability
\\\tilde{w}\_k\\ and then sampling from the corresponding lognormal law.
Because a finite mixture of lognormals does not admit a closed-form
inverse CDF, `qLognormalMix()` computes quantiles by numerical
inversion.

The analytical mixture mean is \$\$ E(X) = \sum\_{k=1}^K \tilde{w}\_k
\exp(\mu_k + \sigma_k^2/2), \$\$ which is the expression used by the
package whenever an ordinary predictive mean exists.

## Functions

- `dLognormalMix()`: Lognormal mixture density

- `pLognormalMix()`: Lognormal mixture distribution function

- `rLognormalMix()`: Lognormal mixture random generation

- `qLognormalMix()`: Lognormal mixture quantile function

## See also

[`lognormal_mixgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_mixgpd.md),
[`lognormal_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_gpd.md),
[`lognormal_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_lowercase.md),
[`build_nimble_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md),
[`kernel_support_table()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/kernel_support_table.md).

Other lognormal kernel families:
[`lognormal_gpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_gpd.md),
[`lognormal_mixgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_mixgpd.md)

## Examples

``` r
w <- c(0.60, 0.25, 0.15)
meanlog <- c(-0.2, 0.6, 1.2)
sdlog <- c(0.4, 0.3, 0.5)

dLognormalMix(2.0, w = w, meanlog = meanlog, sdlog = sdlog, log = FALSE)
#> [1] 0.2189383
pLognormalMix(2.0, w = w, meanlog = meanlog, sdlog = sdlog,
             lower.tail = TRUE, log.p = FALSE)
#> [1] 0.7711135
qLognormalMix(0.50, w = w, meanlog = meanlog, sdlog = sdlog)
#> [1] 1.151134
qLognormalMix(0.95, w = w, meanlog = meanlog, sdlog = sdlog)
#> [1] 4.147585
replicate(10, rLognormalMix(1, w = w, meanlog = meanlog, sdlog = sdlog))
#>  [1] 0.6011902 0.5184476 4.1293830 0.6873494 1.3316464 0.6494833 0.9846285
#>  [8] 1.6379267 3.3315677 0.7021537
```
