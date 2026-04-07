# Cauchy mixture distribution

Finite mixture of Cauchy components for symmetric heavy-tailed bulk
modeling on the real line.

## Usage

``` r
dCauchyMix(x, w, location, scale, log = 0)

pCauchyMix(q, w, location, scale, lower.tail = 1, log.p = 0)

rCauchyMix(n, w, location, scale)

qCauchyMix(
  p,
  w,
  location,
  scale,
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

- location, scale:

  Numeric vectors of length \\K\\ giving component locations and scales.

- log:

  Logical; if `TRUE`, return the log-density.

- q:

  Numeric scalar giving the point at which the distribution function is
  evaluated.

- lower.tail:

  Logical; if `TRUE` (default), probabilities are \\P(X \le q)\\.

- log.p:

  Logical; if `TRUE`, probabilities are returned on the log scale.

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

Density/CDF/RNG functions return numeric scalars. `qCauchyMix()` returns
a numeric vector with the same length as `p`.

## Details

The mixture density is \$\$ f(x) = \sum\_{k = 1}^K \tilde{w}\_k f_C(x
\mid \ell_k, s_k), \$\$ with normalized weights \\\tilde{w}\_k\\. These
scalar functions are NIMBLE-compatible; for vectorized R usage, use
[`cauchy_mix_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cauchy_mix_lowercase.md).

The mixture CDF is the weighted average of component CDFs, \$\$ F(x) =
\sum\_{k=1}^K \tilde{w}\_k \left\\\frac{1}{2} +
\frac{1}{\pi}\arctan\left(\frac{x-\ell_k}{s_k}\right)\right\\. \$\$
Random generation first selects a component according to the normalized
weights and then draws from the chosen Cauchy law by inverse-CDF
sampling.

Because each Cauchy component has undefined mean and variance, the
mixture also lacks an ordinary mean in general. That is why the package
exposes Cauchy kernels for densities, CDFs, quantiles, medians, survival
functions, and restricted means, but not for ordinary predictive means.

## Functions

- `dCauchyMix()`: Cauchy mixture density

- `pCauchyMix()`: Cauchy mixture distribution function

- `rCauchyMix()`: Cauchy mixture random generation

- `qCauchyMix()`: Cauchy mixture quantile function

## See also

[`cauchy()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cauchy.md),
[`cauchy_mix_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cauchy_mix_lowercase.md),
[`build_nimble_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md),
[`kernel_support_table()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/kernel_support_table.md).

## Examples

``` r
w <- c(0.50, 0.30, 0.20)
location <- c(-2, 0, 3)
scale <- c(1.0, 0.7, 1.5)

dCauchyMix(0.5, w = w, location = location, scale = scale, log = FALSE)
#> [1] 0.1235181
pCauchyMix(0.5, w = w, location = location, scale = scale,
           lower.tail = TRUE, log.p = FALSE)
#> [1] 0.6830742
qCauchyMix(0.50, w = w, location = location, scale = scale)
#> [1] -0.6639507
qCauchyMix(0.95, w = w, location = location, scale = scale)
#> [1] 6.996407
replicate(10, rCauchyMix(1, w = w, location = location, scale = scale))
#>  [1] -2.5795875 -0.6903653  1.7792099  5.3486299  3.3920039 45.6839639
#>  [7] -0.8740658 -1.1633733  1.3870253  3.5761715
```
