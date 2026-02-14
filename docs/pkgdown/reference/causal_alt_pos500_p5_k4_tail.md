# causal_alt_pos500_p5_k4_tail dataset

Causal dataset (N=500, p=5) with different positive-support kernels by
arm and tail-designed exceedances (GPD=TRUE).

## Usage

``` r
causal_alt_pos500_p5_k4_tail
```

## Format

A list with:

- y:

  Numeric outcome vector.

- A:

  Binary treatment indicator (0/1).

- X:

  data.frame with x1-x5.

- meta:

  List with N, support, p, K0, K1, tail, exceed_frac.

- truth:

  List with kernel0, kernel1, params0, params1, tail_params.

## Examples

``` r
data("causal_alt_pos500_p5_k4_tail")
head(causal_alt_pos500_p5_k4_tail$X)
#>           x1         x2         x3        x4         x5
#> 1  0.1869275 -0.3299569 -1.3595622 0.5118787  1.3010224
#> 2  0.1146152  0.0386302  0.5330725 0.5270846 -0.7850332
#> 3  0.4088704  0.5742201  0.2672072 0.6773349 -0.6377091
#> 4  1.3616934 -0.8692484  1.6633955 0.1069206  0.6794232
#> 5 -1.6666606  0.8669734 -1.0698512 0.6892755  0.4582010
#> 6  0.8113426 -0.6225343 -0.1493747 0.3253448 -0.1951937
```
