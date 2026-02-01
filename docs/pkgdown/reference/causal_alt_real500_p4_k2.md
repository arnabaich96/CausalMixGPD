# causal_alt_real500_p4_k2 dataset

Causal dataset (N=500, p=4) with different real-line kernels by arm.
Intended for alternating-kernel causal vignettes (GPD=FALSE).

## Usage

``` r
causal_alt_real500_p4_k2
```

## Format

A list with:

- y:

  Numeric outcome vector.

- T:

  Binary treatment indicator (0/1).

- X:

  data.frame with x1-x4.

- meta:

  List with N, support, p, K0, K1, tail, exceed_frac.

- truth:

  List with kernel0, kernel1, params0, params1, tail_params.

## Examples

``` r
data("causal_alt_real500_p4_k2")
head(causal_alt_real500_p4_k2$X)
#>        x1     x2     x3    x4
#> 1 -0.7360 -0.795 -1.407 0.403
#> 2  1.1063 -0.249  0.688 0.881
#> 3  0.6582  0.592 -0.231 0.149
#> 4  0.5109  0.589  1.081 0.224
#> 5 -0.0615  0.970  2.272 0.228
#> 6 -0.4195  0.125 -0.977 0.762
```
