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

- T:

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
#>       x1      x2     x3    x4     x5
#> 1  0.187 -0.3300 -1.360 0.512  1.301
#> 2  0.115  0.0386  0.533 0.527 -0.785
#> 3  0.409  0.5742  0.267 0.677 -0.638
#> 4  1.362 -0.8692  1.663 0.107  0.679
#> 5 -1.667  0.8670 -1.070 0.689  0.458
#> 6  0.811 -0.6225 -0.149 0.325 -0.195
```
