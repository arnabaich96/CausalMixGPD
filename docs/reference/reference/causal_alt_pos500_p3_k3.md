# causal_alt_pos500_p3_k3 dataset

Causal dataset (N=500, p=3) with different positive-support kernels by
arm. Intended for alternating-kernel causal vignettes (GPD=FALSE).

## Usage

``` r
causal_alt_pos500_p3_k3
```

## Format

A list with:

- y:

  Numeric outcome vector.

- T:

  Binary treatment indicator (0/1).

- X:

  data.frame with x1-x3.

- meta:

  List with N, support, p, K0, K1, tail, exceed_frac.

- truth:

  List with kernel0, kernel1, params0, params1, tail_params.

## Examples

``` r
data("causal_alt_pos500_p3_k3")
head(causal_alt_pos500_p3_k3$X)
#>        x1      x2      x3
#> 1  0.3814 -0.4794 -0.1474
#> 2 -0.0459  0.0651  0.0223
#> 3  0.8227 -0.3014 -1.2010
#> 4 -0.2819 -0.1389 -1.8264
#> 5 -0.8326  0.0424  0.0598
#> 6  0.1782  0.7871  0.7992
```
