# causal_pos500_p3_k2 dataset

Causal dataset (N=500, p=3) with the same positive-support kernel for
both arms. Intended for same-kernel causal baselines (GPD=FALSE).

## Usage

``` r
causal_pos500_p3_k2
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
data("causal_pos500_p3_k2")
head(causal_pos500_p3_k2$X)
#>         x1      x2     x3
#> 1 -0.00378 -0.6949 -1.588
#> 2  0.52316 -0.0319  2.201
#> 3  0.96922  0.5585  0.719
#> 4  0.25590 -0.3950  1.774
#> 5  0.52141 -0.7697  0.365
#> 6  0.53725 -0.9307  0.747
```
