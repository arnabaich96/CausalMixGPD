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
#>            x1          x2          x3
#> 1  0.38137182 -0.47943924 -0.14737691
#> 2 -0.04586294  0.06507704  0.02226763
#> 3  0.82269976 -0.30143661 -1.20095919
#> 4 -0.28191615 -0.13892273 -1.82635785
#> 5 -0.83260201  0.04237285  0.05980613
#> 6  0.17823357  0.78714464  0.79917768
```
