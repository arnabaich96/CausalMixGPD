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
#>            x1         x2         x3        x4
#> 1 -0.73603830 -0.7947724 -1.4074481 0.4032571
#> 2  1.10631861 -0.2493202  0.6877691 0.8812323
#> 3  0.65815022  0.5916320 -0.2308202 0.1494736
#> 4  0.51087674  0.5889225  1.0805712 0.2244676
#> 5 -0.06152613  0.9699535  2.2721089 0.2278626
#> 6 -0.41950298  0.1252092 -0.9769777 0.7616170
```
