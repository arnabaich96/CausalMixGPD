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

- A:

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
#>             x1          x2         x3
#> 1 -0.003781787 -0.69492869 -1.5881776
#> 2  0.523162994 -0.03191112  2.2011539
#> 3  0.969218062  0.55849252  0.7193627
#> 4  0.255903968 -0.39499972  1.7737434
#> 5  0.521408231 -0.76974360  0.3653396
#> 6  0.537248460 -0.93069596  0.7469898
```
