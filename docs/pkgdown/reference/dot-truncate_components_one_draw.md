# Truncate and reorder mixture components by cumulative weight mass

Truncate and reorder mixture components by cumulative weight mass

## Usage

``` r
.truncate_components_one_draw(w, params, epsilon = 0.01)
```

## Arguments

- w:

  Numeric vector of component weights (length K).

- params:

  Named list of numeric vectors, each length K (component-specific
  params).

- epsilon:

  Numeric in \[0,1). Keep the smallest k s.t. cumweight \>= 1-epsilon.

## Value

A list with reordered+truncated weights/params and bookkeeping.

## Details

This helper operates on one posterior draw at a time. It first orders
mixture components by decreasing weight, then keeps the smallest
effective subset of components implied by the package truncation rule,
and finally renormalizes the retained weights so they sum to one.

The same permutation is applied to every component-specific parameter
vector in `params`, which keeps the retained parameter blocks aligned
with the retained weights.
