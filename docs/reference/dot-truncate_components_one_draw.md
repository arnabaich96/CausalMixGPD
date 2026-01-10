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
