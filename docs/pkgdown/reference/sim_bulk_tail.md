# Simulate bulk + tail data

Generates a positive outcome with a DP mixture-like bulk and a GPD tail.

## Usage

``` r
sim_bulk_tail(n = 200, tail_prob = 0.12, seed = NULL)
```

## Arguments

- n:

  Sample size.

- tail_prob:

  Proportion of excesses drawn from the tail (approximated).

- seed:

  Optional seed for reproducibility.

## Value

A numeric vector of outcomes.
