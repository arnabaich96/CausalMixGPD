# Run MCMC from a bundle (short wrapper)

Run MCMC from a bundle (short wrapper)

## Usage

``` r
mcmc(b, ...)
```

## Arguments

- b:

  A non-causal or causal bundle.

- ...:

  Optional MCMC overrides (`niter`, `nburnin`, `thin`, `nchains`,
  `seed`, `waic`) and runner controls (`show_progress`, `quiet`).

## Value

A fitted object (`"mixgpd_fit"` or `"dpmixgpd_causal_fit"`).
