# Safely coerce MCMC samples to coda::mcmc.list

Safely coerce MCMC samples to coda::mcmc.list

## Usage

``` r
.get_samples_mcmclist(object)
```

## Arguments

- object:

  A mixgpd_fit.

## Value

A coda::mcmc.list object.

## Details

Downstream summary and plotting code relies on the `coda` interface.
This helper validates the fit, locates the stored posterior draws, and
converts a single-chain `mcmc` object into an `mcmc.list` so later code
can treat the one-chain and multi-chain cases uniformly.
