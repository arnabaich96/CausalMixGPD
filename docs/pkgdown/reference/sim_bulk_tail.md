# Simulate positive bulk-tail data

Generate synthetic outcomes with a light-to-moderate bulk and a heavier
upper tail. The sample is assembled from a lognormal-gamma bulk and a
shifted tail sample, then sorted. This is a quick data generator for
examples, help pages, and workflow checks rather than a formal
generative model matching the full package hierarchy exactly.

## Usage

``` r
sim_bulk_tail(n = 200, tail_prob = 0.12, seed = NULL)
```

## Arguments

- n:

  Integer sample size.

- tail_prob:

  Approximate tail probability \\\Pr(X \> u)\\ used to split the sample
  into bulk and tail draws.

- seed:

  Optional random seed for reproducibility.

## Value

Numeric vector of length `n` containing positive outcomes sorted in
ascending order.

## Details

The generator approximates a spliced sample \$\$ X \sim (1 - \pi_u)
F\_{bulk} + \pi_u F\_{tail}, \$\$ where \\\pi_u =\\ `tail_prob`. The
bulk component is itself a simple two-component mixture, while the tail
component is a shifted positive distribution that produces larger
values.

Use this helper when you need a fast toy sample for
[`bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md),
[`dpmix()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.md),
or
[`dpmgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md).
It should not be interpreted as posterior predictive simulation from a
fitted object.

## See also

[`sim_causal_qte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/sim_causal_qte.md),
[`sim_survival_tail()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/sim_survival_tail.md),
[`bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md),
[`dpmgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md).

Other simulation helpers:
[`sim_causal_qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/sim_causal_qte.md)`()`,
[`sim_survival_tail`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/sim_survival_tail.md)`()`
