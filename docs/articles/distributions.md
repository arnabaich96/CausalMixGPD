# Distributions

## Kernel families supported

DPmixGPD provides mixture kernels with optional GPD tails. Each kernel
has a **bulk** version (e.g., `dGammaMix`) and a **bulk+GPD** version
(e.g., `dGammaMixGpd`).

### Full list of kernels and parameters

| Kernel | Support | Parameters | Notes |
|----|----|----|----|
| Normal | $`\mathbb{R}`$ | `mean`, `sd` | Symmetric, light tails |
| Gamma | $`(0,\infty)`$ | `shape`, `scale` | Right‑skewed, positive support |
| Lognormal | $`(0,\infty)`$ | `meanlog`, `sdlog` | Log‑scale parameters |
| Laplace | $`\mathbb{R}`$ | `location`, `scale` | Sharper peak, exponential tails |
| Inverse Gaussian | $`(0,\infty)`$ | `mean`, `shape` | Positive, heavy right tail |
| Amoroso | $`(0,\infty)`$ (by default) | `loc`, `scale`, `shape1`, `shape2` | Very flexible; `scale` can flip support |
| Cauchy | $`\mathbb{R}`$ | `location`, `scale` | Heavy tails, weak moments |

**GPD tail parameters** (for `*MixGpd` functions):

- `threshold` (splicing point $`u`$)
- `tail_scale` ($`\sigma`$)
- `tail_shape` ($`\xi`$)

## Strengths and weaknesses (center vs tail)

- **Normal**: clean center, light tails; not ideal for heavy tails.
- **Gamma / Lognormal / InvGauss**: good for positive, right‑skewed
  outcomes.
- **Laplace**: sharp center, exponential tails; robust to mild outliers.
- **Cauchy**: extreme tails; use only when outliers dominate.
- **Amoroso**: most flexible; can mimic several positive‑support
  families.

## How kernels interact with GPD tails

The GPD tail is **spliced** above `threshold` so the bulk kernel governs
central behavior while the GPD handles extreme exceedances. This
isolates tail modeling from the bulk and avoids overfitting the center.

## Parameterization warnings

- **Gamma** uses `shape` and `scale` (not rate).
- **Lognormal** uses `meanlog` and `sdlog` (log‑scale parameters).
- **Laplace** uses `location` and `scale`.
- **Inverse Gaussian** uses `mean` and `shape`.
- **Amoroso** uses `loc`, `scale`, `shape1`, `shape2` and can flip
  support if `scale < 0`.

Always verify parameter meanings in the kernel help pages:

``` r
?dGammaMix
?dLognormalMix
?dLaplaceMix
?dInvGaussMix
?dAmorosoMix
?dCauchyMix
```

## What this vignette does NOT cover

- DP mixture modeling
- MCMC
- Backends
- Causal interpretation

Those are covered in the dedicated vignettes listed in
[introduction](https://arnabaich96.github.io/DPmixGPD/articles/introduction.Rmd).
