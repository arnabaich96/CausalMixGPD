# CausalMixGPD: Causal modeling (two-arm outcomes, ATE/QTE)

This vignette introduces CausalMixGPD’s causal interface. The goal is to
estimate treatment effects on *distributional* features of the outcome,
not just a mean difference. In heavy-tailed settings, this matters:
effects can be small near the center but large (or unstable) in the
extremes.

CausalMixGPD implements a pragmatic approach: fit an outcome model under
treatment and under control, then compute causal estimands from the two
posterior predictive distributions. The outcome models can be
unconditional or conditional on covariates, and can optionally include a
spliced GPD tail.

## Identification assumptions (briefly)

The causal layer follows the standard potential outcomes framework
([Rubin 1974](#ref-rubin1974)). For observational studies, the usual
conditions are required:

- Consistency / SUTVA.
- Ignorability (unconfoundedness) given measured covariates.
- Positivity (overlap).

Propensity score (PS) augmentation can be enabled to stabilize the
conditional outcome modeling. The PS idea and its central role are
classical ([Rosenbaum and Rubin 1983](#ref-rosenbaum1983)), with
efficiency improvements using estimated PS discussed in ([Hirano et al.
2003](#ref-hirano2003)).

## Estimands

CausalMixGPD supports both marginal and conditional estimands.

Quantile treatment effect (QTE) at quantile level $`\tau`$:
``` math
  \mathrm{QTE}(\tau) = Q_Y^{(1)}(\tau) - Q_Y^{(0)}(\tau),
```
where $`Q_Y^{(a)}(\tau)`$ is the $`\tau`$-quantile under arm
$`a\in\{0,1\}`$. QTEs provide a distributional view of effects and are
central in causal quantile regression ([Koenker and Bassett
1978](#ref-koenker1978); [Firpo 2007](#ref-firpo2007)).

Average treatment effect (ATE) is typically defined on the mean of the
outcome. Under heavy tails, the mean may be unstable; CausalMixGPD also
supports restricted-mean type summaries via
[`ate_rmean()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate_rmean.md).

Conditional versions
([`cqte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md),
[`cate()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md))
evaluate effects at specific covariate profiles.

## Example workflow

We use a shipped dataset with a positive-support outcome and an injected
upper tail: `causal_alt_pos500_p5_k4_tail`.

``` r

library(CausalMixGPD)
data("causal_alt_pos500_p5_k4_tail")

dat <- causal_alt_pos500_p5_k4_tail

y <- dat$y
A <- dat$A                      # 0/1 treatment indicator
X <- model.matrix(~ ., data = dat$X)

# Build a causal bundle (two outcome models + optional PS model)
cb <- bundle(
  y = y,
  X = X,
  treat = A,
  backend = "sb",
  kernel = c("lognormal", "lognormal"),
  GPD = c(TRUE, TRUE),
  components = c(4, 4),
  PS = "logit",            # or "probit" / "naive" / FALSE
  ps_scale = "logit",
  ps_summary = "mean",
  mcmc_outcome = list(niter = 2500, nburnin = 600, thin = 5, nchains = 1, seed = 303),
  mcmc_ps      = list(niter = 1800, nburnin = 500, thin = 5, nchains = 1, seed = 304)
)

fit <- mcmc(cb)

# Marginal effects
q <- qte(fit, probs = c(0.5, 0.9, 0.95), level = 0.90)
a <- ate(fit, level = 0.90)

# Conditional effects at new covariates
Xnew <- X[1:5, , drop = FALSE]
q_x <- cqte(fit, probs = c(0.5, 0.9), newdata = Xnew, level = 0.90)
a_x <- cate(fit, newdata = Xnew, level = 0.90)
```

## Causal estimates (precomputed)

Marginal QTEs (posterior mean with 90% credible interval):

| Quantile (τ) | QTE estimate \[L,U\]     |
|--------------|--------------------------|
| 0.5          | 0.274 \[-0.485, 1.127\]  |
| 0.9          | -0.295 \[-2.770, 2.347\] |
| 0.95         | -0.386 \[-3.484, 2.728\] |

Marginal ATE (posterior mean with 90% credible interval):

| Estimand | Estimate \[L,U\]         |
|----------|--------------------------|
| ATE      | -0.282 \[-0.915, 0.240\] |

A QTE curve across quantile levels (precomputed):

![QTE curve as a function of quantile level
(precomputed).](../extdata/causal_qte.png)

QTE curve as a function of quantile level (precomputed).

## Practical notes

1.  Two-arm flexibility: `kernel`, `backend`, `GPD`, and `components`
    can each be length-2 vectors. This is useful when, for example, the
    treated arm has a visibly different tail behavior.

2.  Heavy tails and “mean effects”: if you see wide uncertainty on the
    ATE but sharper QTEs near the center, that’s not a bug. It usually
    reflects that the mean is sensitive to rare extremes, while
    median-like functionals are not.

3.  Conditional effects: use
    [`cqte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md)
    /
    [`cate()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md)
    when you care about subgroup effects. Start with a few clinically
    meaningful covariate profiles and only then move to dense grids.

## References

Firpo, Sergio. 2007. “Efficient Semiparametric Estimation of Quantile
Treatment Effects.” *Econometrica* 75 (1): 259–76.
<https://doi.org/10.1111/j.1468-0262.2007.00738.x>.

Hirano, Keisuke, Guido W. Imbens, and Geert Ridder. 2003. “Efficient
Estimation of Average Treatment Effects Using the Estimated Propensity
Score.” *Econometrica* 71 (4): 1161–89.
<https://doi.org/10.1111/1468-0262.00442>.

Koenker, Roger, and Gilbert Bassett. 1978. “Regression Quantiles.”
*Econometrica* 46 (1): 33–50. <https://doi.org/10.2307/1913643>.

Rosenbaum, Paul R., and Donald B. Rubin. 1983. “The Central Role of the
Propensity Score in Observational Studies for Causal Effects.”
*Biometrika* 70 (1): 41–55. <https://doi.org/10.1093/biomet/70.1.41>.

Rubin, Donald B. 1974. “Estimating Causal Effects of Treatments in
Randomized and Nonrandomized Studies.” *Journal of Educational
Psychology* 66 (5): 688–701. <https://doi.org/10.1037/h0037350>.
