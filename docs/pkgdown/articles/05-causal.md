# CausalMixGPD: Causal workflow (ATE/QTE from two-arm models)

This vignette illustrates the causal interface: fitting two outcome
models (treated vs control) and extracting treatment effects on
distributional functionals. The spliced bulk–tail model and posterior
sampling steps are defined in the `basic` vignette; here we focus on the
causal layer.

CausalMixGPD’s causal tools are designed for observational studies with
standard identification conditions (SUTVA, ignorability/unconfoundedness
given measured covariates, and positivity). Propensity score (PS)
augmentation is available to stabilize conditional outcome modeling; see
([Rosenbaum and Rubin 1983](#ref-rosenbaum1983); [Hirano et al.
2003](#ref-hirano2003)) for background.

We use a shipped causal dataset with a positive-support outcome and an
injected tail: `causal_alt_pos500_p5_k4_tail`. The object contains `y`,
`T`, `X` (a `data.frame` with `x1`–`x5`), plus metadata and simulation
truth.

Build and fit (code shown for reproducibility, but not executed during
CRAN checks)

``` r

cb <- build_causal_bundle(
  y = y,
  X = X,
  T = T,
  backend = "sb",
  kernel = c("lognormal", "lognormal"),  # treated, control (can differ)
  GPD = c(TRUE, TRUE),
  components = c(4, 4),
  PS = "logit",            # "logit", "probit", "naive", or FALSE
  ps_scale = "logit",      # "logit" or "prob"
  ps_summary = "mean",     # "mean" or "median"
  mcmc_outcome = list(niter = 2500, nburnin = 600, thin = 5, nchains = 1, seed = 303),
  mcmc_ps      = list(niter = 1800, nburnin = 500, thin = 5, nchains = 1, seed = 304)
)

fit <- run_mcmc_causal(cb, show_progress = TRUE)
```

Treatment effects

The causal interface returns two common effect summaries:

1.  Quantile treatment effects (QTE), defined as
    ``` math
      q_Y(\tau\mid\boldsymbol{x}) = Q_Y^{(1)}(\tau\mid\boldsymbol{x}) - Q_Y^{(0)}(\tau\mid\boldsymbol{x}),
    ```
    where $`Q_Y^{(a)}(\tau\mid\boldsymbol{x})`$ is the conditional
    $`\tau`$-quantile under arm $`a\in\{0,1\}`$. See ([Koenker and
    Bassett 1978](#ref-koenker1978); [Firpo 2007](#ref-firpo2007)).

2.  Average treatment effects (ATE), typically on the posterior
    predictive mean. Under heavy tails the mean may be unstable, so
    CausalMixGPD also supports restricted means.

``` r

q <- qte(fit, probs = c(0.5, 0.9, 0.95), interval = "credible", level = 0.90)
a <- ate(fit, interval = "credible", level = 0.90, nsim_mean = 100)
```

Results (precomputed)

      prob estimate lower  upper
    1 0.50   -0.769 -4.48 0.4878
    2 0.90   -1.274 -3.23 0.0509
    3 0.95   -1.761 -4.27 0.1167

      estimand estimate  lower  upper
    1      ATE 8.19e+10  -8.83 1680.2
    2      ATE 7.85e+06  -3.96   20.9
    3      ATE 1.94e+07  -4.92   37.7
    4      ATE 2.61e+06  -2.23 2281.6
    5      ATE 5.49e+08 -27.20 2357.7
    6      ATE 5.45e+07  -2.18   37.5

![QTE curve as a function of quantile level
(precomputed).](data:image/png;base64,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)

QTE curve as a function of quantile level (precomputed).

Notes on customization

1.  Asymmetric arms: `kernel`, `backend`, `GPD`, and `components` can be
    vectors of length 2, allowing treated and control models to differ.

2.  PS augmentation: set `PS` to `"logit"`/`"probit"` to jointly model
    PS and outcome; set `PS=FALSE` to fit outcome models without PS
    augmentation.

3.  Conditional effects: pass `newdata` to
    [`qte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md)
    and
    [`ate()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md)
    to obtain subgroup-specific treatment effects.

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
