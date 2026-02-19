# DPmixGPD: Causal workflow (ATE/QTE from two-arm models)

This vignette illustrates the causal interface: fitting two outcome
models (treated vs control) and extracting treatment effects on
distributional functionals. The spliced bulk–tail model and posterior
sampling steps are defined in the `basic` vignette; here we focus on the
causal layer.

DPmixGPD’s causal tools are designed for observational studies with
standard identification conditions (SUTVA, ignorability/unconfoundedness
given measured covariates, and positivity). Propensity score (PS)
augmentation is available to stabilize conditional outcome modeling; see
([Rosenbaum and Rubin 1983](#ref-rosenbaum1983); [Hirano et al.
2003](#ref-hirano2003)) for background.

We use a shipped causal dataset with a positive-support outcome and an
injected tail: `causal_alt_pos500_p5_k4_tail`. The object contains `y`,
`A`, `X` (a `data.frame` with `x1`–`x5`), plus metadata and simulation
truth.

Build and fit (code shown for reproducibility, but not executed during
CRAN checks)

``` r

cb <- bundle(
  y = y,
  X = X,
  treat = A,
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

fit <- mcmc(cb, show_progress = TRUE)
```

Treatment effects

The causal interface supports both marginal and conditional estimands:

1.  Quantile treatment effects (QTE), defined as
    ``` math
      q_Y(\tau\mid\boldsymbol{x}) = Q_Y^{(1)}(\tau\mid\boldsymbol{x}) - Q_Y^{(0)}(\tau\mid\boldsymbol{x}),
    ```
    where $`Q_Y^{(a)}(\tau\mid\boldsymbol{x})`$ is the conditional
    $`\tau`$-quantile under arm $`a\in\{0,1\}`$. See ([Koenker and
    Bassett 1978](#ref-koenker1978); [Firpo 2007](#ref-firpo2007)).

2.  Average treatment effects (ATE), typically on the posterior
    predictive mean. Under heavy tails the mean may be unstable, so
    DPmixGPD also supports restricted means.

``` r

q <- qte(fit, probs = c(0.5, 0.9, 0.95), interval = "credible", level = 0.90)
a <- ate(fit, interval = "credible", level = 0.90, nsim_mean = 100)
q_tt <- qtt(fit, probs = c(0.5, 0.9, 0.95), interval = "credible", level = 0.90)
a_tt <- att(fit, interval = "credible", level = 0.90, nsim_mean = 100)
```

Results (precomputed)

      prob    estimate     lower    upper
    1 0.50 -0.04473370 -1.087009 1.469857
    2 0.90  0.04762935 -3.092099 3.690023
    3 0.95  0.29090914 -3.843600 4.893324

      estimand  estimate     lower     upper
    1      ATE -240.3653 -222.5154 0.1209172

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
    [`cqte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md)
    and
    [`cate()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md)
    to obtain subgroup-specific treatment effects.

``` r

q_cond <- cqte(fit, probs = c(0.5, 0.9), newdata = X[1:5, ], interval = "credible")
a_cond <- cate(fit, newdata = X[1:5, ], interval = "credible", nsim_mean = 100)

# Optional compact tables:
cate_tbl <- data.frame(
  id = seq_along(a_cond$fit),
  index = 1L,
  estimate = as.numeric(a_cond$fit),
  lower = as.numeric(a_cond$lower),
  upper = as.numeric(a_cond$upper)
)

cqte_tbl <- data.frame(
  id = rep(seq_len(nrow(q_cond$fit)), each = ncol(q_cond$fit)),
  index = rep(seq_len(ncol(q_cond$fit)), times = nrow(q_cond$fit)),
  estimate = as.vector(t(q_cond$fit)),
  lower = as.vector(t(q_cond$lower)),
  upper = as.vector(t(q_cond$upper))
)
```

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
