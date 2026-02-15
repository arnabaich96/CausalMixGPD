# CausalMixGPD: Conditional workflow (covariate-dependent mixtures)

This vignette demonstrates conditional (regression) modeling, where the
outcome distribution varies with covariates. The core spliced bulk–tail
model and posterior computation are defined in the `basic` vignette;
here we focus on how to supply covariates and how to use the general
link mechanism.

We use a shipped positive-support dataset with covariates,
`nc_posX100_p4_k3`, which contains:

- `y`: numeric outcome,
- `X`: a `data.frame` with columns `x1`–`x4`,
- `meta`: metadata,
- `truth`: generation truth.

Design matrix

CausalMixGPD expects a design matrix (typically including an intercept).
A robust default is to use
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html).

Model fit (code shown for reproducibility, but not executed during CRAN
checks)

``` r

bundle <- bundle(
  y = y,
  X = X,
  backend = "sb",
  kernel = "lognormal",   # choose a kernel compatible with support
  GPD = FALSE,             # can be TRUE; see basic vignette for splice definition
  components = 4,
  mcmc = list(niter = 2500, nburnin = 600, thin = 5, nchains = 1, seed = 202)
)

fit <- mcmc(bundle, show_progress = TRUE)
```

General link mode (what “special cases” are really doing)

When you pass `param_specs`, you are not locked into a single model
form. Any parameter that the kernel registry marks as linkable can be
placed into `mode="link"`, meaning it becomes a function of covariates.
Conceptually, for a chosen parameter $`\theta_j(\boldsymbol{x})`$,
``` math
  g\{\theta_j(\boldsymbol{x})\} = \boldsymbol{x}^\top \boldsymbol{\beta}_j,
```
with a chosen link function $`g(\cdot)`$ and component-specific
coefficients $`\boldsymbol{\beta}_j`$.

In other words, the conditional examples in this vignette are concrete
instances of a general feature: link mode can be applied to any
supported parameter, not just the one shown.

Predictions

``` r

# Two covariate profiles (rows of a design matrix)
newdf <- rbind(
  as.list(stats::apply(Xdf, 2, stats::median)),
  as.list(stats::apply(Xdf, 2, function(v) stats::median(v) + stats::sd(v)))
)
newdf <- as.data.frame(newdf)
Xnew <- stats::model.matrix(~ ., data = newdf)

ygrid <- seq(stats::quantile(y, 0.01), stats::quantile(y, 0.99), length.out = 160)

dens_pred <- predict(fit, x = Xnew, y = ygrid, type = "density", cred.level = 0.90)
q_pred    <- predict(fit, x = Xnew, type = "quantile", p = c(0.5, 0.9, 0.95), cred.level = 0.90)
```

Results (precomputed)

| estimate | index |  id |  lower |   upper |
|---------:|------:|----:|-------:|--------:|
|    1.441 |  0.50 |   1 |  1.234 |   1.633 |
|    1.830 |  0.50 |   2 |  1.235 |   2.346 |
|   42.465 |  0.90 |   1 | 13.189 | 129.608 |
|   50.577 |  0.90 |   2 | 14.930 | 144.292 |
|  134.785 |  0.95 |   1 | 25.133 | 478.788 |
|  156.009 |  0.95 |   2 | 28.908 | 518.216 |

![Posterior predictive density for two covariate profiles
(precomputed).](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAA4QAAAImCAMAAAA8M7RYAAAAA1BMVEX///+nxBvIAAAACXBIWXMAABJ0AAASdAHeZh94AAAB90lEQVR4nO3BAQ0AAADCoPdPbQ43oAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAJ4NkCcAARE2voIAAAAASUVORK5CYII=)

Posterior predictive density for two covariate profiles (precomputed).

Notes on customization

1.  Link any supported parameter: use the kernel registry to see which
    parameters are eligible for link mode, then specify `param_specs`
    accordingly.

2.  Tail module in regression: if you set `GPD=TRUE` in a conditional
    model, the splice definition remains the same (see `basic`), but
    $`u(\boldsymbol{x})`$ and/or tail parameters may also be specified
    as fixed, random, or link-based depending on support.

3.  Prediction targets: use `predict(..., type="density")`,
    `"survival"`, `"quantile"`, and `"rmean"` to obtain distributional
    summaries that are coherent under the spliced model.
