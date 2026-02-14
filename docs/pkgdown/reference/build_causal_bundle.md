# Build a causal bundle (design + two outcome arms)

Creates a causal bundle with:

- a propensity score (PS) design model (logit/probit regression of `A`
  on `X` or a naive Bayes classifier)

- an outcome bundle for the control arm (`A = 0`)

- an outcome bundle for the treated arm (`A = 1`)

## Usage

``` r
build_causal_bundle(
  y,
  X,
  A,
  backend = c("sb", "crp"),
  kernel,
  GPD = FALSE,
  components = NULL,
  param_specs = NULL,
  mcmc_outcome = list(niter = 2000, nburnin = 500, thin = 1, nchains = 1, seed = 1),
  mcmc_ps = list(niter = 2000, nburnin = 500, thin = 1, nchains = 1, seed = 1),
  epsilon = 0.025,
  alpha_random = TRUE,
  ps_prior = list(mean = 0, sd = 2),
  include_intercept = TRUE,
  PS = "logit",
  ps_scale = c("logit", "prob"),
  ps_summary = c("mean", "median"),
  ps_clamp = 1e-06
)
```

## Arguments

- y:

  Numeric outcome vector.

- X:

  Design matrix or data.frame of covariates (N x P).

- A:

  Binary treatment indicator (length N, values 0/1).

- backend:

  Character; `"sb"` or `"crp"` for outcome models. If length 2, the
  first entry is used for treated (`A=1`) and the second for control
  (`A=0`).

- kernel:

  Character kernel name for outcome models. If length 2, the first entry
  is used for treated (`A=1`) and the second for control (`A=0`).

- GPD:

  Logical; include GPD tail for outcomes if TRUE. If length 2, the first
  entry is used for treated (`A=1`) and the second for control (`A=0`).

- components:

  Integer \>= 2; truncation parameter for outcome mixtures. If length 2,
  the first entry is used for treated (`A=1`) and the second for control
  (`A=0`).

- param_specs:

  Outcome parameter overrides (same structure as
  [`build_nimble_bundle()`](https://arnabaich96.github.io/DPmixGPD/pkgdown/reference/build_nimble_bundle.md)).
  You can pass a single list used for both arms or a list with `con` and
  `trt` entries.

- mcmc_outcome:

  MCMC settings list for the outcome bundles.

- mcmc_ps:

  MCMC settings list for the PS model.

- epsilon:

  Numeric in \[0,1) used by outcome bundles for posterior truncation
  summaries. If length 2, the first entry is used for treated (`A=1`)
  and the second for control (`A=0`).

- alpha_random:

  Logical; whether outcome concentration `alpha` is stochastic.

- ps_prior:

  Normal prior for PS coefficients. List with `mean` and `sd`.

- include_intercept:

  Logical; if TRUE, an intercept column is prepended to `X` in the PS
  model.

- PS:

  Character or logical; controls propensity score estimation:

  - `"logit"` (default): Logistic regression PS model

  - `"probit"`: Probit regression PS model

  - `"naive"`: Gaussian naive Bayes PS model

  - `FALSE`: No PS estimation; outcome models use only `X`

  The PS model choice is stored in bundle metadata for downstream use in
  prediction and summaries, enabling seamless integration of future PS
  estimation methods.

- ps_scale:

  Scale used when augmenting outcomes with PS: `"logit"` or `"prob"`.

- ps_summary:

  Posterior summary for PS: `"mean"` or `"median"`.

- ps_clamp:

  Numeric epsilon for clamping PS values to \\(\epsilon, 1-\epsilon)\\.

## Value

A list of class `"dpmixgpd_causal_bundle"`.

## Details

The outcome bundles reuse the existing DPM + optional GPD tail
machinery. The PS model is a lightweight NIMBLE estimator supporting
logit/probit regression or a naive Bayes classifier with simple priors.

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(1)
N <- 100
X <- cbind(x1 = rnorm(N), x2 = runif(N))
A <- rbinom(N, 1, plogis(0.3 + 0.5 * X[, 1]))
y <- rexp(N) + 0.1

cb <- build_causal_bundle(
  y = y,
  X = X,
  A = A,
  backend = "sb",
  kernel = "gamma",
  GPD = TRUE,
  components = 10,
  PS = "probit"
)
} # }
```
