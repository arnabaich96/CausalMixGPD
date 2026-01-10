# Build a causal bundle (design + two outcome arms)

Creates a causal bundle with:

- a propensity score (PS) design model (logistic regression of `T` on
  `X`)

- an outcome bundle for the control arm (`T = 0`)

- an outcome bundle for the treated arm (`T = 1`)

## Usage

``` r
build_causal_bundle(
  y,
  X,
  T,
  backend = c("sb", "crp"),
  kernel,
  GPD = FALSE,
  components = NULL,
  J = NULL,
  param_specs = NULL,
  mcmc_outcome = list(niter = 2000, nburnin = 500, thin = 1, nchains = 1, seed = 1),
  mcmc_ps = list(niter = 2000, nburnin = 500, thin = 1, nchains = 1, seed = 1),
  epsilon = 0.025,
  alpha_random = TRUE,
  ps_model = c("logit"),
  ps_prior = list(mean = 0, sd = 2),
  include_intercept = TRUE
)
```

## Arguments

- y:

  Numeric outcome vector.

- X:

  Design matrix or data.frame of covariates (N x P).

- T:

  Binary treatment indicator (length N, values 0/1).

- backend:

  Character; `"sb"` or `"crp"` for outcome models. If length 2, the
  first entry is used for treated (`T=1`) and the second for control
  (`T=0`).

- kernel:

  Character kernel name for outcome models. If length 2, the first entry
  is used for treated (`T=1`) and the second for control (`T=0`).

- GPD:

  Logical; include GPD tail for outcomes if TRUE. If length 2, the first
  entry is used for treated (`T=1`) and the second for control (`T=0`).

- components:

  Deprecated alias for `J`. Only one of `J` or `components` should be
  supplied.

- J:

  Integer \>= 2; truncation parameter for outcome mixtures. If length 2,
  the first entry is used for treated (`T=1`) and the second for control
  (`T=0`).

- param_specs:

  Outcome parameter overrides (same structure as
  [`build_nimble_bundle()`](https://example.com/DPmixGPD/reference/build_nimble_bundle.md)).
  You can pass a single list used for both arms or a list with `con` and
  `trt` entries.

- mcmc_outcome:

  MCMC settings list for the outcome bundles.

- mcmc_ps:

  MCMC settings list for the PS model.

- epsilon:

  Numeric in \[0,1) used by outcome bundles for posterior truncation
  summaries. If length 2, the first entry is used for treated (`T=1`)
  and the second for control (`T=0`).

- alpha_random:

  Logical; whether outcome concentration `alpha` is stochastic.

- ps_model:

  PS model family. Currently supports: `"logit"`.

- ps_prior:

  Normal prior for PS coefficients. List with `mean` and `sd`.

- include_intercept:

  Logical; if TRUE, an intercept column is prepended to `X` in the PS
  model.

## Value

A list of class `"dpmixgpd_causal_bundle"`.

## Details

The outcome bundles reuse the existing DPM + optional GPD tail
machinery. The PS model is a lightweight NIMBLE logistic regression with
normal priors on coefficients.

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(1)
N <- 100
X <- cbind(x1 = rnorm(N), x2 = runif(N))
T <- rbinom(N, 1, plogis(0.3 + 0.5 * X[, 1]))
y <- rexp(N) + 0.1

cb <- build_causal_bundle(
  y = y,
  X = X,
  T = T,
  backend = "sb",
  kernel = "gamma",
  GPD = TRUE,
  J = 10
)
} # }
```
