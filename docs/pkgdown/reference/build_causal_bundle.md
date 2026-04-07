# Build a causal bundle (design + two outcome arms)

`build_causal_bundle()` is the detailed constructor behind
[`bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md)
for causal analyses. It prepares:

- a propensity score (PS) design block for \\A \mid X\\,

- a control-arm outcome bundle for \\Y(0)\\,

- a treated-arm outcome bundle for \\Y(1)\\.

## Usage

``` r
build_causal_bundle(
  y,
  X,
  A,
  backend = c("sb", "crp", "spliced"),
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
  ps_clamp = 1e-06,
  monitor = c("core", "full"),
  monitor_latent = FALSE,
  monitor_v = FALSE
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

  Character; the Dirichlet process representation for outcome models:

  - `"sb"`: stick-breaking truncation

  - `"crp"`: Chinese Restaurant Process

  - `"spliced"`: CRP with GPD tail splicing

  If length 2, the first entry is used for treated (`A=1`) and the
  second for control (`A=0`).

- kernel:

  Character kernel name for outcome models (must exist in
  [`get_kernel_registry()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/get_kernel_registry.md)).
  If length 2:

  - first entry: used for treated (`A=1`)

  - second entry: used for control (`A=0`)

- GPD:

  Logical; include GPD tail for outcomes if TRUE. If length 2:

  - first entry: used for treated (`A=1`)

  - second entry: used for control (`A=0`)

- components:

  Integer \>= 2; truncation parameter for outcome mixtures. If length 2:

  - first entry: used for treated (`A=1`)

  - second entry: used for control (`A=0`)

- param_specs:

  Outcome parameter overrides (same structure as
  [`build_nimble_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md)):

  - a single list: used for both arms

  - a list with `con` and `trt` entries: arm-specific overrides

- mcmc_outcome:

  MCMC settings list for the outcome bundles.

- mcmc_ps:

  MCMC settings list for the PS model.

- epsilon:

  Numeric in \[0,1) used by outcome bundles for posterior truncation
  summaries. If length 2:

  - first entry: used for treated (`A=1`)

  - second entry: used for control (`A=0`)

- alpha_random:

  Logical; whether the outcome-model DP concentration parameter
  \\\kappa\\ is stochastic.

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

  Scale used when augmenting outcomes with PS:

  - `"logit"`: augment on the logit (log-odds) scale

  - `"prob"`: augment on the probability scale

- ps_summary:

  Posterior summary for PS:

  - `"mean"`: posterior mean of propensity scores

  - `"median"`: posterior median of propensity scores

- ps_clamp:

  Numeric epsilon for clamping PS values to \\(\epsilon, 1-\epsilon)\\.

- monitor:

  Character monitor profile:

  - `"core"` (default): monitors only the essential model parameters

  - `"full"`: monitors all model nodes

- monitor_latent:

  Logical; whether to monitor latent cluster labels (`z`) in outcome
  arms.

- monitor_v:

  Logical; whether to monitor stick-breaking `v` terms for SB outcomes.

## Value

A list of class `"causalmixgpd_causal_bundle"` containing the design
bundle, two outcome bundles, training data, arm indices, and metadata
required for posterior prediction and causal effect summaries.

## Details

The outcome bundles reuse the one-arm DPM plus optional GPD machinery.
The PS block provides a shared adjustment object used by
[`run_mcmc_causal`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_causal.md)
and
[`predict.causalmixgpd_causal_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.causalmixgpd_causal_fit.md).

The causal bundle encodes the two arm-specific predictive laws \\F_0(y
\mid x)\\ and \\F_1(y \mid x)\\. Downstream causal estimands are
functionals of these two distributions: \$\$\mathrm{ATE} = E\\Y(1)\\ -
E\\Y(0)\\, \qquad \mathrm{QTE}(\tau) = Q_1(\tau) - Q_0(\tau).\$\$

When `PS` is enabled, the package estimates a propensity score model
\\e(x) = \Pr(A = 1 \mid X = x)\\ and uses a posterior summary of that
score as an augmented covariate in the arm-specific outcome models. This
mirrors the workflow described in the manuscript vignette.

## See also

[`bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md),
[`run_mcmc_causal`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_causal.md),
[`predict.causalmixgpd_causal_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.causalmixgpd_causal_fit.md),
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md),
[`cqte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md).

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
