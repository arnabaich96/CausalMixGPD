# DPmixGPD: Model specification and posterior computation

DPmixGPD is built around a practical modeling principle: the
distributional structure that fits the *bulk* (where most observations
live) is often a poor description of the *tail* (where rare extremes
live). The package therefore models the bulk using a Dirichlet process
mixture (DPM) and, optionally, models the upper tail using a generalized
Pareto distribution (GPD), spliced in a way that yields a single proper
density on the full support.

This vignette defines the full statistical model and the posterior
computation used throughout the package. The workflow vignettes
(unconditional, conditional, causal) use this specification but avoid
restating it.

## Notation

Let $`y_1,\dots,y_N`$ be observed outcomes. When covariates are present,
let $`\boldsymbol{x}_i \in \mathbb{R}^p`$ denote the $`i`$-th row of a
design matrix $`X`$. Mixture components are indexed by $`j`$ (not
$`h`$). The Dirichlet process concentration parameter is $`\kappa>0`$.

Write the bulk density and CDF as
$`f_{\mathrm{DPM}}(\cdot\mid\boldsymbol{x})`$ and
$`F_{\mathrm{DPM}}(\cdot\mid\boldsymbol{x})`$. Write the GPD density and
CDF as
$`f_{\mathrm{GPD}}(\cdot\mid u(\boldsymbol{x}),\sigma(\boldsymbol{x}),\xi)`$
and
$`F_{\mathrm{GPD}}(\cdot\mid u(\boldsymbol{x}),\sigma(\boldsymbol{x}),\xi)`$.

## Bulk model: Dirichlet process mixture

### Hierarchical specification

The bulk model is a kernel mixture with a Dirichlet process prior on the
mixing distribution ([Ferguson 1973](#ref-ferguson1973)). In allocation
form,
``` math
  y_i \mid z_i,\{\Theta_j\}\;\sim\;k\!\left(y_i\mid\Theta_{z_i},\boldsymbol{x}_i\right),
  \qquad
  z_i\mid\boldsymbol{w}\;\sim\;\mathrm{Categorical}(w_1,w_2,\dots),
```
``` math
  \Theta_j\mid H\;\overset{\mathrm{iid}}{\sim}\;H,\qquad
  H\;\sim\;\mathrm{DP}(\kappa,H_0),
```
where $`k(\cdot\mid\Theta_j,\boldsymbol{x})`$ is the chosen kernel
family (Normal, Gamma, Lognormal, Laplace, Inverse Gaussian, Amoroso,
etc.), $`H_0`$ is a base measure for the kernel parameters, and
$`\kappa`$ controls the expected number of occupied clusters.

Marginally this implies an (infinite) mixture representation \[ f\_{}(y)

= \_{j=1}^{} w_j,k!(y_j,). \]

### Stick-breaking representation and truncation

DPmixGPD provides a stick-breaking backend (`backend="sb"`) based on
Sethuraman’s construction ([Sethuraman 1994](#ref-sethuraman1994)). The
weights are
``` math
  w_j = V_j\prod_{\ell<j}(1-V_\ell),
  \qquad
  V_j\overset{\mathrm{iid}}{\sim}\mathrm{Beta}(1,\kappa),
  \qquad
  \Theta_j\overset{\mathrm{iid}}{\sim}H_0.
```

In computation, the package uses a finite truncation level $`J`$
(`components`) and works with
``` math
  f_{\mathrm{DPM}}(y\mid\boldsymbol{x})\;\approx\;\sum_{j=1}^{J} w_j\,k\!\left(y\mid\Theta_j,\boldsymbol{x}\right),
```
where the final weight absorbs leftover mass. This is the standard
blocked-Gibbs approximation for stick-breaking priors ([Ishwaran and
James 2001](#ref-ishwaran2001)).

Define counts
``` math
  n_j = \sum_{i=1}^N\mathbb{I}(z_i=j),\qquad
  m_j = \sum_{i=1}^N\mathbb{I}(z_i>j).
```
Then the stick variables have the conjugate full conditional
``` math
  V_j\mid\boldsymbol{z},\kappa\;\sim\;\mathrm{Beta}(1+n_j,\;\kappa+m_j),
```
and weights are reconstructed as above.

### CRP backend (partition-based view)

DPmixGPD also offers a Chinese restaurant process backend
(`backend="crp"`), which integrates out $`H`$ and updates allocations
under the induced random partition distribution ([Blackwell and MacQueen
1973](#ref-blackwell1973); [Neal 2000](#ref-neal2000)). In this view,
observation $`i`$ joins an existing cluster with probability
proportional to its current size, and starts a new cluster with
probability proportional to $`\kappa`$, coupled with the base measure
$`H_0`$.

## Tail model: generalized Pareto distribution

Let $`u(\boldsymbol{x})`$ be a (possibly covariate-dependent) threshold.
For $`y>u(\boldsymbol{x})`$, define exceedance
$`e=y-u(\boldsymbol{x})`$. The GPD model is
``` math
  Y\mid Y>u(\boldsymbol{x}),\boldsymbol{x}\;\sim\;\mathrm{GPD}\big(u(\boldsymbol{x}),\sigma(\boldsymbol{x}),\xi\big),
```
with scale $`\sigma(\boldsymbol{x})>0`$ and shape $`\xi\in\mathbb{R}`$.
The density is
``` math
f_{\mathrm{GPD}}\big(y\mid u(\boldsymbol{x}),\sigma(\boldsymbol{x}),\xi\big)
=
\begin{cases}
\dfrac{1}{\sigma(\boldsymbol{x})}\left(1+\xi\dfrac{y-u(\boldsymbol{x})}{\sigma(\boldsymbol{x})}\right)^{-1/\xi-1}, & \xi\neq 0,\\[10pt]
\dfrac{1}{\sigma(\boldsymbol{x})}\exp\!\left(-\dfrac{y-u(\boldsymbol{x})}{\sigma(\boldsymbol{x})}\right), & \xi=0,
\end{cases}
```
with support constraint
$`1+\xi\{y-u(\boldsymbol{x})\}/\sigma(\boldsymbol{x})>0`$. The CDF is
``` math
F_{\mathrm{GPD}}\big(y\mid u(\boldsymbol{x}),\sigma(\boldsymbol{x}),\xi\big)
=
\begin{cases}
1-\left(1+\xi\dfrac{y-u(\boldsymbol{x})}{\sigma(\boldsymbol{x})}\right)^{-1/\xi}, & \xi\neq 0,\\[10pt]
1-\exp\!\left(-\dfrac{y-u(\boldsymbol{x})}{\sigma(\boldsymbol{x})}\right), & \xi=0.
\end{cases}
```

The use of the GPD for exceedances is justified by classical
peaks-over-threshold limit theory ([Balkema and Haan
1974](#ref-balkema1974); [Pickands 1975](#ref-pickands1975); [Coles
2001](#ref-coles2001)).

## The spliced bulk–tail model

DPmixGPD defines a single proper density by splicing the bulk DPM and
the GPD tail at $`u(\boldsymbol{x})`$:
``` math
f(y\mid\boldsymbol{x})
=
\begin{cases}
f_{\mathrm{DPM}}(y\mid\boldsymbol{x}), & y\le u(\boldsymbol{x}),\\[6pt]
\Big(1-F_{\mathrm{DPM}}\big(u(\boldsymbol{x})\mid\boldsymbol{x}\big)\Big)\,f_{\mathrm{GPD}}\big(y\mid u(\boldsymbol{x}),\sigma(\boldsymbol{x}),\xi\big), & y>u(\boldsymbol{x}).
\end{cases}
```
The multiplicative factor
$`1-F_{\mathrm{DPM}}\{u(\boldsymbol{x})\mid\boldsymbol{x}\}`$ ensures
that the total tail probability mass matches the bulk survival beyond
the threshold.

Equivalently, the spliced CDF is
``` math
F(y\mid\boldsymbol{x})
=
\begin{cases}
F_{\mathrm{DPM}}(y\mid\boldsymbol{x}), & y\le u(\boldsymbol{x}),\\[6pt]
F_{\mathrm{DPM}}\big(u(\boldsymbol{x})\mid\boldsymbol{x}\big)
+\Big(1-F_{\mathrm{DPM}}\big(u(\boldsymbol{x})\mid\boldsymbol{x}\big)\Big)
\,F_{\mathrm{GPD}}\big(y\mid u(\boldsymbol{x}),\sigma(\boldsymbol{x}),\xi\big), & y>u(\boldsymbol{x}).
\end{cases}
```

This section is the reference point for all later predictive quantities
(density, survival, quantiles, exceedance probabilities).

## Covariate dependence: general link mode

In conditional models, DPmixGPD allows (selected) parameters to depend
on $`\boldsymbol{x}`$ via a link representation. For a generic parameter
$`\theta_j(\boldsymbol{x})`$ in link mode,
``` math
  g\!\left(\theta_j(\boldsymbol{x})\right) = \eta_j(\boldsymbol{x}) = \boldsymbol{x}^\top\boldsymbol{\beta}_j,
```
where $`g(\cdot)`$ is a link function and $`\boldsymbol{\beta}_j`$ is a
component-specific regression coefficient. The package exposes this
mechanism through `param_specs`, and it is not restricted to a single
parameter: in principle, any parameter registered as linkable for a
kernel can be placed in link mode.

The same idea applies to the tail module when a tail parameter is
allowed to vary with $`\boldsymbol{x}`$ (for example, a
covariate-dependent threshold or scale).

## Likelihood and posterior computation

### Likelihood

Under the spliced model, each observation contributes either a bulk
likelihood term or a tail likelihood term, depending on whether it falls
below or above the threshold. For a fixed threshold $`u`$ (no
covariates), the full likelihood is
``` math
  \mathcal{L}(\Theta,\boldsymbol{w},\sigma,\xi\mid\boldsymbol{y})
  =
  \prod_{i: y_i\le u} f_{\mathrm{DPM}}(y_i)
  \times
  \prod_{i: y_i>u} \Big(1-F_{\mathrm{DPM}}(u)\Big)\,f_{\mathrm{GPD}}(y_i\mid u,\sigma,\xi).
```
With covariates, replace $`u`$ by $`u(\boldsymbol{x}_i)`$ and use
$`f_{\mathrm{DPM}}(\cdot\mid\boldsymbol{x}_i)`$ and
$`F_{\mathrm{DPM}}(\cdot\mid\boldsymbol{x}_i)`$.

### Posterior sampling (high-level algorithm)

DPmixGPD targets the joint posterior of mixture allocations, kernel
parameters, weights, and (optionally) tail parameters.

For the stick-breaking backend, a typical blocked-Gibbs iteration
updates:

1.  Allocation indicators $`z_i`$ via categorical probabilities
    proportional to $`w_j\,k(y_i\mid\Theta_j,\boldsymbol{x}_i)`$, with
    splicing handled by the tail module when
    $`y_i>u(\boldsymbol{x}_i)`$.
2.  Stick variables $`V_j`$ (and hence $`w_j`$) from their conjugate
    beta full conditionals.
3.  Kernel parameters $`\Theta_j`$ from their full conditional
    distributions. When conjugacy is unavailable, the package uses
    Metropolis–Hastings steps inside the Gibbs loop.
4.  Tail parameters $`(u(\cdot),\sigma(\cdot),\xi)`$ according to the
    user-specified `param_specs`. Fixed parameters are held constant;
    prior-based parameters are updated by conjugate or Metropolis steps
    depending on the chosen prior family.
5.  The concentration parameter $`\kappa`$ if it is assigned a prior
    (typically a gamma prior), again via conjugate or Metropolis
    updates.

The CRP backend updates allocations under the partition distribution and
then updates cluster parameters accordingly.

### Posterior predictive quantities

Posterior prediction is implemented through the S3 method
[`predict()`](https://rdrr.io/r/stats/predict.html) for `mixgpd_fit`
objects. For any functional $`g(\theta)`$ of the model parameters,
prediction follows the standard Bayesian recipe:
``` math
  p(\tilde{y}\mid\boldsymbol{x},\mathcal{D}) = \int p(\tilde{y}\mid\boldsymbol{x},\theta)\,p(\theta\mid\mathcal{D})\,d\theta,
```
approximated by Monte Carlo over posterior draws $`\theta^{(s)}`$.

In particular,

- density:
  $`f(\tilde{y}\mid\boldsymbol{x},\mathcal{D}) \approx S^{-1}\sum_{s=1}^S f(\tilde{y}\mid\boldsymbol{x},\theta^{(s)})`$,
- survival:
  $`S(\tilde{y}\mid\boldsymbol{x},\mathcal{D}) = 1-F(\tilde{y}\mid\boldsymbol{x},\mathcal{D})`$,
- quantiles: $`Q(p\mid\boldsymbol{x},\mathcal{D})`$ obtained by
  inverting the posterior predictive CDF draw-by-draw and summarizing
  across draws.

## Built-in example datasets

The source package ships several small datasets intended for examples
and testing. Each dataset is an R object in `data/` and can be loaded
with `data("<name>")`. All of them are lists with a standard structure:

- `y`: numeric outcome,
- `X`: covariates (either `NULL` for unconditional or a `data.frame` for
  conditional),
- `T`: treatment indicator (only for causal datasets),
- `meta`: metadata (sample size, support, number of true components,
  whether a tail was injected),
- `truth`: simulation truth (kernel family and parameters used to
  generate the dataset).

The workflow vignettes use these datasets to avoid generating new data.

    List of 7
     $ n          : num 200
     $ support    : chr "positive"
     $ p          : int 0
     $ K_true     : num 4
     $ tail       : logi TRUE
     $ exceed_frac: num 0.12
     $ seed       : num 10103
    List of 5
     $ kernel     : chr "lognormal"
     $ weights    : num [1:4] 0.3 0.25 0.25 0.2
     $ params     :List of 2
      ..$ meanlog: num [1:4] -0.2 0.2 0.6 1
      ..$ sdlog  : num [1:4] 0.4 0.35 0.45 0.5
     $ threshold  : num 3.04
     $ tail_params:List of 2
      ..$ scale: num 2.5
      ..$ shape: num 0.2

Balkema, August A., and Laurens de Haan. 1974. “Residual Life Time at
Great Age.” *The Annals of Probability* 2 (5): 792–804.
<https://doi.org/10.1214/aop/1176996548>.

Blackwell, David, and James B. MacQueen. 1973. “Ferguson Distributions
via pólya Urn Schemes.” *The Annals of Statistics* 1 (2): 353–55.
<https://doi.org/10.1214/aos/1176342372>.

Coles, Stuart. 2001. *An Introduction to Statistical Modeling of Extreme
Values*. Springer. <https://doi.org/10.1007/978-1-4471-3675-0>.

Ferguson, Thomas S. 1973. “A Bayesian Analysis of Some Nonparametric
Problems.” *The Annals of Statistics* 1 (2): 209–30.
<https://doi.org/10.1214/aos/1176342360>.

Ishwaran, Hemant, and Lancelot F. James. 2001. “Gibbs Sampling Methods
for Stick-Breaking Priors.” *Journal of the American Statistical
Association* 96 (453): 161–73.
<https://doi.org/10.1198/016214501750332758>.

Neal, Radford M. 2000. “Markov Chain Sampling Methods for Dirichlet
Process Mixture Models.” *Journal of Computational and Graphical
Statistics* 9 (2): 249–65.
<https://doi.org/10.1080/10618600.2000.10474879>.

Pickands, James. 1975. “Statistical Inference Using Extreme Order
Statistics.” *The Annals of Statistics* 3 (1): 119–31.
<https://doi.org/10.1214/aos/1176343003>.

Sethuraman, Jayaram. 1994. “A Constructive Definition of Dirichlet
Priors.” *Statistica Sinica* 4 (2): 639–50.
