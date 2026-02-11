# DPmixGPD: Model specification and posterior computation

``` r

library(DPmixGPD)
set.seed(1)
```

DPmixGPD is built around a practical modeling principle: the *bulk*
(where most observations live) and the *tail* (where extreme
observations live) often require different statistical structure. The
package therefore models the bulk using a **Dirichlet process mixture**
(DPM) and (optionally) models the upper tail using a **Generalized
Pareto distribution** (GPD), spliced in a way that yields a single
proper density over the full support.

This vignette is the *only* vignette that develops the full mathematical
model and posterior computation. The other vignettes (unconditional,
conditional, causal) should refer back here for model definitions,
likelihoods, and sampling steps.

## Notation

Let $`y_1,\dots,y_N`$ denote observed outcomes. When covariates are
present, let $`\boldsymbol{x}_i \in \mathbb{R}^p`$ denote the $`i`$-th
row of the design matrix $`X`$. The mixture component index is always
$`j`$. The DPM concentration parameter is written $`\kappa>0`$ (called
`alpha` internally in some code paths).

Write the bulk density and CDF as $`f_{\mathrm{DPM}}(\cdot)`$ and
$`F_{\mathrm{DPM}}(\cdot)`$. The GPD tail density and CDF are
$`f_{\mathrm{GPD}}(\cdot)`$ and $`F_{\mathrm{GPD}}(\cdot)`$. The
splicing threshold may be constant $`u`$ or covariate-dependent
$`u(\boldsymbol{x})`$.

## Bulk model: Dirichlet process mixture

### Hierarchical specification

The DPM bulk model can be written in allocation form:
``` math
y_i \mid z_i,\{\Theta_j\} \;\sim\; k(y_i \mid \Theta_{z_i}),
\qquad
z_i \mid \boldsymbol{w} \;\sim\; \mathrm{Categorical}(w_1,w_2,\dots),
```
``` math
\Theta_j \mid H \;\overset{\mathrm{iid}}{\sim}\; H,
\qquad
H \;\sim\; \mathrm{DP}(\kappa, H_0),
```
where $`k(\cdot\mid \Theta_j)`$ is the chosen kernel family (Normal,
Gamma, Lognormal, Amoroso, etc.), $`H_0`$ is the base measure for the
kernel parameters, and $`\kappa`$ controls clustering (smaller
$`\kappa`$ yields fewer active components; larger $`\kappa`$ yields
more) ([Ferguson 1973](#ref-ferguson1973)).

The induced marginal bulk density is an (infinite) mixture:
``` math
f_{\mathrm{DPM}}(y)
\;=\;
\sum_{j=1}^{\infty} w_j \, k(y \mid \Theta_j).
```

### Stick-breaking representation and truncation

DPmixGPD supports two computational backends: stick-breaking
(`backend="sb"`) and CRP (`backend="crp"`). Under stick-breaking,
Sethuramanâ€™s constructive form is ([Sethuraman
1994](#ref-sethuraman1994))
``` math
w_j = V_j \prod_{\ell<j}(1-V_\ell), 
\qquad
V_j \overset{\mathrm{iid}}{\sim} \mathrm{Beta}(1,\kappa),
\qquad
\Theta_j \overset{\mathrm{iid}}{\sim} H_0.
```

In computation, DPmixGPD uses a finite truncation level $`J`$
(`components`) so that
``` math
f_{\mathrm{DPM}}(y)
\;\approx\;
\sum_{j=1}^{J} w_j \, k(y \mid \Theta_j),
```
with the final weight absorbing leftover mass. This is the standard
blocked-Gibbs approximation for stick-breaking priors ([Ishwaran and
James 2001](#ref-ishwaran2001)).

### Allocation counts and a key conjugate update

Define allocation counts
``` math
n_j = \sum_{i=1}^N \mathbb{I}(z_i=j),
\qquad
m_j = \sum_{i=1}^N \mathbb{I}(z_i>j).
```
Then the stick-breaking variables have the conjugate full conditional
``` math
V_j \mid \boldsymbol{z},\kappa \;\sim\; \mathrm{Beta}(1+n_j,\;\kappa+m_j),
```
and weights are reconstructed by
``` math
w_j = V_j \prod_{\ell<j}(1-V_\ell).
```

This update is one reason blocked Gibbs is convenient: it yields
tractable conditional distributions for the weights under truncation.

### CRP backend (partition-based view)

The Chinese restaurant process (CRP) view integrates out the random
measure $`H`$ and samples allocations via the induced partition
distribution ([**blackwell1973?**](#ref-blackwell1973); [Neal
2000](#ref-neal2000)). In this view the probability that observation
$`i`$ joins an existing cluster is proportional to its current size, and
the probability of creating a new cluster is proportional to $`\kappa`$,
coupled with the base measure $`H_0`$.

DPmixGPDâ€™s CRP backend uses a finite representation controlled by
`components`, but conceptually it corresponds to the same DP prior
viewed through its partition structure.

## Tail model: Generalized Pareto distribution (GPD)

Let the threshold be $`u(\boldsymbol{x})`$ (or constant $`u`$). For
$`y>u(\boldsymbol{x})`$, define the exceedance
$`e = y-u(\boldsymbol{x})`$. The GPD model for exceedances is
``` math
Y \mid Y>u(\boldsymbol{x}),\boldsymbol{x}
\;\sim\;
\mathrm{GPD}\big(u(\boldsymbol{x}),\sigma(\boldsymbol{x}),\xi\big),
```
with scale $`\sigma(\boldsymbol{x})>0`$ and shape $`\xi\in\mathbb{R}`$.
The density is
``` math
f_{\mathrm{GPD}}(y \mid u(\boldsymbol{x}),\sigma(\boldsymbol{x}),\xi)
=
\begin{cases}
\dfrac{1}{\sigma(\boldsymbol{x})}
\left(1+\xi\dfrac{y-u(\boldsymbol{x})}{\sigma(\boldsymbol{x})}\right)^{-1/\xi-1},
& \xi\neq 0,\\[10pt]
\dfrac{1}{\sigma(\boldsymbol{x})}\exp\!\left(-\dfrac{y-u(\boldsymbol{x})}{\sigma(\boldsymbol{x})}\right),
& \xi = 0,
\end{cases}
```
with support constraint
$`1+\xi(y-u(\boldsymbol{x}))/\sigma(\boldsymbol{x})>0`$.

The CDF is
``` math
F_{\mathrm{GPD}}(y \mid u(\boldsymbol{x}),\sigma(\boldsymbol{x}),\xi)
=
\begin{cases}
1-\left(1+\xi\dfrac{y-u(\boldsymbol{x})}{\sigma(\boldsymbol{x})}\right)^{-1/\xi},
& \xi\neq 0,\\[10pt]
1-\exp\!\left(-\dfrac{y-u(\boldsymbol{x})}{\sigma(\boldsymbol{x})}\right),
& \xi = 0.
\end{cases}
```

The use of the GPD for exceedances is justified by the classical
peaks-over-threshold limit theory ([Balkema and Haan
1974](#ref-balkema1974); [Pickands 1975](#ref-pickands1975); [Coles
2001](#ref-coles2001)).

## The spliced bulkâ€“tail model (defined once)

DPmixGPD splices the bulk DPM and the GPD tail at $`u(\boldsymbol{x})`$
by defining the full conditional density
``` math
f(y \mid \boldsymbol{x})
=
\begin{cases}
f_{\mathrm{DPM}}(y \mid \boldsymbol{x}),
& y \le u(\boldsymbol{x}),\\[6pt]
\Big(1 - F_{\mathrm{DPM}}(u(\boldsymbol{x}) \mid \boldsymbol{x})\Big)\;
f_{\mathrm{GPD}}(y \mid u(\boldsymbol{x}),\sigma(\boldsymbol{x}),\xi),
& y > u(\boldsymbol{x}).
\end{cases}
```
The multiplicative factor
``` math
S_{\mathrm{DPM}}(u(\boldsymbol{x}) \mid \boldsymbol{x})
=
1 - F_{\mathrm{DPM}}(u(\boldsymbol{x}) \mid \boldsymbol{x})
```
ensures the model is a proper density on the full support and that the
tail probability mass matches the bulk survival beyond the threshold.

Equivalently, the spliced CDF is
``` math
F(y \mid \boldsymbol{x})
=
\begin{cases}
F_{\mathrm{DPM}}(y \mid \boldsymbol{x}),
& y \le u(\boldsymbol{x}),\\[6pt]
F_{\mathrm{DPM}}(u(\boldsymbol{x}) \mid \boldsymbol{x})
+
\Big(1 - F_{\mathrm{DPM}}(u(\boldsymbol{x}) \mid \boldsymbol{x})\Big)\;
F_{\mathrm{GPD}}(y \mid u(\boldsymbol{x}),\sigma(\boldsymbol{x}),\xi),
& y > u(\boldsymbol{x}).
\end{cases}
```

All posterior functionals in DPmixGPD (predictive density/CDF,
quantiles, exceedance probabilities) are derived from this single
definition. Later vignettes should reference this section rather than
restating it.

## Covariate dependence and general â€œlink modeâ€?

In conditional models, DPmixGPD allows parameters to be functions of
$`\boldsymbol{x}`$ through a general link mechanism. The design
philosophy is:

- any *bulk* parameter that is listed in the kernel registry can
  typically be set to `mode="fixed"`, `mode="dist"`, or `mode="link"`;
- tail parameters (threshold, tail scale, tail shape) can be `fixed` or
  `dist`, and selected parameters can also be `link` depending on the
  tail module.

Inspect support through the registry helpers:

``` r
DPmixGPD::kernel_support_table()
             kernel gpd covariates sb crp
normal       normal   ✔          ✔  ✔   ✔
lognormal lognormal   ✔          ✔  ✔   ✔
invgauss   invgauss   ✔          ✔  ✔   ✔
gamma         gamma   ✔          ✔  ✔   ✔
laplace     laplace   ✔          ✔  ✔   ✔
amoroso     amoroso   ✔          ✔  ✔   ✔
cauchy       cauchy  ❌          ✔  ✔   ✔
DPmixGPD::get_kernel_registry()
$normal
$normal$key
[1] "normal"

$normal$bulk_params
[1] "mean" "sd"  

$normal$bulk_support
         mean            sd 
       "real" "positive_sd" 

$normal$param_types
      mean         sd 
"location"       "sd" 

$normal$allow_gpd
[1] TRUE

$normal$defaults_X
$normal$defaults_X$mean
$normal$defaults_X$mean$mode
[1] "link"

$normal$defaults_X$mean$link
[1] "identity"


$normal$defaults_X$sd
$normal$defaults_X$sd$mode
[1] "dist"



$normal$sb
$normal$sb$d
[1] "dNormMix"

$normal$sb$d_gpd
[1] "dNormMixGpd"

$normal$sb$args
[1] "w"    "mean" "sd"  

$normal$sb$args_gpd
[1] "w"          "mean"       "sd"         "threshold"  "tail_scale"
[6] "tail_shape"


$normal$crp
$normal$crp$d_base
[1] "dnorm"

$normal$crp$d_gpd
[1] "dNormGpd"

$normal$crp$args_gpd
[1] "mean"       "sd"         "threshold"  "tail_scale" "tail_shape"


$normal$signatures
$normal$signatures$sb
$normal$signatures$sb$bulk
$normal$signatures$sb$bulk$dist_name
[1] "dNormMix"

$normal$signatures$sb$bulk$args
[1] "w"    "mean" "sd"  


$normal$signatures$sb$gpd
$normal$signatures$sb$gpd$dist_name
[1] "dNormMixGpd"

$normal$signatures$sb$gpd$args
[1] "w"          "mean"       "sd"         "threshold"  "tail_scale"
[6] "tail_shape"



$normal$signatures$crp
$normal$signatures$crp$bulk
$normal$signatures$crp$bulk$dist_name
[1] "dnorm"

$normal$signatures$crp$bulk$args
[1] "mean" "sd"  


$normal$signatures$crp$gpd
$normal$signatures$crp$gpd$dist_name
[1] "dNormGpd"

$normal$signatures$crp$gpd$args
[1] "mean"       "sd"         "threshold"  "tail_scale" "tail_shape"





$lognormal
$lognormal$key
[1] "lognormal"

$lognormal$bulk_params
[1] "meanlog" "sdlog"  

$lognormal$bulk_support
      meanlog         sdlog 
       "real" "positive_sd" 

$lognormal$param_types
   meanlog      sdlog 
"location"       "sd" 

$lognormal$allow_gpd
[1] TRUE

$lognormal$defaults_X
$lognormal$defaults_X$meanlog
$lognormal$defaults_X$meanlog$mode
[1] "link"

$lognormal$defaults_X$meanlog$link
[1] "identity"


$lognormal$defaults_X$sdlog
$lognormal$defaults_X$sdlog$mode
[1] "dist"



$lognormal$sb
$lognormal$sb$d
[1] "dLognormalMix"

$lognormal$sb$d_gpd
[1] "dLognormalMixGpd"

$lognormal$sb$args
[1] "w"       "meanlog" "sdlog"  

$lognormal$sb$args_gpd
[1] "w"          "meanlog"    "sdlog"      "threshold"  "tail_scale"
[6] "tail_shape"


$lognormal$crp
$lognormal$crp$d_base
[1] "dlnorm"

$lognormal$crp$d_gpd
[1] "dLognormalGpd"

$lognormal$crp$args_gpd
[1] "meanlog"    "sdlog"      "threshold"  "tail_scale" "tail_shape"


$lognormal$signatures
$lognormal$signatures$sb
$lognormal$signatures$sb$bulk
$lognormal$signatures$sb$bulk$dist_name
[1] "dLognormalMix"

$lognormal$signatures$sb$bulk$args
[1] "w"       "meanlog" "sdlog"  


$lognormal$signatures$sb$gpd
$lognormal$signatures$sb$gpd$dist_name
[1] "dLognormalMixGpd"

$lognormal$signatures$sb$gpd$args
[1] "w"          "meanlog"    "sdlog"      "threshold"  "tail_scale"
[6] "tail_shape"



$lognormal$signatures$crp
$lognormal$signatures$crp$bulk
$lognormal$signatures$crp$bulk$dist_name
[1] "dlnorm"

$lognormal$signatures$crp$bulk$args
[1] "meanlog" "sdlog"  


$lognormal$signatures$crp$gpd
$lognormal$signatures$crp$gpd$dist_name
[1] "dLognormalGpd"

$lognormal$signatures$crp$gpd$args
[1] "meanlog"    "sdlog"      "threshold"  "tail_scale" "tail_shape"





$invgauss
$invgauss$key
[1] "invgauss"

$invgauss$bulk_params
[1] "mean"  "shape"

$invgauss$bulk_support
               mean               shape 
"positive_location"    "positive_shape" 

$invgauss$param_types
      mean      shape 
"location"    "shape" 

$invgauss$allow_gpd
[1] TRUE

$invgauss$defaults_X
$invgauss$defaults_X$mean
$invgauss$defaults_X$mean$mode
[1] "link"

$invgauss$defaults_X$mean$link
[1] "exp"


$invgauss$defaults_X$shape
$invgauss$defaults_X$shape$mode
[1] "dist"



$invgauss$sb
$invgauss$sb$d
[1] "dInvGaussMix"

$invgauss$sb$d_gpd
[1] "dInvGaussMixGpd"

$invgauss$sb$args
[1] "w"     "mean"  "shape"

$invgauss$sb$args_gpd
[1] "w"          "mean"       "shape"      "threshold"  "tail_scale"
[6] "tail_shape"


$invgauss$crp
$invgauss$crp$d_base
[1] "dInvGauss"

$invgauss$crp$d_gpd
[1] "dInvGaussGpd"

$invgauss$crp$args_gpd
[1] "mean"       "shape"      "threshold"  "tail_scale" "tail_shape"


$invgauss$signatures
$invgauss$signatures$sb
$invgauss$signatures$sb$bulk
$invgauss$signatures$sb$bulk$dist_name
[1] "dInvGaussMix"

$invgauss$signatures$sb$bulk$args
[1] "w"     "mean"  "shape"


$invgauss$signatures$sb$gpd
$invgauss$signatures$sb$gpd$dist_name
[1] "dInvGaussMixGpd"

$invgauss$signatures$sb$gpd$args
[1] "w"          "mean"       "shape"      "threshold"  "tail_scale"
[6] "tail_shape"



$invgauss$signatures$crp
$invgauss$signatures$crp$bulk
$invgauss$signatures$crp$bulk$dist_name
[1] "dInvGauss"

$invgauss$signatures$crp$bulk$args
[1] "mean"  "shape"


$invgauss$signatures$crp$gpd
$invgauss$signatures$crp$gpd$dist_name
[1] "dInvGaussGpd"

$invgauss$signatures$crp$gpd$args
[1] "mean"       "shape"      "threshold"  "tail_scale" "tail_shape"





$gamma
$gamma$key
[1] "gamma"

$gamma$bulk_params
[1] "shape" "scale"

$gamma$bulk_support
           shape            scale 
"positive_shape" "positive_scale" 

$gamma$param_types
  shape   scale 
"shape" "scale" 

$gamma$allow_gpd
[1] TRUE

$gamma$defaults_X
$gamma$defaults_X$shape
$gamma$defaults_X$shape$mode
[1] "dist"


$gamma$defaults_X$scale
$gamma$defaults_X$scale$mode
[1] "link"

$gamma$defaults_X$scale$link
[1] "exp"



$gamma$sb
$gamma$sb$d
[1] "dGammaMix"

$gamma$sb$d_gpd
[1] "dGammaMixGpd"

$gamma$sb$args
[1] "w"     "shape" "scale"

$gamma$sb$args_gpd
[1] "w"          "shape"      "scale"      "threshold"  "tail_scale"
[6] "tail_shape"


$gamma$crp
$gamma$crp$d_base
[1] "dgamma"

$gamma$crp$d_gpd
[1] "dGammaGpd"

$gamma$crp$args_gpd
[1] "shape"      "scale"      "threshold"  "tail_scale" "tail_shape"


$gamma$signatures
$gamma$signatures$sb
$gamma$signatures$sb$bulk
$gamma$signatures$sb$bulk$dist_name
[1] "dGammaMix"

$gamma$signatures$sb$bulk$args
[1] "w"     "shape" "scale"


$gamma$signatures$sb$gpd
$gamma$signatures$sb$gpd$dist_name
[1] "dGammaMixGpd"

$gamma$signatures$sb$gpd$args
[1] "w"          "shape"      "scale"      "threshold"  "tail_scale"
[6] "tail_shape"



$gamma$signatures$crp
$gamma$signatures$crp$bulk
$gamma$signatures$crp$bulk$dist_name
[1] "dgamma"

$gamma$signatures$crp$bulk$args
[1] "shape" "scale"


$gamma$signatures$crp$gpd
$gamma$signatures$crp$gpd$dist_name
[1] "dGammaGpd"

$gamma$signatures$crp$gpd$args
[1] "shape"      "scale"      "threshold"  "tail_scale" "tail_shape"





$laplace
$laplace$key
[1] "laplace"

$laplace$bulk_params
[1] "location" "scale"   

$laplace$bulk_support
        location            scale 
          "real" "positive_scale" 

$laplace$param_types
  location      scale 
"location"    "scale" 

$laplace$allow_gpd
[1] TRUE

$laplace$defaults_X
$laplace$defaults_X$location
$laplace$defaults_X$location$mode
[1] "link"

$laplace$defaults_X$location$link
[1] "identity"


$laplace$defaults_X$scale
$laplace$defaults_X$scale$mode
[1] "dist"



$laplace$sb
$laplace$sb$d
[1] "dLaplaceMix"

$laplace$sb$d_gpd
[1] "dLaplaceMixGpd"

$laplace$sb$args
[1] "w"        "location" "scale"   

$laplace$sb$args_gpd
[1] "w"          "location"   "scale"      "threshold"  "tail_scale"
[6] "tail_shape"


$laplace$crp
$laplace$crp$d_base
[1] "ddexp"

$laplace$crp$d_gpd
[1] NA

$laplace$crp$args_gpd
[1] NA


$laplace$signatures
$laplace$signatures$sb
$laplace$signatures$sb$bulk
$laplace$signatures$sb$bulk$dist_name
[1] "dLaplaceMix"

$laplace$signatures$sb$bulk$args
[1] "w"        "location" "scale"   


$laplace$signatures$sb$gpd
$laplace$signatures$sb$gpd$dist_name
[1] "dLaplaceMixGpd"

$laplace$signatures$sb$gpd$args
[1] "w"          "location"   "scale"      "threshold"  "tail_scale"
[6] "tail_shape"



$laplace$signatures$crp
$laplace$signatures$crp$bulk
$laplace$signatures$crp$bulk$dist_name
[1] "ddexp"

$laplace$signatures$crp$bulk$args
[1] "location" "scale"   


$laplace$signatures$crp$gpd
NULL




$amoroso
$amoroso$key
[1] "amoroso"

$amoroso$bulk_params
[1] "loc"    "scale"  "shape1" "shape2"

$amoroso$bulk_support
             loc            scale           shape1           shape2 
          "real" "positive_scale" "positive_shape" "positive_shape" 

$amoroso$param_types
       loc      scale     shape1     shape2 
"location"    "scale"    "shape"    "shape" 

$amoroso$allow_gpd
[1] TRUE

$amoroso$defaults_X
$amoroso$defaults_X$loc
$amoroso$defaults_X$loc$mode
[1] "link"

$amoroso$defaults_X$loc$link
[1] "identity"


$amoroso$defaults_X$scale
$amoroso$defaults_X$scale$mode
[1] "link"

$amoroso$defaults_X$scale$link
[1] "exp"


$amoroso$defaults_X$shape1
$amoroso$defaults_X$shape1$mode
[1] "fixed"

$amoroso$defaults_X$shape1$value
[1] 1


$amoroso$defaults_X$shape2
$amoroso$defaults_X$shape2$mode
[1] "dist"



$amoroso$sb
$amoroso$sb$d
[1] "dAmorosoMix"

$amoroso$sb$d_gpd
[1] "dAmorosoMixGpd"

$amoroso$sb$args
[1] "w"      "loc"    "scale"  "shape1" "shape2"

$amoroso$sb$args_gpd
[1] "w"          "loc"        "scale"      "shape1"     "shape2"    
[6] "threshold"  "tail_scale" "tail_shape"


$amoroso$crp
$amoroso$crp$d_base
[1] "dAmoroso"

$amoroso$crp$d_gpd
[1] "dAmorosoGpd"

$amoroso$crp$args_gpd
[1] "loc"        "scale"      "shape1"     "shape2"     "threshold" 
[6] "tail_scale" "tail_shape"


$amoroso$signatures
$amoroso$signatures$sb
$amoroso$signatures$sb$bulk
$amoroso$signatures$sb$bulk$dist_name
[1] "dAmorosoMix"

$amoroso$signatures$sb$bulk$args
[1] "w"      "loc"    "scale"  "shape1" "shape2"


$amoroso$signatures$sb$gpd
$amoroso$signatures$sb$gpd$dist_name
[1] "dAmorosoMixGpd"

$amoroso$signatures$sb$gpd$args
[1] "w"          "loc"        "scale"      "shape1"     "shape2"    
[6] "threshold"  "tail_scale" "tail_shape"



$amoroso$signatures$crp
$amoroso$signatures$crp$bulk
$amoroso$signatures$crp$bulk$dist_name
[1] "dAmoroso"

$amoroso$signatures$crp$bulk$args
[1] "loc"    "scale"  "shape1" "shape2"


$amoroso$signatures$crp$gpd
$amoroso$signatures$crp$gpd$dist_name
[1] "dAmorosoGpd"

$amoroso$signatures$crp$gpd$args
[1] "loc"        "scale"      "shape1"     "shape2"     "threshold" 
[6] "tail_scale" "tail_shape"





$cauchy
$cauchy$key
[1] "cauchy"

$cauchy$bulk_params
[1] "location" "scale"   

$cauchy$bulk_support
        location            scale 
          "real" "positive_scale" 

$cauchy$param_types
  location      scale 
"location"    "scale" 

$cauchy$allow_gpd
[1] FALSE

$cauchy$defaults_X
$cauchy$defaults_X$location
$cauchy$defaults_X$location$mode
[1] "link"

$cauchy$defaults_X$location$link
[1] "identity"


$cauchy$defaults_X$scale
$cauchy$defaults_X$scale$mode
[1] "dist"



$cauchy$sb
$cauchy$sb$d
[1] "dCauchyMix"

$cauchy$sb$d_gpd
[1] NA

$cauchy$sb$args
[1] "w"        "location" "scale"   

$cauchy$sb$args_gpd
[1] NA


$cauchy$crp
$cauchy$crp$d_base
[1] "dCauchy"

$cauchy$crp$d_gpd
[1] NA

$cauchy$crp$args_gpd
[1] NA


$cauchy$signatures
$cauchy$signatures$sb
$cauchy$signatures$sb$bulk
$cauchy$signatures$sb$bulk$dist_name
[1] "dCauchyMix"

$cauchy$signatures$sb$bulk$args
[1] "w"        "location" "scale"   


$cauchy$signatures$sb$gpd
NULL


$cauchy$signatures$crp
$cauchy$signatures$crp$bulk
$cauchy$signatures$crp$bulk$dist_name
[1] "dCauchy"

$cauchy$signatures$crp$bulk$args
[1] "location" "scale"   


$cauchy$signatures$crp$gpd
NULL
DPmixGPD::get_tail_registry()
$params
[1] "threshold"  "tail_scale" "tail_shape"

$support
       threshold       tail_scale       tail_shape 
          "real" "positive_scale"           "real" 

$indexed_by_cluster_in_crp
[1] FALSE
```

Conceptually, when a parameter $`\theta`$ is in link mode, DPmixGPD
represents it (component-wise) as
``` math
\theta_{j}(\boldsymbol{x})
=
g\!\left(\boldsymbol{r}(\boldsymbol{x})^\top \boldsymbol{\beta}_{\theta,j}\right),
```
where $`g(\cdot)`$ is a link (identity, exp, softplus, etc.) and
$`\boldsymbol{r}(\boldsymbol{x})`$ is the design vector. This is a
general feature: it is not restricted to a single parameter or a special
kernel case. Whenever you set `mode="link"` for a supported parameter in
`param_specs`, the model builder uses this mechanism.

## Posterior computation: what is being sampled?

DPmixGPD constructs a NIMBLE model and runs MCMC using NIMBLEâ€™s
samplers. In broad terms, the posterior sampler alternates updates for:

- allocations $`\{z_i\}`$,
- component parameters $`\{\Theta_j\}`$ (including regression
  coefficients under link mode),
- mixture weights (SB) or partitions (CRP),
- concentration $`\kappa`$,
- and (when enabled) tail parameters $`\{u(\cdot),\sigma(\cdot),\xi\}`$.

### Allocation update

In the bulk-only case, allocations are sampled from
``` math
\Pr(z_i=j \mid \cdot)\;\propto\; w_j \, k(y_i \mid \Theta_j(\boldsymbol{x}_i)).
```
When `GPD=TRUE`, the likelihood contribution depends on whether
$`y_i \le u(\boldsymbol{x}_i)`$ or $`y_i > u(\boldsymbol{x}_i)`$,
because exceedances contribute via the spliced tail likelihood:
``` math
\Big(1-F_{\mathrm{DPM}}(u(\boldsymbol{x}_i)\mid \boldsymbol{x}_i)\Big)\;
f_{\mathrm{GPD}}(y_i\mid u(\boldsymbol{x}_i),\sigma(\boldsymbol{x}_i),\xi).
```

### Component parameter update

Given allocations, each $`\Theta_j`$ is updated from its conditional
posterior
``` math
p(\Theta_j \mid \{y_i: z_i=j\},\; H_0),
```
which may be conjugate for some kernel parameterizations or may require
Metropolis updates. Under link mode, $`\Theta_j(\boldsymbol{x})`$
involves regression coefficients $`\boldsymbol{\beta}_{\theta,j}`$,
which are sampled jointly with other component parameters.

### Stick-breaking weight update (SB backend)

For SB, the blocked Gibbs update uses:
``` math
V_j \mid \boldsymbol{z},\kappa \sim \mathrm{Beta}(1+n_j,\kappa+m_j),
\qquad
w_j = V_j \prod_{\ell<j}(1-V_\ell).
```

### Concentration update

DPmixGPD can treat $`\kappa`$ as fixed or random (via
`param_specs$concentration`). When random, $`\kappa`$ is updated using
its prior and the current allocations, typically via a Metropolis or
auxiliary-variable update depending on configuration.

### Tail parameter update

When `GPD=TRUE`, tail parameters are updated using the full spliced
likelihood. The key modeling point is that the normalization term
depends on the bulk CDF at the threshold:
``` math
S_{\mathrm{DPM}}(u(\boldsymbol{x})\mid \boldsymbol{x})
=
1-F_{\mathrm{DPM}}(u(\boldsymbol{x})\mid \boldsymbol{x}),
```
so tail inference is coupled to bulk inference rather than being a
separate two-step fit.

## How this maps to the DPmixGPD API

### Build a bundle (model code + data + priors + monitors)

The model is created by
[`build_nimble_bundle()`](https://arnabaich96.github.io/DPmixGPD/pkgdown/reference/build_nimble_bundle.md).
At minimum you specify: outcome `y`, a kernel, a backend, and whether
`GPD=TRUE/FALSE`. Optional arguments include `X` for conditional models,
`components` for SB truncation, and `param_specs` to override default
priors and link configurations.

``` r
y <- abs(rnorm(120)) + 0.1

bundle <- DPmixGPD::build_nimble_bundle(
  y = y,
  X = NULL,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = list(niter = 800, nburnin = 200, thin = 2, nchains = 1, seed = 1)
)

print(bundle)
DPmixGPD bundle
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> Field </th>
   <th style="text-align:center;"> Value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> Backend </td>
   <td style="text-align:center;"> Stick-Breaking Process </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Kernel </td>
   <td style="text-align:center;"> Normal Distribution </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Components </td>
   <td style="text-align:center;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> N </td>
   <td style="text-align:center;"> 120 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> NO </td>
  </tr>
  <tr>
   <td style="text-align:center;"> GPD </td>
   <td style="text-align:center;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Epsilon </td>
   <td style="text-align:center;"> 0.025 </td>
  </tr>
</tbody>
</table>
  contains  : code, constants, data, dimensions, inits, monitors
summary(bundle)
DPmixGPD bundle summary
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> Field </th>
   <th style="text-align:center;"> Value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> Backend </td>
   <td style="text-align:center;"> Stick-Breaking Process </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Kernel </td>
   <td style="text-align:center;"> Normal Distribution </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Components </td>
   <td style="text-align:center;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> N </td>
   <td style="text-align:center;"> 120 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> X </td>
   <td style="text-align:center;"> NO </td>
  </tr>
  <tr>
   <td style="text-align:center;"> GPD </td>
   <td style="text-align:center;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Epsilon </td>
   <td style="text-align:center;"> 0.025 </td>
  </tr>
</tbody>
</table>
Parameter specification
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> block </th>
   <th style="text-align:center;"> parameter </th>
   <th style="text-align:center;"> mode </th>
   <th style="text-align:center;"> level </th>
   <th style="text-align:center;"> prior </th>
   <th style="text-align:center;"> link </th>
   <th style="text-align:center;"> notes </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> meta </td>
   <td style="text-align:center;"> backend </td>
   <td style="text-align:center;"> info </td>
   <td style="text-align:center;"> model </td>
   <td style="text-align:center;"> sb </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:center;"> meta </td>
   <td style="text-align:center;"> kernel </td>
   <td style="text-align:center;"> info </td>
   <td style="text-align:center;"> model </td>
   <td style="text-align:center;"> normal </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:center;"> meta </td>
   <td style="text-align:center;"> components </td>
   <td style="text-align:center;"> info </td>
   <td style="text-align:center;"> model </td>
   <td style="text-align:center;"> 6 </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:center;"> meta </td>
   <td style="text-align:center;"> N </td>
   <td style="text-align:center;"> info </td>
   <td style="text-align:center;"> model </td>
   <td style="text-align:center;"> 120 </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:center;"> meta </td>
   <td style="text-align:center;"> P </td>
   <td style="text-align:center;"> info </td>
   <td style="text-align:center;"> model </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:center;"> concentration </td>
   <td style="text-align:center;"> alpha </td>
   <td style="text-align:center;"> dist </td>
   <td style="text-align:center;"> scalar </td>
   <td style="text-align:center;"> gamma(shape=1, rate=1) </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> stochastic concentration </td>
  </tr>
  <tr>
   <td style="text-align:center;"> bulk </td>
   <td style="text-align:center;"> mean </td>
   <td style="text-align:center;"> dist </td>
   <td style="text-align:center;"> component (1:6) </td>
   <td style="text-align:center;"> normal(mean=0, sd=5) </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> iid across components </td>
  </tr>
  <tr>
   <td style="text-align:center;"> bulk </td>
   <td style="text-align:center;"> sd </td>
   <td style="text-align:center;"> dist </td>
   <td style="text-align:center;"> component (1:6) </td>
   <td style="text-align:center;"> gamma(shape=2, rate=1) </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> iid across components </td>
  </tr>
  <tr>
   <td style="text-align:center;"> gpd </td>
   <td style="text-align:center;"> threshold </td>
   <td style="text-align:center;"> dist </td>
   <td style="text-align:center;"> scalar </td>
   <td style="text-align:center;"> gamma(shape=2, rate=1) </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> scalar threshold </td>
  </tr>
  <tr>
   <td style="text-align:center;"> gpd </td>
   <td style="text-align:center;"> tail_scale </td>
   <td style="text-align:center;"> dist </td>
   <td style="text-align:center;"> scalar </td>
   <td style="text-align:center;"> gamma(shape=2, rate=1) </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:center;"> gpd </td>
   <td style="text-align:center;"> tail_shape </td>
   <td style="text-align:center;"> dist </td>
   <td style="text-align:center;"> scalar </td>
   <td style="text-align:center;"> normal(mean=0, sd=0.2) </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;">  </td>
  </tr>
</tbody>
</table>
Monitors
  n = 8 
  alpha, w[1:6], z[1:120], mean[1:6], sd[1:6], threshold, tail_scale, tail_shape
```

### Run MCMC and obtain a fitted object

``` r
fit <- DPmixGPD::run_mcmc_bundle_manual(bundle, show_progress = FALSE)
===== Monitors =====
thin = 1: alpha, mean, sd, tail_scale, tail_shape, threshold, w, z
===== Samplers =====
RW sampler (21)
  - alpha
  - mean[]  (6 elements)
  - sd[]  (6 elements)
  - threshold
  - tail_scale
  - tail_shape
  - v[]  (5 elements)
categorical sampler (120)
  - z[]  (120 elements)
fit
MixGPD fit | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: TRUE
n = 120 | components = 6 | epsilon = 0.025
MCMC: niter=800, nburnin=200, thin=2, nchains=1 
Fit
Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
summary(fit)
MixGPD summary | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: TRUE | epsilon: 0.025
n = 120 | components = 6
Summary
Initial components: 6 | Components after truncation: 2

WAIC: 95.635
lppd: -11.501 | pWAIC: 36.316

Summary table
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> parameter </th>
   <th style="text-align:center;"> mean </th>
   <th style="text-align:center;"> sd </th>
   <th style="text-align:center;"> q0.025 </th>
   <th style="text-align:center;"> q0.500 </th>
   <th style="text-align:center;"> q0.975 </th>
   <th style="text-align:center;"> ess </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> weights[1] </td>
   <td style="text-align:center;"> 0.484 </td>
   <td style="text-align:center;"> 0.052 </td>
   <td style="text-align:center;"> 0.412 </td>
   <td style="text-align:center;"> 0.475 </td>
   <td style="text-align:center;"> 0.617 </td>
   <td style="text-align:center;"> 25.043 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> weights[2] </td>
   <td style="text-align:center;"> 0.311 </td>
   <td style="text-align:center;"> 0.082 </td>
   <td style="text-align:center;"> 0.158 </td>
   <td style="text-align:center;"> 0.308 </td>
   <td style="text-align:center;"> 0.45 </td>
   <td style="text-align:center;"> 10.366 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> alpha </td>
   <td style="text-align:center;"> 0.96 </td>
   <td style="text-align:center;"> 0.682 </td>
   <td style="text-align:center;"> 0.233 </td>
   <td style="text-align:center;"> 0.774 </td>
   <td style="text-align:center;"> 2.87 </td>
   <td style="text-align:center;"> 6.882 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> tail_scale </td>
   <td style="text-align:center;"> 0.576 </td>
   <td style="text-align:center;"> 0.113 </td>
   <td style="text-align:center;"> 0.399 </td>
   <td style="text-align:center;"> 0.566 </td>
   <td style="text-align:center;"> 0.901 </td>
   <td style="text-align:center;"> 35.862 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> tail_shape </td>
   <td style="text-align:center;"> -0.079 </td>
   <td style="text-align:center;"> 0.141 </td>
   <td style="text-align:center;"> -0.434 </td>
   <td style="text-align:center;"> -0.076 </td>
   <td style="text-align:center;"> 0.16 </td>
   <td style="text-align:center;"> 31.988 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> threshold </td>
   <td style="text-align:center;"> 0.639 </td>
   <td style="text-align:center;"> 0.018 </td>
   <td style="text-align:center;"> 0.597 </td>
   <td style="text-align:center;"> 0.64 </td>
   <td style="text-align:center;"> 0.665 </td>
   <td style="text-align:center;"> 11.598 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> mean[1] </td>
   <td style="text-align:center;"> 1.262 </td>
   <td style="text-align:center;"> 2.175 </td>
   <td style="text-align:center;"> 0.338 </td>
   <td style="text-align:center;"> 0.407 </td>
   <td style="text-align:center;"> 7.12 </td>
   <td style="text-align:center;"> 9.977 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> mean[2] </td>
   <td style="text-align:center;"> 4.412 </td>
   <td style="text-align:center;"> 2.927 </td>
   <td style="text-align:center;"> 0.345 </td>
   <td style="text-align:center;"> 4.196 </td>
   <td style="text-align:center;"> 11.302 </td>
   <td style="text-align:center;"> 62.349 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> sd[1] </td>
   <td style="text-align:center;"> 0.361 </td>
   <td style="text-align:center;"> 0.47 </td>
   <td style="text-align:center;"> 0.162 </td>
   <td style="text-align:center;"> 0.203 </td>
   <td style="text-align:center;"> 1.957 </td>
   <td style="text-align:center;"> 17.546 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> sd[2] </td>
   <td style="text-align:center;"> 1.092 </td>
   <td style="text-align:center;"> 0.871 </td>
   <td style="text-align:center;"> 0.143 </td>
   <td style="text-align:center;"> 0.958 </td>
   <td style="text-align:center;"> 3.066 </td>
   <td style="text-align:center;"> 78.395 </td>
  </tr>
</tbody>
</table>
```

### Customizing priors and links via `param_specs`

`param_specs` is the main â€œcontrol panelâ€? for specifying how each
parameter behaves (fixed / distribution / link). The exact parameter
names depend on the kernel, but the pattern is consistent across kernels
and tail modules.

``` r

X <- cbind(x1 = rnorm(length(y)), x2 = rbinom(length(y), 1, 0.5))

param_specs <- list(
  bulk = list(
    mean = list(
      mode = "link",
      link = "identity",
      beta_prior = list(dist = "normal", args = list(mean = 0, sd = 2))
    ),
    sd = list(
      mode = "dist",
      dist = "invgamma",
      args = list(shape = 2, scale = 1)
    )
  ),
  gpd = list(
    threshold = list(
      mode = "dist",
      dist = "gamma",
      args = list(shape = 2, rate = 0.5)
    ),
    tail_scale = list(
      mode = "link",
      link = "exp",
      beta_prior = list(dist = "normal", args = list(mean = 0, sd = 0.5))
    ),
    tail_shape = list(
      mode = "dist",
      dist = "normal",
      args = list(mean = 0, sd = 0.25)
    )
  ),
  concentration = list(
    mode = "dist",
    dist = "gamma",
    args = list(shape = 2, rate = 1)
  )
)

bundle_x <- DPmixGPD::build_nimble_bundle(
  y = y,
  X = X,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  param_specs = param_specs,
  mcmc = list(niter = 800, nburnin = 200, thin = 2, nchains = 1, seed = 1)
)
```

## References

Balkema, August A., and Laurens de Haan. 1974. “Residual Life Time at
Great Age.” *The Annals of Probability* 2 (5): 792–804.
<https://doi.org/10.1214/aop/1176996548>.

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
