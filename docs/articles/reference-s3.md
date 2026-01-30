# Reference: S3 Methods

## Overview

This vignette provides a reference for S3 methods on `mixgpd_fit`
objects.

## Theory (brief)

S3 methods provide a consistent interface for posterior summaries and
diagnostics on fitted objects. They expose predictions, fitted values,
and standard MCMC visualizations while keeping the underlying model
specification unchanged.

## Model Fitting

``` r

library(DPmixGPD)

data("faithful", package = "datasets")
y <- faithful$eruptions
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)
fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
```

## print()

``` r
print(fit)
MixGPD fit | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: TRUE
n = 272 | components = 6 | epsilon = 0.025
MCMC: niter=300, nburnin=80, thin=2, nchains=1 
Fit
Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
```

## summary()

``` r
summary(fit)
MixGPD summary | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: TRUE | epsilon: 0.025
n = 272 | components = 6
Summary
Initial components: 6 | Components after truncation: 4

WAIC: 739.412
lppd: -363.693 | pWAIC: 6.013

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
   <td style="text-align:center;"> 0.421 </td>
   <td style="text-align:center;"> 0.095 </td>
   <td style="text-align:center;"> 0.304 </td>
   <td style="text-align:center;"> 0.379 </td>
   <td style="text-align:center;"> 0.619 </td>
   <td style="text-align:center;"> 2.803 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> weights[2] </td>
   <td style="text-align:center;"> 0.259 </td>
   <td style="text-align:center;"> 0.052 </td>
   <td style="text-align:center;"> 0.162 </td>
   <td style="text-align:center;"> 0.265 </td>
   <td style="text-align:center;"> 0.342 </td>
   <td style="text-align:center;"> 5.189 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> weights[3] </td>
   <td style="text-align:center;"> 0.188 </td>
   <td style="text-align:center;"> 0.048 </td>
   <td style="text-align:center;"> 0.106 </td>
   <td style="text-align:center;"> 0.202 </td>
   <td style="text-align:center;"> 0.263 </td>
   <td style="text-align:center;"> 2.73 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> weights[4] </td>
   <td style="text-align:center;"> 0.099 </td>
   <td style="text-align:center;"> 0.03 </td>
   <td style="text-align:center;"> 0.044 </td>
   <td style="text-align:center;"> 0.099 </td>
   <td style="text-align:center;"> 0.154 </td>
   <td style="text-align:center;"> 8.607 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> alpha </td>
   <td style="text-align:center;"> 1.285 </td>
   <td style="text-align:center;"> 0.676 </td>
   <td style="text-align:center;"> 0.343 </td>
   <td style="text-align:center;"> 1.278 </td>
   <td style="text-align:center;"> 2.92 </td>
   <td style="text-align:center;"> 15.874 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> tail_scale </td>
   <td style="text-align:center;"> 2.526 </td>
   <td style="text-align:center;"> 0.095 </td>
   <td style="text-align:center;"> 2.348 </td>
   <td style="text-align:center;"> 2.496 </td>
   <td style="text-align:center;"> 2.683 </td>
   <td style="text-align:center;"> 2.87 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> tail_shape </td>
   <td style="text-align:center;"> -0.705 </td>
   <td style="text-align:center;"> 0.045 </td>
   <td style="text-align:center;"> -0.75 </td>
   <td style="text-align:center;"> -0.714 </td>
   <td style="text-align:center;"> -0.618 </td>
   <td style="text-align:center;"> 1.776 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> threshold </td>
   <td style="text-align:center;"> 1.678 </td>
   <td style="text-align:center;"> 0.024 </td>
   <td style="text-align:center;"> 1.664 </td>
   <td style="text-align:center;"> 1.664 </td>
   <td style="text-align:center;"> 1.727 </td>
   <td style="text-align:center;"> 3.888 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> mean[1] </td>
   <td style="text-align:center;"> 7.173 </td>
   <td style="text-align:center;"> 3.027 </td>
   <td style="text-align:center;"> 3.26 </td>
   <td style="text-align:center;"> 6.552 </td>
   <td style="text-align:center;"> 12.93 </td>
   <td style="text-align:center;"> 21.739 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> mean[2] </td>
   <td style="text-align:center;"> 7.146 </td>
   <td style="text-align:center;"> 2.643 </td>
   <td style="text-align:center;"> 3.605 </td>
   <td style="text-align:center;"> 6.608 </td>
   <td style="text-align:center;"> 12.352 </td>
   <td style="text-align:center;"> 112.35 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> mean[3] </td>
   <td style="text-align:center;"> 5.792 </td>
   <td style="text-align:center;"> 2.105 </td>
   <td style="text-align:center;"> 2.497 </td>
   <td style="text-align:center;"> 5.571 </td>
   <td style="text-align:center;"> 10.365 </td>
   <td style="text-align:center;"> 19.338 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> mean[4] </td>
   <td style="text-align:center;"> 8.599 </td>
   <td style="text-align:center;"> 3.396 </td>
   <td style="text-align:center;"> 2.655 </td>
   <td style="text-align:center;"> 8.457 </td>
   <td style="text-align:center;"> 15.258 </td>
   <td style="text-align:center;"> 12.648 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> sd[1] </td>
   <td style="text-align:center;"> 1.551 </td>
   <td style="text-align:center;"> 1.002 </td>
   <td style="text-align:center;"> 0.2 </td>
   <td style="text-align:center;"> 1.314 </td>
   <td style="text-align:center;"> 3.765 </td>
   <td style="text-align:center;"> 15.609 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> sd[2] </td>
   <td style="text-align:center;"> 1.411 </td>
   <td style="text-align:center;"> 0.819 </td>
   <td style="text-align:center;"> 0.362 </td>
   <td style="text-align:center;"> 1.229 </td>
   <td style="text-align:center;"> 3.558 </td>
   <td style="text-align:center;"> 19.133 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> sd[3] </td>
   <td style="text-align:center;"> 1.257 </td>
   <td style="text-align:center;"> 0.736 </td>
   <td style="text-align:center;"> 0.217 </td>
   <td style="text-align:center;"> 1.172 </td>
   <td style="text-align:center;"> 3.168 </td>
   <td style="text-align:center;"> 23.754 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> sd[4] </td>
   <td style="text-align:center;"> 1.354 </td>
   <td style="text-align:center;"> 0.789 </td>
   <td style="text-align:center;"> 0.251 </td>
   <td style="text-align:center;"> 1.248 </td>
   <td style="text-align:center;"> 3.111 </td>
   <td style="text-align:center;"> 39.807 </td>
  </tr>
</tbody>
</table>
```

## if (interactive()) plot()

``` r

if (interactive()) plot(fit, family = "traceplot")
```

## predict()

``` r
predict(fit, type = "mean", cred.level = 0.90, interval = "credible")$fit
  estimate lower upper
1     3.17  3.04   3.3
predict(fit, type = "median", cred.level = 0.90, interval = "credible")$fit
  estimate index lower upper
1     3.05   0.5  2.99  3.13
predict(fit, type = "quantile", index = 0.90, cred.level = 0.90, interval = "hpd")$fit
  estimate index lower upper
1     4.55   0.9  4.44  4.65
predict(fit, type = "quantile", index = 0.90, interval = NULL)$fit
  estimate index lower upper
1     4.55   0.9    NA    NA
```

## fitted()

``` r
f <- fitted(fit, type = "mean", level = 0.90)
head(f)
   fit lower upper residuals
1 3.18  3.05  3.33     0.416
2 3.18  3.05  3.33    -1.384
3 3.18  3.05  3.33     0.149
4 3.18  3.05  3.33    -0.901
5 3.18  3.05  3.33     1.349
6 3.18  3.05  3.33    -0.301
```

## Object Structure

``` r
str(fit, max.level = 2)
List of 14
 $ call      : language run_mcmc_bundle_manual(bundle = bundle, show_progress = FALSE)
 $ spec      :List of 4
  ..$ meta       :List of 9
  ..$ kernel_info:List of 9
  ..$ signatures :List of 2
  ..$ plan       :List of 11
  ..- attr(*, "class")= chr [1:2] "dpmixgpd_spec" "list"
 $ data      :List of 1
  ..$ y: num [1:272] 3.6 1.8 3.33 2.28 4.53 ...
 $ model     :Reference class 'Ccode_MID_1' [package ".GlobalEnv"] with 67 fields
  ..and 88 methods, of which 74 are  possibly relevant:
  ..  calculate, calculateDiff, check, checkBasics, checkConjugacy,
  ..  copyFromModel, expandNodeNames, expandNodeNamesFromGraphIDs, finalize,
  ..  finalizeInternal, getBound, getBuildDerivs, getCode,
  ..  getConditionallyIndependentSets, getConstants, getDeclID, getDeclInfo,
  ..  getDependencies, getDependenciesList, getDependencyPathCountOneNode,
  ..  getDependencyPaths, getDimension, getDistribution, getDownstream,
  ..  getGraph, getLogProb, getMacroInits, getMacroParameters, getMaps,
  ..  getModelDef, getNodeFunctions, getNodeNames, getNodeType, getParam,
  ..  getParamExpr, getParents, getParentsList, getPredictiveNodeIDs,
  ..  getPredictiveRootNodeIDs, getSymbolTable, getUnrolledIndicesList,
  ..  getValueExpr, getVarInfo, getVarNames, init_isDataEnv, initialize,
  ..  initializeInfo, isBinary, isData, isDataFromGraphID, isDeterm,
  ..  isDiscrete, isEndNode, isMultivariate, isStoch, isTruncated,
  ..  isUnivariate, newModel, plot, plotGraph, resetData,
  ..  safeUpdateValidValues, setData, setGraph, setInits, setModel,
  ..  setModelDef, setPredictiveNodeIDs, setupNodes, show#CmodelBaseClass,
  ..  show#envRefClass, simulate, testDataFlags, topologicallySortNodes
 $ mcmc_conf :Reference class 'MCMCconf' [package "nimble"] with 16 fields
  ..and 59 methods, of which 45 are  possibly relevant:
  ..  addConjugateSampler, addDefaultSampler, addDerivedQuantity, addMonitors,
  ..  addMonitors2, addOneDerivedQuantity, addOneSampler, addSampler,
  ..  filterOutDataNodes, findSamplersOnNodes, getDerivedQuantities,
  ..  getDerivedQuantityDefinition, getMonitors, getMonitors2,
  ..  getMvSamplesConf, getSamplerDefinition, getSamplerExecutionOrder,
  ..  getSamplers, getUnsampledNodes, initialize, isMvSamplesReady,
  ..  makeMvSamplesConf, printComments, printDerivedQuantities,
  ..  printDerivedQuantitiesByType, printMonitors, printSamplers,
  ..  printSamplersByType, removeDerivedQuantities, removeDerivedQuantity,
  ..  removeSampler, removeSamplers, replaceSampler, replaceSamplers,
  ..  resetMonitors, setMonitors, setMonitors2, setSampler,
  ..  setSamplerExecutionOrder, setSamplers, setThin, setThin2,
  ..  setUnsampledNodes, show#envRefClass, warnUnsampledNodes
 $ mcmc      :List of 8
  ..$ engine : chr "compiled"
  ..$ niter  : int 300
  ..$ nburnin: int 80
  ..$ thin   : int 2
  ..$ nchains: int 1
  ..$ seed   : int 1
  ..$ samples: 'mcmc' num [1:110, 1:294] 1.21 1.15 1.44 1.52 1.52 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. ..- attr(*, "mcpar")= num [1:3] 1 110 1
  ..$ waic   :Reference class 'waicNimbleList' [package "nimble"] with 7 fields
  .. ..and 17 methods, of which 3 are  possibly relevant
 $ code      : language {     alpha ~ dgamma(1, 1) ...
 $ constants :List of 3
  ..$ N         : int 272
  ..$ P         : int 0
  ..$ components: int 6
 $ dimensions:List of 5
  ..$ v   : int 5
  ..$ w   : int 6
  ..$ z   : int 272
  ..$ mean: int 6
  ..$ sd  : int 6
 $ monitors  : chr [1:8] "alpha" "w[1:6]" "z[1:272]" "mean[1:6]" ...
 $ cache     : list()
 $ epsilon   : num 0.025
 $ samples   : 'mcmc' num [1:110, 1:294] 1.21 1.15 1.44 1.52 1.52 ...
  ..- attr(*, "dimnames")=List of 2
  ..- attr(*, "mcpar")= num [1:3] 1 110 1
 $ waic      :Reference class 'waicNimbleList' [package "nimble"] with 7 fields
  ..and 17 methods, of which 3 are  possibly relevant:
  ..  initialize, initialize#nimbleListBase, show#envRefClass
 - attr(*, "class")= chr [1:2] "mixgpd_fit" "list"
```

## Causal S3 Methods

The [`ate()`](https://arnabaich96.github.io/DPmixGPD/reference/ate.md)
and [`qte()`](https://arnabaich96.github.io/DPmixGPD/reference/qte.md)
functions return objects with their own S3 methods.

### Causal Model Fitting

``` r

data("mtcars", package = "datasets")
df <- mtcars
X <- df[, c("wt", "hp", "qsec", "cyl")]
X <- as.data.frame(X)
T_ind <- df$am
y <- df$mpg

causal_bundle <- build_causal_bundle(
  y = y,
  X = X,
  T = T_ind,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  PS = "logit",
  mcmc_outcome = mcmc,
  mcmc_ps = mcmc
)
```

``` r

causal_fit <- run_mcmc_causal(causal_bundle, show_progress = FALSE)
```

### ATE S3 Methods

``` r
ate_result <- ate(causal_fit, interval = "hpd", nsim_mean = 50)
print(ate_result)
ATE (Average Treatment Effect)
  Prediction points: 32
  Conditional (covariates): YES
  Propensity score used: YES
  PS scale: logit
  Posterior mean draws: 50
  Credible interval: hpd

ATE estimates (treated - control):
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> id </th>
   <th style="text-align:center;"> estimate </th>
   <th style="text-align:center;"> lower </th>
   <th style="text-align:center;"> upper </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> -68.244 </td>
   <td style="text-align:center;"> -300.799 </td>
   <td style="text-align:center;"> 75.211 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> -68.875 </td>
   <td style="text-align:center;"> -302.388 </td>
   <td style="text-align:center;"> 60.352 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 3 </td>
   <td style="text-align:center;"> -66.586 </td>
   <td style="text-align:center;"> -260.851 </td>
   <td style="text-align:center;"> 14.145 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 4 </td>
   <td style="text-align:center;"> -69.374 </td>
   <td style="text-align:center;"> -299.253 </td>
   <td style="text-align:center;"> 85.439 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 5 </td>
   <td style="text-align:center;"> -92.065 </td>
   <td style="text-align:center;"> -579.136 </td>
   <td style="text-align:center;"> 165.576 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 6 </td>
   <td style="text-align:center;"> -66.117 </td>
   <td style="text-align:center;"> -289.57 </td>
   <td style="text-align:center;"> 102.075 </td>
  </tr>
</tbody>
</table>... (26 more rows)
```

``` r
summary(ate_result)
ATE Summary
================================================== 
Prediction points: 32
Conditional: YES | PS used: YES
Posterior mean draws: 50
Interval: hpd

Model specification:
  Backend (trt/con): sb / sb
  Kernel (trt/con): normal / normal
  GPD tail (trt/con): YES / YES

ATE statistics:
  Mean: -84.519 | Median: -76.146
  Range: [-206.516, -40.642]
  SD: 34.424

Credible interval width:
  Mean: 563.362 | Median: 427.586
  Range: [177.145, 1145.375]
```

``` r

ate_plots <- if (interactive()) plot(ate_result)
ate_plots$treatment_effect
NULL
```

``` r

if (interactive()) plot(ate_result, type = "effect")
```

### QTE S3 Methods

``` r
qte_result <- qte(causal_fit, probs = c(0.1, 0.5, 0.9), interval = "hpd")
print(qte_result)
QTE (Quantile Treatment Effect)
  Prediction points: 32
  Quantile grid: 0.1, 0.5, 0.9
  Conditional (covariates): YES
  Propensity score used: YES
  PS scale: logit
  Credible interval: hpd

QTE estimates (treated - control):
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> index </th>
   <th style="text-align:center;"> id </th>
   <th style="text-align:center;"> estimate </th>
   <th style="text-align:center;"> lower </th>
   <th style="text-align:center;"> upper </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> -74.819 </td>
   <td style="text-align:center;"> -292.18 </td>
   <td style="text-align:center;"> 12.888 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> -74.829 </td>
   <td style="text-align:center;"> -291.32 </td>
   <td style="text-align:center;"> 12.892 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 3 </td>
   <td style="text-align:center;"> -65.117 </td>
   <td style="text-align:center;"> -257.106 </td>
   <td style="text-align:center;"> 2.552 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 4 </td>
   <td style="text-align:center;"> -75.26 </td>
   <td style="text-align:center;"> -295.072 </td>
   <td style="text-align:center;"> 12.66 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 5 </td>
   <td style="text-align:center;"> -115.349 </td>
   <td style="text-align:center;"> -458.356 </td>
   <td style="text-align:center;"> 34.528 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 6 </td>
   <td style="text-align:center;"> -72.053 </td>
   <td style="text-align:center;"> -283.789 </td>
   <td style="text-align:center;"> 12.802 </td>
  </tr>
</tbody>
</table>... (90 more rows)
```

``` r
summary(qte_result)
QTE Summary
================================================== 
Prediction points: 32 | Quantiles: 3
Quantile grid: 0.1, 0.5, 0.9
Conditional: YES | PS used: YES
Interval: hpd

Model specification:
  Backend (trt/con): sb / sb
  Kernel (trt/con): normal / normal
  GPD tail (trt/con): YES / YES

QTE by quantile:
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> quantile </th>
   <th style="text-align:center;"> mean_qte </th>
   <th style="text-align:center;"> median_qte </th>
   <th style="text-align:center;"> min_qte </th>
   <th style="text-align:center;"> max_qte </th>
   <th style="text-align:center;"> sd_qte </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> -98.577 </td>
   <td style="text-align:center;"> -83.433 </td>
   <td style="text-align:center;"> -223.833 </td>
   <td style="text-align:center;"> -39.188 </td>
   <td style="text-align:center;"> 43.712 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.5 </td>
   <td style="text-align:center;"> -88.933 </td>
   <td style="text-align:center;"> -78.316 </td>
   <td style="text-align:center;"> -211.551 </td>
   <td style="text-align:center;"> -39.763 </td>
   <td style="text-align:center;"> 36.917 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.9 </td>
   <td style="text-align:center;"> -65.601 </td>
   <td style="text-align:center;"> -58.742 </td>
   <td style="text-align:center;"> -181.964 </td>
   <td style="text-align:center;"> -12.153 </td>
   <td style="text-align:center;"> 33.054 </td>
  </tr>
</tbody>
</table>
Credible interval width:
  Mean: 548.261 | Median: 451.115
  Range: [164.373, 1604.476]
```

``` r

qte_plots <- if (interactive()) plot(qte_result)
qte_plots$treatment_effect
NULL
```

``` r

if (interactive()) plot(qte_result, type = "effect")
```
