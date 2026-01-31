# 0. Start Here

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

## Quick start

This vignette gives a minimal, fully working workflow for an
**unconditional** model and a **conditional** model. Everything uses
short MCMC runs so the vignette renders quickly.

------------------------------------------------------------------------

## Unconditional Model (CRP, bulk-only)

``` r

data("nc_pos200_k3")
y <- nc_pos200_k3$y
```

``` r

bundle_uncond <- build_nimble_bundle(
  y = y,
  backend = "crp",
  kernel = "gamma",
  GPD = FALSE,
  components = 5,
  mcmc = mcmc
)
```

``` r

fit_uncond <- load_or_fit("v00-start-here-fit_uncond", quiet_mcmc(run_mcmc_bundle_manual(bundle_uncond, show_progress = FALSE)))
summary(fit_uncond)
```

    MixGPD summary | backend: Chinese Restaurant Process | kernel: Gamma Distribution | GPD tail: FALSE | epsilon: 0.025
    n = 200 | components = 5
    Summary
    Initial components: 5 | Components after truncation: 1

    WAIC: 929.279
    lppd: -399.887 | pWAIC: 64.752

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
       <td style="text-align:center;"> 0.684 </td>
       <td style="text-align:center;"> 0.216 </td>
       <td style="text-align:center;"> 0.36 </td>
       <td style="text-align:center;"> 0.63 </td>
       <td style="text-align:center;"> 1 </td>
       <td style="text-align:center;"> 25.041 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> alpha </td>
       <td style="text-align:center;"> 0.559 </td>
       <td style="text-align:center;"> 0.388 </td>
       <td style="text-align:center;"> 0.028 </td>
       <td style="text-align:center;"> 0.498 </td>
       <td style="text-align:center;"> 1.526 </td>
       <td style="text-align:center;"> 204.872 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> shape[1] </td>
       <td style="text-align:center;"> 1.784 </td>
       <td style="text-align:center;"> 0.847 </td>
       <td style="text-align:center;"> 0.943 </td>
       <td style="text-align:center;"> 1.483 </td>
       <td style="text-align:center;"> 3.912 </td>
       <td style="text-align:center;"> 27.633 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> scale[1] </td>
       <td style="text-align:center;"> 0.494 </td>
       <td style="text-align:center;"> 0.343 </td>
       <td style="text-align:center;"> 0.212 </td>
       <td style="text-align:center;"> 0.35 </td>
       <td style="text-align:center;"> 1.402 </td>
       <td style="text-align:center;"> 59.82 </td>
      </tr>
    </tbody>
    </table>

``` r

pred_q <- predict(fit_uncond, type = "quantile", p = c(0.5, 0.9), interval = "credible")
head(pred_q$fit)
```

      estimate index lower upper
    1    0.841   0.5 0.139  3.50
    2    1.913   0.9 0.476  7.16

``` r

if (interactive()) plot(pred_q)
```

------------------------------------------------------------------------

## Conditional Model (SB, bulk-only)

``` r

data("nc_posX100_p3_k2")
yc <- nc_posX100_p3_k2$y
X <- as.matrix(nc_posX100_p3_k2$X)
```

``` r

bundle_cond <- build_nimble_bundle(
  y = yc,
  X = X,
  backend = "sb",
  kernel = "lognormal",
  GPD = FALSE,
  components = 5,
  mcmc = mcmc
)
```

``` r

fit_cond <- load_or_fit("v00-start-here-fit_cond", quiet_mcmc(run_mcmc_bundle_manual(bundle_cond, show_progress = FALSE)))
summary(fit_cond)
```

    MixGPD summary | backend: Stick-Breaking Process | kernel: Lognormal Distribution | GPD tail: FALSE | epsilon: 0.025
    n = 100 | components = 5
    Summary
    Initial components: 5 | Components after truncation: 1

    WAIC: 526.697
    lppd: -255.693 | pWAIC: 7.655

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
       <td style="text-align:center;"> 0.973 </td>
       <td style="text-align:center;"> 0.056 </td>
       <td style="text-align:center;"> 0.8 </td>
       <td style="text-align:center;"> 1 </td>
       <td style="text-align:center;"> 1 </td>
       <td style="text-align:center;"> 60.977 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> alpha </td>
       <td style="text-align:center;"> 0.465 </td>
       <td style="text-align:center;"> 0.272 </td>
       <td style="text-align:center;"> 0.111 </td>
       <td style="text-align:center;"> 0.402 </td>
       <td style="text-align:center;"> 1.128 </td>
       <td style="text-align:center;"> 243.504 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> beta_meanlog[1, 1] </td>
       <td style="text-align:center;"> 0.119 </td>
       <td style="text-align:center;"> 0.133 </td>
       <td style="text-align:center;"> -0.138 </td>
       <td style="text-align:center;"> 0.114 </td>
       <td style="text-align:center;"> 0.37 </td>
       <td style="text-align:center;"> 937.946 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> beta_meanlog[2, 1] </td>
       <td style="text-align:center;"> 0.151 </td>
       <td style="text-align:center;"> 1.717 </td>
       <td style="text-align:center;"> -3.398 </td>
       <td style="text-align:center;"> 0.203 </td>
       <td style="text-align:center;"> 3.557 </td>
       <td style="text-align:center;"> 737.604 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> beta_meanlog[3, 1] </td>
       <td style="text-align:center;"> -0.03 </td>
       <td style="text-align:center;"> 1.944 </td>
       <td style="text-align:center;"> -3.97 </td>
       <td style="text-align:center;"> 0.066 </td>
       <td style="text-align:center;"> 3.631 </td>
       <td style="text-align:center;"> 846.559 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> beta_meanlog[4, 1] </td>
       <td style="text-align:center;"> 0.07 </td>
       <td style="text-align:center;"> 2.074 </td>
       <td style="text-align:center;"> -4.033 </td>
       <td style="text-align:center;"> 0.029 </td>
       <td style="text-align:center;"> 4.109 </td>
       <td style="text-align:center;"> 1246.061 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> beta_meanlog[5, 1] </td>
       <td style="text-align:center;"> 0.091 </td>
       <td style="text-align:center;"> 1.977 </td>
       <td style="text-align:center;"> -3.802 </td>
       <td style="text-align:center;"> 0.085 </td>
       <td style="text-align:center;"> 3.816 </td>
       <td style="text-align:center;"> 1124.342 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> beta_meanlog[1, 2] </td>
       <td style="text-align:center;"> -0.17 </td>
       <td style="text-align:center;"> 0.246 </td>
       <td style="text-align:center;"> -0.643 </td>
       <td style="text-align:center;"> -0.168 </td>
       <td style="text-align:center;"> 0.314 </td>
       <td style="text-align:center;"> 431.197 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> beta_meanlog[2, 2] </td>
       <td style="text-align:center;"> 0.193 </td>
       <td style="text-align:center;"> 1.881 </td>
       <td style="text-align:center;"> -3.543 </td>
       <td style="text-align:center;"> 0.208 </td>
       <td style="text-align:center;"> 3.649 </td>
       <td style="text-align:center;"> 365.618 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> beta_meanlog[3, 2] </td>
       <td style="text-align:center;"> 0.006 </td>
       <td style="text-align:center;"> 1.938 </td>
       <td style="text-align:center;"> -3.896 </td>
       <td style="text-align:center;"> -0.013 </td>
       <td style="text-align:center;"> 3.772 </td>
       <td style="text-align:center;"> 1050.905 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> beta_meanlog[4, 2] </td>
       <td style="text-align:center;"> 0.046 </td>
       <td style="text-align:center;"> 2.004 </td>
       <td style="text-align:center;"> -3.936 </td>
       <td style="text-align:center;"> 0.081 </td>
       <td style="text-align:center;"> 3.885 </td>
       <td style="text-align:center;"> 1056.479 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> beta_meanlog[5, 2] </td>
       <td style="text-align:center;"> -0.03 </td>
       <td style="text-align:center;"> 1.999 </td>
       <td style="text-align:center;"> -3.979 </td>
       <td style="text-align:center;"> -0.023 </td>
       <td style="text-align:center;"> 3.947 </td>
       <td style="text-align:center;"> 1236.055 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> beta_meanlog[1, 3] </td>
       <td style="text-align:center;"> 0.063 </td>
       <td style="text-align:center;"> 0.128 </td>
       <td style="text-align:center;"> -0.187 </td>
       <td style="text-align:center;"> 0.064 </td>
       <td style="text-align:center;"> 0.316 </td>
       <td style="text-align:center;"> 804.499 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> beta_meanlog[2, 3] </td>
       <td style="text-align:center;"> -0.049 </td>
       <td style="text-align:center;"> 1.71 </td>
       <td style="text-align:center;"> -3.555 </td>
       <td style="text-align:center;"> -0.002 </td>
       <td style="text-align:center;"> 3.222 </td>
       <td style="text-align:center;"> 495.136 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> beta_meanlog[3, 3] </td>
       <td style="text-align:center;"> 0.074 </td>
       <td style="text-align:center;"> 1.944 </td>
       <td style="text-align:center;"> -3.677 </td>
       <td style="text-align:center;"> 0.078 </td>
       <td style="text-align:center;"> 3.963 </td>
       <td style="text-align:center;"> 982.923 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> beta_meanlog[4, 3] </td>
       <td style="text-align:center;"> 0.017 </td>
       <td style="text-align:center;"> 2.03 </td>
       <td style="text-align:center;"> -3.895 </td>
       <td style="text-align:center;"> 0.052 </td>
       <td style="text-align:center;"> 3.882 </td>
       <td style="text-align:center;"> 1032.749 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> beta_meanlog[5, 3] </td>
       <td style="text-align:center;"> -0.003 </td>
       <td style="text-align:center;"> 1.952 </td>
       <td style="text-align:center;"> -3.996 </td>
       <td style="text-align:center;"> -0.044 </td>
       <td style="text-align:center;"> 3.945 </td>
       <td style="text-align:center;"> 1051.657 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> sdlog[1] </td>
       <td style="text-align:center;"> 0.673 </td>
       <td style="text-align:center;"> 0.099 </td>
       <td style="text-align:center;"> 0.5 </td>
       <td style="text-align:center;"> 0.663 </td>
       <td style="text-align:center;"> 0.897 </td>
       <td style="text-align:center;"> 1200 </td>
      </tr>
    </tbody>
    </table>

``` r

x_new <- X[1:20, , drop = FALSE]
pred_mean <- predict(fit_cond, x = x_new, type = "mean", interval = "credible", nsim_mean = 200)
head(pred_mean$fit)
```

      id estimate lower upper
    1  1    1.519 0.830  2.58
    2  2    1.099 0.688  1.71
    3  3    0.955 0.556  1.54
    4  4    1.272 0.857  1.79
    5  5    1.362 0.891  1.99
    6  6    1.040 0.663  1.60

``` r

if (interactive()) plot(pred_mean)
```

------------------------------------------------------------------------

## Useful S3 Methods

``` r

params(fit_uncond)
```

    Posterior mean parameters

    $alpha
    [1] "0.559"

    $w
    [1] "0.684"

    $shape
    [1] "1.784"

    $scale
    [1] "0.494"

``` r

if (interactive()) plot(fit_uncond, family = c("traceplot", "running"))
```

------------------------------------------------------------------------

## Next Steps

- `vignette 1`: package overview and terminology
- `vignette 5`: full three-phase workflow (spec ? bundle ? MCMC)
- `vignette 6-13`: unconditional/conditional models with and without GPD
- `vignette 14-19`: causal workflows
