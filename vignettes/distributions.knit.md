---
title: "Available distributions"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Available distributions}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## Goal

DPmixGPD supports multiple *bulk kernels* for the mixture components, and can optionally splice a Generalized Pareto tail (GPD) beyond a threshold.

This vignette:

- summarizes the kernel choices,
- shows how the kernel choice appears in the API,
- gives quick distribution sanity checks using base R `d/p/q/r` functions.

## Kernel summary

Below is a compact summary of common bulk kernels and their usual parameters.


``` r
knitr::kable(
  data.frame(
    kernel = c("normal", "gamma", "lognormal", "cauchy", "laplace", "invgauss", "amoroso"),
    typical_parameters = c(
      "mean, sd",
      "shape, rate",
      "meanlog, sdlog",
      "location, scale",
      "location, scale",
      "mean, shape",
      "location/scale/shape (family-specific)"
    )
  ),
  caption = "Common bulk kernels (names may vary by version)"
)
```



Table: Common bulk kernels (names may vary by version)

|kernel    |typical_parameters                     |
|:---------|:--------------------------------------|
|normal    |mean, sd                               |
|gamma     |shape, rate                            |
|lognormal |meanlog, sdlog                         |
|cauchy    |location, scale                        |
|laplace   |location, scale                        |
|invgauss  |mean, shape                            |
|amoroso   |location/scale/shape (family-specific) |



## Where you choose the kernel

The kernel is selected in `build_nimble_bundle()` via the `kernel` argument.


``` r
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",      # or "crp"
  kernel  = "lognormal",
  GPD     = TRUE,
  components = 6,
  mcmc = list(niter = 500, nburnin = 100, thin = 2, nchains = 1, seed = 1)
)
```

## Quick distribution checks

These plots are *not* from the DP mixture model; they are just sanity checks for the underlying parametric families.

### Normal


``` r
x <- seq(-4, 4, length.out = 400)
plot(x, dnorm(x, mean = 0, sd = 1), type = "l", xlab = "x", ylab = "density", main = "Normal(0,1) density")
```

![](D:\OneDrive - Florida State University\MyFSU_OneDrive\R-Codes\DPMGPD_package\DPmixGPD\docs\articles\distributions_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### Gamma


``` r
x <- seq(0, 12, length.out = 400)
plot(x, dgamma(x, shape = 2, rate = 1), type = "l", xlab = "x", ylab = "density", main = "Gamma(shape=2, rate=1) density")
```

![](D:\OneDrive - Florida State University\MyFSU_OneDrive\R-Codes\DPMGPD_package\DPmixGPD\docs\articles\distributions_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Lognormal


``` r
x <- seq(0, 12, length.out = 400)
plot(x, dlnorm(x, meanlog = 0, sdlog = 0.6), type = "l", xlab = "x", ylab = "density", main = "Lognormal(meanlog=0, sdlog=0.6) density")
```

![](D:\OneDrive - Florida State University\MyFSU_OneDrive\R-Codes\DPMGPD_package\DPmixGPD\docs\articles\distributions_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Notes

- Mixture models use these families as *components*; the overall fitted distribution can be much more flexible.
- Turning `GPD = TRUE` adds EVT-motivated tail behavior beyond a threshold.
- When in doubt, start with `kernel = "normal"` for debugging, then switch to heavier-tailed kernels.


