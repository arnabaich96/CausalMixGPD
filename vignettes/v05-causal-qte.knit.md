---
title: "Causal QTE"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Causal QTE}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




``` r
set.seed(101)
dat <- sim_causal_qte(220)
J <- 6
bundle <- build_causal_bundle(
  y = dat$y,
  X = dat$X,
  T = dat$t,
  backend = c("sb", "sb"),
  kernel = c("normal", "normal"),
  GPD = c(FALSE, FALSE),
  J = c(J, J),
  mcmc_outcome = list(niter = 200, nburnin = 50, thin = 1, nchains = 2, seed = c(1, 2)),
  mcmc_ps = list(niter = 200, nburnin = 50, thin = 1, nchains = 2, seed = c(3, 4))
)
if (use_cached_fit) {
  cf <- fit_causal_small
} else {
  cf <- run_mcmc_causal(bundle, show_progress = FALSE)
}
print(cf)
#> DPmixGPD causal fit
#> PS model: Bayesian logit (T | X)
#> Outcome (treated): backend = sb | kernel = normal 
#> Outcome (control): backend = sb | kernel = normal 
#> GPD tail (treated/control): FALSE / FALSE
summary(cf$outcome_fit$trt)
#> MixGPD summary | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: FALSE | epsilon: 0.025
#> n = 64 | components = 4
#> Summary
#> Initial components: 4 | Components after truncation: 4
#> 
#> Summary table
#>        parameter   mean    sd q0.025 q0.500 q0.975    ess
#>       weights[1]  0.908 0.156  0.504  0.984  1.000  2.877
#>       weights[2]  0.158 0.158  0.031  0.062  0.425  1.488
#>       weights[3]  0.044 0.012  0.031  0.047  0.062 10.000
#>       weights[4]  0.038 0.009  0.031  0.031  0.047  5.000
#>            alpha  0.825 0.711  0.196  0.490  2.698  7.769
#>  beta_mean[1, 1]  2.259 0.685  1.010  2.237  3.620 19.662
#>  beta_mean[2, 1]  0.141 2.026 -2.592 -0.101  4.693  6.047
#>  beta_mean[3, 1] -0.537 1.546 -3.474 -0.353  2.075 12.519
#>  beta_mean[4, 1]  0.426 2.224 -3.552  0.618  4.758  2.862
#>  beta_mean[1, 2]  1.516 0.849  0.061  1.462  3.589 14.244
#>  beta_mean[2, 2] -0.192 1.535 -2.666 -0.188  3.065  8.873
#>  beta_mean[3, 2] -0.194 2.500 -4.215 -0.040  4.058  3.244
#>  beta_mean[4, 2] -1.316 1.797 -3.658 -1.749  2.432  8.851
#>  beta_mean[1, 3]  1.826 1.000 -0.333  1.777  3.633  9.688
#>  beta_mean[2, 3] -0.953 1.234 -3.041 -0.904  1.439  7.933
#>  beta_mean[3, 3] -0.424 1.497 -2.574 -0.764  2.667  9.761
#>  beta_mean[4, 3] -1.307 1.520 -3.185 -1.678  1.943  6.690
#>            sd[1]  0.030 0.007  0.022  0.029  0.041 73.257
#>            sd[2]  0.832 1.005  0.033  0.425  3.162  5.904
#>            sd[3]  2.131 1.367  0.881  1.714  4.999 10.000
#>            sd[4]  1.714 2.131  0.418  0.668  5.052  5.000
```


``` r
ps_df <- data.frame(dat$X, T = dat$t)
ps_fit <- glm(T ~ x1 + x2 + x3, data = ps_df, family = binomial())
ps_df$propensity <- predict(ps_fit, type = "response")
ggplot(ps_df, aes(x = propensity, fill = factor(T, labels = c("con", "trt")))) +
  geom_density(alpha = 0.4) +
  labs(title = "Propensity score overlap", fill = "Arm")
```

![Propensity score distributions by arm (logistic GLM proxy).](D:\OneDrive - Florida State University\MyFSU_OneDrive\R-Codes\DPMGPD_package\DPmixGPD\vignettes\v05-causal-qte_files/figure-html/overlap-plot-1.png)


``` r
plot(cf$outcome_fit$trt, family = "traceplot", params = c("alpha", "w[1]"))
```

![Trace plots for tail_scale and tail_shape (treated arm).](D:\OneDrive - Florida State University\MyFSU_OneDrive\R-Codes\DPMGPD_package\DPmixGPD\vignettes\v05-causal-qte_files/figure-html/tail-trace-1.png)


``` r
grid <- expand.grid(
  x1 = seq(-1.2, 1.2, length.out = 15),
  x2 = seq(-0.8, 0.8, length.out = 5),
  x3 = 0
)
taus <- c(.1, .5, .9, .95, .99)
pr_trt <- predict(cf$outcome_fit$trt, newdata = grid, type = "quantile", p = taus)
pr_con <- predict(cf$outcome_fit$con, newdata = grid, type = "quantile", p = taus)
qte_df <- data.frame(
  x1 = rep(grid$x1, each = length(taus)),
  tau = rep(taus, times = nrow(grid)),
  qte = c(pr_trt$fit - pr_con$fit)
)
slice_df <- qte_df[qte_df$tau %in% c(.5, .95, .99) & qte_df$x1 %in% c(-1, 0, 1), ]
ggplot(slice_df, aes(x = x1, y = qte, color = factor(tau))) +
  geom_line() +
  labs(title = "QTE slices (selected taus)", color = "tau")
```

![](D:\OneDrive - Florida State University\MyFSU_OneDrive\R-Codes\DPMGPD_package\DPmixGPD\vignettes\v05-causal-qte_files/figure-html/qte-slices-1.png)<!-- -->


``` r
surf_grid <- expand.grid(x1 = seq(-1, 1, length.out = 25), x2 = seq(-1, 1, length.out = 25), x3 = 0)
tau_surface <- 0.95
surf_trt <- predict(cf$outcome_fit$trt, newdata = surf_grid, type = "quantile", p = tau_surface)
surf_con <- predict(cf$outcome_fit$con, newdata = surf_grid, type = "quantile", p = tau_surface)
surf_grid$qte <- as.numeric(surf_trt$fit - surf_con$fit)
ggplot(surf_grid, aes(x = x1, y = x2, fill = qte)) +
  geom_raster() +
  scale_fill_viridis_c(option = "C") +
  labs(title = "QTE surface (tau = 0.95)", fill = "QTE")
```

![QTE surface for tau=0.95 varying x1 / x2 at x3=0.](D:\OneDrive - Florida State University\MyFSU_OneDrive\R-Codes\DPMGPD_package\DPmixGPD\vignettes\v05-causal-qte_files/figure-html/qte-surface-1.png)






