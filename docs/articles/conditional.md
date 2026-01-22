# Conditional Models

## Overview

This vignette demonstrates fitting a conditional model with covariates
and generating predictions on a grid of new covariate values.

## Theory (brief)

Conditional models allow the bulk mixture parameters to vary with
covariates. We write the conditional density as \$\$ f(y_i \\mid x_i) =
\\int K(y_i; \\theta(x_i))\\, dG(\\theta), \$\$ where the link between
\$\\theta\$ and $`x_i`$ is encoded through the kernel-specific
regression structure. The DP prior provides flexible, data-driven
clustering of local distributions across covariate space.

## Data Setup

``` r
library(DPmixGPD)

data("mtcars", package = "datasets")
df <- mtcars
y <- df$mpg
X <- df[, c("wt", "hp")]
X <- as.data.frame(X)
```

## Model Fitting

``` r
bundle <- build_nimble_bundle(
  y = y,
  X = X,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)

fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
```

## Fitted Values

``` r
f <- fitted(fit, type = "mean", level = 0.90)
head(f)
#>        fit     lower    upper  residuals
#> 1 14.53927  7.155984 23.35003   6.460725
#> 2 18.31677  9.232510 30.85224   2.683232
#> 3 11.36667  6.597628 17.53469  11.433335
#> 4 25.01445 12.414485 45.68892  -3.614445
#> 5 28.50104  9.535786 53.95542  -9.801045
#> 6 32.37875 15.538596 58.20715 -14.278754
summary(f$residuals)
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -177.965  -17.858   -7.979  -17.704    7.004   26.321
```

## Predictions on New Data

``` r
new_X <- data.frame(
  wt = seq(min(X$wt), max(X$wt), length.out = 25),
  hp = stats::median(X$hp)
)

pred_mean <- predict(fit, x = new_X, type = "mean", cred.level = 0.90, interval = "credible")
pred_med  <- predict(fit, x = new_X, type = "median", cred.level = 0.90, interval = "credible")

head(pred_mean$fit)
#>    estimate    lower    upper
#> 1  5.923982 3.118031  9.11613
#> 2  6.733985 3.417078 10.21538
#> 3  7.585048 3.855476 11.89324
#> 4  8.662212 4.615547 13.33712
#> 5  9.794758 4.818015 16.57881
#> 6 11.239667 5.562320 18.24414
head(pred_med$fit)
#>   estimate index id    lower     upper
#> 1 3.413474   0.5  1 2.426814  4.544073
#> 2 3.909395   0.5  2 2.693864  5.385009
#> 3 4.481646   0.5  3 2.985334  6.418755
#> 4 5.143251   0.5  4 3.334559  7.601785
#> 5 5.909897   0.5  5 3.704631  8.986027
#> 6 6.800397   0.5  6 4.132824 10.510483
```

## Quantile Curves

``` r
q_levels <- c(0.1, 0.5, 0.9)
q_fits <- lapply(q_levels, function(tau) {
  predict(fit, x = new_X, type = "quantile", index = tau, cred.level = 0.90, interval = "credible")$fit
})

q_df <- do.call(rbind, Map(function(tau, df) {
  data.frame(wt = new_X$wt, tau = tau, estimate = df$estimate, lower = df$lower, upper = df$upper)
}, q_levels, q_fits))

head(q_df)
#>         wt tau   estimate      lower    upper
#> 1 1.513000 0.1 -0.2925464 -3.6660604 1.934948
#> 2 1.675958 0.1  0.2137438 -3.0245694 2.076944
#> 3 1.838917 0.1  0.6850905 -2.2964308 2.237707
#> 4 2.001875 0.1  1.1297091 -1.5130531 2.418173
#> 5 2.164833 0.1  1.5285199 -0.9637076 2.620008
#> 6 2.327792 0.1  1.8950226 -0.4024057 2.830299
```

``` r
ggplot(q_df, aes(x = wt, y = estimate, color = factor(tau))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(tau)), alpha = 0.2, color = NA) +
  labs(x = "Weight (wt)", y = "Predicted Quantile", color = "Quantile", fill = "Quantile",
       title = "Conditional Quantile Curves at Median Horsepower") +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](conditional_files/figure-html/unnamed-chunk-6-1.png)
