# 10. Conditional DPmix with CRP Backend

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

## Conditional DPmix: CRP Backend with Covariates

**Purpose**: Show how the CRP backend can model $`y | X`$ via a
covariate-dependent Dirichlet Process mixture. This vignette parallels
`v04` but includes $`X`$ so we can explore conditional predictions.

------------------------------------------------------------------------

### Data Setup

``` r

data("nc_posX100_p3_k2")
y <- nc_posX100_p3_k2$y
X <- as.matrix(nc_posX100_p3_k2$X)
if (is.null(colnames(X))) {
  colnames(X) <- paste0("x", seq_len(ncol(X)))
}

summary_tbl <- tibble(
  statistic = c("N", "Mean", "SD", "Min", "Max"),
  value = c(length(y), mean(y), sd(y), min(y), max(y))
)

df_cov <- data.frame(y = y, x1 = X[, 1], x2 = X[, 2])

p_cov <- ggplot(df_cov, aes(x = x1, y = y)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "loess", color = "firebrick", fill = NA) +
  labs(title = "y vs X1 (loess smoother)", x = "X1", y = "y") +
  theme_minimal()

grid.arrange(p_cov, ncol = 1)
```

![](articles/workflows/legacy-cache/figure-htmldata-setup-1.png)

| statistic |  value  |
|:---------:|:-------:|
|     N     | 100.000 |
|   Mean    |  3.454  |
|    SD     |  2.406  |
|    Min    |  0.377  |
|    Max    | 10.870  |

Summary of Conditional Dataset {.table .table .table-striped
.table-hover
style="width: auto !important; margin-left: auto; margin-right: auto;"}

------------------------------------------------------------------------

### Model Specification & Bundle

``` r

bundle_cond_normal <- build_nimble_bundle(
  y = y,
  X = X,
  kernel = "normal",
  backend = "crp",
  GPD = FALSE,
  components = 5,
  mcmc = mcmc
)

bundle_cond_amoroso <- build_nimble_bundle(
  y = y,
  X = X,
  kernel = "amoroso",
  backend = "crp",
  GPD = FALSE,
  components = 5,
  mcmc = mcmc
)
```

------------------------------------------------------------------------

### Running MCMC

``` r

fit_cond_normal <- load_or_fit("v10-conditional-DPmix-CRP-fit_cond_normal", run_mcmc_bundle_manual(bundle_cond_normal))
fit_cond_amoroso <- load_or_fit("v10-conditional-DPmix-CRP-fit_cond_amoroso", run_mcmc_bundle_manual(bundle_cond_amoroso))
summary(fit_cond_normal)
```

    MixGPD summary | backend: Chinese Restaurant Process | kernel: Normal Distribution | GPD tail: FALSE | epsilon: 0.025
    n = 100 | components = 5
    Summary
    Initial components: 5 | Components after truncation: 2

    WAIC: 360.041
    lppd: -143.749 | pWAIC: 36.271

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
       <td style="text-align:center;"> 0.524 </td>
       <td style="text-align:center;"> 0.091 </td>
       <td style="text-align:center;"> 0.34 </td>
       <td style="text-align:center;"> 0.53 </td>
       <td style="text-align:center;"> 0.69 </td>
       <td style="text-align:center;"> 151.15 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> weights[2] </td>
       <td style="text-align:center;"> 0.368 </td>
       <td style="text-align:center;"> 0.078 </td>
       <td style="text-align:center;"> 0.21 </td>
       <td style="text-align:center;"> 0.37 </td>
       <td style="text-align:center;"> 0.49 </td>
       <td style="text-align:center;"> 186.257 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> alpha </td>
       <td style="text-align:center;"> 0.599 </td>
       <td style="text-align:center;"> 0.427 </td>
       <td style="text-align:center;"> 0.062 </td>
       <td style="text-align:center;"> 0.501 </td>
       <td style="text-align:center;"> 1.645 </td>
       <td style="text-align:center;"> 583.078 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> mean[1] </td>
       <td style="text-align:center;"> 2.685 </td>
       <td style="text-align:center;"> 1.435 </td>
       <td style="text-align:center;"> 1.427 </td>
       <td style="text-align:center;"> 1.976 </td>
       <td style="text-align:center;"> 5.834 </td>
       <td style="text-align:center;"> 520.913 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> mean[2] </td>
       <td style="text-align:center;"> 4.609 </td>
       <td style="text-align:center;"> 1.894 </td>
       <td style="text-align:center;"> 1.298 </td>
       <td style="text-align:center;"> 5.417 </td>
       <td style="text-align:center;"> 7.019 </td>
       <td style="text-align:center;"> 506.123 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> sd[1] </td>
       <td style="text-align:center;"> 1.157 </td>
       <td style="text-align:center;"> 0.714 </td>
       <td style="text-align:center;"> 0.149 </td>
       <td style="text-align:center;"> 1.182 </td>
       <td style="text-align:center;"> 2.782 </td>
       <td style="text-align:center;"> 562.406 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> sd[2] </td>
       <td style="text-align:center;"> 0.811 </td>
       <td style="text-align:center;"> 0.925 </td>
       <td style="text-align:center;"> 0.149 </td>
       <td style="text-align:center;"> 0.341 </td>
       <td style="text-align:center;"> 3.418 </td>
       <td style="text-align:center;"> 451.317 </td>
      </tr>
    </tbody>
    </table>

``` r

summary(fit_cond_amoroso)
```

    MixGPD summary | backend: Chinese Restaurant Process | kernel: Amoroso Distribution | GPD tail: FALSE | epsilon: 0.025
    n = 100 | components = 5
    Summary
    Initial components: 5 | Components after truncation: 1

    WAIC: 399.181
    lppd: -170.578 | pWAIC: 29.013

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
       <td style="text-align:center;"> 0.81 </td>
       <td style="text-align:center;"> 0.198 </td>
       <td style="text-align:center;"> 0.45 </td>
       <td style="text-align:center;"> 0.88 </td>
       <td style="text-align:center;"> 1 </td>
       <td style="text-align:center;"> 21.07 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> alpha </td>
       <td style="text-align:center;"> 0.396 </td>
       <td style="text-align:center;"> 0.357 </td>
       <td style="text-align:center;"> 0.009 </td>
       <td style="text-align:center;"> 0.31 </td>
       <td style="text-align:center;"> 1.305 </td>
       <td style="text-align:center;"> 241.632 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> loc[1] </td>
       <td style="text-align:center;"> 0.065 </td>
       <td style="text-align:center;"> 0.734 </td>
       <td style="text-align:center;"> -2.358 </td>
       <td style="text-align:center;"> 0.255 </td>
       <td style="text-align:center;"> 0.784 </td>
       <td style="text-align:center;"> 151.27 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> scale[1] </td>
       <td style="text-align:center;"> 2.376 </td>
       <td style="text-align:center;"> 0.882 </td>
       <td style="text-align:center;"> 0.993 </td>
       <td style="text-align:center;"> 2.249 </td>
       <td style="text-align:center;"> 4.335 </td>
       <td style="text-align:center;"> 49.281 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> shape1[1] </td>
       <td style="text-align:center;"> 1.603 </td>
       <td style="text-align:center;"> 0.871 </td>
       <td style="text-align:center;"> 0.457 </td>
       <td style="text-align:center;"> 1.44 </td>
       <td style="text-align:center;"> 4.083 </td>
       <td style="text-align:center;"> 159.78 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> shape2[1] </td>
       <td style="text-align:center;"> 1.386 </td>
       <td style="text-align:center;"> 0.708 </td>
       <td style="text-align:center;"> 0.788 </td>
       <td style="text-align:center;"> 1.191 </td>
       <td style="text-align:center;"> 3.319 </td>
       <td style="text-align:center;"> 24.708 </td>
      </tr>
    </tbody>
    </table>

``` r

params_cond <- params(fit_cond_normal)
params_cond
```

    Posterior mean parameters

    $alpha
    [1] "0.599"

    $w
    [1] "0.524" "0.368"

    $mean
    [1] "2.685" "4.609"

    $sd
    [1] "1.157" "0.811"

------------------------------------------------------------------------

### Conditional Predictions

``` r

X_new <- expand.grid(
  x1 = seq(-2, 2, length.out = 3),
  x2 = c(0, 1),
  x3 = 0
)
colnames(X_new) <- colnames(X)

y_grid <- seq(-1, 10, length.out = 200)
densities_normal <- lapply(seq_len(nrow(X_new)), function(i) {
  pred <- predict(fit_cond_normal, x = as.matrix(X_new[i, , drop = FALSE]), y = y_grid, type = "density")
  data.frame(
    y = pred$fit$y,
    density = pred$fit$density,
    group = paste0("x1=", round(X_new[i, "x1"], 1), ", x2=", X_new[i, "x2"]),
    model = "Normal"
  )
})

densities_amoroso <- lapply(seq_len(nrow(X_new)), function(i) {
  pred <- predict(fit_cond_amoroso, x = as.matrix(X_new[i, , drop = FALSE]), y = y_grid, type = "density")
  data.frame(
    y = pred$fit$y,
    density = pred$fit$density,
    group = paste0("x1=", round(X_new[i, "x1"], 1), ", x2=", X_new[i, "x2"]),
    model = "Amoroso (shape1=1)"
  )
})

df_dens <- bind_rows(densities_normal, densities_amoroso)

ggplot(df_dens, aes(x = y, y = density, color = group)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ model) +
  labs(title = "Conditional Predictive Densities", x = "y", y = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](articles/workflows/legacy-cache/figure-htmlcond-predict-1.png)

------------------------------------------------------------------------

### Covariate Effect on Conditional Quantiles

``` r

X_grid <- cbind(
  x1 = seq(-2, 2, length.out = 5),
  x2 = 0,
  x3 = 0
)
colnames(X_grid) <- colnames(X)

quant_probs <- c(0.25, 0.5, 0.75)
pred_q_normal <- predict(fit_cond_normal, x = as.matrix(X_grid), type = "quantile", index = quant_probs)
pred_q_amoroso <- predict(fit_cond_amoroso, x = as.matrix(X_grid), type = "quantile", index = quant_probs)

quant_df_normal <- pred_q_normal$fit
quant_df_normal$x1 <- X_grid[quant_df_normal$id, "x1"]
quant_df_normal$model <- "Normal"

quant_df_amoroso <- pred_q_amoroso$fit
quant_df_amoroso$x1 <- X_grid[quant_df_amoroso$id, "x1"]
quant_df_amoroso$model <- "Amoroso (shape1=1)"

bind_rows(quant_df_normal, quant_df_amoroso) %>%
  ggplot(aes(x = x1, y = estimate, color = factor(index), group = index)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ model) +
  labs(title = "Conditional Quantiles vs x1 (x2=0)", x = "x1", y = "y", color = "Quantile") +
  theme_minimal()
```

![](articles/workflows/legacy-cache/figure-htmlquantile-effect-1.png)

------------------------------------------------------------------------

### Residuals & Diagnostics

``` r

fit_vals <- fitted(fit_cond_normal)
if (interactive()) plot(fit_vals)
```

``` r

if (interactive()) plot(fit_cond_normal, family = c("traceplot", "autocorrelation", "geweke"))
if (interactive()) plot(fit_cond_amoroso, family = c("density", "running", "caterpillar"))
```

------------------------------------------------------------------------

### Takeaways

- Covariate-informed DP mixtures predict outcome distributions that
  shift with `x1` (and other covariates).
- Use `predict(..., type = "density")` to visualize conditional
  densities and `type = "quantile"` for posterior-mean location shifts.
- Diagnostics (`if (interactive()) plot(fit_cond_normal)`,
  `fitted(fit_cond_normal)`) ensure the chains mix before relying on
  predictions.
- Next vignette extends the same idea to the SB backend before adding
  tails.
