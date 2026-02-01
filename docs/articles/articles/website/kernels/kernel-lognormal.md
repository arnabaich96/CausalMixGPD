# Lognormal

## Lognormal

### Lognormal mixture kernel

A lognormal component is defined by
$`\log Y \sim \mathcal{N}(\mu,\sigma^2)`$, i.e.,
``` math
f(y\mid\mu,\sigma)=\frac{1}{y\,\sigma\sqrt{2\pi}}\exp\!\left(-\frac{(\log y-\mu)^2}{2\sigma^2}\right),\quad y>0.
```

A finite lognormal mixture has density
``` math
f(y)=\sum_{j=1}^J w_j\,\text{Lognormal}(y\mid\mu_j,\sigma_j^2),\quad y>0.
```

**Parameter mapping (math $`\rightarrow`$ code):**
$`\mu_j\to`$`meanlog[j]`, $`\sigma_j\to`$`sdlog[j]`, $`w_j\to`$`w[j]`.

### Lognormal mixture with GPD tail

The `*MixGpd` variant uses the same splicing idea: lognormal mixture
below $`u`$, GPD tail above $`u`$.

**Tail mapping (math $`\rightarrow`$ code):** $`u\to`$`threshold`,
$`\sigma\to`$`tail_scale`, $`\xi\to`$`tail_shape`.

### Without GPD (mixture kernel)

``` r

grid <- seq(0, 8, length.out = 400)
logn_sets <- list(
  list(label = "Mix A", w = c(0.6, 0.3, 0.1), meanlog = c(0.0, 0.3, 0.6), sdlog = c(0.4, 0.5, 0.6)),
  list(label = "Mix B", w = c(0.5, 0.3, 0.2), meanlog = c(0.1, 0.4, 0.7), sdlog = c(0.35, 0.45, 0.55)),
  list(label = "Mix C", w = c(0.4, 0.35, 0.25), meanlog = c(0.2, 0.5, 2), sdlog = c(0.3, 0.4, 0.5))
)

example <- logn_sets[[1]]
```

``` r

dLognormalMix(1, w = example$w, meanlog = example$meanlog, sdlog = example$sdlog)
```

    [1] 0.839

``` r

dLognormalMix(1, w = example$w, meanlog = example$meanlog, sdlog = example$sdlog, log = TRUE)
```

    [1] -0.176

``` r

pLognormalMix(1, w = example$w, meanlog = example$meanlog, sdlog = example$sdlog)
```

    [1] 0.398

``` r

pLognormalMix(1, w = example$w, meanlog = example$meanlog, sdlog = example$sdlog, lower.tail = FALSE)
```

    [1] 0.602

``` r

pLognormalMix(1, w = example$w, meanlog = example$meanlog, sdlog = example$sdlog, log.p = TRUE)
```

    [1] -0.921

``` r

q_vec(qLognormalMix, c(0.25, 0.5, 0.75), w = example$w,
      meanlog = example$meanlog, sdlog = example$sdlog)
```

    [1] 0.828 1.128 1.575

``` r

q_vec(qLognormalMix, c(0.25, 0.5, 0.75), w = example$w,
      meanlog = example$meanlog, sdlog = example$sdlog, lower.tail = FALSE)
```

    [1] 1.575 1.128 0.828

``` r

q_vec(qLognormalMix, c(log(0.25), log(0.5), log(0.75)), w = example$w,
      meanlog = example$meanlog, sdlog = example$sdlog, log.p = TRUE)
```

    [1] 0.828 1.128 1.575

``` r

draw_many(rLognormalMix, list(w = example$w, meanlog = example$meanlog, sdlog = example$sdlog))
```

    [1] 0.878 1.104 2.337 0.720 1.165

``` r

df_logn <- do.call(rbind, lapply(logn_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dLognormalMix, list(w = ps$w, meanlog = ps$meanlog, sdlog = ps$sdlog)), label = ps$label)
}))

ggplot(df_logn, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Lognormal mixtures (bulk)", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")
```

![](kernel-lognormal_files/figure-html/lognormal-mix-plot-1.png)

### With GPD tail

``` r

logn_gpd_sets <- list(
  list(label = "Mix A", w = c(0.6, 0.4), meanlog = c(0, 1), sdlog = c(0.3, 0.5), threshold = 2.5, tail_scale = 0.5, tail_shape = 0.2),
  list(label = "Mix B", w = c(0.5, 0.5), meanlog = c(0.5, 1.2), sdlog = c(0.4, 0.4), threshold = 2.0, tail_scale = 0.4, tail_shape = 0.15),
  list(label = "Mix C", w = c(0.7, 0.3), meanlog = c(0.8, 1.5), sdlog = c(0.25, 0.6), threshold = 3.0, tail_scale = 0.6, tail_shape = 0.18)
)

example <- logn_gpd_sets[[1]]
```

``` r

dLognormalMixGpd(2.5, w = example$w, meanlog = example$meanlog, sdlog = example$sdlog, threshold = example$threshold, tail_scale = example$tail_scale, tail_shape = example$tail_shape)
```

    [1] 0.455

``` r

pLognormalMixGpd(2.5, w = example$w, meanlog = example$meanlog, sdlog = example$sdlog, threshold = example$threshold, tail_scale = example$tail_scale, tail_shape = example$tail_shape)
```

    [1] 0.773

``` r

q_vec(qLognormalMixGpd, c(0.5, 0.9), w = example$w, meanlog = example$meanlog, sdlog = example$sdlog, threshold = example$threshold, tail_scale = example$tail_scale, tail_shape = example$tail_shape)
```

    [1] 1.27 2.95

``` r

draw_many(rLognormalMixGpd, example)
```

    [1] 9.046 4.315 1.531 0.914 1.079

``` r

df_logn_gpd <- do.call(rbind, lapply(logn_gpd_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dLognormalMixGpd, list(w = ps$w, meanlog = ps$meanlog, sdlog = ps$sdlog, threshold = ps$threshold, tail_scale = ps$tail_scale, tail_shape = ps$tail_shape)), label = ps$label)
}))

ggplot(df_logn_gpd, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Lognormal mixtures with GPD tail (different thresholds)", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")
```

![](kernel-lognormal_files/figure-html/lognormal-gpd-plot-1.png)
