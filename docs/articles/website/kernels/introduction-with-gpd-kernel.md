# Introduction with GPD kernel

## Introduction with GPD kernel

### Generalized Pareto distribution (standalone tail)

The standalone GPD functions `dGpd`, `pGpd`, `qGpd`, and `rGpd`
implement the distribution of $`X`$**above a threshold** $`u`$. For
$`x\ge u`$, the density is

``` math
f(x;u,\sigma,\xi)=\frac{1}{\sigma}\left(1+\xi\frac{x-u}{\sigma}\right)^{-1/\xi-1},
\quad \sigma>0,
\quad 1+\xi\frac{x-u}{\sigma}>0.
```

The CDF is
``` math
F(x)=1-\left(1+\xi\frac{x-u}{\sigma}\right)^{-1/\xi},
\quad x\ge u,
```
and the quantile is
``` math
Q(p)=u+\frac{\sigma}{\xi}\Big[(1-p)^{-\xi}-1\Big],\quad p\in(0,1),\ \xi\ne 0,
```
with the exponential-limit case obtained as $`\xi\to 0`$.

**Parameter mapping (math $`\rightarrow`$ code):** $`u\to`$`threshold`,
$`\sigma\to`$`scale`, $`\xi\to`$`shape`. Log-densities are returned when
`log = TRUE`.

We show the DPmixGPD kernels in pairs: bulk-only and GPD-augmented. A
GPD tail above threshold $`u`$ uses

``` math
f_{GPD}(x; u, \sigma, \xi) = \frac{1}{\sigma} \left(1 + \xi \frac{x-u}{\sigma}\right)^{-1/\xi - 1} \ \text{for } x \ge u.
```

The standalone GPD tail distribution is available via `dGpd`, `pGpd`,
`qGpd`, and `rGpd` functions.

``` r

dGpd(1.8, threshold = 1.5, scale = 0.5, shape = 0.2)
```

    [1] 1.01

``` r

dGpd(1.8, threshold = 1.5, scale = 0.5, shape = 0.2, log = TRUE)
```

    [1] 0.0132

``` r

pGpd(1.8, threshold = 1.5, scale = 0.5, shape = 0.2)
```

    [1] 0.433

``` r

pGpd(1.8, threshold = 1.5, scale = 0.5, shape = 0.2, lower.tail = FALSE)
```

    [1] 0.567

``` r

pGpd(1.8, threshold = 1.5, scale = 0.5, shape = 0.2, log.p = TRUE)
```

    [1] -0.838

``` r

q_vec(qGpd, c(0.25, 0.5, 0.75), threshold = 1.5, scale = 0.5, shape = 0.2)
```

    [1] 1.65 1.87 2.30

``` r

q_vec(qGpd, c(0.25, 0.5, 0.75), threshold = 1.5, scale = 0.5, shape = 0.2,
      lower.tail = FALSE)
```

    [1] 2.30 1.87 1.65

``` r

q_vec(qGpd, c(log(0.25), log(0.5), log(0.75)), threshold = 1.5, scale = 0.5,
      shape = 0.2, log.p = TRUE)
```

    [1] 1.65 1.87 2.30

``` r

draw_many(rGpd, list(threshold = 1.5, scale = 0.5, shape = 0.2))
```

    [1] 1.66 1.74 1.96 3.03 1.62

``` r

grid <- seq(-4, 15, length.out = 500)
gpd_sets <- list(
  list(label = "GPD A", threshold = 1.5, tail_scale = 0.5, tail_shape = 0.2),
  list(label = "GPD B", threshold = 2.0, tail_scale = 0.6, tail_shape = 0.15),
  list(label = "GPD C", threshold = 1.0, tail_scale = 0.4, tail_shape = 0.25)
)

df_gpd <- do.call(rbind, lapply(gpd_sets, function(ps) {
  data.frame(x = grid, density = density_curve(grid, dGpd, list(threshold = ps$threshold, scale = ps$tail_scale, shape = ps$tail_shape)), label = ps$label)
}))

ggplot(df_gpd, aes(x = x, y = density, color = label)) +
  geom_line(linewidth = 1) +
  labs(title = "Standalone GPD tails", x = "x", y = "density") +
  theme_minimal() + theme(legend.position = "top")
```

![](introduction-with-gpd-kernel_files/figure-html/gpd-plot-1.png)

Each subsection below defines the available $`d/p/q/r`$ functions,
prints example outputs for fixed parameters, and overlays density curves
for **three parameter sets** with clear legends. The same parameter sets
are reused within the bulk and GPD variants of a section.
