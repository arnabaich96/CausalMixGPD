# Internal prediction engine: evaluate per posterior draw, then summarize.

Project rules:

- density/survival: either provide both (x,y) or neither (defaults to
  training X and training y).

- quantile/sample/mean: y must be NULL; x may be provided (new X) or
  NULL (defaults to training X).

- CRP predictions use posterior weights derived from z for each draw.

- Stores per-draw results in object\$cache\$predict (environment) for
  reuse in treatment effects.

## Usage

``` r
.predict_mixgpd(
  object,
  x = NULL,
  y = NULL,
  ps = NULL,
  id = NULL,
  type = c("density", "survival", "quantile", "sample", "mean", "rmean", "median", "fit"),
  p = NULL,
  index = NULL,
  nsim = NULL,
  level = 0.95,
  interval = "credible",
  probs = c(0.025, 0.5, 0.975),
  store_draws = TRUE,
  nsim_mean = 200L,
  cutoff = NULL,
  ndraws_pred = NULL,
  chunk_size = NULL,
  show_progress = TRUE,
  ncores = 1L
)
```
