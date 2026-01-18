## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  fig.width = 8,
  fig.height = 6,
  message = FALSE,
  warning = FALSE,
  eval = TRUE,
  cache = TRUE,
  cache.path = "../.cache/vignettes/"
)
library(DPmixGPD)
if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(quiet = TRUE)
library(ggplot2)
library(gridExtra)
library(kableExtra)
library(dplyr)
library(tibble)
set.seed(123)

## ----data-setup---------------------------------------------------------------
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

ggplot(data.frame(y = y, x1 = X[, 1]), aes(x = x1, y = y)) +
  geom_point(alpha = 0.6, color = "darkorange") +
  geom_smooth(method = "loess", color = "navy", fill = NA) +
  labs(title = "y vs X1 (SB)", x = "X1", y = "y") +
  theme_minimal()

## ----data-table, echo=FALSE---------------------------------------------------
summary_tbl %>%
  mutate(value = signif(value, 4)) %>%
  kable(caption = "Conditional Dataset Summary (SB)", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "center")

## ----spec---------------------------------------------------------------------
bundle_sb_normal <- build_nimble_bundle(
  y = y,
  X = X,
  kernel = "normal",
  backend = "sb",
  GPD = FALSE,
  components = 5,
  mcmc = list(
    niter = 60,
    nburnin = 10,
    nchains = 2,
    thin = 1
  )
)

bundle_sb_cauchy <- build_nimble_bundle(
  y = y,
  X = X,
  kernel = "cauchy",
  backend = "sb",
  GPD = FALSE,
  components = 5,
  mcmc = list(
    niter = 60,
    nburnin = 10,
    nchains = 1,
    thin = 1
  )
)

## ----mcmc---------------------------------------------------------------------
fit_sb_normal <- run_mcmc_bundle_manual(bundle_sb_normal)
fit_sb_cauchy <- run_mcmc_bundle_manual(bundle_sb_cauchy)
summary(fit_sb_normal)
summary(fit_sb_cauchy)

## ----params-cond-sb-----------------------------------------------------------
params_sb <- params(fit_sb_normal)
params_sb

## ----cond-predict-------------------------------------------------------------
X_new <- expand.grid(x1 = seq(-2, 2, length.out = 3), x2 = c(-1, 1), x3 = 0)
colnames(X_new) <- colnames(X)
y_min <- max(min(y), .Machine$double.eps)
y_grid <- seq(y_min, max(y) * 1.1, length.out = 200)

densities_normal <- lapply(seq_len(nrow(X_new)), function(i) {
  pred <- predict(fit_sb_normal, x = as.matrix(X_new[i, , drop = FALSE]), y = y_grid, type = "density")
  data.frame(
    y = pred$fit$y,
    density = pred$fit$density,
    label = paste0("x1=", round(X_new[i, "x1"], 1), ", x2=", X_new[i, "x2"]),
    model = "Normal"
  )
})

densities_cauchy <- lapply(seq_len(nrow(X_new)), function(i) {
  pred <- predict(fit_sb_cauchy, x = as.matrix(X_new[i, , drop = FALSE]), y = y_grid, type = "density")
  data.frame(
    y = pred$fit$y,
    density = pred$fit$density,
    label = paste0("x1=", round(X_new[i, "x1"], 1), ", x2=", X_new[i, "x2"]),
    model = "Cauchy"
  )
})

df_cond <- bind_rows(densities_normal, densities_cauchy)

ggplot(df_cond, aes(x = y, y = density, color = label)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ model) +
  labs(title = "SB Conditional Predictive Densities", x = "y", y = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom")

