## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  fig.width = 7,
  fig.height = 4.5,
  fig.align = "center"
)
library(ggplot2)
library(kableExtra)
set.seed(1)
FAST <- TRUE
mcmc <- if (FAST) {
  list(niter = 600, nburnin = 150, thin = 2, nchains = 1, seed = 1)
} else {
  list(niter = 9000, nburnin = 2500, thin = 5, nchains = 2, seed = c(1, 2))
}

## -----------------------------------------------------------------------------
library(DPmixGPD)

data("mtcars", package = "datasets")
df <- mtcars
y <- df$mpg
X <- df[, c("wt", "hp")]
X <- as.data.frame(X)

## ----results='hide'-----------------------------------------------------------
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

## -----------------------------------------------------------------------------
f <- fitted(fit, type = "mean", level = 0.90)
head(f)
summary(f$residuals)

## -----------------------------------------------------------------------------
new_X <- data.frame(
  wt = seq(min(X$wt), max(X$wt), length.out = 25),
  hp = stats::median(X$hp)
)

pred_mean <- predict(fit, x = new_X, type = "mean", cred.level = 0.90, interval = "credible")
pred_med  <- predict(fit, x = new_X, type = "median", cred.level = 0.90, interval = "credible")

head(pred_mean$fit)
head(pred_med$fit)

## -----------------------------------------------------------------------------
q_levels <- c(0.1, 0.5, 0.9)
q_fits <- lapply(q_levels, function(tau) {
  predict(fit, x = new_X, type = "quantile", index = tau, cred.level = 0.90, interval = "credible")$fit
})

q_df <- do.call(rbind, Map(function(tau, df) {
  data.frame(wt = new_X$wt, tau = tau, estimate = df$estimate, lower = df$lower, upper = df$upper)
}, q_levels, q_fits))

head(q_df)

## -----------------------------------------------------------------------------
ggplot(q_df, aes(x = wt, y = estimate, color = factor(tau))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(tau)), alpha = 0.2, color = NA) +
  labs(x = "Weight (wt)", y = "Predicted Quantile", color = "Quantile", fill = "Quantile",
       title = "Conditional Quantile Curves at Median Horsepower") +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()

