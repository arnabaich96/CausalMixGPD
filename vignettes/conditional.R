## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
<<<<<<< HEAD
<<<<<<< HEAD
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 7,
  fig.height = 4.5
)
set.seed(1)
FAST <- TRUE
mcmc <- if (FAST) {
  list(niter = 600, nburnin = 150, thin = 2, nchains = 1, seed = 1)
} else {
  list(niter = 9000, nburnin = 2500, thin = 5, nchains = 2, seed = c(1, 2))
}

## -----------------------------------------------------------------------------
library(DPmixGPD)

n <- 120
x <- rnorm(n)
X <- data.frame(x = x)

# simple signal + heavy-ish noise
y <- 0.5 + 0.8 * x + abs(rnorm(n)) + 0.1

## -----------------------------------------------------------------------------
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
fit

## -----------------------------------------------------------------------------
f <- fitted(fit, type = "mean", level = 0.90)
head(f)

# residual summary
summary(f$residuals)

## -----------------------------------------------------------------------------
new_X <- data.frame(x = seq(min(x), max(x), length.out = 25))

pred_mean <- predict(fit, x = new_X, type = "mean", cred.level = 0.90, interval = "credible")
pred_med  <- predict(fit, x = new_X, type = "median", cred.level = 0.90, interval = "credible")

head(pred_mean$fit)
head(pred_med$fit)

## -----------------------------------------------------------------------------
q_levels <- c(0.1, 0.5, 0.9)
q_fits <- lapply(q_levels, function(tau) {
  predict(fit, x = new_X, type = "quantile", index = tau, cred.level = 0.90, interval = "credible")$fit
})

# Bind into a simple long data.frame for plotting
q_df <- do.call(rbind, Map(function(tau, df) {
  data.frame(x = new_X$x, tau = tau, estimate = df$estimate, lower = df$lower, upper = df$upper)
}, q_levels, q_fits))

head(q_df)

## -----------------------------------------------------------------------------
# Minimal base plotting to avoid extra dependencies
plot(q_df$x[q_df$tau == 0.5], q_df$estimate[q_df$tau == 0.5], type = "l",
     xlab = "x", ylab = "Predicted quantile", main = "Conditional quantile curves")
lines(q_df$x[q_df$tau == 0.1], q_df$estimate[q_df$tau == 0.1])
lines(q_df$x[q_df$tau == 0.9], q_df$estimate[q_df$tau == 0.9])
legend("topleft", legend = paste0("tau=", q_levels), lty = 1, bty = "n")

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()
=======
=======
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090
  comment = NA,
  fig.width = 7,
  fig.height = 4.5,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)
library(DPmixGPD)

## ----data---------------------------------------------------------------------
# data("nc_posX100_p3_k2")
# X <- as.matrix(nc_posX100_p3_k2$X)
# y <- nc_posX100_p3_k2$y

## ----cond---------------------------------------------------------------------
# bundle_cond <- build_nimble_bundle(
#   y = y,
#   X = X,
#   kernel = "lognormal",
#   backend = "sb",
#   GPD = TRUE,
#   J = 4,
#   mcmc = list(niter = 250, nburnin = 50, nchains = 1)
# )
# fit_cond <- run_mcmc_bundle_manual(bundle_cond, show_progress = FALSE)

## ----cond-pred----------------------------------------------------------------
# x_new <- X[1:10, , drop = FALSE]
# q_pred <- predict(fit_cond, x = x_new, type = "quantile", index = c(0.1, 0.5, 0.9))
# plot(q_pred)
<<<<<<< HEAD
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090
=======
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090

