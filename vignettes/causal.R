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
  list(niter = 700, nburnin = 200, thin = 2, nchains = 1, seed = 1)
} else {
  list(niter = 12000, nburnin = 3000, thin = 5, nchains = 2, seed = c(1, 2))
}

## -----------------------------------------------------------------------------
library(DPmixGPD)

n <- 160
x <- rnorm(n)
X <- data.frame(x = x)

# treatment assignment (no propensity score used here)
a <- rbinom(n, 1, 0.5)

# outcome with heterogeneous effect + heavy-ish noise
te <- 0.4 + 0.6 * (x > 0)
y0 <- 0.5 + 0.7 * x + abs(rnorm(n)) + 0.1
y1 <- y0 + te

y <- ifelse(a == 1, y1, y0)

## -----------------------------------------------------------------------------
X0 <- X[a == 0, , drop = FALSE]
X1 <- X[a == 1, , drop = FALSE]
y0_obs <- y[a == 0]
y1_obs <- y[a == 1]

bundle0 <- build_nimble_bundle(
  y = y0_obs,
  X = X0,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)

bundle1 <- build_nimble_bundle(
  y = y1_obs,
  X = X1,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)

fit0 <- run_mcmc_bundle_manual(bundle0, show_progress = FALSE)
fit1 <- run_mcmc_bundle_manual(bundle1, show_progress = FALSE)

## -----------------------------------------------------------------------------
new_X <- data.frame(x = seq(min(x), max(x), length.out = 25))

taus <- c(0.1, 0.5, 0.9)

cqte_list <- lapply(taus, function(tau) {
  q0 <- predict(fit0, x = new_X, type = "quantile", index = tau, cred.level = 0.90, interval = "credible")$fit
  q1 <- predict(fit1, x = new_X, type = "quantile", index = tau, cred.level = 0.90, interval = "credible")$fit

  data.frame(
    x = new_X$x,
    tau = tau,
    estimate = q1$estimate - q0$estimate,
    lower = q1$lower - q0$upper,
    upper = q1$upper - q0$lower
  )
})

cqte <- do.call(rbind, cqte_list)
head(cqte)

## -----------------------------------------------------------------------------
plot(cqte$x[cqte$tau == 0.5], cqte$estimate[cqte$tau == 0.5], type = "l",
     xlab = "x", ylab = "CQTE", main = "Conditional quantile treatment effect")
lines(cqte$x[cqte$tau == 0.1], cqte$estimate[cqte$tau == 0.1])
lines(cqte$x[cqte$tau == 0.9], cqte$estimate[cqte$tau == 0.9])
legend("topleft", legend = paste0("tau=", taus), lty = 1, bty = "n")

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()
=======
=======
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090
  comment = NA,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)
library(DPmixGPD)

## ----causal-outline-----------------------------------------------------------
# # Pseudocode only (not executed)
# fit0 <- build_nimble_bundle(y = y0, X = X0, kernel = "gamma", GPD = TRUE)
# fit1 <- build_nimble_bundle(y = y1, X = X1, kernel = "gamma", GPD = TRUE)
# # run MCMC for each, then compare predictive distributions
<<<<<<< HEAD
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090
=======
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090

