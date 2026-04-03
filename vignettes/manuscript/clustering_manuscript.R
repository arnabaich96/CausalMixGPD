
  library(CausalMixGPD)

library(MASS)
mcmc_fixed <- list(
  niter = 1000,
  nburnin = 200,
  thin = 1,
  nchains = 1,
  seed = 2026,
  show_progress = TRUE,
  quiet = FALSE,
  waic = FALSE,
  timing = TRUE
)
set.seed(123)
data("Boston", package = "MASS")

dat <- Boston
dat <- subset(dat, select = c(medv, lstat, rm, nox))

n <- nrow(dat)
idx_train <- sample(seq_len(n), size = floor(0.80 * n), replace = FALSE)
train_dat <- dat[idx_train, ]
test_dat  <- dat[-idx_train, ]
fit_clust <- dpmix.cluster(
  formula    = medv ~ lstat + rm + nox,
  data       = train_dat,
  kernel     = "normal",
  type       = "weights",
  components = 10,
  mcmc       = mcmc_fixed
)
z_train_psm <- predict(fit_clust, type = "psm")
plot(z_train_psm, type = "summary")

z_train_lab <- predict(fit_clust, type = "label")
plot(z_train_lab, type = "summary")


z_test <- predict(
  fit_clust,
  newdata = test_dat,
  type    = "label"
)
summary(z_test)
plot(z_test, type = "size")
