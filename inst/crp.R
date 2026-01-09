devtools::load_all()
set.seed(1)
fast <- TRUE
N <- 100
X <- cbind(x1 = stats::rnorm(N), x2 = stats::rbinom(N, 1, 0.5), x3 = stats::runif(N, -1, 1))
y <- stats::rexp(N) + 0.1

mcmc_args <- if (isTRUE(fast)) {
  list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
} else {
  list(niter = 800, nburnin = 200, thin = 2, nchains = 3, seed = c(1, 3, 32))
}

bundle_crp <- build_nimble_bundle(
  y = y,
  X = X,
  backend = "crp",
  kernel = "invgauss",
  GPD = TRUE,
  components = N,
  mcmc = mcmc_args
)
# bundle S3
print(summary(bundle_crp))

print(bundle_crp, code = TRUE)



# fit (runner)
fit_crp_invgauss_gpd <- run_mcmc_bundle_manual(bundle_crp)
print(fit_crp_invgauss_gpd)
print(summary(fit_crp_invgauss_gpd))

class(fit_crp_invgauss_gpd$mcmc$samples)
cn <- colnames(as.matrix(fit_crp_invgauss_gpd$mcmc$samples))
head(cn, 40)

# Quick grep checks
grep("^beta_", cn, value = TRUE)
grep("tail|threshold|shape|scale", cn, value = TRUE)[1:30]
if (interactive()) {
  plot(fit_crp_invgauss_gpd,
       family = c("histogram","density","traceplot","running","compare_partial",
                  "autocorrelation","crosscorrelation","Rhat","grb","effective",
                  "geweke","caterpillar","pairs"),
       params = "beta_|threshold|tail_|weights\\[")
}

## ========== 1) density/survival with DEFAULT old X and old y ==========
fit <- fit_crp_invgauss_gpd

if (isTRUE(fast)) {
  y_grid <- seq(min(y), quantile(y, 0.99), length.out = 50)
  X_new <- X[1:3, , drop = FALSE]
  p_grid <- c(0.5, 0.9, 0.99)
  nsim_pred <- 50
} else {
  y_grid <- seq(min(y), quantile(y, 0.99), length.out = 80)
  X_new <- X[1:5, , drop = FALSE]
  p_grid <- c(0.25, 0.5, 0.75, 0.9, 0.95, 0.99)
  nsim_pred <- 200
}

# choose a new X (optional)
# density / survival with parallel (CDF = 1 - survival)
pr_den  <- predict(fit, x = X_new, y = y_grid, type = "density", ncores = 1)
pr_surv <- predict(fit, x = X_new, y = y_grid, type = "survival", ncores = 1)
pr_cdf  <- list(fit = 1 - pr_surv$fit)

# quantiles with parallel
pr_q <- predict(fit, x = X_new, type = "quantile",
                p = p_grid,
                ncores = 1)


str(pr_den)
str(pr_cdf)
str(pr_surv)

# sanity: dimensions should be (N x length(y_train)) typically N x N if y_train used as grid
dim(pr_den$fit)
dim(pr_cdf$fit)
dim(pr_surv$fit)

## ========== 2) density/survival with NEW (x,y) ==========
# new X: take 10 rows, jitter a bit
X_new <- X_new
X_new[,1] <- X_new[,1] + 0.2

# new y grid (finite, increasing grid usually nice)
y_grid <- if (isTRUE(fast)) {
  seq(min(y), quantile(y, 0.99), length.out = 40)
} else {
  seq(min(y), quantile(y, 0.99), length.out = 60)
}

pr_den_new  <- predict(fit, x = X_new, y = y_grid, type = "density")
pr_surv_new <- predict(fit, x = X_new, y = y_grid, type = "survival")
pr_cdf_new  <- list(fit = 1 - pr_surv_new$fit)

dim(pr_den_new$fit)   # 10 x 80
dim(pr_cdf_new$fit)   # 10 x 80
dim(pr_surv_new$fit)  # 10 x 80

# quick checks
range(pr_cdf_new$fit, na.rm = TRUE)     # should sit in [0,1]
range(pr_surv_new$fit, na.rm = TRUE)    # should sit in [0,1]

## ========== 3) quantile (y must be NULL), old X default ==========
pr_q_old <- predict(fit, type = "quantile", p = p_grid)
dim(pr_q_old$fit)  # N x length(p_grid)
head(pr_q_old$fit)

## ========== 4) quantile with NEW X (still y=NULL) ==========
pr_q_new <- predict(fit, x = X_new, type = "quantile", p = p_grid)
dim(pr_q_new$fit)  # 10 x length(p_grid)
pr_q_new$fit[1:3, ]

## ========== 5) sample (posterior predictive draws), old X default ==========
pr_samp_old <- predict(fit, type = "sample", nsim = nsim_pred)
dim(pr_samp_old$fit)  # N x 200
summary(as.vector(pr_samp_old$fit))

## ========== 6) sample with NEW X ==========
pr_samp_new <- predict(fit, x = X_new, type = "sample", nsim = nsim_pred)
dim(pr_samp_new$fit)  # 10 x 200
summary(as.vector(pr_samp_new$fit))

## ========== 7) mean (Monte Carlo mean per posterior draw), old X default ==========
# NOTE: mean is approximated via sampling internally (nsim_mean); you can pass nsim_mean if exposed.
nsim_mean <- if (isTRUE(fast)) 50 else 200
pr_mean_old <- predict(fit, type = "mean", nsim_mean = nsim_mean)
length(pr_mean_old$fit)  # N
head(pr_mean_old$fit)

## ========== 8) mean with NEW X ==========
pr_mean_new <- predict(fit, x = X_new, type = "mean", nsim_mean = nsim_mean)
length(pr_mean_new$fit)  # 10
pr_mean_new$fit

## ========== 9) credible intervals (optional) ==========
if (!isTRUE(fast)) {
  pr_q_new_ci <- predict(fit, x = X_new, type = "quantile", p = p_grid,
                         interval = "credible", probs = c(0.025, 0.5, 0.975))
  dim(pr_q_new_ci$fit)
  dim(pr_q_new_ci$lower)
  dim(pr_q_new_ci$upper)
}

## ========== 10) if draw-wise predictions are cached for treatment effect ==========
# If we stored draws in fit$cache$predict (as an environment),
# you can inspect keys like this:
if (!is.null(fit$cache) && is.environment(fit$cache) && !is.null(fit$cache$predict)) {
  ls(fit$cache$predict)
  # Example: pull one cached object
  # fit$cache$predict[[ ls(fit$cache$predict)[1] ]]
}
