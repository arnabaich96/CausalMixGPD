source("data-raw/_helpers.R")

set.seed(20103)
N <- 500
K_true <- 2
X <- data.frame(
  x1 = rnorm(N),
  x2 = runif(N, -1, 1),
  x3 = rnorm(N),
  x4 = runif(N)
)
lin_ps <- 0.15 + 0.45 * X$x1 - 0.25 * X$x3
A <- rbinom(N, 1, stats::plogis(lin_ps))

weights0 <- c(0.6, 0.4)
params0 <- list(
  list(mean = -0.8, sd = 0.9),
  list(mean = 1.6, sd = 1.2)
)
weights1 <- c(0.55, 0.45)
params1 <- list(
  list(location = -0.5, scale = 0.9),
  list(location = 1.4, scale = 1.1)
)

y <- numeric(N)
idx0 <- which(A == 0L)
idx1 <- which(A == 1L)
y[idx0] <- mix_sample(length(idx0), weights0, rnorm, params0)$y
y[idx1] <- mix_sample(length(idx1), weights1, rlaplace, params1)$y

causal_alt_real500_p4_k2 <- list(
  y = y,
  A = A,
  X = X,
  meta = list(
    N = N,
    support = "real",
    p = ncol(X),
    K0 = K_true,
    K1 = K_true,
    tail = FALSE,
    exceed_frac = 0
  ),
  truth = list(
    kernel0 = "normal",
    kernel1 = "laplace",
    params0 = list(weights = weights0,
                   mean = sapply(params0, `[[`, "mean"),
                   sd = sapply(params0, `[[`, "sd")),
    params1 = list(weights = weights1,
                   location = sapply(params1, `[[`, "location"),
                   scale = sapply(params1, `[[`, "scale")),
    tail_params = NULL
  )
)

usethis::use_data(causal_alt_real500_p4_k2, overwrite = TRUE)
