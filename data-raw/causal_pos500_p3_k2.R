source("data-raw/_helpers.R")

set.seed(20101)
N <- 500
K_true <- 2
X <- data.frame(
  x1 = rnorm(N),
  x2 = runif(N, -1, 1),
  x3 = rnorm(N)
)
lin_ps <- 0.3 + 0.4 * X$x1 - 0.3 * X$x2
A <- rbinom(N, 1, stats::plogis(lin_ps))

weights0 <- c(0.6, 0.4)
params0 <- list(
  list(shape = 2.0, rate = 1.4),
  list(shape = 4.0, rate = 1.0)
)
weights1 <- c(0.5, 0.5)
params1 <- list(
  list(shape = 2.5, rate = 1.2),
  list(shape = 5.5, rate = 0.9)
)

y <- numeric(N)
idx0 <- which(A == 0L)
idx1 <- which(A == 1L)
y[idx0] <- mix_sample(length(idx0), weights0, rgamma, params0)$y
y[idx1] <- mix_sample(length(idx1), weights1, rgamma, params1)$y

causal_pos500_p3_k2 <- list(
  y = y,
  A = A,
  X = X,
  meta = list(
    N = N,
    support = "positive",
    p = ncol(X),
    K0 = K_true,
    K1 = K_true,
    tail = FALSE,
    exceed_frac = 0
  ),
  truth = list(
    kernel0 = "gamma",
    kernel1 = "gamma",
    params0 = list(weights = weights0,
                   shape = sapply(params0, `[[`, "shape"),
                   rate = sapply(params0, `[[`, "rate")),
    params1 = list(weights = weights1,
                   shape = sapply(params1, `[[`, "shape"),
                   rate = sapply(params1, `[[`, "rate")),
    tail_params = NULL
  )
)

usethis::use_data(causal_pos500_p3_k2, overwrite = TRUE)
