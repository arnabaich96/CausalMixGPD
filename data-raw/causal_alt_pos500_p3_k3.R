source("data-raw/_helpers.R")

set.seed(20102)
N <- 500
K_true <- 3
X <- data.frame(
  x1 = rnorm(N),
  x2 = runif(N, -1, 1),
  x3 = rnorm(N)
)
lin_ps <- 0.25 + 0.5 * X$x1 - 0.35 * X$x2
T <- rbinom(N, 1, stats::plogis(lin_ps))

weights0 <- c(0.4, 0.35, 0.25)
params0 <- list(
  list(meanlog = -0.2, sdlog = 0.35),
  list(meanlog = 0.3, sdlog = 0.45),
  list(meanlog = 0.8, sdlog = 0.55)
)
weights1 <- c(0.45, 0.3, 0.25)
params1 <- list(
  list(shape = 2.2, rate = 1.3),
  list(shape = 4.5, rate = 1.0),
  list(shape = 6.5, rate = 0.8)
)

y <- numeric(N)
idx0 <- which(T == 0L)
idx1 <- which(T == 1L)
y[idx0] <- mix_sample(length(idx0), weights0, rlnorm, params0)$y
y[idx1] <- mix_sample(length(idx1), weights1, rgamma, params1)$y

causal_alt_pos500_p3_k3 <- list(
  y = y,
  T = T,
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
    kernel0 = "lognormal",
    kernel1 = "gamma",
    params0 = list(weights = weights0,
                   meanlog = sapply(params0, `[[`, "meanlog"),
                   sdlog = sapply(params0, `[[`, "sdlog")),
    params1 = list(weights = weights1,
                   shape = sapply(params1, `[[`, "shape"),
                   rate = sapply(params1, `[[`, "rate")),
    tail_params = NULL
  )
)

usethis::use_data(causal_alt_pos500_p3_k3, overwrite = TRUE)
