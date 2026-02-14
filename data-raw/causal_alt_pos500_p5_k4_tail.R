source("data-raw/_helpers.R")

set.seed(20104)
N <- 500
K_true <- 4
X <- data.frame(
  x1 = rnorm(N),
  x2 = runif(N, -1, 1),
  x3 = rnorm(N),
  x4 = runif(N),
  x5 = rnorm(N, 0.4, 1.1)
)
lin_ps <- 0.2 + 0.35 * X$x1 - 0.2 * X$x2 + 0.15 * X$x5
A <- rbinom(N, 1, stats::plogis(lin_ps))

weights0 <- c(0.3, 0.25, 0.25, 0.2)
params0 <- list(
  list(mean = 1.1, shape = 3.5),
  list(mean = 1.6, shape = 4.5),
  list(mean = 2.2, shape = 5.5),
  list(mean = 2.9, shape = 6.5)
)
weights1 <- c(0.28, 0.26, 0.25, 0.21)
params1 <- list(
  list(loc = 0.2, scale = 0.9, shape1 = 1, shape2 = 1.4),
  list(loc = 0.3, scale = 1.1, shape1 = 1, shape2 = 1.2),
  list(loc = 0.5, scale = 1.3, shape1 = 1, shape2 = 1.1),
  list(loc = 0.7, scale = 1.6, shape1 = 1, shape2 = 1.0)
)

y <- numeric(N)
idx0 <- which(A == 0L)
idx1 <- which(A == 1L)

bulk0 <- mix_sample(length(idx0), weights0, rInvGauss, params0)$y
bulk1 <- mix_sample(length(idx1), weights1, rAmoroso, params1)$y

tail_frac <- 0.12
tail_scale <- 2.0
tail_shape <- 0.18
tail_out0 <- tail_design(bulk0, tail_frac = tail_frac, tail_scale = tail_scale, tail_shape = tail_shape)
tail_out1 <- tail_design(bulk1, tail_frac = tail_frac, tail_scale = tail_scale, tail_shape = tail_shape)

y[idx0] <- tail_out0$y
y[idx1] <- tail_out1$y
exceed_frac <- (tail_out0$exceed_frac * length(idx0) + tail_out1$exceed_frac * length(idx1)) / N

causal_alt_pos500_p5_k4_tail <- list(
  y = y,
  A = A,
  X = X,
  meta = list(
    N = N,
    support = "positive",
    p = ncol(X),
    K0 = K_true,
    K1 = K_true,
    tail = TRUE,
    exceed_frac = exceed_frac
  ),
  truth = list(
    kernel0 = "invgauss",
    kernel1 = "amoroso",
    params0 = list(weights = weights0,
                   mean = sapply(params0, `[[`, "mean"),
                   shape = sapply(params0, `[[`, "shape")),
    params1 = list(weights = weights1,
                   loc = sapply(params1, `[[`, "loc"),
                   scale = sapply(params1, `[[`, "scale"),
                   shape1 = sapply(params1, `[[`, "shape1"),
                   shape2 = sapply(params1, `[[`, "shape2")),
    tail_params = list(
      threshold0 = tail_out0$threshold,
      threshold1 = tail_out1$threshold,
      scale = tail_scale,
      shape = tail_shape
    )
  )
)

usethis::use_data(causal_alt_pos500_p5_k4_tail, overwrite = TRUE)
