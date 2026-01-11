source("data-raw/_helpers.R")

set.seed(10203)
n <- 100
K_true <- 4
X <- data.frame(
  x1 = rnorm(n),
  x2 = runif(n, -1, 1),
  x3 = rnorm(n),
  x4 = runif(n),
  x5 = rnorm(n, 0.5, 1.2)
)
weights <- c(0.3, 0.25, 0.25, 0.2)
params <- list(
  list(mean = 1.2, shape = 3.0),
  list(mean = 1.8, shape = 4.5),
  list(mean = 2.4, shape = 6.0),
  list(mean = 3.0, shape = 7.0)
)

mix <- mix_sample(n, weights, rInvGauss, params)
y <- mix$y

nc_posX100_p5_k4 <- list(
  y = y,
  X = X,
  meta = list(
    n = n,
    support = "positive",
    p = ncol(X),
    K_true = K_true,
    tail = FALSE,
    exceed_frac = 0,
    seed = 10203
  ),
  truth = list(
    kernel = "invgauss",
    weights = weights,
    params = list(mean = sapply(params, `[[`, "mean"),
                  shape = sapply(params, `[[`, "shape")),
    threshold = NULL,
    tail_params = NULL
  )
)

usethis::use_data(nc_posX100_p5_k4, overwrite = TRUE)
