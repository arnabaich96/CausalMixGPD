source("data-raw/_helpers.R")

set.seed(10201)
n <- 100
K_true <- 2
X <- data.frame(
  x1 = rnorm(n),
  x2 = runif(n, -1, 1),
  x3 = rnorm(n)
)
weights <- c(0.55, 0.45)
params <- list(
  list(shape = 2.5, rate = 1.2),
  list(shape = 5.0, rate = 0.9)
)

mix <- mix_sample(n, weights, rgamma, params)
y <- mix$y

nc_posX100_p3_k2 <- list(
  y = y,
  X = X,
  meta = list(
    n = n,
    support = "positive",
    p = ncol(X),
    K_true = K_true,
    tail = FALSE,
    exceed_frac = 0,
    seed = 10201
  ),
  truth = list(
    kernel = "gamma",
    weights = weights,
    params = list(shape = sapply(params, `[[`, "shape"),
                  rate = sapply(params, `[[`, "rate")),
    threshold = NULL,
    tail_params = NULL
  )
)

usethis::use_data(nc_posX100_p3_k2, overwrite = TRUE)
