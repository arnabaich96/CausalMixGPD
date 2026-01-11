source("data-raw/_helpers.R")

set.seed(10302)
n <- 100
K_true <- 3
X <- data.frame(
  x1 = rnorm(n),
  x2 = runif(n, -1, 1),
  x3 = rnorm(n),
  x4 = runif(n),
  x5 = rnorm(n, 0.2, 1.1)
)
weights <- c(0.45, 0.35, 0.2)
params <- list(
  list(location = -1.0, scale = 0.7),
  list(location = 0.5, scale = 1.0),
  list(location = 2.0, scale = 1.3)
)

mix <- mix_sample(n, weights, rlaplace, params)
y <- mix$y

nc_realX100_p5_k3 <- list(
  y = y,
  X = X,
  meta = list(
    n = n,
    support = "real",
    p = ncol(X),
    K_true = K_true,
    tail = FALSE,
    exceed_frac = 0,
    seed = 10302
  ),
  truth = list(
    kernel = "laplace",
    weights = weights,
    params = list(location = sapply(params, `[[`, "location"),
                  scale = sapply(params, `[[`, "scale")),
    threshold = NULL,
    tail_params = NULL
  )
)

usethis::use_data(nc_realX100_p5_k3, overwrite = TRUE)
