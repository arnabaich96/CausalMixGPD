source("data-raw/_helpers.R")

set.seed(10301)
n <- 100
K_true <- 2
X <- data.frame(
  x1 = rnorm(n),
  x2 = runif(n, -1, 1),
  x3 = rnorm(n)
)
weights <- c(0.6, 0.4)
params <- list(
  list(mean = -0.5, sd = 0.9),
  list(mean = 1.8, sd = 1.1)
)

mix <- mix_sample(n, weights, rnorm, params)
y <- mix$y

nc_realX100_p3_k2 <- list(
  y = y,
  X = X,
  meta = list(
    n = n,
    support = "real",
    p = ncol(X),
    K_true = K_true,
    tail = FALSE,
    exceed_frac = 0,
    seed = 10301
  ),
  truth = list(
    kernel = "normal",
    weights = weights,
    params = list(mean = sapply(params, `[[`, "mean"),
                  sd = sapply(params, `[[`, "sd")),
    threshold = NULL,
    tail_params = NULL
  )
)

usethis::use_data(nc_realX100_p3_k2, overwrite = TRUE)
