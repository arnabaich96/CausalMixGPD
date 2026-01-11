source("data-raw/_helpers.R")

set.seed(10101)
n <- 200
K_true <- 2
weights <- c(0.6, 0.4)
params <- list(
  list(mean = -1.0, sd = 0.8),
  list(mean = 2.0, sd = 1.2)
)

mix <- mix_sample(n, weights, rnorm, params)
y <- mix$y

nc_real200_k2 <- list(
  y = y,
  X = NULL,
  meta = list(
    n = n,
    support = "real",
    p = 0L,
    K_true = K_true,
    tail = FALSE,
    exceed_frac = 0,
    seed = 10101
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

usethis::use_data(nc_real200_k2, overwrite = TRUE)
