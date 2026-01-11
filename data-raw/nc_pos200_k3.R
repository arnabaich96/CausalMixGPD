source("data-raw/_helpers.R")

set.seed(10102)
n <- 200
K_true <- 3
weights <- c(0.4, 0.35, 0.25)
params <- list(
  list(shape = 2.0, rate = 1.5),
  list(shape = 4.0, rate = 1.0),
  list(shape = 7.0, rate = 0.7)
)

mix <- mix_sample(n, weights, rgamma, params)
y <- mix$y

nc_pos200_k3 <- list(
  y = y,
  X = NULL,
  meta = list(
    n = n,
    support = "positive",
    p = 0L,
    K_true = K_true,
    tail = FALSE,
    exceed_frac = 0,
    seed = 10102
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

usethis::use_data(nc_pos200_k3, overwrite = TRUE)
