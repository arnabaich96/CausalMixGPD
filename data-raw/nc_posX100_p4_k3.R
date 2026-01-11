source("data-raw/_helpers.R")

set.seed(10202)
n <- 100
K_true <- 3
X <- data.frame(
  x1 = rnorm(n),
  x2 = runif(n, -1, 1),
  x3 = rnorm(n),
  x4 = runif(n)
)
weights <- c(0.4, 0.35, 0.25)
params <- list(
  list(meanlog = -0.1, sdlog = 0.35),
  list(meanlog = 0.4, sdlog = 0.45),
  list(meanlog = 0.9, sdlog = 0.55)
)

mix <- mix_sample(n, weights, rlnorm, params)
y <- mix$y

nc_posX100_p4_k3 <- list(
  y = y,
  X = X,
  meta = list(
    n = n,
    support = "positive",
    p = ncol(X),
    K_true = K_true,
    tail = FALSE,
    exceed_frac = 0,
    seed = 10202
  ),
  truth = list(
    kernel = "lognormal",
    weights = weights,
    params = list(meanlog = sapply(params, `[[`, "meanlog"),
                  sdlog = sapply(params, `[[`, "sdlog")),
    threshold = NULL,
    tail_params = NULL
  )
)

usethis::use_data(nc_posX100_p4_k3, overwrite = TRUE)
