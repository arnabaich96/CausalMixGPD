source("data-raw/_helpers.R")

set.seed(10103)
n <- 200
K_true <- 4
weights <- c(0.3, 0.25, 0.25, 0.2)
params <- list(
  list(meanlog = -0.2, sdlog = 0.4),
  list(meanlog = 0.2, sdlog = 0.35),
  list(meanlog = 0.6, sdlog = 0.45),
  list(meanlog = 1.0, sdlog = 0.5)
)

mix <- mix_sample(n, weights, rlnorm, params)
tail_out <- tail_design(mix$y, tail_frac = 0.12, tail_scale = 2.5, tail_shape = 0.2)

nc_pos_tail200_k4 <- list(
  y = tail_out$y,
  X = NULL,
  meta = list(
    n = n,
    support = "positive",
    p = 0L,
    K_true = K_true,
    tail = TRUE,
    exceed_frac = tail_out$exceed_frac,
    seed = 10103
  ),
  truth = list(
    kernel = "lognormal",
    weights = weights,
    params = list(meanlog = sapply(params, `[[`, "meanlog"),
                  sdlog = sapply(params, `[[`, "sdlog")),
    threshold = tail_out$threshold,
    tail_params = tail_out$tail_params
  )
)

usethis::use_data(nc_pos_tail200_k4, overwrite = TRUE)
