mix_sample <- function(n, weights, rfun, params) {
  K <- length(weights)
  z <- sample.int(K, size = n, replace = TRUE, prob = weights)
  out <- numeric(n)
  for (k in seq_len(K)) {
    idx <- which(z == k)
    if (!length(idx)) next
    args <- params[[k]]
    out[idx] <- do.call(rfun, c(list(n = length(idx)), args))
  }
  list(y = out, z = z)
}

rlaplace <- function(n, location, scale) {
  u <- runif(n, -0.5, 0.5)
  location - scale * sign(u) * log(1 - 2 * abs(u))
}

rInvGauss <- function(n, mean, shape) {
  mu <- mean
  lambda <- shape
  v <- rnorm(n)^2
  x <- mu + (mu^2 * v) / (2 * lambda) -
    (mu / (2 * lambda)) * sqrt(4 * mu * lambda * v + mu^2 * v^2)
  u <- runif(n)
  ifelse(u <= mu / (mu + x), x, (mu^2) / x)
}

rAmoroso <- function(n, loc, scale, shape1, shape2) {
  p <- runif(n)
  if (shape2 < 0) p <- 1 - p
  z <- qgamma(p, shape = shape1, scale = 1)
  loc + scale * (z^(1 / shape2))
}

rgpd <- function(n, scale, shape) {
  u <- runif(n)
  if (abs(shape) < .Machine$double.eps) return(-scale * log(1 - u))
  scale / shape * ((1 - u)^(-shape) - 1)
}

tail_design <- function(y_bulk, tail_frac = 0.12, threshold = NULL,
                        tail_scale = 2, tail_shape = 0.15) {
  n <- length(y_bulk)
  n_tail <- ceiling(n * tail_frac)
  if (is.null(threshold)) threshold <- as.numeric(stats::quantile(y_bulk, probs = 0.85))
  tail_excess <- rgpd(n_tail, scale = tail_scale, shape = tail_shape)
  y <- c(y_bulk[seq_len(n - n_tail)], threshold + tail_excess)
  list(
    y = y,
    exceed_frac = n_tail / n,
    threshold = threshold,
    tail_params = list(scale = tail_scale, shape = tail_shape)
  )
}
