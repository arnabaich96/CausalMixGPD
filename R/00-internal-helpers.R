#' Internal helper distributions for mixture kernels
#'
#' These helpers are used by user-facing mixture functions. They are not meant
#' to be called directly in most workflows.
#'
#' @name mixgpd_internal_helpers
#' @keywords internal
NULL

# Laplace(location, scale)
dlaplace_nimble <- nimble::nimbleFunction(
  run = function(x = double(0),
                 location = double(0),
                 scale = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (!is.finite(scale) || scale <= 0 || !is.finite(x) || !is.finite(location)) {
      if (log == 1) return(log(eps)) else return(eps)
    }
    z <- abs(x - location) / scale
    val <- (1.0 / (2.0 * scale)) * exp(-z)
    if (val < eps) val <- eps
    if (log == 1) return(log(val)) else return(val)
  }
)

plaplace_nimble <- nimble::nimbleFunction(
  run = function(q = double(0),
                 location = double(0),
                 scale = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (!is.finite(scale) || scale <= 0 || !is.finite(q) || !is.finite(location)) {
      cdf <- 0.0
      if (lower.tail == 0) cdf <- 1.0 - cdf
      if (log.p != 0) cdf <- log(max(cdf, eps))
      return(cdf)
    }
    if (q < location) cdf <- 0.5 * exp((q - location) / scale)
    else cdf <- 1.0 - 0.5 * exp(-(q - location) / scale)
    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

rlaplace_nimble <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 location = double(0),
                 scale = double(0)) {
    returnType(double(0))
    if (n != 1) return(location)
    u <- runif(1, 0.0, 1.0)
    if (u < 0.5) return(location + scale * log(2.0*u))
    return(location - scale * log(2.0*(1.0-u)))
  }
)

# Inverse Gaussian(mean, shape) where mean>0 and shape(lambda)>0
dinvgauss_nimble <- nimble::nimbleFunction(
  run = function(x = double(0),
                 mean = double(0),
                 shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (!is.finite(x) || !is.finite(mean) || !is.finite(shape) || x <= 0 || mean <= 0 || shape <= 0) {
      if (log == 1) return(log(eps)) else return(eps)
    }
    pi <- 3.141592653589793
    a <- sqrt(shape / (2.0 * pi * x^3))
    b <- exp(-shape * (x - mean)^2 / (2.0 * mean^2 * x))
    val <- a * b
    if (val < eps) val <- eps
    if (log == 1) return(log(val)) else return(val)
  }
)

pinvgauss_nimble <- nimble::nimbleFunction(
  run = function(q = double(0),
                 mean = double(0),
                 shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (!is.finite(q) || !is.finite(mean) || !is.finite(shape) || q <= 0 || mean <= 0 || shape <= 0) {
      cdf <- 0.0
      if (lower.tail == 0) cdf <- 1.0 - cdf
      if (log.p != 0) cdf <- log(max(cdf, eps))
      return(cdf)
    }
    z1 <- sqrt(shape/q) * (q/mean - 1.0)
    z2 <- -sqrt(shape/q) * (q/mean + 1.0)
    phi1 <- pnorm(z1, 0.0, 1.0, 1, 0)
    phi2 <- pnorm(z2, 0.0, 1.0, 1, 0)
    cdf <- phi1 + exp(2.0*shape/mean) * phi2
    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

rinvgauss_nimble <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 mean = double(0),
                 shape = double(0)) {
    returnType(double(0))
    if (n != 1) return(mean)
    v <- rnorm(1, 0.0, 1.0)
    y <- v*v
    mu <- mean
    lam <- shape
    x <- mu + (mu*mu*y)/(2.0*lam) - (mu/(2.0*lam))*sqrt(4.0*mu*lam*y + mu*mu*y*y)
    u <- runif(1, 0.0, 1.0)
    if (u <= mu/(mu + x)) return(x)
    return(mu*mu/x)
  }
)

# Pareto(scale, shape), x>=scale>0
dpareto_nimble <- nimble::nimbleFunction(
  run = function(x = double(0),
                 scale = double(0),
                 shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (!is.finite(x) || !is.finite(scale) || !is.finite(shape) || scale <= 0 || shape <= 0 || x < scale) {
      if (log == 1) return(log(eps)) else return(eps)
    }
    val <- shape * pow(scale, shape) / pow(x, shape + 1.0)
    if (val < eps) val <- eps
    if (log == 1) return(log(val)) else return(val)
  }
)

ppareto_nimble <- nimble::nimbleFunction(
  run = function(q = double(0),
                 scale = double(0),
                 shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (!is.finite(q) || !is.finite(scale) || !is.finite(shape) || scale <= 0 || shape <= 0 || q < scale) {
      cdf <- 0.0
      if (lower.tail == 0) cdf <- 1.0 - cdf
      if (log.p != 0) cdf <- log(max(cdf, eps))
      return(cdf)
    }
    cdf <- 1.0 - pow(scale/q, shape)
    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

rpareto_nimble <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 scale = double(0),
                 shape = double(0)) {
    returnType(double(0))
    if (n != 1) return(scale)
    u <- runif(1, 0.0, 1.0)
    return(scale / pow(1.0 - u, 1.0/shape))
  }
)
