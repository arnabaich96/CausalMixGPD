#' Simulate bulk + tail data
#'
#' Generates a positive outcome with a DP mixture-like bulk and a GPD tail.
#'
#' @param n Sample size.
#' @param tail_prob Proportion of excesses drawn from the tail (approximated).
#' @param seed Optional seed for reproducibility.
#' @return A numeric vector of outcomes.
#' @importFrom stats rbinom rexp
#' @export
sim_bulk_tail <- function(n = 200, tail_prob = 0.12, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  bulk_n <- ceiling(n * (1 - tail_prob))
  tail_n <- n - bulk_n
  bulk <- c(
    rlnorm(bulk_n %/% 2, meanlog = 0, sdlog = 0.5),
    rgamma(bulk_n - bulk_n %/% 2, shape = 2, scale = 1.5)
  )
  tail <- stats::rgamma(tail_n, shape = 0.9, scale = 5) + 5
  sort(c(bulk, tail))
}

#' Simulate causal QTE data
#'
#' @param n Sample size.
#' @param seed Optional RNG seed.
#' @return A list with \code{y}, \code{t}, and \code{X}; \code{A} is also
#'   included as a backward-compatible alias for \code{t}.
#' @export
sim_causal_qte <- function(n = 300, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  X <- data.frame(
    x1 = rnorm(n, 0, 1),
    x2 = runif(n, -1, 1),
    x3 = rnorm(n)
  )
  lin_ps <- 0.2 + 0.6 * X$x1 - 0.4 * X$x2
  A <- rbinom(n, 1, stats::plogis(lin_ps))
  y_base <- 2 + 0.5 * X$x1 + 0.8 * X$x3 + rnorm(n)
  tail_effect <- 1.5 * X$x2 * (A == 1)
  y <- y_base + tail_effect + 2 * A + rexp(n, rate = 0.5)
  list(y = y, t = A, X = X, A = A)
}

#' Simulate survival tail data
#'
#' @param n Sample size.
#' @param seed Optional seed.
#' @return A data.frame with time, status, and covariates.
#' @export
sim_survival_tail <- function(n = 250, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  X <- data.frame(
    x1 = rnorm(n),
    x2 = rbinom(n, 1, 0.4)
  )
  base <- exp(3 - 0.5 * X$x1 + 0.3 * X$x2)
  time <- stats::rexp(n, rate = 1 / base)
  censor <- stats::runif(n, 0, 10)
  status <- as.integer(time <= censor)
  data.frame(time = pmin(time, censor), status = status, X)
}
