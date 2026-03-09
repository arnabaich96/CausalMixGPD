#' Simulate positive bulk-tail data
#'
#' Generate synthetic outcomes with a light-to-moderate bulk and a heavier upper tail.
#' The sample is assembled from a lognormal-gamma bulk and a shifted tail sample, then sorted.
#' This is a quick data generator for examples, help pages, and workflow checks rather than a
#' formal generative model matching the full package hierarchy exactly.
#'
#' @param n Integer sample size.
#' @param tail_prob Approximate tail probability \eqn{\Pr(X > u)} used to split the sample into
#'   bulk and tail draws.
#' @param seed Optional random seed for reproducibility.
#'
#' @return Numeric vector of length `n` containing positive outcomes sorted in ascending order.
#'
#' @details
#' The generator approximates a spliced sample
#' \deqn{
#' X \sim (1 - \pi_u) F_{bulk} + \pi_u F_{tail},
#' }
#' where \eqn{\pi_u =} `tail_prob`. The bulk component is itself a simple two-component mixture,
#' while the tail component is a shifted positive distribution that produces larger values.
#'
#' Use this helper when you need a fast toy sample for [bundle()], [dpmix()], or [dpmgpd()].
#' It should not be interpreted as posterior predictive simulation from a fitted object.
#'
#' @seealso [sim_causal_qte()], [sim_survival_tail()], [bundle()], [dpmgpd()].
#' @family simulation helpers
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

#' Simulate causal quantile-treatment-effect data
#'
#' Generate a treatment indicator, covariates, and a continuous outcome with both location and
#' tail heterogeneity. The resulting structure is intended for examples involving
#' [dpmix.causal()], [dpmgpd.causal()], [qte()], and [cqte()].
#'
#' @param n Integer sample size.
#' @param seed Optional random seed.
#'
#' @return List with components `y`, `t`, and `X`; `A` is included as a backward-compatible alias
#'   for `t`.
#'
#' @details
#' Treatment assignment is generated from a logistic propensity score
#' \deqn{
#' \Pr(T = 1 \mid X) = \operatorname{logit}^{-1}(\eta(X)),
#' }
#' and the observed outcome combines baseline covariate effects, an average treatment shift, and
#' a covariate-dependent tail amplification for treated units. This produces data where marginal
#' and conditional quantile effects differ across the outcome distribution.
#'
#' The returned list can be converted directly into the arguments expected by the causal fitting
#' wrappers after minor formatting.
#'
#' @seealso [sim_bulk_tail()], [dpmgpd.causal()], [qte()], [cqte()].
#' @family simulation helpers
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

#' Simulate censored survival-style tail data
#'
#' Generate event times, censoring times, an event indicator, and covariates for examples where
#' right tail behavior and positive support matter.
#'
#' @param n Integer sample size.
#' @param seed Optional random seed.
#'
#' @return Data frame containing observed time `time`, event indicator `status`, and covariates.
#'
#' @details
#' Event times are sampled from an exponential model with covariate-dependent mean, then censored
#' by an independent uniform censoring time. The observed time is
#' \deqn{
#' \tilde{T} = \min(T, C), \qquad \Delta = I(T \le C).
#' }
#'
#' This helper is mainly for experimentation and stress-testing positive-support kernels; it does
#' not implement a dedicated survival model from the package API.
#'
#' @seealso [sim_bulk_tail()], [build_nimble_bundle()], [dpmgpd()].
#' @family simulation helpers
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
