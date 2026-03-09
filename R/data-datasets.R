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









#' nc_real200_k2 dataset
#'
#' Real-line, bulk-only mixture dataset with K=2 components and no covariates.
#' Intended for non-causal bulk-only vignettes (normal/laplace/cauchy, GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{X}{NULL.}
#'   \item{meta}{List with n, support, p, K_true, tail, exceed_frac, seed.}
#'   \item{truth}{List with kernel, weights, params, threshold, tail_params.}
#' }
#' @examples
#' data("nc_real200_k2")
#' head(nc_real200_k2$y)
#' @keywords datasets
"nc_real200_k2"

#' nc_pos200_k3 dataset
#'
#' Positive-support, bulk-only mixture dataset with K=3 components and no covariates.
#' Intended for non-causal bulk-only positive-kernel vignettes (gamma/lognormal/invgauss/amoroso).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{X}{NULL.}
#'   \item{meta}{List with n, support, p, K_true, tail, exceed_frac, seed.}
#'   \item{truth}{List with kernel, weights, params, threshold, tail_params.}
#' }
#' @examples
#' data("nc_pos200_k3")
#' head(nc_pos200_k3$y)
#' @keywords datasets
"nc_pos200_k3"

#' nc_pos_tail200_k4 dataset
#'
#' Positive-support, tail-designed mixture dataset with K=4 components and no covariates.
#' Intended for GPD vignettes (gamma/lognormal/invgauss/amoroso with GPD=TRUE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{X}{NULL.}
#'   \item{meta}{List with n, support, p, K_true, tail, exceed_frac, seed.}
#'   \item{truth}{List with kernel, weights, params, threshold, tail_params.}
#' }
#' @examples
#' data("nc_pos_tail200_k4")
#' head(nc_pos_tail200_k4$y)
#' @keywords datasets
"nc_pos_tail200_k4"

#' nc_posX100_p3_k2 dataset
#'
#' Positive-support dataset with covariates (p=3) and K=2 mixture components.
#' Intended for covariate and prediction vignettes (GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{X}{data.frame with x1-x3.}
#'   \item{meta}{List with n, support, p, K_true, tail, exceed_frac, seed.}
#'   \item{truth}{List with kernel, weights, params, threshold, tail_params.}
#' }
#' @examples
#' data("nc_posX100_p3_k2")
#' head(nc_posX100_p3_k2$X)
#' @keywords datasets
"nc_posX100_p3_k2"

#' nc_posX100_p4_k3 dataset
#'
#' Positive-support dataset with covariates (p=4) and K=3 mixture components.
#' Intended for covariate and prediction vignettes (GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{X}{data.frame with x1-x4.}
#'   \item{meta}{List with n, support, p, K_true, tail, exceed_frac, seed.}
#'   \item{truth}{List with kernel, weights, params, threshold, tail_params.}
#' }
#' @examples
#' data("nc_posX100_p4_k3")
#' head(nc_posX100_p4_k3$X)
#' @keywords datasets
"nc_posX100_p4_k3"

#' nc_posX100_p5_k4 dataset
#'
#' Positive-support dataset with covariates (p=5) and K=4 mixture components.
#' Intended for covariate and prediction vignettes (GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{X}{data.frame with x1-x5.}
#'   \item{meta}{List with n, support, p, K_true, tail, exceed_frac, seed.}
#'   \item{truth}{List with kernel, weights, params, threshold, tail_params.}
#' }
#' @examples
#' data("nc_posX100_p5_k4")
#' head(nc_posX100_p5_k4$X)
#' @keywords datasets
"nc_posX100_p5_k4"

#' nc_realX100_p3_k2 dataset
#'
#' Real-line dataset with covariates (p=3) and K=2 mixture components.
#' Intended for covariate and prediction vignettes (GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{X}{data.frame with x1-x3.}
#'   \item{meta}{List with n, support, p, K_true, tail, exceed_frac, seed.}
#'   \item{truth}{List with kernel, weights, params, threshold, tail_params.}
#' }
#' @examples
#' data("nc_realX100_p3_k2")
#' head(nc_realX100_p3_k2$X)
#' @keywords datasets
"nc_realX100_p3_k2"

#' nc_realX100_p5_k3 dataset
#'
#' Real-line dataset with covariates (p=5) and K=3 mixture components.
#' Intended for covariate and prediction vignettes (GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{X}{data.frame with x1-x5.}
#'   \item{meta}{List with n, support, p, K_true, tail, exceed_frac, seed.}
#'   \item{truth}{List with kernel, weights, params, threshold, tail_params.}
#' }
#' @examples
#' data("nc_realX100_p5_k3")
#' head(nc_realX100_p5_k3$X)
#' @keywords datasets
"nc_realX100_p5_k3"

#' causal_pos500_p3_k2 dataset
#'
#' Causal dataset (N=500, p=3) with the same positive-support kernel for both arms.
#' Intended for same-kernel causal baselines (GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{A}{Binary treatment indicator (0/1).}
#'   \item{X}{data.frame with x1-x3.}
#'   \item{meta}{List with N, support, p, K0, K1, tail, exceed_frac.}
#'   \item{truth}{List with kernel0, kernel1, params0, params1, tail_params.}
#' }
#' @examples
#' data("causal_pos500_p3_k2")
#' head(causal_pos500_p3_k2$X)
#' @keywords datasets
"causal_pos500_p3_k2"

#' causal_alt_pos500_p3_k3 dataset
#'
#' Causal dataset (N=500, p=3) with different positive-support kernels by arm.
#' Intended for alternating-kernel causal vignettes (GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{A}{Binary treatment indicator (0/1).}
#'   \item{X}{data.frame with x1-x3.}
#'   \item{meta}{List with N, support, p, K0, K1, tail, exceed_frac.}
#'   \item{truth}{List with kernel0, kernel1, params0, params1, tail_params.}
#' }
#' @examples
#' data("causal_alt_pos500_p3_k3")
#' head(causal_alt_pos500_p3_k3$X)
#' @keywords datasets
"causal_alt_pos500_p3_k3"

#' causal_alt_real500_p4_k2 dataset
#'
#' Causal dataset (N=500, p=4) with different real-line kernels by arm.
#' Intended for alternating-kernel causal vignettes (GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{A}{Binary treatment indicator (0/1).}
#'   \item{X}{data.frame with x1-x4.}
#'   \item{meta}{List with N, support, p, K0, K1, tail, exceed_frac.}
#'   \item{truth}{List with kernel0, kernel1, params0, params1, tail_params.}
#' }
#' @examples
#' data("causal_alt_real500_p4_k2")
#' head(causal_alt_real500_p4_k2$X)
#' @keywords datasets
"causal_alt_real500_p4_k2"

#' causal_alt_pos500_p5_k4_tail dataset
#'
#' Causal dataset (N=500, p=5) with different positive-support kernels by arm
#' and tail-designed exceedances (GPD=TRUE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{A}{Binary treatment indicator (0/1).}
#'   \item{X}{data.frame with x1-x5.}
#'   \item{meta}{List with N, support, p, K0, K1, tail, exceed_frac.}
#'   \item{truth}{List with kernel0, kernel1, params0, params1, tail_params.}
#' }
#' @examples
#' data("causal_alt_pos500_p5_k4_tail")
#' head(causal_alt_pos500_p5_k4_tail$X)
#' @keywords datasets
"causal_alt_pos500_p5_k4_tail"
