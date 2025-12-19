#' Fit a stick-breaking mixture model with a GPD tail extension
#'
#' Fits a bulk Dirichlet process mixture model using the stick-breaking back end
#' and augments the upper tail using a generalized Pareto distribution (GPD)
#' applied to exceedances over a threshold \code{u}.
#'
#' The current implementation fits the bulk mixture by MCMC and then fits the
#' tail parameters \code{sigma} and \code{xi} using a Metropolis--Hastings
#' sampler on exceedances conditional on \code{u}. This keeps the tail logic
#' kernel-agnostic and compatible with the unified Param1/Param2 interface.
#'
#' @param formula Model formula.
#' @param data Data frame.
#' @param kernel Two-parameter kernel name.
#' @param num_comp Number of mixture components (fixed truncation).
#' @param mcmc MCMC controls for the bulk mixture.
#' @param alpha Concentration parameter for stick-breaking.
#' @param tail_ctrl Tail control list. Supported entries:
#'   \describe{
#'     \item{u}{Numeric threshold. If missing, uses \code{u_prob}.}
#'     \item{u_prob}{Probability level for empirical threshold if \code{u} is not provided.}
#'     \item{tail_niter}{Number of tail MH iterations. Defaults to the number of bulk draws.}
#'     \item{sigma_init}{Initial value for \code{sigma}.}
#'     \item{xi_init}{Initial value for \code{xi}.}
#'     \item{prop_sd_logsigma}{Proposal SD for \code{log(sigma)}.}
#'     \item{prop_sd_xi}{Proposal SD for \code{xi}.}
#'   }
#' @return A fitted object of class \code{"mixgpd_fit_dpmgpd_crp"}.
#' @examples
#' \dontrun{
#' fit <- dpmgpd.crp(y ~ 1, data = my_data, kernel = "gamma", num_comp = 3,
#'                  mcmc = list(niter = 2000, nburn = 1000),
#'                  tail_ctrl = list(u_prob = 0.9))
#' plot(fit)
#' }
#' @export
dpmgpd.crp <- function(formula, data, kernel,
                      num_comp = 3,
                      mcmc = list(niter = 2000, nburn = 1000, nthin = 1, nchains = 1),
                      alpha = 1,
                      tail_ctrl = list(u_prob = 0.9)) {
  .check_formula_data(formula, data)
  if (!.kernel_is_param12(kernel)) {
    stop("Kernel '", kernel, "' is not available in this unified setup.", call. = FALSE)
  }
  .check_kernel_support(kernel, tail = TRUE)

  # Fit bulk model
  bulk_fit <- dpm.crp(formula = formula, data = data, kernel = kernel,
                     num_comp = num_comp, mcmc = mcmc, alpha = alpha)

  y <- bulk_fit$y
  u <- tail_ctrl$u %||% as.numeric(stats::quantile(y, probs = tail_ctrl$u_prob %||% 0.9, names = FALSE))
  exc <- y[y > u] - u
  if (!length(exc)) stop("No exceedances above threshold u; choose a smaller u.", call. = FALSE)

  # Tail MH
  ndraw <- nrow(bulk_fit$draws$Param1)
  tail_niter <- as.integer(tail_ctrl$tail_niter %||% ndraw)
  sigma <- numeric(tail_niter)
  xi <- numeric(tail_niter)

  sigma_cur <- as.numeric(tail_ctrl$sigma_init %||% stats::sd(exc))
  if (!is.finite(sigma_cur) || sigma_cur <= 0) sigma_cur <- 1
  xi_cur <- as.numeric(tail_ctrl$xi_init %||% 0.1)

  prop_sd_logsigma <- as.numeric(tail_ctrl$prop_sd_logsigma %||% 0.2)
  prop_sd_xi <- as.numeric(tail_ctrl$prop_sd_xi %||% 0.15)

  logpost <- function(sig, xii) {
    if (!is.finite(sig) || sig <= 0) return(-Inf)
    # Support constraint: 1 + xi * e / sigma > 0 for all e
    if (any(1 + xii * exc / sig <= 0)) return(-Inf)
    # log-likelihood for exceedances (no threshold term)
    ll <- sum(log(sig) * (-1) + (-1 / xii - 1) * log1p(xii * exc / sig))
    # Priors: sigma ~ half-normal(0, 5), xi ~ normal(0, 0.5)
    lp <- stats::dnorm(sig, mean = 0, sd = 5, log = TRUE) + log(2)  # half-normal
    lp <- lp + stats::dnorm(xii, mean = 0, sd = 0.5, log = TRUE)
    ll + lp
  }

  lp_cur <- logpost(sigma_cur, xi_cur)

  for (t in seq_len(tail_niter)) {
    sig_prop <- exp(log(sigma_cur) + stats::rnorm(1, 0, prop_sd_logsigma))
    xi_prop <- xi_cur + stats::rnorm(1, 0, prop_sd_xi)
    lp_prop <- logpost(sig_prop, xi_prop)

    if (log(stats::runif(1)) < (lp_prop - lp_cur)) {
      sigma_cur <- sig_prop
      xi_cur <- xi_prop
      lp_cur <- lp_prop
    }

    sigma[t] <- sigma_cur
    xi[t] <- xi_cur
  }

  # Align tail draws with bulk draws by cycling if needed
  sigma <- sigma[(seq_len(ndraw) - 1L) %% length(sigma) + 1L]
  xi <- xi[(seq_len(ndraw) - 1L) %% length(xi) + 1L]

  spec <- bulk_fit$spec
  spec$tail <- TRUE

  draws <- bulk_fit$draws
  draws$u <- rep(u, ndraw)
  draws$sigma <- sigma
  draws$xi <- xi

  fit <- bulk_fit
  fit$spec <- spec
  fit$draws <- draws
  class(fit) <- "mixgpd_fit_dpmgpd_crp"
  fit
}

#' @export
print.mixgpd_fit_dpmgpd_crp <- function(x, ...) {
  cat("DPmixGPD fit (DPMGPD-CRP)\n")
  cat("  Kernel:", x$spec$kernel, "\n")
  cat("  Components (K):", x$spec$K, "\n")
  cat("  Tail:", "yes", "\n")
  cat("  Regression:", if (isTRUE(x$spec$has_regression)) "yes" else "no", "\n")
  cat("  Threshold u:", unique(x$draws$u), "\n\n")
  cat("NIMBLE model code (bulk):\n")
  print(x$nimble$code)
  invisible(x)
}

#' @export
summary.mixgpd_fit_dpmgpd_crp <- function(object, prob = c(0.025, 0.975), ...) {
  d <- object$draws
  out <- list()
  if (!is.null(d$Param1)) out$Param1 <- .summarize_draw_matrix(d$Param1, prob = prob)
  if (!is.null(d$Param2)) out$Param2 <- .summarize_draw_matrix(d$Param2, prob = prob)
  if (!is.null(d$w)) out$w <- .summarize_draw_matrix(d$w, prob = prob)
  out$tail <- c(
    u = unique(d$u),
    sigma = stats::quantile(d$sigma, probs = prob, names = FALSE),
    xi = stats::quantile(d$xi, probs = prob, names = FALSE)
  )
  out$kernel <- object$spec$kernel
  out$model_code <- object$nimble$code
  class(out) <- "summary_mixgpd_fit"
  out
}

#' @export
predict.mixgpd_fit_dpmgpd_crp <- function(object, newdata = NULL,
                                        type = c("sample", "mean"),
                                        n = 1L, ...) {
  type <- match.arg(type)
  spec <- object$spec
  K <- spec$K
  kernel <- spec$kernel
  Xnew <- .prepare_newdata_matrix(newdata, spec$terms, object$X_train)
  nnew <- nrow(Xnew)
  ndraw <- nrow(object$draws$Param1)
  nsim <- as.integer(n)

  out <- array(NA_real_, dim = c(ndraw, nnew, nsim))

  for (s in seq_len(ndraw)) {
    wt <- object$draws$w[s, ]
    wt <- wt / sum(wt)
    base1 <- object$draws$Param1[s, ]
    base2 <- object$draws$Param2[s, ]
    u <- object$draws$u[s]
    sigma <- object$draws$sigma[s]
    xi <- object$draws$xi[s]

    if (spec$p > 0L) {
      b1 <- object$draws$beta_Param1[s, , , drop = TRUE]
      b2 <- object$draws$beta_Param2[s, , , drop = TRUE]
    } else {
      b1 <- matrix(0, K, 0L)
      b2 <- matrix(0, K, 0L)
    }

    for (i in seq_len(nnew)) {
      if (spec$p > 0L) {
        lp1 <- drop(b1 %*% Xnew[i, , drop = FALSE])
        lp2 <- drop(b2 %*% Xnew[i, , drop = FALSE])
        p1 <- base1 + .apply_trans(lp1, spec$trans1)
        p2 <- base2 * .apply_trans(lp2, spec$trans2)
      } else {
        p1 <- base1
        p2 <- base2
      }

      out[s, i, ] <- .pp_sample_mixgpd(kernel, wt, p1, p2, u = u, sigma = sigma, xi = xi, n = nsim)
    }
  }

  if (type == "mean") {
    apply(out, 2L, mean)
  } else {
    out
  }
}

#' @export
fitted.mixgpd_fit_dpmgpd_crp <- function(object, ...) {
  samp <- predict(object, newdata = NULL, type = "sample", n = 1L, ...)
  drop(colMeans(samp[, , 1, drop = FALSE]))
}

#' @export
plot.mixgpd_fit_dpmgpd_crp <- function(x, ...) {
  y <- x$y
  yrep <- as.numeric(predict(x, type = "sample", n = 1L)[, , 1])
  graphics::hist(y, breaks = "FD", freq = FALSE, main = "Observed vs posterior predictive (MixGPD)",
                 xlab = "y", ...)
  graphics::lines(stats::density(yrep, na.rm = TRUE))
  invisible(x)
}
