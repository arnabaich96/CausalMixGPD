#' Fit a stick-breaking Dirichlet process mixture model
#'
#' Fits a Dirichlet process mixture model for a single outcome using a
#' finite stick-breaking truncation with \code{num_comp} mixture components.
#' The kernel must be a two-parameter kernel supported by the package's
#' Param1/Param2 interface.
#'
#' The fitted object is of class \code{"mixgpd_fit_dpm_sb"} and supports
#' \code{print()}, \code{summary()}, \code{predict()}, \code{fitted()}, and
#' \code{plot()}.
#'
#' @param formula Model formula. Use \code{y ~ 1} for an unconditional model,
#'   or include covariates on the right-hand side for a regression model.
#' @param data A data frame containing variables referenced in \code{formula}.
#' @param kernel Character scalar naming the kernel (for example \code{"gamma"},
#'   \code{"normal"}, or \code{"lognormal"}).
#' @param num_comp Positive integer giving the truncation level (number of
#'   mixture components). Defaults to \code{3}.
#' @param mcmc List of MCMC controls with entries \code{niter}, \code{nburn},
#'   \code{nthin}, \code{nchains}. See details.
#' @param alpha Positive concentration parameter for the stick-breaking prior.
#' @return A fitted object of class \code{"mixgpd_fit_dpm_sb"}.
#' @examples
#' \dontrun{
#' fit <- dpm.sb(y ~ 1, data = my_data, kernel = "gamma", num_comp = 3,
#'               mcmc = list(niter = 2000, nburn = 1000))
#' print(fit)
#' yhat <- fitted(fit)
#' }
#' @export
dpm.sb <- function(formula, data, kernel,
                   num_comp = 3,
                   mcmc = list(niter = 2000, nburn = 1000, nthin = 1, nchains = 1),
                   alpha = 1) {
  .check_formula_data(formula, data)
  if (!.kernel_is_param12(kernel)) {
    stop("Kernel '", kernel, "' is not available in this unified setup.", call. = FALSE)
  }
  .check_kernel_support(kernel, tail = FALSE)

  K <- as.integer(num_comp)
  if (K <= 0L) stop("`num_comp` must be a positive integer.", call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1L || alpha <= 0) {
    stop("`alpha` must be a positive numeric scalar.", call. = FALSE)
  }
  mcmc <- .check_mcmc(mcmc)

  dm <- .prepare_design_matrix(formula, data)
  y <- dm$y
  X <- dm$X
  n <- length(y)
  p <- ncol(X)
  has_reg <- isTRUE(dm$has_regression)

  # Prior structure: decide whether Param1 must be positive.
  param1_positive <- kernel %in% c("gamma", "inverse_gaussian", "pareto")
  trans1 <- if (param1_positive) "exp" else "identity"
  trans2 <- "exp"

  # NIMBLE model code ---------------------------------------------------------
  if (!requireNamespace("nimble", quietly = TRUE)) {
    stop("Package 'nimble' is required for fitting.", call. = FALSE)
  }

code <- nimble::nimbleCode({
  # Stick-breaking weights
  v[1:(K-1)] ~ dbeta(1, alpha)
  w_raw[1] <- v[1]
  for (k in 2:(K-1)) {
    w_raw[k] <- v[k] * prod(1 - v[1:(k-1)])
  }
  w_raw[K] <- 1 - sum(w_raw[1:(K-1)])
  for (k in 1:K) {
    w[k] <- max(1e-12, w_raw[k])
  }
  w[1:K] <- w[1:K] / sum(w[1:K])

  # Component-level parameters
  for (k in 1:K) {
    if (param1_positive) {
      Param1[k] ~ dlnorm(0, sd = 1.0)
    } else {
      Param1[k] ~ dnorm(0, sd = 10.0)
    }
    Param2[k] ~ dlnorm(0, sd = 1.0)
    for (j in 1:p) {
      beta_Param1[k, j] ~ dnorm(0, sd = 5.0)
      beta_Param2[k, j] ~ dnorm(0, sd = 5.0)
    }
  }

  for (i in 1:n) {
    # Per-component linear predictors and transformed parameters
    for (k in 1:K) {
      lp1[i, k] <- inprod(X[i, 1:p], beta_Param1[k, 1:p])
      lp2[i, k] <- inprod(X[i, 1:p], beta_Param2[k, 1:p])
      if (param1_positive == 1) {
        Param1_i[i, k] <- exp(lp1[i, k]) + Param1[k]
      } else {
        Param1_i[i, k] <- lp1[i, k] + Param1[k]
      }
      Param2_i[i, k] <- exp(lp2[i, k]) * Param2[k]
    }

    # Mixture likelihood: log \{ sum_k w_k f_k(y_i) \}
    for (k in 1:K) {
      if (kernel == "gamma") {
        log_f[i, k] <- dgamma(y[i], shape = Param1_i[i, k], rate = Param1_i[i, k] / Param2_i[i, k], log = 1)
      } else if (kernel == "normal") {
        log_f[i, k] <- dnorm(y[i], mean = Param1_i[i, k], sd = Param2_i[i, k], log = 1)
      } else if (kernel == "lognormal") {
        log_f[i, k] <- dlnorm(y[i], meanlog = Param1_i[i, k], sdlog = Param2_i[i, k], log = 1)
      } else if (kernel == "cauchy") {
        log_f[i, k] <- dcauchy(y[i], location = Param1_i[i, k], scale = Param2_i[i, k], log = 1)
      } else if (kernel == "laplace") {
        log_f[i, k] <- dlaplace(y[i], location = Param1_i[i, k], scale = Param2_i[i, k], log = 1)
      } else if (kernel == "inverse_gaussian") {
        log_f[i, k] <- dinvgauss(y[i], mean = Param1_i[i, k], shape = Param2_i[i, k], log = 1)
      } else if (kernel == "pareto") {
        log_f[i, k] <- dpareto(y[i], scale = Param2_i[i, k], shape = Param1_i[i, k], log = 1)
      } else {
        log_f[i, k] <- dnorm(y[i], mean = Param1_i[i, k], sd = Param2_i[i, k], log = 1)
      }
      log_comp[i, k] <- log(w[k]) + log_f[i, k]
    }

    # Stable log-sum-exp
    m[i] <- max(log_comp[i, 1:K])
    s[i] <- 0.0
    for (k in 1:K) {
      s[i] <- s[i] + exp(log_comp[i, k] - m[i])
    }
    loglik[i] <- m[i] + log(s[i])

    ones[i] ~ dpois(exp(loglik[i]))
  }
})

  constants <- list(n = n, K = K, p = max(p, 1L), alpha = alpha,
                    param1_positive = param1_positive, kernel = kernel)
  data_list <- list(y = y, X = if (p > 0L) X else matrix(0, n, 1), ones = rep(1, n))
  inits <- list(
    v = stats::rbeta(max(K - 1L, 1L), 1, alpha),
    Param1 = rep(1, K),
    Param2 = rep(1, K),
    beta_Param1 = matrix(0, K, max(p, 1L)),
    beta_Param2 = matrix(0, K, max(p, 1L))
  )

  monitors <- c("Param1", "Param2", "w")
  if (p > 0L) monitors <- c(monitors, "beta_Param1", "beta_Param2")

  # Run MCMC
  if (!is.null(mcmc$setSeed)) set.seed(mcmc$setSeed)
  samples <- nimble::nimbleMCMC(
    code = code, constants = constants, data = data_list, inits = inits,
    monitors = monitors,
    niter = mcmc$niter, nburnin = mcmc$nburn, thin = mcmc$nthin,
    nchains = mcmc$nchains, summary = FALSE, samplesAsCodaMCMC = FALSE
  )
  if (is.list(samples) && length(samples) == 1L) samples <- samples[[1L]]
  samples <- as.matrix(samples)

  spec <- list(
    kernel = kernel,
    backend = "sb",
    tail = FALSE,
    K = K,
    p = p,
    has_regression = has_reg,
    trans1 = trans1,
    trans2 = trans2,
    terms = dm$terms
  )

  draws <- .extract_draws_param12(samples, spec)

  fit <- list(
    spec = spec,
    draws = draws,
    nimble = list(code = code, constants = constants, data = data_list, inits = inits,
                  monitors = monitors),
    y = y,
    X_train = if (p > 0L) X else matrix(numeric(0), n, 0L)
  )
  class(fit) <- "mixgpd_fit_dpm_sb"
  fit
}

#' @export
print.mixgpd_fit_dpm_sb <- function(x, ...) {
  cat("DPmixGPD fit (DPM-SB)\n")
  cat("  Kernel:", x$spec$kernel, "\n")
  cat("  Components (K):", x$spec$K, "\n")
  cat("  Tail:", "no", "\n")
  cat("  Regression:", if (isTRUE(x$spec$has_regression)) "yes" else "no", "\n\n")
  cat("NIMBLE model code:\n")
  print(x$nimble$code)
  invisible(x)
}

#' @export
summary.mixgpd_fit_dpm_sb <- function(object, prob = c(0.025, 0.975), ...) {
  d <- object$draws
  out <- list()
  if (!is.null(d$Param1)) out$Param1 <- .summarize_draw_matrix(d$Param1, prob = prob)
  if (!is.null(d$Param2)) out$Param2 <- .summarize_draw_matrix(d$Param2, prob = prob)
  if (!is.null(d$w)) out$w <- .summarize_draw_matrix(d$w, prob = prob)
  out$kernel <- object$spec$kernel
  out$model_code <- object$nimble$code
  class(out) <- "summary_mixgpd_fit"
  out
}

#' @export
predict.mixgpd_fit_dpm_sb <- function(object, newdata = NULL,
                                     type = c("sample", "mean"),
                                     n = 1L, ...) {
  type <- match.arg(type)
  spec <- object$spec
  K <- spec$K
  kernel <- spec$kernel
  Xnew <- .prepare_newdata_matrix(newdata, spec$terms, object$X_train)
  nnew <- nrow(Xnew)
  ndraw <- nrow(object$draws$Param1)

  fns <- .kernel_component_funs(kernel)

  # One posterior predictive sample per draw per observation (n replicates allowed).
  nsim <- as.integer(n)
  out <- array(NA_real_, dim = c(ndraw, nnew, nsim))

  for (s in seq_len(ndraw)) {
    wt <- object$draws$w[s, ]
    wt <- wt / sum(wt)
    base1 <- object$draws$Param1[s, ]
    base2 <- object$draws$Param2[s, ]

    if (spec$p > 0L) {
      b1 <- object$draws$beta_Param1[s, , , drop = TRUE]
      b2 <- object$draws$beta_Param2[s, , , drop = TRUE]
      # b1: K x p, b2: K x p
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

      out[s, i, ] <- .pp_sample_mix(kernel, wt, p1, p2, n = nsim)
    }
  }

  if (type == "mean") {
    apply(out, 2L, mean)
  } else {
    out
  }
}

#' @export
fitted.mixgpd_fit_dpm_sb <- function(object, ...) {
  samp <- predict(object, newdata = NULL, type = "sample", n = 1L, ...)
  # samp: draws x n x 1
  drop(colMeans(samp[, , 1, drop = FALSE]))
}

#' @export
plot.mixgpd_fit_dpm_sb <- function(x, ...) {
  y <- x$y
  yrep <- as.numeric(predict(x, type = "sample", n = 1L)[, , 1])
  graphics::hist(y, breaks = "FD", freq = FALSE, main = "Observed vs posterior predictive",
                 xlab = "y", ...)
  graphics::lines(stats::density(yrep, na.rm = TRUE))
  invisible(x)
}
