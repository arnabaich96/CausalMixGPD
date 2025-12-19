#' Fit a CRP-style Dirichlet process mixture model
#'
#' Fits a Dirichlet process mixture model for a single outcome using a finite
#' approximation with Dirichlet-distributed mixture weights. This is the CRP
#' (Chinese restaurant process) back end used in the package.
#'
#' The fitted object is of class \code{"mixgpd_fit_dpm_crp"} and supports
#' \code{print()}, \code{summary()}, \code{predict()}, \code{fitted()}, and
#' \code{plot()}.
#'
#' @param formula Model formula. Use \code{y ~ 1} for an unconditional model,
#'   or include covariates on the right-hand side for a regression model.
#' @param data A data frame containing variables referenced in \code{formula}.
#' @param kernel Character scalar naming the kernel.
#' @param num_comp Positive integer giving the truncation level (number of
#'   mixture components). Defaults to \code{20}.
#' @param mcmc List of MCMC controls with entries \code{niter}, \code{nburn},
#'   \code{nthin}, \code{nchains}.
#' @param alpha Positive concentration parameter.
#' @return A fitted object of class \code{"mixgpd_fit_dpm_crp"}.
#' @examples
#' \dontrun{
#' fit <- dpm.crp(y ~ 1, data = my_data, kernel = "normal", num_comp = 20,
#'                mcmc = list(niter = 2000, nburn = 1000))
#' summary(fit)
#' }
#' @export
dpm.crp <- function(formula, data, kernel,
                    num_comp = 20,
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

  param1_positive <- kernel %in% c("gamma", "inverse_gaussian", "pareto")
  trans1 <- if (param1_positive) "exp" else "identity"
  trans2 <- "exp"

  if (!requireNamespace("nimble", quietly = TRUE)) {
    stop("Package 'nimble' is required for fitting.", call. = FALSE)
  }

  code <- nimble::nimbleCode({
    w[1:K] ~ nimble::ddirch(rep(alpha / K, K))
    for (k in 1:K) {
      if (param1_positive) {
        Param1[k] ~ nimble::dlnorm(0, sd = 1.0)
      } else {
        Param1[k] ~ nimble::dnorm(0, sd = 10.0)
      }
      Param2[k] ~ nimble::dlnorm(0, sd = 1.0)
      for (j in 1:p) {
        beta_Param1[k, j] ~ nimble::dnorm(0, sd = 5.0)
        beta_Param2[k, j] ~ nimble::dnorm(0, sd = 5.0)
      }
    }

    for (i in 1:n) {
      z[i] ~ nimble::dcat(w[1:K])
      for (k in 1:K) {
        lp1[i, k] <- inprod(X[i, 1:p], beta_Param1[k, 1:p])
        lp2[i, k] <- inprod(X[i, 1:p], beta_Param2[k, 1:p])

        Param1_i[i, k] <- (param1_positive ? exp(lp1[i, k]) : lp1[i, k]) + Param1[k]
        Param2_i[i, k] <- exp(lp2[i, k]) * Param2[k]
      }

      if (kernel == "gamma") {
        y[i] ~ nimble::dgamma(shape = Param1_i[i, z[i]], rate = Param1_i[i, z[i]] / Param2_i[i, z[i]])
      } else if (kernel == "normal") {
        y[i] ~ nimble::dnorm(mean = Param1_i[i, z[i]], sd = Param2_i[i, z[i]])
      } else if (kernel == "lognormal") {
        y[i] ~ nimble::dlnorm(meanlog = Param1_i[i, z[i]], sdlog = Param2_i[i, z[i]])
      } else if (kernel == "cauchy") {
        y[i] ~ dcauchy_nimble(location = Param1_i[i, z[i]], scale = Param2_i[i, z[i]])
      } else if (kernel == "laplace") {
        y[i] ~ dlaplace_nimble(location = Param1_i[i, z[i]], scale = Param2_i[i, z[i]])
      } else if (kernel == "inverse_gaussian") {
        y[i] ~ dinvgauss_nimble(mean = Param1_i[i, z[i]], shape = Param2_i[i, z[i]])
      } else if (kernel == "pareto") {
        y[i] ~ dpareto_nimble(shape = Param1_i[i, z[i]], scale = Param2_i[i, z[i]])
      } else {
        y[i] ~ nimble::dnorm(mean = Param1_i[i, z[i]], sd = Param2_i[i, z[i]])
      }
    }
  })

  constants <- list(n = n, K = K, p = max(p, 1L), alpha = alpha,
                    param1_positive = param1_positive, kernel = kernel)
  data_list <- list(y = y, X = if (p > 0L) X else matrix(0, n, 1))
  inits <- list(
    w = rep(1 / K, K),
    Param1 = rep(1, K),
    Param2 = rep(1, K),
    beta_Param1 = matrix(0, K, max(p, 1L)),
    beta_Param2 = matrix(0, K, max(p, 1L)),
    z = sample.int(K, n, replace = TRUE)
  )

  monitors <- c("Param1", "Param2", "w")
  if (p > 0L) monitors <- c(monitors, "beta_Param1", "beta_Param2")

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
    backend = "crp",
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
  class(fit) <- "mixgpd_fit_dpm_crp"
  fit
}

#' @export
print.mixgpd_fit_dpm_crp <- function(x, ...) {
  cat("DPmixGPD fit (DPM-CRP)\n")
  cat("  Kernel:", x$spec$kernel, "\n")
  cat("  Components (K):", x$spec$K, "\n")
  cat("  Tail:", "no", "\n")
  cat("  Regression:", if (isTRUE(x$spec$has_regression)) "yes" else "no", "\n\n")
  cat("NIMBLE model code:\n")
  print(x$nimble$code)
  invisible(x)
}

#' @export
summary.mixgpd_fit_dpm_crp <- function(object, prob = c(0.025, 0.975), ...) {
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
predict.mixgpd_fit_dpm_crp <- function(object, newdata = NULL,
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
fitted.mixgpd_fit_dpm_crp <- function(object, ...) {
  samp <- predict(object, newdata = NULL, type = "sample", n = 1L, ...)
  drop(colMeans(samp[, , 1, drop = FALSE]))
}

#' @export
plot.mixgpd_fit_dpm_crp <- function(x, ...) {
  y <- x$y
  yrep <- as.numeric(predict(x, type = "sample", n = 1L)[, , 1])
  graphics::hist(y, breaks = "FD", freq = FALSE, main = "Observed vs posterior predictive",
                 xlab = "y", ...)
  graphics::lines(stats::density(yrep, na.rm = TRUE))
  invisible(x)
}
