# S3 methods and helpers -------------------------------------------------------

#' @export
print.mixgpd_fit <- function(x, ...) {
  cat("Dirichlet process mixture model\n")
  cat("Kernel:", x$spec$kernel, "\n")
  cat("Tail:  ", x$spec$tail, "\n")
  cat("Mode:  ", x$spec$mode, "\n")
  cat("Alpha: ", ifelse(is.null(x$alpha), 0.05, x$alpha), "\n\n")

  if (!is.null(x$param_summary)) {
    print(x$param_summary)
  }
  invisible(x)
}

# internal helper: summarize MCMC draws into param table
.summarize_mcmc <- function(mcmc_draws, alpha) {
  # Normalize mcmc_draws into a list of matrices
  if (inherits(mcmc_draws, "mcmc.list")) {
    mats <- lapply(mcmc_draws, as.matrix)
  } else if (inherits(mcmc_draws, "mcmc")) {
    mats <- list(as.matrix(mcmc_draws))
  } else if (is.matrix(mcmc_draws)) {
    mats <- list(mcmc_draws)
  } else {
    stop("mcmc_draws must be an 'mcmc.list', 'mcmc', or matrix.")
  }

  # Drop any empty matrices (just in case)
  mats <- mats[vapply(mats, nrow, integer(1)) > 0L]

  if (!length(mats)) {
    stop("No MCMC samples to summarize.")
  }

  m <- do.call(rbind, mats)

  # Ensure we have column names
  if (is.null(colnames(m))) {
    colnames(m) <- paste0("param", seq_len(ncol(m)))
  }

  lower_p <- alpha / 2
  upper_p <- 1 - alpha / 2

  data.frame(
    param  = colnames(m),
    mean   = apply(m, 2L, mean),
    sd     = apply(m, 2L, stats::sd),
    lower  = apply(m, 2L, stats::quantile, probs = lower_p),
    median = apply(m, 2L, stats::quantile, probs = 0.5),
    upper  = apply(m, 2L, stats::quantile, probs = upper_p),
    row.names = NULL
  )
}


#' @export
summary.mixgpd_fit <- function(object, ...) {
  alpha <- object$alpha
  if (is.null(alpha)) alpha <- 0.05

  # if param_summary not present (backward compat), compute on the fly
  if (is.null(object$param_summary)) {
    param_summary <- .summarize_mcmc(object$mcmc_draws, alpha)
  } else {
    param_summary <- object$param_summary
  }

  out <- list(
    call          = object$call,
    spec          = object$spec,
    alpha         = alpha,
    param_summary = param_summary
  )
  class(out) <- "summary.mixgpd_fit"
  out
}

#' @export
print.summary.mixgpd_fit <- function(x, ...) {
  cat("Summary of mixgpd_fit object\n")
  cat("Kernel:", x$spec$kernel, "\n")
  cat("Tail:  ", x$spec$tail, "\n")
  cat("Mode:  ", x$spec$mode, "\n")
  cat("Alpha: ", x$alpha, "\n\n")
  print(x$param_summary, row.names = FALSE)
  invisible(x)
}

#' @export
coef.mixgpd_fit <- function(object, ...) {
  alpha <- object$alpha
  if (is.null(alpha)) alpha <- 0.05

  if (is.null(object$param_summary)) {
    ps <- .summarize_mcmc(object$mcmc_draws, alpha)
  } else {
    ps <- object$param_summary
  }

  out <- cbind(estimate = ps$mean, sd = ps$sd)
  rownames(out) <- ps$param
  out
}

#' @export
fitted.mixgpd_fit <- function(object, ...) {
  # Fitted values for original data: posterior mean only
  preds <- predict.mixgpd_fit(object, newdata = NULL)
  preds$mean
}

#' @export
predict.mixgpd_fit <- function(object, newdata = NULL, ...) {
  alpha <- object$alpha
  if (is.null(alpha)) alpha <- 0.05

  mcmc_obj <- object$mcmc_draws
  m <- do.call(rbind, lapply(mcmc_obj, as.matrix))

  if (!"mu" %in% colnames(m)) {
    stop("No 'mu' parameter found in MCMC object; cannot form predictions in this skeleton.")
  }

  mu_draws <- m[, "mu"]

  mean_hat <- mean(mu_draws)
  sd_hat   <- stats::sd(mu_draws)
  lower    <- stats::quantile(mu_draws, probs = alpha / 2)
  upper    <- stats::quantile(mu_draws, probs = 1 - alpha / 2)

  # Determine number of prediction points
  n_pred <- if (is.null(newdata)) {
    object$spec$N
  } else {
    if (is.data.frame(newdata) || is.matrix(newdata)) {
      nrow(newdata)
    } else {
      stop("`newdata` must be a data.frame or matrix (or NULL).")
    }
  }

  data.frame(
    mean  = rep(mean_hat, n_pred),
    sd    = rep(sd_hat, n_pred),
    lower = rep(lower, n_pred),
    upper = rep(upper, n_pred)
  )
}

#' Extract MCMC draws from a mixgpd_fit object
#' @export
as_mcmc <- function(object, ...) {
  UseMethod("as_mcmc")
}

#' @export
as_mcmc.mixgpd_fit <- function(object, ...) {
  object$mcmc_draws
}

#' Basic MCMC diagnostics using coda
#' @param object mixgpd_fit
#' @param what character, "trace" or "density"
#' @export
mcmc_ggdiag <- function(object, what = c("trace", "density"), ...) {
  what <- match.arg(what)
  mcmc_obj <- object$mcmc_draws
  if (what == "trace") {
    plot(mcmc_obj)
  } else {
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    par(mfrow = c(1, 2))
    m <- as.matrix(mcmc_obj[[1L]])
    for (j in seq_len(ncol(m))) {
      plot(stats::density(m[, j]), main = colnames(m)[j], xlab = "", ylab = "Density")
    }
  }
  invisible(NULL)
}
