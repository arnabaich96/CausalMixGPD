# small helper: null-coalescing
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' @export
print.mixgpd_fit <- function(x, ...) {
  # Try to pull key bits from top level, then from spec
  kernel <- x$kernel %||% (x$spec$kernel %||% NA_character_)
  tail   <- x$tail   %||% (x$spec$tail   %||% NA_character_)
  mode   <- x$mode   %||% (x$spec$mode   %||% NA_character_)
  alpha  <- x$alpha  %||% NA_real_

  # sample size if we stored it
  N <- x$N %||% x$spec$N %||% NA_integer_

  # truncation K if present
  K <- tryCatch(x$spec$dp_ctrl$K, error = function(e) NA_integer_)

  cat("Dirichlet process mixture model\n")
  if (!is.na(N)) cat("N:      ", N, "\n", sep = "")
  if (!is.na(kernel)) cat("Kernel: ", kernel, "\n", sep = "")
  if (!is.na(tail) && tail != "none") {
    cat("Tail:   ", tail, " (GPD)\n", sep = "")
  } else if (!is.na(tail)) {
    cat("Tail:   none\n")
  }
  if (!is.na(mode)) cat("Mode:   ", mode, "\n", sep = "")
  if (!is.na(K))    cat("DP K:   ", K, "\n", sep = "")
  if (!is.na(alpha)) cat("Alpha:  ", alpha, "\n", sep = "")

  invisible(x)
}



# internal helper: summarize MCMC draws into param table
.summarize_mcmc <- function(mcmc_draws, alpha = 0.05) {
  # Accept a variety of formats:
  #  - coda::mcmc.list
  #  - single coda::mcmc
  #  - plain list of matrices
  #  - single matrix

  if (inherits(mcmc_draws, "mcmc.list")) {
    chain_list <- mcmc_draws
  } else if (inherits(mcmc_draws, "mcmc")) {
    chain_list <- list(mcmc_draws)
  } else if (is.list(mcmc_draws)) {
    # assume each element is an iterations x parameters matrix
    chain_list <- lapply(mcmc_draws, coda::as.mcmc)
  } else if (is.matrix(mcmc_draws)) {
    chain_list <- list(coda::as.mcmc(mcmc_draws))
  } else {
    stop(
      "'object$mcmc_draws' must be a matrix, mcmc, mcmc.list, or list of matrices.",
      call. = FALSE
    )
  }

  # stack all chains
  combined <- do.call(rbind, chain_list)
  if (!is.matrix(combined)) {
    combined <- as.matrix(combined)
  }

  pnames <- colnames(combined)
  if (is.null(pnames)) {
    pnames <- paste0("param_", seq_len(ncol(combined)))
    colnames(combined) <- pnames
  }

  lower <- alpha / 2
  upper <- 1 - alpha / 2

  means <- colMeans(combined)
  sds   <- apply(combined, 2L, stats::sd)
  q_lo  <- apply(combined, 2L, stats::quantile, probs = lower, names = FALSE)
  q_hi  <- apply(combined, 2L, stats::quantile, probs = upper, names = FALSE)

  out <- data.frame(
    parameter = pnames,
    mean      = as.numeric(means),
    sd        = as.numeric(sds),
    q_lo      = as.numeric(q_lo),
    q_hi      = as.numeric(q_hi),
    row.names = NULL
  )

  out
}


summary.mixgpd_fit <- function(object, ...) {
  # use stored alpha if available, else default to 0.05
  alpha <- object$alpha %||% 0.05
  .summarize_mcmc(object$mcmc_draws, alpha = alpha)
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
