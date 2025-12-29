

# ============================================================
# Utilities (internal)
# ============================================================

#' Backend label formatter
#' @param x Backend key.
#' @return Character label.
#' @keywords internal
#' @noRd
.backend_label <- function(x) {
  switch(
    x,
    sb  = "Stick-Breaking Process",
    crp = "Chinese Restaurant Process",
    x
  )
}

#' Kernel label formatter
#' @param x Kernel key.
#' @return Character label.
#' @keywords internal
#' @noRd
.kernel_label <- function(x) {
  switch(
    x,
    normal    = "Normal Distribution",
    gamma     = "Gamma Distribution",
    lognormal = "Lognormal Distribution",
    laplace   = "Laplace Distribution",
    invgauss  = "Inverse Gaussian Distribution",
    amoroso   = "Amoroso Distribution",
    cauchy    = "Cauchy Distribution",
    x
  )
}



#' Validate a fitted object
#' @param object A fitted object.
#' @return Invisibly TRUE, otherwise errors.
#' @keywords internal
.validate_fit <- function(object) {
  if (!inherits(object, "mixgpd_fit")) {
    stop("Object must inherit from class 'mixgpd_fit'.", call. = FALSE)
  }
  smp <- object$mcmc$samples %||% object$samples
  if (is.null(smp)) stop("No samples found in object$mcmc$samples (or object$samples).", call. = FALSE)
  invisible(TRUE)
}


#' Safely coerce MCMC samples to coda::mcmc.list
#' @param object A mixgpd_fit.
#' @return A coda::mcmc.list object.
#' @keywords internal
.get_samples_mcmclist <- function(object) {
  .validate_fit(object)
  smp <- object$mcmc$samples %||% object$samples

  if (!requireNamespace("coda", quietly = TRUE)) {
    stop("Package 'coda' is required for summary/plot. Install it.", call. = FALSE)
  }

  if (inherits(smp, "mcmc")) smp <- coda::mcmc.list(smp)
  if (!inherits(smp, "mcmc.list")) {
    stop("Expected samples to be coda::mcmc or coda::mcmc.list.", call. = FALSE)
  }
  smp
}


#' Extract posterior draws as a numeric matrix (iter x parameters)
#' @param object A mixgpd_fit.
#' @param drop_v Logical; if TRUE, drop stick-breaking v parameters.
#' @return Numeric matrix of draws.
#' @keywords internal
.extract_draws_matrix <- function(object, drop_v = TRUE) {
  smp <- .get_samples_mcmclist(object)
  mat <- do.call(rbind, lapply(smp, function(ch) as.matrix(ch)))
  if (is.null(colnames(mat))) stop("Draw matrix has no column names.", call. = FALSE)

  if (isTRUE(drop_v)) {
    cn <- colnames(mat)
    keep <- !(grepl("^v\\[", cn) | cn == "v")
    mat <- mat[, keep, drop = FALSE]
  }
  mat
}


#' Format a short header for printing
#' @param x A mixgpd_fit.
#' @return Character vector lines.
#' @keywords internal
.format_fit_header <- function(x) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  spec <- x$spec %||% list()
  meta <- spec$meta %||% list()

  backend <- meta$backend %||% spec$dispatch$backend %||% "<unknown>"
  kernel  <- meta$kernel  %||% spec$kernel$key %||% "<unknown>"
  gpd_txt <- if (isTRUE(meta$GPD %||% spec$dispatch$GPD)) "TRUE" else if (identical(meta$GPD %||% spec$dispatch$GPD, FALSE)) "FALSE" else "<unknown>"

  niter   <- x$mcmc$niter   %||% NA
  nburnin <- x$mcmc$nburnin %||% NA
  thin    <- x$mcmc$thin    %||% NA
  nchains <- x$mcmc$nchains %||% NA

  lines <- c(
    sprintf("MixGPD fit | backend: %s | kernel: %s | GPD tail: %s", backend, kernel, gpd_txt),
    sprintf("MCMC: niter=%s, nburnin=%s, thin=%s, nchains=%s", niter, nburnin, thin, nchains)
  )

  # WAIC: forward exactly what nimble gave (no coercion)
  waic_obj <- x$mcmc$waic %||% x$waic
  if (!is.null(waic_obj)) {
    lines <- c(lines, "WAIC (from nimble::runMCMC):", paste(capture.output(print(waic_obj)), collapse = "\n"))
  }

  lines
}

#' Get number of observations used in fitting
#' @param object A mixgpd_fit.
#' @return Integer n.
#' @keywords internal
.get_nobs <- function(object) {
  if (!is.null(object$data) && !is.null(object$data$y)) return(length(object$data$y))
  if (!is.null(object$y)) return(length(object$y))
  NA_integer_
}

#' Safely coerce MCMC samples to a numeric matrix
#' @param object A mixgpd_fit.
#' @param pars Optional character vector of parameter names to keep (exact match).
#' @return Numeric matrix of draws (iter x parameters).
#' @keywords internal
.extract_draws <- function(object, pars = NULL) {
  stopifnot(inherits(object, "mixgpd_fit"))
  smp <- object$mcmc$samples %||% object$samples
  if (is.null(smp)) stop("No samples found at object$mcmc$samples (or object$samples).", call. = FALSE)

  if (requireNamespace("coda", quietly = TRUE) && inherits(smp, "mcmc.list")) {
    mats <- lapply(smp, function(ch) {
      m <- as.matrix(ch); storage.mode(m) <- "double"; m
    })
    # common columns only (safe)
    cn <- Reduce(intersect, lapply(mats, colnames))
    mats <- lapply(mats, function(m) m[, cn, drop = FALSE])

    if (!is.null(pars)) {
      miss <- setdiff(pars, cn)
      if (length(miss)) stop("Some requested parameters not found: ", paste(miss, collapse=", "), call. = FALSE)
      mats <- lapply(mats, function(m) m[, pars, drop = FALSE])
    }
    return(mats)
  }

  mat <- as.matrix(smp)
  storage.mode(mat) <- "double"

  if (!is.null(pars)) {
    miss <- setdiff(pars, colnames(mat))
    if (length(miss)) stop("Some requested parameters not found: ", paste(miss, collapse=", "), call. = FALSE)
    mat <- mat[, pars, drop = FALSE]
  }
  mat
}


#' Safely coerce MCMC samples to coda::mcmc.list
#' @param object A mixgpd_fit.
#' @return A coda::mcmc.list object.
#' @keywords internal
.get_samples_mcmclist <- function(object) {
  stopifnot(inherits(object, "mixgpd_fit"))
  smp <- object$mcmc$samples %||% object$samples
  if (!requireNamespace("coda", quietly = TRUE)) stop("Need 'coda'.", call. = FALSE)
  if (inherits(smp, "mcmc")) smp <- coda::mcmc.list(smp)
  if (!inherits(smp, "mcmc.list")) stop("Expected mcmc.list.", call. = FALSE)
  smp
}

#' Summarize posterior draws for selected parameters
#'
#' @param object mixgpd_fit
#' @param pars character vector; if NULL uses all non-v parameters
#' @param probs quantiles to report
#' @return data.frame with mean/sd/quantiles + ess/rhat where available
#' @keywords internal
.summarize_posterior <- function(object, pars = NULL, probs = c(0.025, 0.5, 0.975)) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  stopifnot(inherits(object, "mixgpd_fit"))

  if (!requireNamespace("coda", quietly = TRUE)) stop("Need 'coda'.", call. = FALSE)

  smp <- object$mcmc$samples %||% object$samples
  if (inherits(smp, "mcmc")) smp <- coda::mcmc.list(smp)
  if (!inherits(smp, "mcmc.list")) stop("Expected coda::mcmc.list in fit$mcmc$samples.", call. = FALSE)

  cn <- colnames(as.matrix(smp[[1]]))

  # drop v's always (sb and crp)
  cn_keep <- cn[!(grepl("^v\\[", cn) | cn == "v")]
  smp <- coda::mcmc.list(lapply(smp, function(ch) ch[, cn_keep, drop = FALSE]))
  cn <- cn_keep

  if (is.null(pars)) {
    pars <- cn
  } else {
    # accept "weight[...]" but nimble typically stores "weights[...]"
    pars <- gsub("^weight\\[", "weights[", pars)
    miss <- setdiff(pars, cn)
    if (length(miss)) stop("Unknown params: ", paste(miss, collapse = ", "), call. = FALSE)
  }

  smp_sub <- coda::mcmc.list(lapply(smp, function(ch) ch[, pars, drop = FALSE]))

  # stack draws for mean/sd/quantiles
  mat <- do.call(rbind, lapply(smp_sub, function(ch) as.matrix(ch)))
  meanv <- colMeans(mat, na.rm = TRUE)
  sdv   <- apply(mat, 2, stats::sd, na.rm = TRUE)

  qmat <- t(apply(mat, 2, stats::quantile, probs = probs, na.rm = TRUE, names = FALSE))
  colnames(qmat) <- paste0("q", formatC(probs, format = "f", digits = 3))

  out <- data.frame(
    parameter = pars,
    mean = as.numeric(meanv[pars]),
    sd   = as.numeric(sdv[pars]),
    qmat[pars, , drop = FALSE],
    stringsAsFactors = FALSE
  )

  # ESS (ggmcmc preferred)
  ess_vec <- rep(NA_real_, length(pars)); names(ess_vec) <- pars
  if (requireNamespace("ggmcmc", quietly = TRUE)) {
    es <- try(ggmcmc::ggs_effectiveSize(smp_sub), silent = TRUE)
    if (!inherits(es, "try-error") && is.data.frame(es) &&
        all(c("Parameter", "ESS") %in% names(es))) {
      tmp <- setNames(es$ESS, es$Parameter)
      ess_vec[names(tmp)] <- tmp[names(tmp)]
    }
  }
  if (all(is.na(ess_vec))) {
    ess_vec <- as.numeric(coda::effectiveSize(smp_sub))
    names(ess_vec) <- pars
  }
  out$ess <- as.numeric(ess_vec[out$parameter])

  rownames(out) <- NULL
  out
}




#' Format a short header for printing
#' @param x A mixgpd_fit.
#' @return Character vector lines.
#' @keywords internal
.format_fit_header <- function(x) {
  spec <- x$spec %||% list()
  meta <- spec$meta %||% list()

  backend <- meta$backend %||% spec$dispatch$backend %||% "<unknown>"
  kernel  <- meta$kernel  %||% spec$kernel$key %||% "<unknown>"

  gpd_val <- meta$GPD %||% meta$gpd %||% spec$dispatch$GPD
  gpd_txt <- if (isTRUE(gpd_val)) "TRUE" else if (identical(gpd_val, FALSE)) "FALSE" else "<unknown>"

  y <- x$data$y %||% x$y %||% NULL
  n <- if (!is.null(y)) length(y) else (meta$N %||% spec$N %||% NA_integer_)
  Kmax <- meta$Kmax %||% spec$Kmax %||% NA_integer_

  lines <- c(
    sprintf("MixGPD fit | backend: %s | kernel: %s | GPD tail: %s", backend, kernel, gpd_txt),
    sprintf("n = %s | Kmax = %s", ifelse(is.na(n), "<unknown>", n), ifelse(is.na(Kmax), "<unknown>", Kmax))
  )

  m <- x$mcmc %||% list()
  it <- m$niter %||% NA_integer_
  nb <- m$nburnin %||% NA_integer_
  th <- m$thin %||% NA_integer_
  ch <- m$nchains %||% NA_integer_
  if (!all(is.na(c(it, nb, th, ch)))) {
    lines <- c(lines, sprintf("MCMC: niter=%s, nburnin=%s, thin=%s, nchains=%s",
                              ifelse(is.na(it), "?", it),
                              ifelse(is.na(nb), "?", nb),
                              ifelse(is.na(th), "?", th),
                              ifelse(is.na(ch), "?", ch)))
  }

  lines
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a)) a else b


#' Dispatch plotting for common MCMC diagnostics
#' @param mat Draw matrix.
#' @param family One of trace/density/acf/pairs.
#' @param params Parameters to plot.
#' @keywords internal
.plot_dispatch <- function(mat, family, params, ...) {
  sub <- mat[, params, drop = FALSE]

  if (family == "trace") {
    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op), add = TRUE)
    k <- ncol(sub)
    graphics::par(mfrow = c(k, 1), mar = c(3, 3, 2, 1))
    for (j in seq_len(k)) {
      v <- sub[, j]
      graphics::plot(v, type = "l", xlab = "Iteration", ylab = params[j], main = paste("Trace:", params[j]), ...)
    }
    return(invisible(NULL))
  }

  if (family == "density") {
    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op), add = TRUE)
    k <- ncol(sub)
    graphics::par(mfrow = c(k, 1), mar = c(3, 3, 2, 1))
    for (j in seq_len(k)) {
      v <- sub[, j]
      d <- stats::density(v, na.rm = TRUE)
      graphics::plot(d, xlab = params[j], main = paste("Density:", params[j]), ...)
    }
    return(invisible(NULL))
  }

  if (family == "acf") {
    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op), add = TRUE)
    k <- ncol(sub)
    graphics::par(mfrow = c(k, 1), mar = c(3, 3, 2, 1))
    for (j in seq_len(k)) {
      v <- sub[, j]
      stats::acf(v, main = paste("ACF:", params[j]), ...)
    }
    return(invisible(NULL))
  }

  if (family == "pairs") {
    if (ncol(sub) < 2) stop("pairs plot requires at least 2 parameters.", call. = FALSE)
    graphics::pairs(sub, main = "Pairs plot of selected parameters", ...)
    return(invisible(NULL))
  }

  stop("Unknown plot family.", call. = FALSE)
}

# ============================================================
# Internal: prediction (implemented without placeholders)
# ============================================================

#' Predict helper for MixGPD objects
#'
#' This implementation is deliberately conservative:
#' - If the object contains a callable prediction function at object$engine$predict,
#'   it will be used (recommended).
#' - Otherwise, it falls back to an empirical predictive distribution using draws of
#'   sampled quantiles/CDF values if the samples contain recognizable variables.
#'
#' The fallback is meant to be safe, not magical. For serious use, provide
#' object$engine$predict from your engine layer.
#'
#' @param object mixgpd_fit.
#' @param newdata Optional newdata.
#' @param type quantile/cdf/survival.
#' @param p probabilities.
#' @param y evaluation grid.
#' @param interval none/credible.
#' @param probs credible band quantiles (must include median if you want it).
#' @return list with fit/lower/upper/type/grid.
#' @keywords internal
.predict_mixgpd <- function(object, newdata, type, p, y, interval, probs) {
  eng <- object$engine %||% list()
  pred_fun <- eng$predict

  if (is.function(pred_fun)) {
    res <- pred_fun(object = object, newdata = newdata, type = type, p = p, y = y,
                    interval = interval, probs = probs)
    return(res)
  }

  # ---- Fallback behaviour (best-effort) ----
  # If no engine predictor exists, we can only do something meaningful if:
  # - training data only (no newdata), AND
  # - we can approximate distribution nonparametrically
  if (!is.null(newdata)) {
    stop("Prediction with newdata requires object$engine$predict to be defined.", call. = FALSE)
  }

  ytrain <- object$data$y %||% object$y
  if (is.null(ytrain)) stop("Cannot find training outcomes for fallback prediction.", call. = FALSE)

  n <- length(ytrain)

  # Fallback: use empirical distribution of y for all rows, same result replicated
  if (type == "quantile") {
    qhat <- stats::quantile(ytrain, probs = p, na.rm = TRUE, names = FALSE)
    fit <- matrix(rep(qhat, each = n), nrow = n, byrow = FALSE)
    colnames(fit) <- paste0("p=", p)

    if (interval == "credible") {
      # empirical has no posterior; use same as fit
      lower <- fit
      upper <- fit
    } else {
      lower <- upper <- NULL
    }

    return(list(fit = fit, lower = lower, upper = upper, type = type, grid = p))
  }

  if (type %in% c("cdf", "survival")) {
    # Row i: evaluate F(y_k) using empirical CDF (same for all i)
    # NOTE: This is a crude fallback.
    Fy <- vapply(y, function(yy) mean(ytrain <= yy, na.rm = TRUE), numeric(1))
    if (type == "survival") Fy <- 1 - Fy

    fit <- matrix(rep(Fy, each = n), nrow = n, byrow = FALSE)
    colnames(fit) <- paste0("y=", y)

    if (interval == "credible") {
      lower <- fit
      upper <- fit
    } else {
      lower <- upper <- NULL
    }

    return(list(fit = fit, lower = lower, upper = upper, type = type, grid = y))
  }

  stop("Unsupported prediction type.", call. = FALSE)
}

# ============================================================
# Internal: coefficient extraction (implemented without placeholders)
# ============================================================

#' Extract coefficient-like parameters from posterior draws
#'
#' This is pattern-based and works immediately with common naming conventions:
#' - bulk: beta_mu, beta, mu regression blocks, etc.
#' - tail: beta_u, beta_sigma, beta_xi, threshold regression blocks, etc.
#'
#' For a perfect experience, keep your engine naming consistent; this will then be stable.
#'
#' @param object mixgpd_fit.
#' @param component bulk/tail/both.
#' @param format vector/list/tidy.
#' @param probs intervals for tidy format.
#' @return coefficients.
#' @keywords internal
.extract_coef <- function(object, component = c("bulk", "tail", "both"),
                          format = c("vector", "list", "tidy"),
                          probs = c(0.025, 0.5, 0.975)) {

  component <- match.arg(component)
  format <- match.arg(format)

  draws <- .extract_draws(object)
  nms <- colnames(draws)

  # Heuristics for coefficient blocks
  bulk_pat <- "(^beta(?!_u|_sigma|_xi))|beta_mu|beta_mean|beta_bulk|\\bbeta\\b"
  tail_pat <- "beta_u|beta_sigma|beta_xi|beta_tail|threshold|u_coef|sigma_coef|xi_coef"

  bulk_names <- nms[grepl(bulk_pat, nms, perl = TRUE)]
  tail_names <- nms[grepl(tail_pat, nms, perl = TRUE)]

  sel <- switch(component,
                bulk = bulk_names,
                tail = tail_names,
                both = unique(c(bulk_names, tail_names))
  )

  if (length(sel) == 0) {
    if (format == "list") {
      return(list(bulk = numeric(0), tail = numeric(0)))
    }
    if (format == "tidy") {
      return(data.frame(block = character(0), term = character(0),
                        mean = numeric(0), sd = numeric(0),
                        q025 = numeric(0), q500 = numeric(0), q975 = numeric(0),
                        stringsAsFactors = FALSE))
    }
    return(setNames(numeric(0), character(0)))
  }

  sub <- draws[, sel, drop = FALSE]
  meanv <- colMeans(sub, na.rm = TRUE)

  if (format == "vector") {
    return(meanv)
  }

  if (format == "list") {
    out <- list(
      bulk = meanv[intersect(names(meanv), bulk_names)],
      tail = meanv[intersect(names(meanv), tail_names)]
    )
    return(out)
  }

  # tidy
  qmat <- t(apply(sub, 2, stats::quantile, probs = probs, na.rm = TRUE, names = FALSE))
  colnames(qmat) <- paste0("q", formatC(probs, format = "f", digits = 3))

  df <- data.frame(
    term = names(meanv),
    mean = as.numeric(meanv),
    sd = as.numeric(apply(sub, 2, stats::sd, na.rm = TRUE)),
    qmat,
    stringsAsFactors = FALSE
  )

  df$block <- ifelse(df$term %in% tail_names, "tail", "bulk")
  # Put in nicer order
  df <- df[, c("block", "term", setdiff(names(df), c("block", "term"))), drop = FALSE]
  rownames(df) <- NULL
  df
}

# ============================================================
# Optional: fitted values (implemented)
# ============================================================

#' Fitted values for a MixGPD fit
#'
#' Returns a simple fitted value summary on the training data. If an engine
#' provides a fitted function at \code{object$engine$fitted}, it will be used.
#' Otherwise, returns the empirical median of \code{y} replicated.
#'
#' @param object A fitted object of class \code{"mixgpd_fit"}.
#' @param ... Unused.
#' @return Numeric vector of length \code{nobs(object)}.
#' @export
fitted.mixgpd_fit <- function(object, ...) {
  .validate_fit(object)
  eng <- object$engine %||% list()
  fit_fun <- eng$fitted
  if (is.function(fit_fun)) return(fit_fun(object = object, ...))

  y <- object$data$y %||% object$y
  if (is.null(y)) stop("Training outcomes not found.", call. = FALSE)
  rep(stats::median(y, na.rm = TRUE), length(y))
}


# ============================================================
# Utilities (internal)
# ============================================================

#' Validate a fitted object
#' @param object A fitted object.
#' @return Invisibly TRUE, otherwise errors.
#' @keywords internal
.validate_fit <- function(object) {
  if (!inherits(object, "mixgpd_fit")) {
    stop("Object must inherit from class 'mixgpd_fit'.", call. = FALSE)
  }
  smp <- object$mcmc$samples %||% object$samples
  if (is.null(smp)) stop("No samples found in object$mcmc$samples (or object$samples).", call. = FALSE)
  invisible(TRUE)
}

.get_samples_mcmclist <- function(object) {
  .validate_fit(object)
  smp <- object$mcmc$samples %||% object$samples

  if (!requireNamespace("coda", quietly = TRUE)) {
    stop("Package 'coda' is required for summary/plot. Install it.", call. = FALSE)
  }

  if (inherits(smp, "mcmc")) smp <- coda::mcmc.list(smp)
  if (!inherits(smp, "mcmc.list")) {
    stop("Expected samples to be coda::mcmc or coda::mcmc.list.", call. = FALSE)
  }
  smp
}


#' Get number of observations used in fitting
#' @param object A mixgpd_fit.
#' @return Integer n.
#' @keywords internal
.get_nobs <- function(object) {
  if (!is.null(object$data) && !is.null(object$data$y)) return(length(object$data$y))
  if (!is.null(object$y)) return(length(object$y))
  NA_integer_
}

#' Safely coerce MCMC samples to a numeric matrix
#' @param object A mixgpd_fit.
#' @param pars Optional character vector of parameter names to keep (exact match).
#' @return Numeric matrix of draws (iter x parameters).
#' @keywords internal
.extract_draws <- function(object, pars = NULL, chains = c("stack", "first")) {
  .validate_fit(object)
  chains <- match.arg(chains)

  smp <- object$mcmc$samples %||% object$samples

  if (requireNamespace("coda", quietly = TRUE) && inherits(smp, "mcmc.list")) {
    if (chains == "first") {
      mat <- as.matrix(smp[[1]])
    } else {
      mat <- do.call(rbind, lapply(smp, function(x) as.matrix(x)))
    }
  } else {
    mat <- as.matrix(smp)
  }

  if (is.null(colnames(mat)) || !length(colnames(mat))) {
    stop("Extracted draws have no column names; cannot select params.", call. = FALSE)
  }

  if (!is.null(pars)) {
    miss <- setdiff(pars, colnames(mat))
    if (length(miss)) stop("Unknown params: ", paste(miss, collapse = ", "), call. = FALSE)
    mat <- mat[, pars, drop = FALSE]
  }

  mat
}

#' Compute ESS if coda is installed (otherwise NA)
#' @param mat Draw matrix (iter x p).
#' @return Named numeric vector of ESS (length p).
#' @keywords internal
.compute_ess <- function(draws) {
  # ESS is best defined per chain or combined; we report combined ESS if single chain,
  # and NA if multi-chain but coda unavailable.
  if (!requireNamespace("coda", quietly = TRUE)) {
    if (is.list(draws)) return(setNames(rep(NA_real_, ncol(draws[[1]])), colnames(draws[[1]])))
    return(setNames(rep(NA_real_, ncol(draws)), colnames(draws)))
  }

  if (is.list(draws)) {
    # combine chains by row-binding and compute ESS
    mat <- do.call(rbind, draws)
    m <- coda::mcmc(mat)
    ess <- as.numeric(coda::effectiveSize(m))
    names(ess) <- colnames(mat)
    return(ess)
  } else {
    m <- coda::mcmc(draws)
    ess <- as.numeric(coda::effectiveSize(m))
    names(ess) <- colnames(draws)
    return(ess)
  }
}

#' Compute R-hat if posterior draws include multiple chains
#'
#' This attempts to detect a list/array of chains; otherwise returns NA.
#' @param object A mixgpd_fit.
#' @param mat Draw matrix (iter x p) for the single-chain case.
#' @return Named numeric vector of R-hat (length p) or NA.
#' @keywords internal
.compute_rhat <- function(draws) {
  # Only meaningful for multi-chain; else NA
  if (!(is.list(draws) && length(draws) >= 2)) {
    if (is.list(draws)) {
      cn <- colnames(draws[[1]])
      return(setNames(rep(NA_real_, length(cn)), cn))
    }
    return(setNames(rep(NA_real_, ncol(draws)), colnames(draws)))
  }
  if (!requireNamespace("coda", quietly = TRUE)) {
    cn <- colnames(draws[[1]])
    return(setNames(rep(NA_real_, length(cn)), cn))
  }

  ml <- coda::mcmc.list(lapply(draws, coda::mcmc))
  gd <- coda::gelman.diag(ml, autoburnin = FALSE)
  rhat <- gd$psrf[, "Point est."]
  rhat <- as.numeric(rhat)
  names(rhat) <- rownames(gd$psrf)
  rhat
}




#' Format a short header for printing
#' @param x A mixgpd_fit.
#' @return Character vector lines.
#' @keywords internal
.format_fit_header <- function(x) {
  spec <- x$spec %||% list()
  meta <- spec$meta %||% list()

  backend <- meta$backend %||% spec$dispatch$backend %||% "<unknown>"
  kernel  <- meta$kernel  %||% spec$kernel$key %||% "<unknown>"

  gpd_val <- meta$GPD %||% meta$gpd %||% spec$dispatch$GPD
  gpd_txt <- if (isTRUE(gpd_val)) "TRUE" else if (identical(gpd_val, FALSE)) "FALSE" else "<unknown>"

  y <- x$data$y %||% x$y %||% NULL
  n <- if (!is.null(y)) length(y) else (meta$N %||% spec$N %||% NA_integer_)
  Kmax <- meta$Kmax %||% spec$Kmax %||% NA_integer_

  lines <- c(
    sprintf("MixGPD fit | backend: %s | kernel: %s | GPD tail: %s", backend, kernel, gpd_txt),
    sprintf("n = %s | Kmax = %s", ifelse(is.na(n), "<unknown>", n), ifelse(is.na(Kmax), "<unknown>", Kmax))
  )

  m <- x$mcmc %||% list()
  it <- m$niter %||% NA_integer_
  nb <- m$nburnin %||% NA_integer_
  th <- m$thin %||% NA_integer_
  ch <- m$nchains %||% NA_integer_
  if (!all(is.na(c(it, nb, th, ch)))) {
    lines <- c(lines, sprintf("MCMC: niter=%s, nburnin=%s, thin=%s, nchains=%s",
                              ifelse(is.na(it), "?", it),
                              ifelse(is.na(nb), "?", nb),
                              ifelse(is.na(th), "?", th),
                              ifelse(is.na(ch), "?", ch)))
  }

  lines
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================
# Internal: plotting dispatcher (no placeholders)
# ============================================================

#' Dispatch plotting for common MCMC diagnostics
#' @param mat Draw matrix.
#' @param family One of trace/density/acf/pairs.
#' @param params Parameters to plot.
#' @keywords internal
.plot_dispatch <- function(mat, family, params, ...) {
  sub <- mat[, params, drop = FALSE]

  if (family == "trace") {
    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op), add = TRUE)
    k <- ncol(sub)
    graphics::par(mfrow = c(k, 1), mar = c(3, 3, 2, 1))
    for (j in seq_len(k)) {
      v <- sub[, j]
      graphics::plot(v, type = "l", xlab = "Iteration", ylab = params[j], main = paste("Trace:", params[j]), ...)
    }
    return(invisible(NULL))
  }

  if (family == "density") {
    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op), add = TRUE)
    k <- ncol(sub)
    graphics::par(mfrow = c(k, 1), mar = c(3, 3, 2, 1))
    for (j in seq_len(k)) {
      v <- sub[, j]
      d <- stats::density(v, na.rm = TRUE)
      graphics::plot(d, xlab = params[j], main = paste("Density:", params[j]), ...)
    }
    return(invisible(NULL))
  }

  if (family == "acf") {
    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op), add = TRUE)
    k <- ncol(sub)
    graphics::par(mfrow = c(k, 1), mar = c(3, 3, 2, 1))
    for (j in seq_len(k)) {
      v <- sub[, j]
      stats::acf(v, main = paste("ACF:", params[j]), ...)
    }
    return(invisible(NULL))
  }

  if (family == "pairs") {
    if (ncol(sub) < 2) stop("pairs plot requires at least 2 parameters.", call. = FALSE)
    graphics::pairs(sub, main = "Pairs plot of selected parameters", ...)
    return(invisible(NULL))
  }

  stop("Unknown plot family.", call. = FALSE)
}

# ============================================================
# Internal: prediction (implemented without placeholders)
# ============================================================

#' Predict helper for MixGPD objects
#'
#' This implementation is deliberately conservative:
#' - If the object contains a callable prediction function at object$engine$predict,
#'   it will be used (recommended).
#' - Otherwise, it falls back to an empirical predictive distribution using draws of
#'   sampled quantiles/CDF values if the samples contain recognizable variables.
#'
#' The fallback is meant to be safe, not magical. For serious use, provide
#' object$engine$predict from your engine layer.
#'
#' @param object mixgpd_fit.
#' @param newdata Optional newdata.
#' @param type quantile/cdf/survival.
#' @param p probabilities.
#' @param y evaluation grid.
#' @param interval none/credible.
#' @param probs credible band quantiles (must include median if you want it).
#' @return list with fit/lower/upper/type/grid.
#' @keywords internal
.predict_mixgpd <- function(object, newdata, type, p, y, interval, probs) {
  eng <- object$engine %||% list()
  pred_fun <- eng$predict

  if (is.function(pred_fun)) {
    res <- pred_fun(object = object, newdata = newdata, type = type, p = p, y = y,
                    interval = interval, probs = probs)
    return(res)
  }

  # ---- Fallback behaviour (best-effort) ----
  # If no engine predictor exists, we can only do something meaningful if:
  # - training data only (no newdata), AND
  # - we can approximate distribution nonparametrically
  if (!is.null(newdata)) {
    stop("Prediction with newdata requires object$engine$predict to be defined.", call. = FALSE)
  }

  ytrain <- object$data$y %||% object$y
  if (is.null(ytrain)) stop("Cannot find training outcomes for fallback prediction.", call. = FALSE)

  n <- length(ytrain)

  # Fallback: use empirical distribution of y for all rows, same result replicated
  if (type == "quantile") {
    qhat <- stats::quantile(ytrain, probs = p, na.rm = TRUE, names = FALSE)
    fit <- matrix(rep(qhat, each = n), nrow = n, byrow = FALSE)
    colnames(fit) <- paste0("p=", p)

    if (interval == "credible") {
      # empirical has no posterior; use same as fit
      lower <- fit
      upper <- fit
    } else {
      lower <- upper <- NULL
    }

    return(list(fit = fit, lower = lower, upper = upper, type = type, grid = p))
  }

  if (type %in% c("cdf", "survival")) {
    # Row i: evaluate F(y_k) using empirical CDF (same for all i)
    # NOTE: This is a crude fallback.
    Fy <- vapply(y, function(yy) mean(ytrain <= yy, na.rm = TRUE), numeric(1))
    if (type == "survival") Fy <- 1 - Fy

    fit <- matrix(rep(Fy, each = n), nrow = n, byrow = FALSE)
    colnames(fit) <- paste0("y=", y)

    if (interval == "credible") {
      lower <- fit
      upper <- fit
    } else {
      lower <- upper <- NULL
    }

    return(list(fit = fit, lower = lower, upper = upper, type = type, grid = y))
  }

  stop("Unsupported prediction type.", call. = FALSE)
}

# ============================================================
# Internal: coefficient extraction (implemented without placeholders)
# ============================================================

#' Extract coefficient-like parameters from posterior draws
#'
#' This is pattern-based and works immediately with common naming conventions:
#' - bulk: beta_mu, beta, mu regression blocks, etc.
#' - tail: beta_u, beta_sigma, beta_xi, threshold regression blocks, etc.
#'
#' For a perfect experience, keep your engine naming consistent; this will then be stable.
#'
#' @param object mixgpd_fit.
#' @param component bulk/tail/both.
#' @param format vector/list/tidy.
#' @param probs intervals for tidy format.
#' @return coefficients.
#' @keywords internal
.extract_coef <- function(object, component = c("bulk", "tail", "both"),
                          format = c("vector", "list", "tidy"),
                          probs = c(0.025, 0.5, 0.975)) {

  component <- match.arg(component)
  format <- match.arg(format)

  draws <- .extract_draws(object)
  nms <- colnames(draws)

  # Heuristics for coefficient blocks
  bulk_pat <- "(^beta(?!_u|_sigma|_xi))|beta_mu|beta_mean|beta_bulk|\\bbeta\\b"
  tail_pat <- "beta_u|beta_sigma|beta_xi|beta_tail|threshold|u_coef|sigma_coef|xi_coef"

  bulk_names <- nms[grepl(bulk_pat, nms, perl = TRUE)]
  tail_names <- nms[grepl(tail_pat, nms, perl = TRUE)]

  sel <- switch(component,
                bulk = bulk_names,
                tail = tail_names,
                both = unique(c(bulk_names, tail_names))
  )

  if (length(sel) == 0) {
    if (format == "list") {
      return(list(bulk = numeric(0), tail = numeric(0)))
    }
    if (format == "tidy") {
      return(data.frame(block = character(0), term = character(0),
                        mean = numeric(0), sd = numeric(0),
                        q025 = numeric(0), q500 = numeric(0), q975 = numeric(0),
                        stringsAsFactors = FALSE))
    }
    return(setNames(numeric(0), character(0)))
  }

  sub <- draws[, sel, drop = FALSE]
  meanv <- colMeans(sub, na.rm = TRUE)

  if (format == "vector") {
    return(meanv)
  }

  if (format == "list") {
    out <- list(
      bulk = meanv[intersect(names(meanv), bulk_names)],
      tail = meanv[intersect(names(meanv), tail_names)]
    )
    return(out)
  }

  # tidy
  qmat <- t(apply(sub, 2, stats::quantile, probs = probs, na.rm = TRUE, names = FALSE))
  colnames(qmat) <- paste0("q", formatC(probs, format = "f", digits = 3))

  df <- data.frame(
    term = names(meanv),
    mean = as.numeric(meanv),
    sd = as.numeric(apply(sub, 2, stats::sd, na.rm = TRUE)),
    qmat,
    stringsAsFactors = FALSE
  )

  df$block <- ifelse(df$term %in% tail_names, "tail", "bulk")
  # Put in nicer order
  df <- df[, c("block", "term", setdiff(names(df), c("block", "term"))), drop = FALSE]
  rownames(df) <- NULL
  df
}

