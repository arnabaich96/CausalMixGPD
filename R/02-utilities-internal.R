

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
# Internal: prediction (posterior draw-based; supports SB + CRP via posterior weights)
# ============================================================

#' Extract indexed parameter block from a draws matrix
#' @param mat Numeric matrix of posterior draws (S x npar) with Nimble-style names like "mean[1]".
#' @param base Base parameter name (e.g. "weights", "mean", "sd", "z").
#' @param K Optional integer length to enforce; if NULL, inferred from max index in column names.
#' @return Numeric matrix S x K with columns ordered by index.
#' @keywords internal
.indexed_block <- function(mat, base, K = NULL) {
  stopifnot(is.matrix(mat), is.character(base), length(base) == 1L)
  cn <- colnames(mat)
  if (is.null(cn)) stop("Draw matrix has no column names.", call. = FALSE)

  pat <- paste0("^", base, "\\[([0-9]+)\\]$")
  hit <- grepl(pat, cn)
  if (!any(hit)) {
    stop(sprintf("No indexed columns found for '%s[i]' in posterior draws.", base), call. = FALSE)
  }

  idx <- as.integer(sub(pat, "\\1", cn[hit]))
  ord <- order(idx)
  idx <- idx[ord]
  cols <- cn[hit][ord]

  if (is.null(K)) {
    K <- max(idx, na.rm = TRUE)
  } else {
    K <- as.integer(K)
    if (K <= 0) stop("K must be positive.", call. = FALSE)
  }

  out <- matrix(0.0, nrow = nrow(mat), ncol = K)
  for (j in seq_along(cols)) {
    k <- idx[j]
    if (!is.na(k) && k >= 1 && k <= K) out[, k] <- mat[, cols[j]]
  }
  out
}

#' Validate/standardize prediction X (design) against training X
#' @keywords internal
.validate_X_pred <- function(Xpred, Xtrain) {
  if (is.null(Xpred)) return(NULL)

  Xpred <- as.matrix(Xpred)
  storage.mode(Xpred) <- "double"

  if (anyNA(Xpred)) stop("Missing values (NA) found in 'x' (new design matrix).", call. = FALSE)

  if (!is.null(Xtrain)) {
    Xtrain <- as.matrix(Xtrain)
    if (!is.null(colnames(Xtrain)) && !is.null(colnames(Xpred))) {
      if (!setequal(colnames(Xpred), colnames(Xtrain))) {
        stop("Column names of 'x' do not match training design matrix.", call. = FALSE)
      }
      Xpred <- Xpred[, colnames(Xtrain), drop = FALSE]
    } else {
      if (ncol(Xpred) != ncol(Xtrain)) {
        stop("Number of columns in 'x' does not match training design matrix.", call. = FALSE)
      }
    }
  }

  Xpred
}

#' Resolve the four MIX functions (d/p/q/r) for a kernel + tail flag
#' @keywords internal
.get_mix_fns <- function(kernel, GPD) {
  kdef <- get_kernel_registry()[[kernel]]
  if (is.null(kdef)) stop(sprintf("Kernel '%s' not found in registry.", kernel), call. = FALSE)

  if (isTRUE(GPD) && isFALSE(kdef$allow_gpd)) {
    stop(sprintf("Kernel '%s' does not allow a GPD tail.", kernel), call. = FALSE)
  }

  d_name <- if (isTRUE(GPD)) kdef$sb$d_gpd else kdef$sb$d
  if (is.na(d_name) || !nzchar(d_name)) stop("Missing MIX density dispatch in kernel registry.", call. = FALSE)

  p_name <- sub("^d", "p", d_name)
  q_name <- sub("^d", "q", d_name)
  r_name <- sub("^d", "r", d_name)

  ns <- asNamespace("DPmixGPD")

  list(
    d = get(d_name, envir = ns),
    p = get(p_name, envir = ns),
    q = get(q_name, envir = ns),
    r = get(r_name, envir = ns),
    bulk_params = kdef$bulk_params
  )
}

#' Internal prediction engine: evaluate per posterior draw, then summarize.
#'
#' Project rules:
#' - density/probs/surv: either provide both (x,y) or neither (defaults to training X and training y).
#' - quantile/sample/mean: y must be NULL; x may be provided (new X) or NULL (defaults to training X).
#' - CRP predictions use posterior weights derived from z for each draw.
#' - Stores per-draw results in object$cache$predict (environment) for reuse in treatment effects.
#'
#' @keywords internal
.predict_mixgpd <- function(object,
                            x = NULL, y = NULL,
                            type = c("density", "probs", "surv", "quantile", "sample", "mean",
                                     "cdf", "survival"),
                            p = NULL, nsim = NULL,
                            interval = c("none", "credible"),
                            probs = c(0.025, 0.5, 0.975),
                            store_draws = TRUE,
                            nsim_mean = 200L,
                            ncores = 1L) {

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  .validate_fit(object)
  type <- match.arg(type)
  if (type == "cdf") type <- "probs"
  if (type == "survival") type <- "surv"

  interval <- match.arg(interval)

  ncores <- as.integer(ncores)
  if (is.na(ncores) || ncores < 1L) stop("'ncores' must be an integer >= 1.", call. = FALSE)

  # -----------------------------
  # Spec / meta
  # -----------------------------
  spec <- object$spec %||% list()
  meta <- spec$meta %||% list()

  backend <- meta$backend %||% spec$dispatch$backend %||% "<unknown>"
  kernel  <- meta$kernel  %||% spec$kernel$key %||% "<unknown>"
  GPD     <- isTRUE(meta$GPD %||% spec$dispatch$GPD)

  # training data
  Xtrain <- object$data$X %||% object$X %||% NULL
  ytrain <- object$data$y %||% object$y %||% NULL

  # whether model uses X
  # (if you already store meta$has_X, it will be used; otherwise infer from stored Xtrain)
  has_X <- isTRUE(meta$has_X %||% (!is.null(Xtrain)))

  # -----------------------------
  # helper: validate X
  # -----------------------------
  .validate_X_pred <- function(Xpred, Xtrain) {
    Xpred <- as.matrix(Xpred)
    storage.mode(Xpred) <- "double"
    if (anyNA(Xpred)) stop("Missing values (NA) found in 'x'.", call. = FALSE)

    if (!is.null(Xtrain)) {
      Xtrain <- as.matrix(Xtrain)

      if (!is.null(colnames(Xtrain)) && !is.null(colnames(Xpred))) {
        if (!setequal(colnames(Xpred), colnames(Xtrain))) {
          stop("Column names of 'x' do not match training design matrix.", call. = FALSE)
        }
        Xpred <- Xpred[, colnames(Xtrain), drop = FALSE]
      } else {
        if (ncol(Xpred) != ncol(Xtrain)) {
          stop("Number of columns in 'x' does not match training design matrix.", call. = FALSE)
        }
      }
    }

    Xpred
  }

  # -----------------------------
  # Resolve MIX functions for kernel
  # -----------------------------
  .get_mix_fns <- function(kernel, GPD) {
    kdef <- get_kernel_registry()[[kernel]]
    if (is.null(kdef)) stop(sprintf("Kernel '%s' not found in registry.", kernel), call. = FALSE)
    if (isTRUE(GPD) && isFALSE(kdef$allow_gpd)) stop(sprintf("Kernel '%s' does not allow GPD.", kernel), call. = FALSE)

    d_name <- if (isTRUE(GPD)) kdef$sb$d_gpd else kdef$sb$d
    if (is.na(d_name) || !nzchar(d_name)) stop("Missing sb dispatch in kernel registry.", call. = FALSE)

    p_name <- sub("^d", "p", d_name)
    q_name <- sub("^d", "q", d_name)
    r_name <- sub("^d", "r", d_name)

    ns <- asNamespace("DPmixGPD")
    list(
      d = get(d_name, envir = ns),
      p = get(p_name, envir = ns),
      q = get(q_name, envir = ns),
      r = get(r_name, envir = ns),
      bulk_params = kdef$bulk_params
    )
  }

  fns <- .get_mix_fns(kernel = kernel, GPD = GPD)
  bulk_params <- fns$bulk_params

  # -----------------------------
  # Resolve inputs by type (YOUR CONTRACT)
  # -----------------------------
  Xpred <- NULL
  ygrid <- NULL
  pgrid <- NULL

  if (type %in% c("density", "probs", "surv")) {
    # either BOTH (x,y) provided or NEITHER (defaults to training X and training y)
    if (is.null(x) && is.null(y)) {
      if (has_X && is.null(Xtrain)) stop("Training X not found in fit object.", call. = FALSE)
      if (is.null(ytrain)) stop("Training y not found in fit object.", call. = FALSE)
      Xpred <- if (has_X) Xtrain else NULL
      ygrid <- ytrain
    } else if (!is.null(x) && !is.null(y)) {
      Xpred <- if (has_X) x else NULL
      ygrid <- y
    } else {
      stop("For type='density'/'probs'/'surv', provide BOTH 'x' and 'y', or provide NEITHER to use training defaults.",
           call. = FALSE)
    }

    if (is.null(ygrid) || length(ygrid) == 0) stop("y grid is empty.", call. = FALSE)
    ygrid <- as.numeric(ygrid)
    if (anyNA(ygrid) || !all(is.finite(ygrid))) stop("y must be finite and contain no NA.", call. = FALSE)

  } else if (type == "quantile") {
    if (!is.null(y)) stop("For type='quantile', y must be NULL.", call. = FALSE)
    if (is.null(p) || length(p) == 0) stop("For type='quantile', provide non-empty 'p'.", call. = FALSE)
    pgrid <- as.numeric(p)
    if (anyNA(pgrid) || !all(is.finite(pgrid)) || any(pgrid <= 0 | pgrid >= 1)) {
      stop("p must be in (0,1), finite, no NA.", call. = FALSE)
    }
    Xpred <- if (has_X) (x %||% Xtrain) else NULL

  } else if (type == "sample") {
    if (!is.null(y)) stop("For type='sample', y must be NULL.", call. = FALSE)
    nsim <- nsim %||% NA_integer_
    if (!is.numeric(nsim) || length(nsim) != 1 || is.na(nsim) || nsim < 1) {
      stop("For type='sample', provide a positive integer 'nsim'.", call. = FALSE)
    }
    nsim <- as.integer(nsim)
    Xpred <- if (has_X) (x %||% Xtrain) else NULL

  } else if (type == "mean") {
    if (!is.null(y)) stop("For type='mean', y must be NULL.", call. = FALSE)
    Xpred <- if (has_X) (x %||% Xtrain) else NULL

  } else {
    stop("Unsupported prediction type.", call. = FALSE)
  }

  if (!has_X && !is.null(x)) stop("This model was fit without X; 'x' is not allowed for prediction.", call. = FALSE)

  if (has_X) {
    if (is.null(Xpred)) stop("Could not resolve X for prediction.", call. = FALSE)
    Xpred <- .validate_X_pred(Xpred, Xtrain)
  }

  n_pred <- if (has_X) nrow(Xpred) else 1L
  if (is.na(n_pred) || n_pred < 1L) stop("Could not determine number of prediction rows.", call. = FALSE)

  # -----------------------------
  # Extract posterior draws
  # -----------------------------
  draw_mat <- .extract_draws_matrix(object, drop_v = TRUE)
  S <- nrow(draw_mat)

  # helper: indexed block
  .indexed_block <- function(mat, base, K = NULL) {
    cn <- colnames(mat)
    pat <- paste0("^", base, "\\[([0-9]+)\\]$")
    hit <- grepl(pat, cn)
    if (!any(hit)) stop(sprintf("No columns for '%s[i]' in posterior draws.", base), call. = FALSE)

    idx <- as.integer(sub(pat, "\\1", cn[hit]))
    ord <- order(idx)
    idx <- idx[ord]
    cols <- cn[hit][ord]

    if (is.null(K)) K <- max(idx, na.rm = TRUE)
    K <- as.integer(K)

    out <- matrix(0.0, nrow = nrow(mat), ncol = K)
    for (j in seq_along(cols)) {
      k <- idx[j]
      if (!is.na(k) && k >= 1 && k <= K) out[, k] <- mat[, cols[j]]
    }
    out
  }

  # Determine K and weights per draw
  if (backend == "sb") {
    # infer K from weights[] if needed
    cn <- colnames(draw_mat)
    widx <- as.integer(sub("^weights\\[([0-9]+)\\]$", "\\1", cn[grepl("^weights\\[[0-9]+\\]$", cn)]))
    K <- max(widx, na.rm = TRUE)
    if (!is.finite(K) || K < 1) stop("Could not infer K from weights[] in posterior draws.", call. = FALSE)
    K <- as.integer(K)
    W_draws <- .indexed_block(draw_mat, "weights", K = K)

  } else if (backend == "crp") {
    # infer K from the first bulk param indexed columns
    cn <- colnames(draw_mat)
    firstp <- bulk_params[1]
    idx <- as.integer(sub(paste0("^", firstp, "\\[([0-9]+)\\]$"), "\\1",
                          cn[grepl(paste0("^", firstp, "\\[[0-9]+\\]$"), cn)]))
    K <- max(idx, na.rm = TRUE)
    if (!is.finite(K) || K < 1) stop("Could not infer Kmax from component parameter draws.", call. = FALSE)
    K <- as.integer(K)

    Zmat <- .indexed_block(draw_mat, "z")     # S x N (N inferred)
    storage.mode(Zmat) <- "integer"
    Nobs <- ncol(Zmat)

    W_draws <- matrix(0.0, nrow = S, ncol = K)
    for (s in 1:S) {
      z_s <- Zmat[s, ]
      z_s <- z_s[is.finite(z_s)]
      z_s <- z_s[z_s >= 1 & z_s <= K]
      if (length(z_s)) W_draws[s, ] <- tabulate(z_s, nbins = K) / length(z_s)
    }

  } else {
    stop("Unknown backend: ", backend, call. = FALSE)
  }

  # Component parameter matrices (S x K)
  bulk_draws <- list()
  for (nm in bulk_params) bulk_draws[[nm]] <- .indexed_block(draw_mat, nm, K = K)

  # Tail / threshold draws
  tail_scale <- tail_shape <- NULL
  threshold_scalar <- NULL
  threshold_mat <- NULL

  if (GPD) {
    if (!("tail_scale" %in% colnames(draw_mat))) stop("tail_scale not found in posterior draws.", call. = FALSE)
    if (!("tail_shape" %in% colnames(draw_mat))) stop("tail_shape not found in posterior draws.", call. = FALSE)
    tail_scale <- as.numeric(draw_mat[, "tail_scale"])
    tail_shape <- as.numeric(draw_mat[, "tail_shape"])

    if (has_X) {
      P <- ncol(Xpred)
      beta_mat <- .indexed_block(draw_mat, "beta_threshold", K = P)  # S x P
      threshold_mat <- matrix(NA_real_, nrow = S, ncol = n_pred)
      for (s in 1:S) threshold_mat[s, ] <- as.numeric(exp(Xpred %*% beta_mat[s, ]))
    } else {
      if (!("threshold" %in% colnames(draw_mat))) stop("threshold not found in posterior draws.", call. = FALSE)
      threshold_scalar <- as.numeric(draw_mat[, "threshold"])
    }
  }

  # -----------------------------
  # Parallel helper
  # -----------------------------
  .lapply_draws <- function(FUN) {
    idx <- seq_len(S)
    if (ncores == 1L) return(lapply(idx, FUN))

    # Export function objects explicitly (Windows PSOCK-safe)
    d_fun <- fns$d
    p_fun <- fns$p
    q_fun <- fns$q
    r_fun <- fns$r

    if (!is.function(d_fun) || !is.function(p_fun) || !is.function(q_fun) || !is.function(r_fun)) {
      stop("Internal error: kernel dispatch functions not resolved.", call. = FALSE)
    }

    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterExport(
      cl,
      varlist = c("W_draws", "bulk_draws", "bulk_params", "GPD",
                  "tail_scale", "tail_shape", "threshold_mat", "threshold_scalar",
                  "has_X", "n_pred", "ygrid", "pgrid", "type",
                  "d_fun", "p_fun", "q_fun", "r_fun"),
      envir = environment()
    )

    parallel::parLapply(cl, idx, FUN)
  }


  # -----------------------------
  # density / probs / surv
  # -----------------------------
  if (type %in% c("density", "probs", "surv")) {
    G <- length(ygrid)

    .one_draw <- function(s) {
      w_s <- as.numeric(W_draws[s, ])
      args0 <- list(w = w_s)
      for (nm in bulk_params) args0[[nm]] <- as.numeric(bulk_draws[[nm]][s, ])
      if (GPD) { args0$tail_scale <- tail_scale[s]; args0$tail_shape <- tail_shape[s] }

      out <- matrix(NA_real_, nrow = n_pred, ncol = G)
      for (i in 1:n_pred) {
        args <- args0
        if (GPD) args$threshold <- if (has_X) threshold_mat[s, i] else threshold_scalar[s]

        if (type == "density") {
          out[i, ] <- vapply(ygrid, function(yy) {
            do.call(d_fun, c(list(x = yy, log = 0L), args))
          }, numeric(1))
        } else {
          cdfv <- vapply(ygrid, function(yy) {
            do.call(d_fun, c(list(x = yy, log = 0L), args))
          }, numeric(1))
          if (type == "surv") cdfv <- 1 - cdfv
          out[i, ] <- cdfv
        }
      }
      out
    }

    mats <- .lapply_draws(.one_draw)

    draws_arr <- array(NA_real_, dim = c(S, n_pred, G))
    for (s in 1:S) draws_arr[s, , ] <- mats[[s]]

    fit <- apply(draws_arr, c(2, 3), mean, na.rm = TRUE)

    lower <- upper <- NULL
    if (interval == "credible") {
      qarr <- apply(draws_arr, c(2, 3), stats::quantile, probs = probs, na.rm = TRUE)
      lower <- qarr[1, , , drop = TRUE]
      upper <- qarr[length(probs), , , drop = TRUE]
    }

    if (isTRUE(store_draws) && is.environment(object$cache)) {
      if (is.null(object$cache$predict)) object$cache$predict <- new.env(parent = emptyenv())
      key <- paste0(type, "_", backend, "_", kernel, "_", ifelse(GPD, "gpd", "nogpd"),
                    "_n", n_pred, "_g", length(ygrid), "_S", S)
      object$cache$predict[[key]] <- list(type = type, grid = ygrid, draws = draws_arr, fit = fit,
                                          backend = backend, kernel = kernel, GPD = GPD)
    }

    return(list(fit = fit, lower = lower, upper = upper, type = type, grid = ygrid))
  }

  # -----------------------------
  # quantile
  # -----------------------------
  if (type == "quantile") {
    M <- length(pgrid)

    .one_draw <- function(s) {
      w_s <- as.numeric(W_draws[s, ])
      args0 <- list(w = w_s)
      for (nm in bulk_params) args0[[nm]] <- as.numeric(bulk_draws[[nm]][s, ])
      if (GPD) { args0$tail_scale <- tail_scale[s]; args0$tail_shape <- tail_shape[s] }

      out <- matrix(NA_real_, nrow = n_pred, ncol = M)
      for (i in 1:n_pred) {
        args <- args0
        if (GPD) args$threshold <- if (has_X) threshold_mat[s, i] else threshold_scalar[s]
        out[i, ] <- as.numeric(do.call(q_fun, c(list(p = pgrid), args)))
      }
      out
    }

    mats <- .lapply_draws(.one_draw)

    draws_arr <- array(NA_real_, dim = c(S, n_pred, M))
    for (s in 1:S) draws_arr[s, , ] <- mats[[s]]

    fit <- apply(draws_arr, c(2, 3), mean, na.rm = TRUE)

    lower <- upper <- NULL
    if (interval == "credible") {
      qarr <- apply(draws_arr, c(2, 3), stats::quantile, probs = probs, na.rm = TRUE)
      lower <- qarr[1, , , drop = TRUE]
      upper <- qarr[length(probs), , , drop = TRUE]
    }

    if (isTRUE(store_draws) && is.environment(object$cache)) {
      if (is.null(object$cache$predict)) object$cache$predict <- new.env(parent = emptyenv())
      key <- paste0(type, "_", backend, "_", kernel, "_", ifelse(GPD, "gpd", "nogpd"),
                    "_n", n_pred, "_m", length(pgrid), "_S", S)
      object$cache$predict[[key]] <- list(type = type, grid = pgrid, draws = draws_arr, fit = fit,
                                          backend = backend, kernel = kernel, GPD = GPD)
    }

    return(list(fit = fit, lower = lower, upper = upper, type = type, grid = pgrid))
  }

  # -----------------------------
  # sample (posterior predictive)
  # -----------------------------
  if (type == "sample") {
    idx <- sample.int(S, size = nsim, replace = TRUE)
    out <- matrix(NA_real_, nrow = n_pred, ncol = nsim)

    for (t in 1:nsim) {
      s <- idx[t]
      w_s <- as.numeric(W_draws[s, ])
      args0 <- list(w = w_s)
      for (nm in bulk_params) args0[[nm]] <- as.numeric(bulk_draws[[nm]][s, ])
      if (GPD) { args0$tail_scale <- tail_scale[s]; args0$tail_shape <- tail_shape[s] }

      for (i in 1:n_pred) {
        args <- args0
        if (GPD) args$threshold <- if (has_X) threshold_mat[s, i] else threshold_scalar[s]
        out[i, t] <- as.numeric(do.call(r_fun, c(list(n = 1L), args)))
      }
    }

    if (is.environment(object$cache)) {
      if (is.null(object$cache$predict)) object$cache$predict <- new.env(parent = emptyenv())
      key <- paste0(type, "_", backend, "_", kernel, "_", ifelse(GPD, "gpd", "nogpd"),
                    "_n", n_pred, "_nsim", nsim)
      object$cache$predict[[key]] <- list(type = type, draws = out, posterior_index = idx,
                                          backend = backend, kernel = kernel, GPD = GPD)
    }

    return(list(fit = out, lower = NULL, upper = NULL, type = type, grid = NULL))
  }

  # -----------------------------
  # mean (Monte Carlo approximation via sampling)
  # -----------------------------
  if (type == "mean") {
    nsim_mean <- as.integer(nsim_mean)
    if (is.na(nsim_mean) || nsim_mean < 10L) nsim_mean <- 200L

    mean_draws <- matrix(NA_real_, nrow = S, ncol = n_pred)

    for (s in 1:S) {
      w_s <- as.numeric(W_draws[s, ])
      args0 <- list(w = w_s)
      for (nm in bulk_params) args0[[nm]] <- as.numeric(bulk_draws[[nm]][s, ])
      if (GPD) { args0$tail_scale <- tail_scale[s]; args0$tail_shape <- tail_shape[s] }

      for (i in 1:n_pred) {
        args <- args0
        if (GPD) args$threshold <- if (has_X) threshold_mat[s, i] else threshold_scalar[s]
        yy <- vapply(seq_len(nsim_mean), function(k) {
          as.numeric(do.call(r_fun, c(list(n = 1L), args)))
        }, numeric(1))
        mean_draws[s, i] <- mean(yy)
      }
    }

    fit <- colMeans(mean_draws, na.rm = TRUE)
    lower <- upper <- NULL
    if (interval == "credible") {
      qmat <- t(apply(mean_draws, 2, stats::quantile, probs = probs, na.rm = TRUE, names = FALSE))
      lower <- qmat[, 1]
      upper <- qmat[, length(probs)]
    }

    if (isTRUE(store_draws) && is.environment(object$cache)) {
      if (is.null(object$cache$predict)) object$cache$predict <- new.env(parent = emptyenv())
      key <- paste0(type, "_", backend, "_", kernel, "_", ifelse(GPD, "gpd", "nogpd"),
                    "_n", n_pred, "_S", S)
      object$cache$predict[[key]] <- list(type = type, draws = mean_draws, fit = fit,
                                          backend = backend, kernel = kernel, GPD = GPD)
    }

    return(list(fit = fit, lower = lower, upper = upper, type = type, grid = NULL))
  }

  stop("Unsupported prediction type.", call. = FALSE)
}

