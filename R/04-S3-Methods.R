# 04-S3-Methods.R for DPMixGPD bundles and fits

# S3 methods for Bundle objects -------------------------------------------



#' Print a prepared DPMixGPD bundle
#'
#' Shows a compact overview of the bundle plus realized initial values (not the inits function).
#'
#' @param x A \code{dpmixgpd_bundle}.
#' @param ... Unused.
#' @return \code{x} invisibly.
#' @export
print.dpmixgpd_bundle <- function(x, code = FALSE, ...) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  stopifnot(inherits(x, "dpmixgpd_bundle"))

  spec <- x$spec %||% list()
  meta <- spec$meta %||% list()

  backend <- meta$backend %||% "<unknown>"
  kernel  <- meta$kernel  %||% (spec$kernel$key %||% "<unknown>")
  gpd_txt <- if (isTRUE(meta$GPD)) "TRUE" else if (identical(meta$GPD, FALSE)) "FALSE" else "<unknown>"

  y <- x$data$y %||% NULL
  n <- if (!is.null(y)) length(y) else (meta$N %||% spec$N %||% "<unknown>")
  Kmax <- meta$Kmax %||% spec$Kmax %||% "<unknown>"
  J <- meta$J %||% spec$J %||% "<unknown>"

  cat("DPMixGPD bundle\n")
  backend_raw <- x$spec$meta$backend
  kernel_raw  <- x$spec$meta$kernel

  cat(
    "  backend:", .backend_label(backend_raw),
    "| kernel:",  .kernel_label(kernel_raw),
    "| GPD:",     x$spec$meta$GPD,
    "\n"
  )

  cat(sprintf("  n: %s | Kmax: %s | J: %s\n", n, Kmax, J))

  m <- x$mcmc %||% list()
  if (length(m)) {
    cat(sprintf("  MCMC: niter=%s nburnin=%s thin=%s nchains=%s seed=%s\n",
                m$niter %||% "?", m$nburnin %||% "?", m$thin %||% "?",
                m$nchains %||% "?", ifelse(is.null(m$seed), "NULL", paste(m$seed, collapse=","))))
  }

  mons <- x$monitors %||% character(0)
  if (length(mons)) cat("  monitors: ", paste(mons, collapse=", "), "\n", sep = "")

  # quick preflight
  chk <- check_dpmixgpd_bundle(x, strict = FALSE)
  if (!chk$ok) {
    cat("  status: NOT OK\n")
    for (e in chk$errors) cat("    ERROR: ", e, "\n", sep="")
  } else {
    cat("  status: OK\n")
  }
  if (length(chk$warnings)) {
    for (w in chk$warnings) cat("    WARN: ", w, "\n", sep="")
  }

  if (isTRUE(code)) {
    cat("\n--- Model code (nimbleCode) ---\n")
    if (!is.null(x$code)) print(x$code) else cat("<no code stored>\n")
  }

  invisible(x)
}

#' Summarize a prepared DPMixGPD bundle
#'
#' Shows a compact overview of the bundle, plus prior table.
#' @param object A \code{dpmixgpd_bundle}.
#' @param ... Unused.
#' @return \code{object} invisibly.
#' @export
summary.dpmixgpd_bundle <- function(object, ...) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  stopifnot(inherits(object, "dpmixgpd_bundle"))

  spec <- object$spec
  meta <- spec$meta

  chk <- check_dpmixgpd_bundle(object, strict = FALSE)

  cat(
    "Bundle summary | backend=",
    .backend_label(meta$backend),
    " | kernel=",
    .kernel_label(meta$kernel),
    " | GPD=",
    meta$GPD,
    "\n",
    sep = ""
  )

  cat(sprintf("n=%s | P=%s\n", chk$N, chk$P))

  if (!chk$ok) {
    cat("\nErrors:\n")
    for (e in chk$errors) cat(" - ", e, "\n", sep="")
  }
  if (length(chk$warnings)) {
    cat("\nWarnings:\n")
    for (w in chk$warnings) cat(" - ", w, "\n", sep="")
  }

  # prior table (uses your internal builder)
  cat("\nPrior table:\n")
  pt <- build_prior_table_from_spec(spec)
  print(pt, row.names = FALSE)

  invisible(object)
}


# S3 methods for Fit objects ----------------------------------------------

# ============================================================
# Public S3 generics (export in your NAMESPACE if packaging)
# ============================================================

#' Print a MixGPD fitted object
#'
#' @param x A fitted object of class \code{"mixgpd_fit"}.
#' @param ... Unused.
#' @return \code{x} invisibly.
#' @export
print.mixgpd_fit <- function(x, ...) {
  cat(paste(.format_fit_header(x), collapse = "\n"), "\n")
  cat("Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.\n")
  invisible(x)
}

#' Summarize a MixGPD fitted object
#'
#' @param object A fitted object of class \code{"mixgpd_fit"}.
#' @param pars Optional character vector of parameters to summarize. If NULL, summarize all (excluding v's).
#' @param probs Numeric vector of quantiles to report.
#' @param ... Unused.
#' @return An object of class \code{"mixgpd_summary"}.
#' @export
summary.mixgpd_fit <- function(object, pars = NULL, probs = c(0.025, 0.5, 0.975), ...) {
  stopifnot(inherits(object, "mixgpd_fit"))
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  tab <- .summarize_posterior(object, pars = pars, probs = probs)

  spec <- object$spec %||% list()
  meta <- spec$meta %||% list()

  out <- list(
    table = tab,
    spec = list(
      backend = meta$backend %||% spec$dispatch$backend %||% "<unknown>",
      kernel  = meta$kernel  %||% spec$kernel$key %||% "<unknown>",
      gpd     = meta$GPD %||% spec$dispatch$GPD
    ),
    waic = object$mcmc$waic %||% object$waic %||% NULL
  )
  class(out) <- "mixgpd_summary"
  out
}


#' Print a MixGPD summary object
#'
#' @param x A \code{"mixgpd_summary"} object.
#' @param digits Number of digits to print.
#' @param max_rows Maximum rows to print.
#' @param ... Unused.
#' @return \code{x} invisibly.
#' @export
print.mixgpd_summary <- function(x, digits = 3, max_rows = 60, ...) {
  stopifnot(inherits(x, "mixgpd_summary"))
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  gpd_txt <- if (isTRUE(x$spec$gpd)) "TRUE" else if (identical(x$spec$gpd, FALSE)) "FALSE" else "<unknown>"

  cat(sprintf("MixGPD summary | backend: %s | kernel: %s | GPD tail: %s\n",
              x$spec$backend %||% "<unknown>",
              x$spec$kernel  %||% "<unknown>",
              gpd_txt))

  if (!is.null(x$waic)) {
    cat("\nWAIC (from nimble::runMCMC):\n")
    print(x$waic)   # forward exactly; do not coerce
  }
  cat("\n")

  tab <- x$table
  tab_print <- tab
  num_cols <- vapply(tab_print, is.numeric, logical(1))
  tab_print[num_cols] <- lapply(tab_print[num_cols], function(v) round(v, digits))

  if (nrow(tab_print) > max_rows) {
    cat(sprintf("Showing first %d of %d parameters.\n\n", max_rows, nrow(tab_print)))
    tab_print <- tab_print[seq_len(max_rows), , drop = FALSE]
  }

  print(tab_print, row.names = FALSE)
  invisible(x)
}



#' Plot MCMC diagnostics for a MixGPD fit (ggmcmc backend)
#'
#' Uses ggmcmc to produce standard MCMC diagnostic plots. Works with 1+ chains.
#'
#' @param x A fitted object of class \code{"mixgpd_fit"}.
#' @param family Character vector of plot names (ggmcmc plot types) or a single one.
#'   Supported: \code{histogram, density, traceplot, running, compare_partial,
#'   autocorrelation, crosscorrelation, Rhat, grb, effective, geweke,
#'   caterpillar, pairs}.
#' @param params Optional parameter selector. Either:
#'   (i) character vector of exact parameter names (e.g. \code{c("alpha","threshold")}),
#'   or (ii) a single regex string (e.g. \code{"alpha|threshold|tail_"}).
#'   If \code{NULL}, plots a reasonable default.
#' @param nLags Number of lags for autocorrelation (ggmcmc).
#' @param ... Passed through to the underlying ggmcmc plotting functions when applicable.
#' @return Invisibly returns a named list of ggplot objects.
#' @export
plot.mixgpd_fit <- function(x,
                            family = c("traceplot", "density"),
                            params = NULL,
                            nLags = 50,
                            ...) {
  stopifnot(inherits(x, "mixgpd_fit"))
  if (!requireNamespace("ggmcmc", quietly = TRUE)) {
    stop("Package 'ggmcmc' is required for plot.mixgpd_fit(). Install it first.", call. = FALSE)
  }
  if (!requireNamespace("coda", quietly = TRUE)) {
    stop("Package 'coda' is required (nimble samplesAsCodaMCMC=TRUE).", call. = FALSE)
  }

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # ---- pull samples (prefer x$mcmc$samples, then x$samples) ----
  smp <- x$mcmc$samples %||% x$samples
  if (is.null(smp)) stop("No samples found in x$mcmc$samples or x$samples.", call. = FALSE)

  # Ensure coda format that ggmcmc understands
  if (inherits(smp, "mcmc")) {
    smp <- coda::mcmc.list(smp)
  } else if (!inherits(smp, "mcmc.list")) {
    # try coercion
    smp <- tryCatch(coda::as.mcmc.list(smp), error = function(e) NULL)
    if (is.null(smp)) stop("Samples are not coercible to coda::mcmc.list.", call. = FALSE)
  }

  # ---- choose default params ----
  cn <- colnames(as.matrix(smp[[1]]))
  if (is.null(params)) {
    # Prefer alpha + threshold/tail params + weights if present
    pref <- unique(c(
      grep("^alpha$", cn, value = TRUE),
      grep("threshold", cn, value = TRUE),
      grep("^tail_", cn, value = TRUE),
      grep("^weights\\[", cn, value = TRUE)
    ))
    params <- if (length(pref) > 0) pref else head(cn, 6)
  }

  # params can be vector of names OR single regex
  if (is.character(params) && length(params) > 1) {
    keep <- params
  } else if (is.character(params) && length(params) == 1) {
    keep <- grep(params, cn, value = TRUE)
    if (length(keep) == 0) stop("No parameters match regex: ", params, call. = FALSE)
  } else {
    stop("'params' must be NULL, a character vector of names, or a single regex string.", call. = FALSE)
  }

  missing <- setdiff(keep, cn)
  if (length(missing) > 0) {
    stop("Unknown params: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  # ---- build ggmcmc long format (DO NOT pass family here) ----
  D <- ggmcmc::ggs(smp, family = NA, burnin = FALSE)
  D <- D[D$Parameter %in% keep, , drop = FALSE]

  # ---- normalize family input ----
  family <- unique(as.character(family))
  allowed <- c("histogram", "density", "traceplot", "running", "compare_partial",
               "autocorrelation", "crosscorrelation", "Rhat", "grb", "effective",
               "geweke", "caterpillar", "pairs")
  bad <- setdiff(family, allowed)
  if (length(bad) > 0) stop("Unknown plot family: ", paste(bad, collapse = ", "), call. = FALSE)

  nChains <- attr(D, "nChains") %||% length(smp)

  # Helper: only run chain-comparison diagnostics when possible
  .need_multi <- function(f) f %in% c("crosscorrelation", "Rhat", "grb", "effective")
  if (nChains < 2) {
    family <- family[!vapply(family, .need_multi, logical(1))]
  }

  plots <- list()

  for (f in family) {
    p <- switch(
      f,
      histogram        = ggmcmc::ggs_histogram(D, family = NA, ...),
      density          = ggmcmc::ggs_density(D, family = NA, ...),
      traceplot        = ggmcmc::ggs_traceplot(D, family = NA, ...),
      running          = ggmcmc::ggs_running(D, family = NA, ...),
      compare_partial  = ggmcmc::ggs_compare_partial(D, family = NA, ...),
      autocorrelation  = ggmcmc::ggs_autocorrelation(D, family = NA, nLags = nLags, ...),
      crosscorrelation = ggmcmc::ggs_crosscorrelation(D, family = NA, ...),
      Rhat             = ggmcmc::ggs_Rhat(D, family = NA, ...),
      grb              = ggmcmc::ggs_grb(D, family = NA, ...),
      effective        = ggmcmc::ggs_effective(D, family = NA, ...),
      geweke           = ggmcmc::ggs_geweke(D, family = NA, ...),
      caterpillar      = ggmcmc::ggs_caterpillar(D, family = NA, ...),
      pairs            = ggmcmc::ggs_pairs(D, family = NA, ...)
    )

    plots[[f]] <- p
    print(p)
  }

  invisible(plots)
}




#' Predict from a MixGPD fit
#'
#' This provides a stable interface for distributional predictions.
#' The default implementation supports:
#' - \code{type="quantile"} using \code{p}
#' - \code{type="cdf"} / \code{"survival"} using \code{y}
#'
#' If your object stores dedicated predictive machinery, you can keep this signature
#' and swap the internals without breaking user code.
#'
#' @param object A fitted object of class \code{"mixgpd_fit"}.
#' @param newdata Optional new data. If \code{NULL}, uses training design (if stored).
#' @param type Prediction type: \code{"quantile"}, \code{"cdf"}, \code{"survival"}.
#' @param p Numeric vector of probabilities for quantiles (required for \code{type="quantile"}).
#' @param y Numeric vector of evaluation points (required for \code{type="cdf"} or \code{"survival"}).
#' @param interval \code{"none"} or \code{"credible"} for posterior credible bands.
#' @param probs Quantiles for credible interval bands.
#' @param ... Unused.
#' @return A list with elements:
#'   \itemize{
#'     \item \code{fit}: matrix (nrow = n_newdata, ncol = length(p or y)) of posterior medians.
#'     \item \code{lower}, \code{upper}: matrices for credible interval if requested (else \code{NULL}).
#'     \item \code{type}, \code{grid}: metadata.
#'   }
#' @export
predict.mixgpd_fit <- function(object,
                               newdata = NULL,
                               type = c("quantile", "cdf", "survival"),
                               p = NULL,
                               y = NULL,
                               interval = c("none", "credible"),
                               probs = c(0.025, 0.5, 0.975),
                               ...) {
  .validate_fit(object)
  type <- match.arg(type)
  interval <- match.arg(interval)

  if (type == "quantile" && (is.null(p) || length(p) == 0)) {
    stop("For type='quantile', provide non-empty 'p'.", call. = FALSE)
  }
  if (type %in% c("cdf", "survival") && (is.null(y) || length(y) == 0)) {
    stop("For type='cdf' or 'survival', provide non-empty 'y'.", call. = FALSE)
  }

  .predict_mixgpd(object, newdata = newdata, type = type, p = p, y = y,
                  interval = interval, probs = probs)
}

#' Extract coefficients from a MixGPD fit
#'
#' @param object A fitted object of class \code{"mixgpd_fit"}.
#' @param component Which coefficients to extract: \code{"bulk"}, \code{"tail"}, or \code{"both"}.
#' @param format Output format: \code{"vector"}, \code{"list"}, or \code{"tidy"}.
#' @param probs Quantiles for intervals when \code{format="tidy"}.
#' @param ... Unused.
#' @return Coefficients in the requested format.
#' @export
coef.mixgpd_fit <- function(object,
                            component = c("bulk", "tail", "both"),
                            format = c("vector", "list", "tidy"),
                            probs = c(0.025, 0.5, 0.975),
                            ...) {
  .validate_fit(object)
  component <- match.arg(component)
  format <- match.arg(format)

  mat <- .extract_draws_matrix(object, drop_v = TRUE)
  nms <- colnames(mat)

  bulk_pat <- "(^beta(?!_u|_sigma|_xi))|beta_mu|beta_mean|beta_bulk|\\bbeta\\b"
  tail_pat <- "beta_u|beta_sigma|beta_xi|beta_tail|threshold|u_coef|sigma_coef|xi_coef"

  bulk_names <- nms[grepl(bulk_pat, nms, perl = TRUE)]
  tail_names <- nms[grepl(tail_pat, nms, perl = TRUE)]

  sel <- switch(component,
                bulk = bulk_names,
                tail = tail_names,
                both = unique(c(bulk_names, tail_names)))

  if (!length(sel)) {
    if (format == "list") return(list(bulk = numeric(0), tail = numeric(0)))
    if (format == "tidy") return(data.frame())
    return(setNames(numeric(0), character(0)))
  }

  sub <- mat[, sel, drop = FALSE]
  meanv <- colMeans(sub, na.rm = TRUE)

  if (format == "vector") return(meanv)

  if (format == "list") {
    return(list(
      bulk = meanv[intersect(names(meanv), bulk_names)],
      tail = meanv[intersect(names(meanv), tail_names)]
    ))
  }

  qmat <- t(apply(sub, 2, stats::quantile, probs = probs, na.rm = TRUE, names = FALSE))
  colnames(qmat) <- paste0("q", formatC(probs, format = "f", digits = 3))

  df <- data.frame(
    term = names(meanv),
    mean = as.numeric(meanv),
    sd   = as.numeric(apply(sub, 2, stats::sd, na.rm = TRUE)),
    qmat,
    stringsAsFactors = FALSE
  )
  df$block <- ifelse(df$term %in% tail_names, "tail", "bulk")
  df <- df[, c("block", "term", setdiff(names(df), c("block", "term"))), drop = FALSE]
  rownames(df) <- NULL
  df
}

#' Variance-covariance matrix for coefficient draws
#'
#' Computes the posterior covariance matrix for the selected coefficient block
#' using the MCMC draws and returns a numeric covariance matrix.
#'
#' @param object A fitted object of class \code{"mixgpd_fit"}.
#' @param component Which coefficients: \code{"bulk"} or \code{"tail"}.
#' @param ... Unused.
#' @return Numeric covariance matrix.
#' @export
vcov.mixgpd_fit <- function(object, component = c("bulk", "tail"), ...) {
  .validate_fit(object)
  component <- match.arg(component)

  coefs <- coef(object, component = component, format = "vector")
  if (!length(coefs)) stop("No coefficients found for component='", component, "'.", call. = FALSE)

  mat <- .extract_draws_matrix(object, drop_v = TRUE)
  pars <- intersect(names(coefs), colnames(mat))
  if (!length(pars)) stop("Coefficient names not found in draw matrix.", call. = FALSE)

  stats::cov(mat[, pars, drop = FALSE])
}

#' Residual-style diagnostics for a MixGPD fit
#'
#' Currently supports PIT (probability integral transform) residuals only when
#' a working \code{predict(type="cdf")} is available for the training data.
#'
#' @param object A fitted object of class \code{"mixgpd_fit"}.
#' @param type Residual type. Only \code{"pit"} is implemented here.
#' @param ... Unused.
#' @return Numeric vector of residuals (length nobs).
#' @export
residuals.mixgpd_fit <- function(object, type = c("pit"), ...) {
  .validate_fit(object)
  type <- match.arg(type)

  y <- object$data$y %||% object$y
  if (is.null(y)) stop("Training outcomes not found in object$data$y or object$y.", call. = FALSE)

  if (type == "pit") {
    pr <- predict(object, type = "cdf", y = y, interval = "none")
    # pr$fit is n x length(ygrid) normally; here ygrid == y (vector)
    # We interpret diagonal if dimensions align; otherwise take row-wise matching.
    Fhat <- pr$fit
    if (is.matrix(Fhat)) {
      if (nrow(Fhat) == length(y) && ncol(Fhat) == length(y)) {
        pit <- diag(Fhat)
      } else if (nrow(Fhat) == length(y) && ncol(Fhat) == 1) {
        pit <- as.numeric(Fhat[, 1])
      } else {
        # fallback: use first column if ambiguous
        pit <- as.numeric(Fhat[, 1])
      }
    } else {
      pit <- as.numeric(Fhat)
    }
    return(pit)
  }

  stop("Unsupported residual type.", call. = FALSE)
}


#' Log-likelihood for a MixGPD fit
#'
#' This method returns an approximate log-likelihood using a point summary
#' of parameters (posterior median by default) IF the object contains a
#' callable log-likelihood function at \code{object$engine$loglik}.
#'
#' Why the indirection? Because marginal likelihood for mixtures can be subtle;
#' the engine is the right place to define it. This S3 method is a clean wrapper.
#'
#' @param object A fitted object of class \code{"mixgpd_fit"}.
#' @param point Point summary used when the engine needs parameter values:
#'   \code{"posterior_median"} or \code{"posterior_mean"}.
#' @param ... Passed to the engine log-likelihood.
#' @return An object of class \code{"logLik"} if available; otherwise errors.
#' @export
logLik.mixgpd_fit <- function(object, point = c("posterior_median", "posterior_mean"), ...) {
  .validate_fit(object)
  point <- match.arg(point)

  eng <- object$engine %||% list()
  llfun <- eng$loglik
  if (!is.function(llfun)) {
    stop("No engine log-likelihood found at object$engine$loglik (must be a function).", call. = FALSE)
  }

  draws <- .extract_draws(object)
  theta <- if (point == "posterior_median") {
    apply(draws, 2, stats::median, na.rm = TRUE)
  } else {
    colMeans(draws, na.rm = TRUE)
  }

  val <- llfun(theta = theta, object = object, ...)
  val <- as.numeric(val)[1]
  out <- structure(val, df = length(theta), nobs = .get_nobs(object), class = "logLik")
  out
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
