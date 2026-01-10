# 04-S3-Methods.R for DPMixGPD bundles and fits

# S3 methods for Bundle objects -------------------------------------------



#' Print a dpmixgpd bundle
#'
#' User-facing print method for pre-run bundles produced by \code{build_nimble_bundle()}.
#' This prints a compact description of the model structure (backend/kernel/components),
#' whether covariates are used, and whether a GPD tail is enabled.
#'
#' @param x A \code{"dpmixgpd_bundle"} object.
#' @param code Logical; if TRUE, print the generated NIMBLE model code.
#' @param max_code_lines Integer; maximum number of code lines to print when \code{code=TRUE}.
#' @param ... Unused.
#' @return The object \code{x}, invisibly.
#' @examples
#' \dontrun{
#' y <- abs(stats::rnorm(50)) + 0.1
#' bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
#'                              GPD = FALSE, components = 6)
#' print(bundle)
#' print(bundle, code = TRUE, max_code_lines = 30)
#' }
#' @export
print.dpmixgpd_bundle <- function(x, code = FALSE, max_code_lines = 200L, ...) {
  stopifnot(inherits(x, "dpmixgpd_bundle"))
  spec <- x$spec
  meta <- spec$meta

  backend <- meta$backend
  kernel  <- meta$kernel
  K       <- meta$components
  N       <- meta$N
  P       <- meta$P %||% 0L
  has_X   <- isTRUE(meta$has_X)
  GPD     <- isTRUE(meta$GPD)

  cat("DPmixGPD bundle\n")
  tbl <- data.frame(
    Field = c("Backend", "Kernel", "Components", "N", "X", "GPD", "Epsilon"),
    Value = c(.backend_label(backend),
              .kernel_label(kernel),
              as.character(K),
              as.character(N),
              if (has_X) sprintf("YES (P=%d)", P) else "NO",
              if (GPD) "TRUE" else "FALSE",
              as.character(x$epsilon %||% 0.025)),
    stringsAsFactors = FALSE
  )
  print(tbl, row.names = FALSE)
  cat("\n  contains  : code, constants, data, dimensions, inits, monitors\n")

  if (isTRUE(code)) {
    cat("\nModel code\n")
    if (is.null(x$code)) {
      cat("  <no code available>\n")
    } else {
      out <- paste(deparse(x$code), collapse = "\n")
      out <- strsplit(out, "\n", fixed = TRUE)[[1]]
      if (!is.finite(max_code_lines) || max_code_lines <= 0L) {
        cat(paste(out, collapse = "\n"), "\n")
      } else {
        max_code_lines <- as.integer(max_code_lines)
        show_n <- min(length(out), max_code_lines)
        if (show_n > 0L) {
          cat(paste(out[seq_len(show_n)], collapse = "\n"), "\n")
        }
        if (length(out) > show_n) {
          cat(sprintf("... (%d more lines)\n", length(out) - show_n))
        }
      }
    }
  }

  invisible(x)
}

#' Print a causal bundle
#'
#' User-facing print method for causal bundles produced by \code{build_causal_bundle()}.
#'
#' @param x A \code{"dpmixgpd_causal_bundle"} object.
#' @param code Logical; if TRUE, print generated NIMBLE code for each block.
#' @param max_code_lines Integer; maximum number of code lines to print when \code{code=TRUE}.
#' @param ... Unused.
#' @return The input object (invisibly).
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal")
#' print(cb)
#' }
#' @export
print.dpmixgpd_causal_bundle <- function(x, code = FALSE, max_code_lines = 200L, ...) {
  stopifnot(inherits(x, "dpmixgpd_causal_bundle"))

  meta <- x$meta %||% list()
  cat("DPmixGPD causal bundle\n")
  cat("PS model: Bayesian logit (T | X)\n")
  backend <- meta$backend %||% list()
  kernel <- meta$kernel %||% list()
  gpd <- meta$GPD %||% list()
  comps <- meta$components %||% list()
  eps <- meta$epsilon %||% list()

  cat("Outcome (treated): backend =", backend$trt %||% "?", "| kernel =", kernel$trt %||% "?", "\n")
  cat("Outcome (control): backend =", backend$con %||% "?", "| kernel =", kernel$con %||% "?", "\n")
  cat("GPD tail (treated/control):", ifelse(isTRUE(gpd$trt), "TRUE", "FALSE"),
      "/", ifelse(isTRUE(gpd$con), "TRUE", "FALSE"), "\n")
  cat("components (treated/control):", comps$trt %||% "?", "/", comps$con %||% "?", "\n")
  cat("epsilon (treated/control):", eps$trt %||% "?", "/", eps$con %||% "?", "\n")
  cat("n (control) =", length(x$index$con %||% integer(0)),
      "| n (treated) =", length(x$index$trt %||% integer(0)), "\n")

  if (isTRUE(code)) {
    cat("\n-- PS code --\n")
    print(x$design, code = TRUE, max_code_lines = max_code_lines)
    cat("\n-- Outcome code (control) --\n")
    print(x$outcome$con, code = TRUE, max_code_lines = max_code_lines)
    cat("\n-- Outcome code (treated) --\n")
    print(x$outcome$trt, code = TRUE, max_code_lines = max_code_lines)
  }

  invisible(x)
}

#' Summarize a causal bundle
#'
#' User-facing summary for causal bundles produced by \code{build_causal_bundle()}.
#'
#' @param object A \code{"dpmixgpd_causal_bundle"} object.
#' @param code Logical; if TRUE, print generated NIMBLE code for each block.
#' @param max_code_lines Integer; maximum number of code lines to print when \code{code=TRUE}.
#' @param ... Unused.
#' @return The input object (invisibly).
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal")
#' summary(cb)
#' }
#' @export
summary.dpmixgpd_causal_bundle <- function(object, code = FALSE, max_code_lines = 200L, ...) {
  stopifnot(inherits(object, "dpmixgpd_causal_bundle"))

  cat("DPmixGPD causal bundle summary\n")
  print.dpmixgpd_causal_bundle(object, code = FALSE, max_code_lines = max_code_lines)
  if (isTRUE(code)) {
    cat("\n-- PS code --\n")
    print(object$design, code = TRUE, max_code_lines = max_code_lines)
    cat("\n-- Outcome code (control) --\n")
    print(object$outcome$con, code = TRUE, max_code_lines = max_code_lines)
    cat("\n-- Outcome code (treated) --\n")
    print(object$outcome$trt, code = TRUE, max_code_lines = max_code_lines)
  }
  invisible(object)
}

#' Print a propensity score bundle
#'
#' @param x A \code{"dpmixgpd_ps_bundle"} object.
#' @param code Logical; if TRUE, print generated NIMBLE code for the PS model.
#' @param max_code_lines Integer; maximum number of code lines to print when \code{code=TRUE}.
#' @param ... Unused.
#' @return The input object (invisibly).
#' @keywords internal
#' @noRd
print.dpmixgpd_ps_bundle <- function(x, code = FALSE, max_code_lines = 200L, ...) {
  stopifnot(inherits(x, "dpmixgpd_ps_bundle"))

  meta <- x$spec$meta %||% list()
  cat("PS bundle\n")
  cat("model:", meta$type %||% "ps_logit", "\n")
  cat("include_intercept:", isTRUE(meta$include_intercept), "\n")
  if (isTRUE(code)) {
    cat("code:\n")
    txt <- paste(deparse(x$code), collapse = "\n")
    lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    nshow <- min(length(lines), as.integer(max_code_lines))
    if (nshow > 0) {
      cat(paste(lines[seq_len(nshow)], collapse = "\n"), "\n")
    }
    if (length(lines) > nshow) {
      cat("... (truncated)\n")
    }
  }
  invisible(x)
}

#' @keywords internal
#' @noRd
summary.dpmixgpd_ps_bundle <- function(object, code = FALSE, max_code_lines = 200L, ...) {
  print.dpmixgpd_ps_bundle(object, code = isTRUE(code), max_code_lines = max_code_lines)
  invisible(object)
}

#' Print a causal fit
#'
#' @param x A \code{"dpmixgpd_causal_fit"} object.
#' @param ... Unused.
#' @return The input object (invisibly).
#' @export
print.dpmixgpd_causal_fit <- function(x, ...) {
  stopifnot(inherits(x, "dpmixgpd_causal_fit"))

  meta <- x$bundle$meta %||% list()
  cat("DPmixGPD causal fit\n")
  cat("PS model: Bayesian logit (T | X)\n")
  cat("Outcome (treated): backend =", (meta$backend %||% list())$trt %||% "?", "| kernel =",
      (meta$kernel %||% list())$trt %||% "?", "\n")
  cat("Outcome (control): backend =", (meta$backend %||% list())$con %||% "?", "| kernel =",
      (meta$kernel %||% list())$con %||% "?", "\n")
  cat("GPD tail (treated/control):", ifelse(isTRUE((meta$GPD %||% list())$trt), "TRUE", "FALSE"),
      "/", ifelse(isTRUE((meta$GPD %||% list())$con), "TRUE", "FALSE"), "\n")
  invisible(x)
}

#' Summarize a causal fit
#'
#' @param object A \code{"dpmixgpd_causal_fit"} object.
#' @param ... Unused.
#' @return The input object (invisibly).
#' @export
summary.dpmixgpd_causal_fit <- function(object, ...) {
  stopifnot(inherits(object, "dpmixgpd_causal_fit"))
  cat("-- PS fit --\n")
  print(object$ps_fit)
  cat("\n-- Outcome fits --\n")
  cat("[control]\n")
  print(object$outcome_fit$con)
  cat("\n[treated]\n")
  print(object$outcome_fit$trt)
  invisible(object)
}

#' Print a propensity score fit
#'
#' @param x A \code{"dpmixgpd_ps_fit"} object.
#' @param ... Unused.
#' @return The input object (invisibly).
#' @keywords internal
#' @noRd
print.dpmixgpd_ps_fit <- function(x, ...) {
  stopifnot(inherits(x, "dpmixgpd_ps_fit"))
  cat("DPmixGPD PS fit\n")
  cat("model: ps_logit\n")
  invisible(x)
}

#' @keywords internal
#' @noRd
summary.dpmixgpd_ps_fit <- function(object, ...) {
  print.dpmixgpd_ps_fit(object)
  invisible(object)
}

#' Plot a causal fit
#'
#' @param x A \code{"dpmixgpd_causal_fit"} object.
#' @param arm Integer or character; \code{1} or \code{"treated"} for treatment,
#'   \code{0} or \code{"control"} for control.
#' @param ... Additional arguments forwarded to the underlying outcome plot method.
#' @return The result of the underlying plot call (invisibly).
#' @export
plot.dpmixgpd_causal_fit <- function(x, arm = 1, ...) {
  stopifnot(inherits(x, "dpmixgpd_causal_fit"))
  if (is.character(arm)) {
    arm <- match.arg(tolower(arm), c("treated", "control"))
  }
  if (is.numeric(arm)) {
    if (length(arm) != 1L || is.na(arm)) {
      stop("arm must be a single numeric value (0 = control, 1 = treated).", call. = FALSE)
    }
    if (arm == 1) {
      arm <- "treated"
    } else if (arm == 0) {
      arm <- "control"
    } else {
      stop("arm must be 0 (control) or 1 (treated).", call. = FALSE)
    }
  }
  if (identical(arm, "treated")) {
    out <- plot.mixgpd_fit(x$outcome_fit$trt, ...)
  } else if (identical(arm, "control")) {
    out <- plot.mixgpd_fit(x$outcome_fit$con, ...)
  } else {
    stop("arm must be 0/1 or 'treated'/'control'.", call. = FALSE)
  }
  invisible(out)
}
# helper
`%||%` <- function(a, b) if (!is.null(a)) a else b


#' Summarize a dpmixgpd bundle
#'
#' User-facing summary for pre-run bundles produced by \code{build_nimble_bundle()}.
#' This prints:
#' \itemize{
#'   \item meta information (backend, kernel, components, N, covariates, GPD flag)
#'   \item a readable prior/parameter table derived from \code{spec$plan}
#'   \item monitor set overview
#' }
#'
#' @param object A \code{"dpmixgpd_bundle"} object.
#' @param ... Unused.
#' @return An invisible list with elements \code{meta}, \code{priors}, \code{monitors}.
#' @examples
#' \dontrun{
#' y <- abs(stats::rnorm(50)) + 0.1
#' bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
#'                              GPD = FALSE, components = 6)
#' summary(bundle)
#' }
#' @export
summary.dpmixgpd_bundle <- function(object, ...) {
  stopifnot(inherits(object, "dpmixgpd_bundle"))
  spec <- object$spec
  meta <- spec$meta

  backend <- meta$backend
  kernel  <- meta$kernel
  K       <- meta$components
  N       <- meta$N
  P       <- meta$P %||% 0L
  has_X   <- isTRUE(meta$has_X)
  GPD     <- isTRUE(meta$GPD)

  cat("DPmixGPD bundle summary\n")
  meta_tbl <- data.frame(
    Field = c("Backend", "Kernel", "Components", "N", "X", "GPD", "Epsilon"),
    Value = c(.backend_label(backend),
              .kernel_label(kernel),
              as.character(K),
              as.character(N),
              if (has_X) sprintf("YES (P=%d)", P) else "NO",
              if (GPD) "TRUE" else "FALSE",
              as.character(object$epsilon %||% 0.025)),
    stringsAsFactors = FALSE
  )
  print(meta_tbl, row.names = FALSE)
  cat("\n")

  # Prior/parameter table
  pri <- build_prior_table_from_spec(spec)
  cat("Parameter specification\n")
  print(pri, row.names = FALSE)
  cat("\n")

  # Monitor overview (compact)
  mons <- object$monitors %||% character()
  cat("Monitors\n")
  cat("  n =", length(mons), "\n")
  if (length(mons)) {
    # show first few, but don't spam
    show_n <- min(12L, length(mons))
    cat("  ", paste(mons[seq_len(show_n)], collapse = ", "), if (length(mons) > show_n) ", ..." else "", "\n", sep = "")
  }
  cat("\n")

  invisible(list(
    meta = meta,
    priors = pri,
    monitors = mons
  ))
}

# helper
`%||%` <- function(a, b) if (!is.null(a)) a else b



# S3 methods for Fit objects ----------------------------------------------

# ============================================================
# Public S3 generics (export in your NAMESPACE if packaging)
# ============================================================

#' Print a MixGPD fitted object
#'
#' @param x A fitted object of class \code{"mixgpd_fit"}.
#' @param ... Unused.
#' @return \code{x} invisibly.
#' @examples
#' \dontrun{
#' y <- abs(stats::rnorm(50)) + 0.1
#' bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
#'                              GPD = TRUE, components = 6,
#'                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
#' fit <- run_mcmc_bundle_manual(bundle)
#' print(fit)
#' }
#' @export
print.mixgpd_fit <- function(x, ...) {
  cat(paste(.format_fit_header(x), collapse = "\n"), "\n")
  cat("Fit\n")
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
#' @examples
#' \dontrun{
#' y <- abs(stats::rnorm(50)) + 0.1
#' bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
#'                              GPD = TRUE, components = 6,
#'                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
#' fit <- run_mcmc_bundle_manual(bundle)
#' summary(fit, pars = c("alpha", "threshold"))
#' }
#' @export
summary.mixgpd_fit <- function(object, pars = NULL, probs = c(0.025, 0.5, 0.975), ...) {
  stopifnot(inherits(object, "mixgpd_fit"))
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  tab <- .summarize_posterior(object, pars = pars, probs = probs)

  spec <- object$spec %||% list()
  meta <- spec$meta %||% list()
  eps <- .get_epsilon(object, epsilon = NULL)
  trunc <- .truncation_info(object, epsilon = eps)
  waic <- object$mcmc$waic %||% object$waic %||% NULL

  model <- list(
    backend = meta$backend %||% spec$dispatch$backend %||% "<unknown>",
    kernel  = meta$kernel  %||% spec$kernel$key %||% "<unknown>",
    gpd     = meta$GPD %||% spec$dispatch$GPD,
    epsilon = eps,
    truncation = trunc,
    n = .get_nobs(object),
    components = meta$components %||% spec$components %||% NA_integer_
  )

  out <- list(
    model = model,
    waic = waic,
    table = tab
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
#' @examples
#' \dontrun{
#' y <- abs(stats::rnorm(50)) + 0.1
#' bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
#'                              GPD = TRUE, components = 6,
#'                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
#' fit <- run_mcmc_bundle_manual(bundle)
#' s <- summary(fit)
#' print(s, digits = 2)
#' }
#' @export
print.mixgpd_summary <- function(x, digits = 3, max_rows = 60, ...) {
  stopifnot(inherits(x, "mixgpd_summary"))
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  model <- x$model %||% list()
  waic <- x$waic
  trunc <- model$truncation %||% list()

  gpd_txt <- if (isTRUE(model$gpd)) "TRUE" else if (identical(model$gpd, FALSE)) "FALSE" else "<unknown>"
  eps <- model$epsilon %||% NA_real_

  cat(sprintf("MixGPD summary | backend: %s | kernel: %s | GPD tail: %s | epsilon: %s\n",
              .backend_label(model$backend %||% "<unknown>"),
              .kernel_label(model$kernel  %||% "<unknown>"),
              gpd_txt,
              ifelse(is.na(eps), "<unknown>", eps)))
  cat(sprintf("n = %s | components = %s\n",
              ifelse(is.na(model$n), "<unknown>", model$n),
              ifelse(is.na(model$components), "<unknown>", model$components)))
  cat("Summary\n")

  if (!is.na(eps) && eps > 0) {
    cat(sprintf("Initial components: %s | Components after truncation: %s\n",
                ifelse(is.na(model$components), "<unknown>", model$components),
                trunc$Kt %||% "<unknown>"))
  }

  if (!is.null(waic)) {
    wa <- waic$WAIC %||% waic$waic %||% waic[["WAIC"]] %||% NA_real_
    lp <- waic$lppd %||% waic[["lppd"]] %||% NA_real_
    pw <- waic$pWAIC %||% waic[["pWAIC"]] %||% NA_real_
    cat(sprintf("\nWAIC: %s\n",
                ifelse(is.na(wa), "<unknown>", formatC(wa, format = "f", digits = digits))))
    if (is.finite(lp) || is.finite(pw)) {
      cat(sprintf("lppd: %s | pWAIC: %s\n",
                  ifelse(is.finite(lp), formatC(lp, format = "f", digits = digits), "<unknown>"),
                  ifelse(is.finite(pw), formatC(pw, format = "f", digits = digits), "<unknown>")))
    }
  }
  cat("\nSummary table\n")

  tab_print <- x$table
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
#' @examples
#' \dontrun{
#' y <- abs(stats::rnorm(50)) + 0.1
#' bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
#'                              GPD = TRUE, components = 6,
#'                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
#' fit <- run_mcmc_bundle_manual(bundle)
#' plot(fit, family = c("traceplot", "density"))
#' }
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
#' - \code{type="density"} using \code{y}
#' - \code{type="survival"} using \code{y}
#' - \code{type="quantile"} using \code{p}
#' - \code{type="sample"} (posterior predictive draws)
#' - \code{type="mean"} (posterior predictive mean)
#'
#' If your object stores dedicated predictive machinery, you can keep this signature
#' and swap the internals without breaking user code.
#'
#' @param object A fitted object of class \code{"mixgpd_fit"}.
#' @param x Optional new data. Alias for \code{newdata}.
#' @param newdata Optional new data. If \code{NULL}, uses training design (if stored).
#' @param type Prediction type: \code{"density"}, \code{"survival"}, \code{"quantile"},
#'   \code{"sample"}, \code{"mean"}.
#' @param p Numeric vector of probabilities for quantiles (required for \code{type="quantile"}).
#' @param y Numeric vector of evaluation points (required for \code{type="density"} or \code{"survival"}).
#' @param interval \code{"none"} or \code{"credible"} for posterior credible bands.
#' @param probs Quantiles for credible interval bands.
#' @param nsim Number of posterior predictive samples (for \code{type="sample"}).
#' @param store_draws Logical; whether to store all posterior draws (for \code{type="sample"}).
#' @param nsim_mean Number of posterior predictive samples to use for posterior mean estimation (for \code{type="mean"}).
#' @param ncores Number of CPU cores to use for parallel prediction (if supported).
#' @param ... Unused.
#' @return A list with elements:
#'   \itemize{
#'     \item \code{fit}: matrix (nrow = n_newdata, ncol = length(p or y)) of posterior medians.
#'     \item \code{lower}, \code{upper}: matrices for credible interval if requested (else \code{NULL}).
#'     \item \code{type}, \code{grid}: metadata.
#'   }
#' @examples
#' \dontrun{
#' y <- abs(stats::rnorm(50)) + 0.1
#' bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
#'                              GPD = TRUE, components = 6,
#'                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
#' fit <- run_mcmc_bundle_manual(bundle)
#' pr <- predict(fit, type = "quantile", p = c(0.5, 0.9))
#' pr_surv <- predict(fit, y = sort(y), type = "survival")
#' pr_cdf <- list(fit = 1 - pr_surv$fit)
#' }
#' @export
predict.mixgpd_fit <- function(object,
                               x = NULL,
                               y = NULL,
                               newdata = NULL,
                               type = c("density", "survival",
                                        "quantile", "sample", "mean"),
                               p = NULL,
                               nsim = NULL,
                               interval = c("none", "credible"),
                               probs = c(0.025, 0.5, 0.975),
                               store_draws = TRUE,
                               nsim_mean = 200L,
                               ncores = 1L,
                               ...) {
  .validate_fit(object)

  type <- match.arg(type)
  interval <- match.arg(interval)

  # Backwards-compat: allow newdata alias for x
  if (!is.null(newdata) && !is.null(x)) {
    stop("Provide only one of 'x' or 'newdata' (they are aliases).", call. = FALSE)
  }
  if (!is.null(newdata) && is.null(x)) x <- newdata

  ncores <- as.integer(ncores)
  if (is.na(ncores) || ncores < 1L) stop("'ncores' must be an integer >= 1.", call. = FALSE)

  .predict_mixgpd(object,
                  x = x,
                  y = y,
                  type = type,
                  p = p,
                  nsim = nsim,
                  interval = interval,
                  probs = probs,
                  store_draws = store_draws,
                  nsim_mean = nsim_mean,
                  ncores = ncores)
}



#' Extract coefficients from a MixGPD fit
#'
#' @param object A fitted object of class \code{"mixgpd_fit"}.
#' @param component Which coefficients to extract: \code{"bulk"}, \code{"tail"}, or \code{"both"}.
#' @param format Output format: \code{"vector"}, \code{"list"}, or \code{"tidy"}.
#' @param probs Quantiles for intervals when \code{format="tidy"}.
#' @param ... Unused.
#' @return Coefficients in the requested format.
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
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
#' a working \code{predict(type="survival")} is available for the training data.
#'
#' @param object A fitted object of class \code{"mixgpd_fit"}.
#' @param type Residual type. Only \code{"pit"} is implemented here.
#' @param ... Unused.
#' @return Numeric vector of residuals (length nobs).
#' @examples
#' \dontrun{
#' y <- abs(stats::rnorm(50)) + 0.1
#' bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
#'                              GPD = TRUE, components = 6,
#'                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
#' fit <- run_mcmc_bundle_manual(bundle)
#' pit <- residuals(fit, type = "pit")
#' }
#' @export
residuals.mixgpd_fit <- function(object, type = c("pit"), ...) {
  .validate_fit(object)
  type <- match.arg(type)
  y <- object$data$y %||% object$y
  if (is.null(y)) stop("Training outcomes not found in object$data$y or object$y.", call. = FALSE)
  X <- object$data$X %||% object$X %||% NULL

  if (type == "pit") {
    if (!is.null(X)) {
      pr <- predict(object, x = X, type = "survival", y = y, interval = "none")
    } else {
      pr <- predict(object, type = "survival", y = y, interval = "none")
    }
    # pr$fit is n x length(ygrid) normally; here ygrid == y (vector)
    # We interpret diagonal if dimensions align; otherwise take row-wise matching.
    Fhat <- 1 - pr$fit
    if (is.matrix(Fhat)) {
      if (nrow(Fhat) == length(y) && ncol(Fhat) == length(y)) {
        pit <- diag(Fhat)
      } else if (nrow(Fhat) == 1 && ncol(Fhat) == length(y)) {
        pit <- as.numeric(Fhat[1, ])
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
#' @keywords internal
#' @noRd
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
#' @examples
#' \dontrun{
#' y <- abs(stats::rnorm(50)) + 0.1
#' bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
#'                              GPD = TRUE, components = 6,
#'                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
#' fit <- run_mcmc_bundle_manual(bundle)
#' fitted(fit)
#' }
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
