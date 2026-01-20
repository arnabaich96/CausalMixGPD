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
    code_obj <- .extract_nimble_code(x$code)
    if (is.null(code_obj)) {
      cat("  <no code available>\n")
    } else {
      out <- paste(deparse(code_obj), collapse = "\n")
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
  ps_meta <- meta$ps %||% list()
  ps_model <- ps_meta$model_type %||% FALSE
  ps_label <- if (!isTRUE(ps_meta$enabled) || isFALSE(ps_model)) {
    "PS model: disabled"
  } else {
    switch(ps_model,
           logit = "PS model: Bayesian logit (T | X)",
           probit = "PS model: Bayesian probit (T | X)",
           naive = "PS model: Gaussian naive Bayes",
           sprintf("PS model: %s", ps_model))
  }
  cat(ps_label, "\n")
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
  cat("Outcome PS included:", ifelse(isTRUE(meta$ps$enabled), "TRUE", "FALSE"), "\n")
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
  model_type <- meta$type %||% "ps_logit"
  model_label <- switch(model_type,
                        ps_logit = "logit",
                        ps_probit = "probit",
                        ps_naive = "naive",
                        model_type)
  cat("model:", model_label, "\n")
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
  ps_meta <- meta$ps %||% list()
  ps_model <- ps_meta$model_type %||% FALSE
  ps_label <- if (!isTRUE(ps_meta$enabled) || isFALSE(ps_model)) {
    "PS model: disabled"
  } else {
    switch(ps_model,
           logit = "PS model: Bayesian logit (T | X)",
           probit = "PS model: Bayesian probit (T | X)",
           naive = "PS model: Gaussian naive Bayes",
           sprintf("PS model: %s", ps_model))
  }
  cat(ps_label, "\n")
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
  bundle <- object$bundle %||% list()
  has_X <- !is.null(bundle$data$X %||% NULL)
  ps_enabled <- isTRUE(bundle$meta$ps$enabled) && has_X
  if (ps_enabled) {
    cat("-- PS fit --\n")
    print(object$ps_fit)
  }
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
  model_type <- x$bundle$spec$meta$type %||% "ps_logit"
  model_label <- switch(model_type,
                        ps_logit = "logit",
                        ps_probit = "probit",
                        ps_naive = "naive",
                        model_type)
  cat("model:", model_label, "\n")
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
plot.dpmixgpd_causal_fit <- function(x, arm = "both", ...) {
  stopifnot(inherits(x, "dpmixgpd_causal_fit"))
  if (is.null(arm)) arm <- "both"
  if (is.character(arm)) {
    arm <- match.arg(tolower(arm), c("treated", "control", "both"))
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
  if (identical(arm, "both")) {
    out <- list(
      treated = plot.mixgpd_fit(x$outcome_fit$trt, ...),
      control = plot.mixgpd_fit(x$outcome_fit$con, ...)
    )
    class(out) <- c("dpmixgpd_causal_fit_plots", "list")
    return(out)
  }
  if (identical(arm, "treated")) {
    out <- plot.mixgpd_fit(x$outcome_fit$trt, ...)
  } else if (identical(arm, "control")) {
    out <- plot.mixgpd_fit(x$outcome_fit$con, ...)
  } else {
    stop("arm must be 0/1 or 'treated'/'control'/'both'.", call. = FALSE)
  }
  out
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

#' Extract posterior mean parameters in original form
#'
#' Returns posterior means reshaped to their natural dimensions (scalars, vectors,
#' or matrices). Intended as a lightweight extractor for model parameters.
#'
#' @param object A fitted object of class \code{"mixgpd_fit"}.
#' @param ... Unused.
#' @return An object of class \code{"mixgpd_params"} (a named list).
#' @examples
#' \dontrun{
#' y <- abs(stats::rnorm(50)) + 0.1
#' bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
#'                              GPD = TRUE, components = 6,
#'                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
#' fit <- run_mcmc_bundle_manual(bundle)
#' params(fit)
#' p <- params(fit)
#' }
#' @export
params <- function(object, ...) {
  UseMethod("params")
}

#' @export
params.mixgpd_fit <- function(object, ...) {
  stopifnot(inherits(object, "mixgpd_fit"))
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  mat <- .extract_draws_matrix(object, drop_v = TRUE, epsilon = NULL)
  means <- colMeans(mat, na.rm = TRUE)
  means[!is.finite(means)] <- NA_real_
  cn <- names(means)

  spec <- object$spec %||% list()
  plan <- spec$plan %||% list()
  meta <- spec$meta %||% list()
  bulk <- plan$bulk %||% list()
  gpd <- plan$gpd %||% list()

  X <- object$data$X %||% object$X %||% NULL
  xnames <- if (!is.null(X) && !is.null(colnames(X))) colnames(X) else NULL

  .get_vector <- function(prefix) {
    cols <- grep(paste0("^", prefix, "\\[[0-9]+\\]$"), cn, value = TRUE)
    if (!length(cols)) return(NULL)
    idx <- as.integer(sub(paste0("^", prefix, "\\[([0-9]+)\\]$"), "\\1", cols))
    ord <- order(idx, na.last = NA)
    as.numeric(means[cols][ord])
  }

  .get_matrix <- function(prefix) {
    cols <- grep(paste0("^", prefix, "\\[[0-9]+,\\s*[0-9]+\\]$"), cn, value = TRUE)
    if (!length(cols)) return(NULL)
    idx1 <- as.integer(sub(paste0("^", prefix, "\\[([0-9]+),\\s*([0-9]+)\\]$"), "\\1", cols))
    idx2 <- as.integer(sub(paste0("^", prefix, "\\[([0-9]+),\\s*([0-9]+)\\]$"), "\\2", cols))
    n1 <- max(idx1)
    n2 <- max(idx2)
    out <- matrix(NA_real_, nrow = n1, ncol = n2)
    for (i in seq_along(cols)) {
      out[idx1[i], idx2[i]] <- means[cols[i]]
    }
    if (!is.null(xnames) && length(xnames) == n2) colnames(out) <- xnames
    rownames(out) <- paste0("comp", seq_len(n1))
    out
  }

  out <- list()

  if ("alpha" %in% cn) out$alpha <- unname(means["alpha"])

  w <- .get_vector("w")
  if (!is.null(w)) out$w <- w

  for (nm in names(bulk)) {
    ent <- bulk[[nm]] %||% list()
    mode <- ent$mode %||% NA_character_

    if (mode %in% c("fixed", "dist")) {
      vec <- .get_vector(nm)
      if (!is.null(vec)) out[[nm]] <- vec
    } else if (identical(mode, "link")) {
      beta <- .get_matrix(paste0("beta_", nm))
      if (is.null(beta)) {
        beta_vec <- .get_vector(paste0("beta_", nm))
        if (!is.null(beta_vec)) {
          beta <- matrix(beta_vec, ncol = 1)
          rownames(beta) <- paste0("comp", seq_len(nrow(beta)))
          if (!is.null(xnames) && length(xnames) == 1L) colnames(beta) <- xnames
        }
      }
      if (!is.null(beta)) out[[paste0("beta_", nm)]] <- beta
      beta_ps <- .get_vector(paste0("beta_ps_", nm))
      if (!is.null(beta_ps)) out[[paste0("beta_ps_", nm)]] <- beta_ps
    }
  }

  if (!is.null(gpd$threshold)) {
    thr_mode <- gpd$threshold$mode %||% NA_character_
    if (thr_mode %in% c("fixed", "dist")) {
      thr <- .get_vector("threshold")
      if (!is.null(thr)) out$threshold <- mean(thr, na.rm = TRUE)
    } else if (identical(thr_mode, "link")) {
      beta_thr <- .get_vector("beta_threshold")
      if (!is.null(beta_thr)) out$beta_threshold <- beta_thr
      if (!is.null(gpd$threshold$link_dist) &&
          identical(gpd$threshold$link_dist$dist, "lognormal") &&
          "sdlog_u" %in% cn) {
        out$sdlog_u <- unname(means["sdlog_u"])
      }
    }
  }

  if (!is.null(gpd$tail_scale)) {
    ts_mode <- gpd$tail_scale$mode %||% NA_character_
    if (identical(ts_mode, "link")) {
      beta_ts <- .get_vector("beta_tail_scale")
      if (!is.null(beta_ts)) out$beta_tail_scale <- beta_ts
    } else if (ts_mode %in% c("fixed", "dist")) {
      if ("tail_scale" %in% cn) out$tail_scale <- unname(means["tail_scale"])
    }
  }

  if (!is.null(gpd$tail_shape) && "tail_shape" %in% cn) {
    out$tail_shape <- unname(means["tail_shape"])
  }

  class(out) <- "mixgpd_params"
  out
}

#' @export
params.dpmixgpd_causal_fit <- function(object, ...) {
  stopifnot(inherits(object, "dpmixgpd_causal_fit"))
  out <- list(
    treated = params(object$outcome_fit$trt, ...),
    control = params(object$outcome_fit$con, ...)
  )
  class(out) <- "mixgpd_params_pair"
  out
}

#' @export
print.mixgpd_params <- function(x, digits = 4, ...) {
  cat("Posterior mean parameters\n")
  if (!length(x)) {
    cat("<empty>\n")
    return(invisible(x))
  }
  for (nm in names(x)) {
    cat("\n$", nm, "\n", sep = "")
    val <- x[[nm]]
    if (is.null(dim(val))) {
      if (is.numeric(val)) {
        print(signif(val, digits))
      } else {
        print(val)
      }
    } else if (is.matrix(val) || is.data.frame(val)) {
      print(round(val, digits))
    } else {
      print(val)
    }
  }
  invisible(x)
}

#' @export
print.mixgpd_params_pair <- function(x, digits = 4, ...) {
  cat("Posterior mean parameters (causal)\n")
  cat("\n[treated]\n")
  print(x$treated, digits = digits, ...)
  cat("\n[control]\n")
  print(x$control, digits = digits, ...)
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
#'   Use \code{"auto"} (or \code{"all"}) to include all plots supported for the
#'   available number of chains/parameters. Supported: \code{histogram, density,
#'   traceplot, running, compare_partial, autocorrelation, crosscorrelation, Rhat,
#'   grb, effective, geweke, caterpillar, pairs}.
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
                            family = "auto",
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
    thr_cols <- grep("^threshold\\[[0-9]+\\]$", cn, value = TRUE)
    thr_keep <- if ("threshold" %in% cn) {
      "threshold"
    } else if (length(thr_cols)) {
      thr_cols[order(as.integer(sub("^threshold\\[([0-9]+)\\]$", "\\1", thr_cols)))[1]]
    } else {
      character(0)
    }
    pref <- unique(c(
      grep("^alpha$", cn, value = TRUE),
      thr_keep,
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
  if ("Chain" %in% names(D)) D$Chain <- as.factor(D$Chain)
  if ("Parameter" %in% names(D)) D$Parameter <- as.factor(D$Parameter)
  n_chain <- if ("Chain" %in% names(D)) nlevels(D$Chain) else 1L
  n_param <- if ("Parameter" %in% names(D)) nlevels(D$Parameter) else 1L
  pal <- .plot_palette(max(2L, n_chain, n_param))

  # ---- normalize family input ----
  family <- unique(as.character(family))
  allowed <- c("histogram", "density", "traceplot", "running", "compare_partial",
               "autocorrelation", "crosscorrelation", "Rhat", "grb", "effective",
               "geweke", "caterpillar", "pairs")
  if (length(family) == 1L && family %in% c("auto", "all")) {
    family <- allowed
  }
  bad <- setdiff(family, allowed)
  if (length(bad) > 0) stop("Unknown plot family: ", paste(bad, collapse = ", "), call. = FALSE)

  nChains <- attr(D, "nChains") %||% length(smp)

  # Helper: only run chain-comparison diagnostics when possible
  .need_multi <- function(f) f %in% c("crosscorrelation", "Rhat", "grb", "effective")
  .need_params <- function(f) f %in% c("pairs", "crosscorrelation")
  if (nChains < 2) {
    family <- family[!vapply(family, .need_multi, logical(1))]
  }
  if (n_param < 2) {
    family <- family[!vapply(family, .need_params, logical(1))]
  }
  if (length(family) == 0) {
    stop("No applicable MCMC plot families for the available chains/parameters.", call. = FALSE)
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

    p <- p +
      .plot_theme() +
      ggplot2::scale_color_manual(values = pal) +
      ggplot2::scale_fill_manual(values = pal)
    plots[[f]] <- p
  }

  class(plots) <- c("mixgpd_fit_plots", "list")
  plots
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
#' @param ps Optional numeric vector of propensity scores for conditional prediction.
#'   Used when the model was fit with propensity score augmentation.
#' @param type Prediction type: \code{"density"}, \code{"survival"}, \code{"quantile"},
#'   \code{"sample"}, \code{"mean"}, \code{"median"}, \code{"location"}.
#' @param p Numeric vector of probabilities for quantiles (required for \code{type="quantile"}).
#' @param index Alias for \code{p}; numeric vector of quantile levels.
#' @param y Numeric vector of evaluation points (required for \code{type="density"} or \code{"survival"}).
#' @param cred.level Credible level for credible intervals (default 0.95 for 95\% credible intervals).
#' @param interval Character or NULL; type of credible interval: \code{NULL} for no interval,
#'   \code{"credible"} for equal-tailed quantile intervals (default), or \code{"hpd"} for
#'   highest posterior density intervals.
#' @param probs Quantiles for credible interval bands.
#' @param nsim Number of posterior predictive samples (for \code{type="sample"}).
#' @param store_draws Logical; whether to store all posterior draws (for \code{type="sample"}).
#' @param nsim_mean Number of posterior predictive samples to use for posterior mean estimation (for \code{type="mean"}).
#' @param ncores Number of CPU cores to use for parallel prediction (if supported).
#' @param ... Unused.
#' @return A list with elements:
#'   \itemize{
#'     \item \code{fit}: data frame with \code{estimate}/\code{lower}/\code{upper} columns (posterior means
#'       over draws) plus any index columns (e.g. \code{id}, \code{y}, \code{index}).
#'     \item \code{lower}, \code{upper}: reserved for backward compatibility (typically \code{NULL}).
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
#' # HPD intervals
#' pr_hpd <- predict(fit, type = "quantile", p = c(0.5, 0.9), interval = "hpd")
#' # No intervals
#' pr_none <- predict(fit, type = "quantile", p = c(0.5, 0.9), interval = NULL)
#' }
#' @export
predict.mixgpd_fit <- function(object,
                               x = NULL,
                               y = NULL,
                               ps = NULL,
                               newdata = NULL,
                               type = c("density", "survival",
                                        "quantile", "sample", "mean", "median", "location"),
                               p = NULL,
                               index = NULL,
                               nsim = NULL,
                               cred.level = 0.95,
                               interval = "credible",
                               probs = c(0.025, 0.5, 0.975),
                               store_draws = TRUE,
                               nsim_mean = 200L,
                               ncores = 1L,
                               ...) {
  .validate_fit(object)

  type <- match.arg(type)

  # Handle interval: NULL means no interval, otherwise match to credible/hpd
  if (!is.null(interval)) {
    interval <- match.arg(interval, choices = c("credible", "hpd"))
  }

  # Backwards-compat: allow newdata alias for x
  if (!is.null(newdata) && !is.null(x)) {
    stop("Provide only one of 'x' or 'newdata' (they are aliases).", call. = FALSE)
  }
  if (!is.null(newdata) && is.null(x)) x <- newdata

  # Alias p -> index for quantile, with conflict check
  if (type == "quantile") {
    if (!is.null(p) && is.null(index)) {
      index <- p
    } else if (!is.null(p) && !is.null(index)) {
      if (!isTRUE(all.equal(as.numeric(p), as.numeric(index)))) {
        stop("Provide only one of 'p' or 'index' for quantile predictions.", call. = FALSE)
      }
    }
    if (is.null(index)) index <- c(0.25, 0.5, 0.75)
  } else if (type == "median") {
    if (!is.null(p) && is.null(index)) index <- p
    if (!is.null(index) && !isTRUE(all.equal(as.numeric(index), 0.5))) {
      stop("Provide index = 0.5 for median predictions.", call. = FALSE)
    }
    index <- 0.5
  } else if (!is.null(p)) {
    warning("'p' is only used for type = 'quantile'; ignoring for other types.", call. = FALSE)
  }

  # Construct probs from cred.level for non-sample types
  if (type != "sample") {
    if (!is.numeric(cred.level) || length(cred.level) != 1 || cred.level <= 0 || cred.level >= 1) {
      stop("'cred.level' must be a numeric value between 0 and 1.", call. = FALSE)
    }
    probs <- c((1 - cred.level) / 2, 0.5, (1 + cred.level) / 2)
  }

  ncores <- as.integer(ncores)
  if (is.na(ncores) || ncores < 1L) stop("'ncores' must be an integer >= 1.", call. = FALSE)

  if (type == "location") {
    mean_pred <- .predict_mixgpd(object,
                                x = x,
                                y = y,
                                ps = ps,
                                type = "mean",
                                p = p,
                                index = index,
                                nsim = nsim,
                                cred.level = cred.level,
                                interval = interval,
                                probs = probs,
                                store_draws = store_draws,
                                nsim_mean = nsim_mean,
                                ncores = ncores)

    median_pred <- .predict_mixgpd(object,
                                  x = x,
                                  y = y,
                                  ps = ps,
                                  type = "median",
                                  p = p,
                                  index = index,
                                  nsim = nsim,
                                  cred.level = cred.level,
                                  interval = interval,
                                  probs = probs,
                                  store_draws = store_draws,
                                  nsim_mean = nsim_mean,
                                  ncores = ncores)

    fit_mean <- mean_pred$fit
    fit_median <- median_pred$fit
    extras <- setdiff(names(fit_mean), c("estimate", "lower", "upper"))
    if (length(extras)) {
      out_df <- fit_mean[, extras, drop = FALSE]
    } else {
      out_df <- data.frame(row.names = seq_len(nrow(fit_mean)))
    }
    out_df$mean <- fit_mean$estimate
    out_df$mean_lower <- fit_mean$lower
    out_df$mean_upper <- fit_mean$upper
    out_df$median <- fit_median$estimate
    out_df$median_lower <- fit_median$lower
    out_df$median_upper <- fit_median$upper

    out <- list(fit = out_df, lower = NULL, upper = NULL, type = "location", grid = mean_pred$grid)
    class(out) <- "mixgpd_predict"
    return(out)
  }

  .predict_mixgpd(object,
                  x = x,
                  y = y,
                  ps = ps,
                  type = type,
                  p = p,
                  index = index,
                  nsim = nsim,
                  cred.level = cred.level,
                  interval = interval,
                  probs = probs,
                  store_draws = store_draws,
                  nsim_mean = nsim_mean,
                  ncores = ncores)
}


#' Fitted values and residuals for a MixGPD fit
#'
#' Computes fitted values and residuals on the original training data.
#' Returns a data frame with point estimates, credible intervals, and residuals.
#' For unconditional models (no covariates), returns the population mean replicated
#' for all observations. For conditional models, returns individual predictions.
#'
#' @param object A fitted object of class \code{"mixgpd_fit"}.
#' @param type Which fitted location to return: mean, median, quantile, or both (\code{"location"}).
#' @param p Quantile level used when \code{type = "quantile"}.
#' @param level Credible level for confidence intervals (default 0.95 for 95\% credible intervals).
#' @param interval Character or NULL; type of credible interval: \code{NULL} for no interval,
#'   \code{"credible"} for equal-tailed quantile intervals (default), or \code{"hpd"} for
#'   highest posterior density intervals.
#' @param seed Random seed used for deterministic fitted values.
#' @param ... Unused.
#' @return A data frame with columns:
#'   \code{fit} (point estimates), \code{lower} (lower credible bound),
#'   \code{upper} (upper credible bound), and \code{residuals} (y - fit).
#' @examples
#' \dontrun{
#' y <- abs(stats::rnorm(50)) + 0.1
#' bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
#'                              GPD = TRUE, components = 6,
#'                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
#' fit <- run_mcmc_bundle_manual(bundle)
#' fitted(fit)
#' fitted(fit, level = 0.90)
#' fitted(fit, interval = "hpd")  # HPD intervals
#' fitted(fit, interval = NULL)   # No intervals
#' }
#' @export
fitted.mixgpd_fit <- function(object, type = c("location", "mean", "median", "quantile"),
                              p = 0.5, level = 0.95,
                              interval = "credible",
                              seed = 1, ...) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  type <- match.arg(type)
  # Handle interval: NULL means no interval, otherwise match to credible/hpd
  if (!is.null(interval)) {
    interval <- match.arg(interval, choices = c("credible", "hpd"))
  }
  y <- object$data$y %||% object$y
  X <- object$data$X %||% object$X %||% NULL

  if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) .Random.seed else NULL
    on.exit({
      if (!is.null(old_seed)) .Random.seed <<- old_seed
    }, add = TRUE)
    set.seed(as.integer(seed))
  }

  if (is.null(y)) stop("Could not extract y from fitted object.", call. = FALSE)

  if (type == "location") {
    pred_mean <- predict(object, x = X, type = "mean",
                        cred.level = level, interval = interval)
    pred_median <- predict(object, x = X, type = "median",
                          cred.level = level, interval = interval)
    fit_df <- pred_mean$fit
    if ("id" %in% names(fit_df)) fit_df <- fit_df[order(fit_df$id), , drop = FALSE]
    fit_vals <- fit_df$estimate
    lower_vals <- fit_df$lower
    upper_vals <- fit_df$upper

    med_df <- pred_median$fit
    if ("id" %in% names(med_df)) med_df <- med_df[order(med_df$id), , drop = FALSE]
    med_vals <- med_df$estimate
    med_lower <- med_df$lower
    med_upper <- med_df$upper
  } else if (type == "quantile") {
    pred <- predict(object, x = X, type = "quantile",
                    index = p, cred.level = level, interval = interval)
    fit_df <- pred$fit
    if ("id" %in% names(fit_df)) fit_df <- fit_df[order(fit_df$id), , drop = FALSE]
    fit_vals <- fit_df$estimate
    lower_vals <- fit_df$lower
    upper_vals <- fit_df$upper

    if (is.null(X)) {
      if (length(fit_vals) == 1L) {
        fit_vals <- rep(fit_vals, length(y))
        lower_vals <- rep(lower_vals, length(y))
        upper_vals <- rep(upper_vals, length(y))
      }
    }
  } else if (!is.null(X)) {
    pred <- predict(object, x = X, type = type,
                    cred.level = level, interval = interval)
    fit_df <- pred$fit
    if ("id" %in% names(fit_df)) fit_df <- fit_df[order(fit_df$id), , drop = FALSE]
    fit_vals <- fit_df$estimate
    lower_vals <- fit_df$lower
    upper_vals <- fit_df$upper
  } else {
    pred <- predict(object, type = type,
                    cred.level = level, interval = interval)
    fit_df <- pred$fit
    fit_vals <- rep(fit_df$estimate[1], length(y))
    lower_vals <- rep(fit_df$lower[1], length(y))
    upper_vals <- rep(fit_df$upper[1], length(y))
  }

  result <- data.frame(fit = fit_vals,
                       lower = lower_vals,
                       upper = upper_vals,
                       residuals = y - fit_vals)
  if (type == "location") {
    if (is.null(X)) {
      med_vals <- rep(med_vals[1], length(y))
      med_lower <- rep(med_lower[1], length(y))
      med_upper <- rep(med_upper[1], length(y))
    }
    result$mean <- fit_vals
    result$mean_lower <- lower_vals
    result$mean_upper <- upper_vals
    result$median <- med_vals
    result$median_lower <- med_lower
    result$median_upper <- med_upper
  }
  class(result) <- c("mixgpd_fitted", "data.frame")
  attr(result, "object") <- object
  attr(result, "level") <- level
  attr(result, "interval") <- interval
  return(result)
}

#' Residuals for a MixGPD fit
#'
#' Returns residuals aligned with the training data. For \code{type = "raw"},
#' this uses fitted means. For \code{type = "pit"}, this returns approximate PIT
#' values via the predictive survival function.
#'
#' @param object A fitted object of class \code{"mixgpd_fit"}.
#' @param type Residual type: \code{"raw"} or \code{"pit"}.
#' @param fitted_type For \code{type = "raw"}, use fitted means or medians.
#' @param ... Unused.
#' @return Numeric vector of residuals with length equal to the training sample size.
#' @export
residuals.mixgpd_fit <- function(object, type = c("raw", "pit"),
                                 fitted_type = c("mean", "median"), ...) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  type <- match.arg(type)
  y <- object$data$y %||% object$y
  if (is.null(y)) stop("Could not extract y from fitted object.", call. = FALSE)

  if (type == "raw") {
    fitted_type <- match.arg(fitted_type)
    fit_vals <- fitted(object, type = fitted_type)
    return(as.numeric(fit_vals$residuals))
  }

  X <- object$data$X %||% object$X %||% NULL
  pr_surv <- predict(object,
                     x = X,
                     y = y,
                     type = "survival",
                     interval = "none",
                     store_draws = FALSE,
                     ncores = 1L)
  fit_df <- pr_surv$fit
  surv_col <- if ("survival" %in% names(fit_df)) "survival" else "estimate"

  if (!("id" %in% names(fit_df))) {
    return(as.numeric(1 - fit_df[[surv_col]]))
  }

  pit <- numeric(length(y))
  for (i in seq_along(y)) {
    rows <- fit_df[fit_df$id == i, , drop = FALSE]
    if (!nrow(rows)) next
    idx <- which.min(abs(rows$y - y[i]))
    pit[i] <- 1 - rows[[surv_col]][idx]
  }
  pit
}


#' Plot prediction results
#'
#' Generates type-specific visualizations for prediction objects returned by
#' \code{predict.mixgpd_fit()}. Each prediction type produces a tailored plot:
#' \itemize{
#'   \item \code{quantile}: Quantile indices vs estimates with credible intervals
#'   \item \code{sample}: Histogram of samples with density overlay
#'   \item \code{mean}: Histogram density with posterior mean vertical line and CI bounds
#'   \item \code{density}: Density values vs evaluation points
#'   \item \code{survival}: Survival function (decreasing y values)
#' }
#'
#' @param x A prediction object returned by \code{predict.mixgpd_fit()}.
#' @param y Ignored; included for S3 compatibility.
#' @param ... Additional arguments passed to ggplot2 functions.
#' @return Invisibly returns the ggplot object.
#' @examples
#' \dontrun{
#' y <- abs(stats::rnorm(50)) + 0.1
#' bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
#'                              GPD = TRUE, components = 6,
#'                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
#' fit <- run_mcmc_bundle_manual(bundle)
#'
#' # Quantile prediction with plot
#' pred_q <- predict(fit, type = "quantile", index = c(0.25, 0.5, 0.75))
#' plot(pred_q)
#'
#' # Sample prediction with plot
#' pred_s <- predict(fit, type = "sample", nsim = 500)
#' plot(pred_s)
#'
#' # Mean prediction with plot
#' pred_m <- predict(fit, type = "mean", nsim_mean = 300)
#' plot(pred_m)
#' }
#' @export
plot.mixgpd_predict <- function(x, y = NULL, ...) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  if (!is.list(x)) {
    stop("x must be a prediction object from predict.mixgpd_fit().", call. = FALSE)
  }

  pred_type <- x$type %||% NA_character_

  if (is.na(pred_type)) {
    stop("Prediction object missing 'type' field.", call. = FALSE)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Install it first.", call. = FALSE)
  }

  result <- switch(pred_type,
         quantile = .plot_quantile_pred(x, ...),
         median = .plot_quantile_pred(x, ...),
         sample = .plot_sample_pred(x, ...),
         mean = .plot_mean_pred(x, ...),
         location = .plot_location_pred(x, ...),
         density = .plot_density_pred(x, ...),
         survival = .plot_survival_pred(x, ...),
         {warning("Unknown prediction type: ", pred_type); NULL})

  if (!is.null(result)) {
    class(result) <- c("mixgpd_predict_plots", class(result))
  }
  result
}

#' Plot causal prediction outputs
#'
#' S3 method for visualizing causal predictions from \code{predict.dpmixgpd_causal_fit()}.
#' For mean/quantile, plots treated/control and treatment effect versus PS (or index).
#' For density/prob, plots treated/control values versus y.
#'
#' @param x Object of class \code{dpmixgpd_causal_predict}.
#' @param y Ignored.
#' @param ... Additional arguments passed to ggplot2 functions.
#' @return A ggplot object or a list of ggplot objects.
#' @export
plot.dpmixgpd_causal_predict <- function(x, y = NULL, ...) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Install it first.", call. = FALSE)
  }

  pred_type <- attr(x, "type") %||% NA_character_
  if (is.na(pred_type)) stop("Causal prediction object missing 'type' attribute.", call. = FALSE)

  .extract_stats <- function(pr, n_pred) {
    fit <- pr$fit
    if (is.data.frame(fit)) {
      if ("id" %in% names(fit)) fit <- fit[order(fit$id), , drop = FALSE]
      est <- if ("estimate" %in% names(fit)) fit$estimate else as.numeric(fit[[1]])
      lower <- if ("lower" %in% names(fit)) fit$lower else rep(NA_real_, length(est))
      upper <- if ("upper" %in% names(fit)) fit$upper else rep(NA_real_, length(est))
    } else if (is.matrix(fit)) {
      est <- as.numeric(fit[, 1])
      lower <- rep(NA_real_, length(est))
      upper <- rep(NA_real_, length(est))
    } else {
      est <- as.numeric(fit)
      lower <- rep(NA_real_, length(est))
      upper <- rep(NA_real_, length(est))
    }
    if (length(est) == 1L && n_pred > 1L) {
      est <- rep(est, n_pred)
      lower <- rep(lower, n_pred)
      upper <- rep(upper, n_pred)
    }
    if (length(est) != n_pred) {
      stop("Unexpected prediction length in causal plot.", call. = FALSE)
    }
    list(estimate = est, lower = lower, upper = upper)
  }

  .x_axis <- function(ps_vec) {
    if (is.null(ps_vec) || !any(is.finite(ps_vec))) {
      list(x = seq_along(ps_vec), label = "Index")
    } else {
      list(x = ps_vec, label = "Estimated PS")
    }
  }

  if (pred_type %in% c("mean", "quantile")) {
    trt <- attr(x, "trt")
    con <- attr(x, "con")
    if (is.null(trt) || is.null(con)) {
      stop("Causal prediction missing treated/control objects for plotting.", call. = FALSE)
    }

    n_pred <- nrow(x)
    ps_vec <- as.numeric(x[, "ps"])
    ax <- .x_axis(ps_vec)

    trt_stats <- .extract_stats(trt, n_pred)
    con_stats <- .extract_stats(con, n_pred)

    df_tc <- rbind(
      data.frame(x = ax$x, group = "Treated", estimate = trt_stats$estimate,
                 lower = trt_stats$lower, upper = trt_stats$upper),
      data.frame(x = ax$x, group = "Control", estimate = con_stats$estimate,
                 lower = con_stats$lower, upper = con_stats$upper)
    )

    pal <- .plot_palette(8L)
    p_tc <- ggplot2::ggplot(df_tc, ggplot2::aes(x = x, y = estimate, color = group, fill = group)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::scale_color_manual(values = pal[1:2]) +
      ggplot2::scale_fill_manual(values = pal[1:2]) +
      .plot_theme() +
      ggplot2::labs(x = ax$label, y = paste0("Outcome ", pred_type), title = "Treated vs Control")

    if (any(is.finite(df_tc$lower)) && any(is.finite(df_tc$upper))) {
      p_tc <- p_tc + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                                          alpha = 0.2, color = NA)
    }

    df_te <- data.frame(
      x = ax$x,
      estimate = as.numeric(x[, "estimate"]),
      lower = as.numeric(x[, "lower"]),
      upper = as.numeric(x[, "upper"])
    )

    p_te <- ggplot2::ggplot(df_te, ggplot2::aes(x = x, y = estimate)) +
      ggplot2::geom_line(color = pal[7], linewidth = 0.8) +
      .plot_theme() +
      ggplot2::labs(x = ax$label, y = "Treatment effect", title = "Treated - Control")

    if (any(is.finite(df_te$lower)) && any(is.finite(df_te$upper))) {
      p_te <- p_te + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                                          alpha = 0.2, fill = pal[5], color = NA)
    }

    result <- list(trt_control = p_tc, treatment_effect = p_te)
    class(result) <- c("dpmixgpd_causal_predict_plots", "list")
    return(result)
  }

  if (pred_type %in% c("density", "survival", "prob")) {
    df <- as.data.frame(x)
    df_long <- rbind(
      data.frame(y = df$y, group = "Treated", estimate = df$trt_estimate,
                 lower = df$trt_lower, upper = df$trt_upper),
      data.frame(y = df$y, group = "Control", estimate = df$con_estimate,
                 lower = df$con_lower, upper = df$con_upper)
    )

    pal <- .plot_palette(2L)
    p <- ggplot2::ggplot(df_long, ggplot2::aes(x = y, y = estimate, color = group, fill = group)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::scale_color_manual(values = pal) +
      ggplot2::scale_fill_manual(values = pal) +
      .plot_theme() +
      ggplot2::labs(x = "y", y = pred_type, title = "Treated vs Control")

    if (any(is.finite(df_long$lower)) && any(is.finite(df_long$upper))) {
      p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                                    alpha = 0.2, color = NA)
    }

    return(p)
  }

  stop("Unsupported causal prediction type for plotting.", call. = FALSE)
}


# S3 methods for QTE/ATE objects -------------------------------------------

#' Print a QTE object
#'
#' User-facing print method for \code{"dpmixgpd_qte"} objects produced by \code{qte()}.
#' Displays a compact summary: prediction points, quantile grid, credible level,
#' and the first few rows of QTE estimates.
#'
#' @param x A \code{"dpmixgpd_qte"} object from \code{qte()}.
#' @param digits Number of digits to display.
#' @param max_rows Maximum number of estimate rows to display.
#' @param ... Unused.
#' @return The object \code{x}, invisibly.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", J = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' q <- qte(fit, probs = c(0.25, 0.5, 0.75), interval = "credible")
#' print(q)
#' }
#' @export
#' @method print dpmixgpd_qte
print.dpmixgpd_qte <- function(x, digits = 3, max_rows = 6, ...) {
  stopifnot(inherits(x, "dpmixgpd_qte"))
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  probs <- x$probs %||% x$grid %||% numeric(0)
  n_pred <- x$n_pred %||% 1L
  level <- x$level %||% 0.95
  interval <- x$interval %||% "none"
  meta <- x$meta %||% list()

  cat("QTE (Quantile Treatment Effect)\n")
  cat(sprintf("  Prediction points: %d\n", n_pred))
  cat(sprintf("  Quantile grid: %s\n", paste(round(probs, digits), collapse = ", ")))

  has_x <- !is.null(x$x)
  ps_used <- !is.null(x$ps) && any(is.finite(x$ps))
  cat(sprintf("  Conditional (covariates): %s\n", if (has_x) "YES" else "NO"))
  cat(sprintf("  Propensity score used: %s\n", if (ps_used) "YES" else "NO"))
  if (ps_used && !is.null(meta$ps_scale)) {
    cat(sprintf("  PS scale: %s\n", meta$ps_scale))
  }
  cat(sprintf("  Credible interval: %s", interval))
  if (interval == "credible") {
    cat(sprintf(" (%.0f%%)\n", level * 100))
  } else {
    cat("\n")
  }

  cat("\nQTE estimates (treated - control):\n")
  qte_fit <- x$qte$fit %||% NULL
  if (!is.null(qte_fit) && is.data.frame(qte_fit)) {
    show_df <- qte_fit
    show_df$estimate <- round(show_df$estimate, digits)
    if ("lower" %in% names(show_df)) show_df$lower <- round(show_df$lower, digits)
    if ("upper" %in% names(show_df)) show_df$upper <- round(show_df$upper, digits)

    if (nrow(show_df) > max_rows) {
      print(utils::head(show_df, max_rows), row.names = FALSE)
      cat(sprintf("... (%d more rows)\n", nrow(show_df) - max_rows))
    } else {
      print(show_df, row.names = FALSE)
    }
  } else if (!is.null(x$fit)) {
    # Fallback to raw matrix
    fit_mat <- x$fit
    show_n <- min(nrow(fit_mat), max_rows)
    cat(sprintf("  (matrix: %d x %d)\n", nrow(fit_mat), ncol(fit_mat)))
    print(round(fit_mat[seq_len(show_n), , drop = FALSE], digits))
    if (nrow(fit_mat) > show_n) {
      cat(sprintf("... (%d more rows)\n", nrow(fit_mat) - show_n))
    }
  }

  invisible(x)
}

#' Print an ATE object
#'
#' User-facing print method for \code{"dpmixgpd_ate"} objects produced by \code{ate()}.
#' Displays a compact summary: prediction points, credible level, and the first
#' few ATE estimates.
#'
#' @param x A \code{"dpmixgpd_ate"} object from \code{ate()}.
#' @param digits Number of digits to display.
#' @param max_rows Maximum number of estimate rows to display.
#' @param ... Unused.
#' @return The object \code{x}, invisibly.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", J = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' a <- ate(fit, interval = "credible")
#' print(a)
#' }
#' @export
#' @method print dpmixgpd_ate
print.dpmixgpd_ate <- function(x, digits = 3, max_rows = 6, ...) {
  stopifnot(inherits(x, "dpmixgpd_ate"))
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  n_pred <- x$n_pred %||% length(x$fit)
  level <- x$level %||% 0.95
  interval <- x$interval %||% "none"
  nsim_mean <- x$nsim_mean %||% NA
  meta <- x$meta %||% list()

  cat("ATE (Average Treatment Effect)\n")
  cat(sprintf("  Prediction points: %d\n", n_pred))

  has_x <- !is.null(x$x)
  ps_used <- !is.null(x$ps) && any(is.finite(x$ps))
  cat(sprintf("  Conditional (covariates): %s\n", if (has_x) "YES" else "NO"))
  cat(sprintf("  Propensity score used: %s\n", if (ps_used) "YES" else "NO"))
  if (ps_used && !is.null(meta$ps_scale)) {
    cat(sprintf("  PS scale: %s\n", meta$ps_scale))
  }
  if (!is.na(nsim_mean)) {
    cat(sprintf("  Posterior mean draws: %d\n", nsim_mean))
  }
  cat(sprintf("  Credible interval: %s", interval))
  if (interval == "credible") {
    cat(sprintf(" (%.0f%%)\n", level * 100))
  } else {
    cat("\n")
  }

  cat("\nATE estimates (treated - control):\n")
  ate_fit <- x$ate$fit %||% NULL
  if (!is.null(ate_fit) && is.data.frame(ate_fit)) {
    show_df <- ate_fit
    show_df$estimate <- round(show_df$estimate, digits)
    if ("lower" %in% names(show_df)) show_df$lower <- round(show_df$lower, digits)
    if ("upper" %in% names(show_df)) show_df$upper <- round(show_df$upper, digits)

    if (nrow(show_df) > max_rows) {
      print(utils::head(show_df, max_rows), row.names = FALSE)
      cat(sprintf("... (%d more rows)\n", nrow(show_df) - max_rows))
    } else {
      print(show_df, row.names = FALSE)
    }
  } else if (!is.null(x$fit)) {
    # Fallback to raw vector
    fit_vec <- x$fit
    show_n <- min(length(fit_vec), max_rows)
    cat(sprintf("  (vector: %d)\n", length(fit_vec)))
    print(round(fit_vec[seq_len(show_n)], digits))
    if (length(fit_vec) > show_n) {
      cat(sprintf("... (%d more)\n", length(fit_vec) - show_n))
    }
  }

  invisible(x)
}

#' Summarize a QTE object
#'
#' Returns a structured summary of QTE results for further analysis or display.
#' Includes overall statistics, per-quantile summaries, and metadata.
#'
#' @param object A \code{"dpmixgpd_qte"} object from \code{qte()}.
#' @param ... Unused.
#' @return An object of class \code{"summary.dpmixgpd_qte"} containing summary statistics.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", J = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' q <- qte(fit, probs = c(0.25, 0.5, 0.75), interval = "credible")
#' summary(q)
#' }
#' @export
#' @method summary dpmixgpd_qte
summary.dpmixgpd_qte <- function(object, ...) {
  stopifnot(inherits(object, "dpmixgpd_qte"))
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  probs <- object$probs %||% object$grid %||% numeric(0)
  n_pred <- object$n_pred %||% 1L
  level <- object$level %||% 0.95
  interval <- object$interval %||% "none"
  meta <- object$meta %||% list()

  # Overall summary
  overall <- list(
    n_pred = n_pred,
    n_quantiles = length(probs),
    quantiles = probs,
    level = level,
    interval = interval,
    has_covariates = !is.null(object$x),
    ps_used = !is.null(object$ps) && any(is.finite(object$ps))
  )

  # Per-quantile summary statistics
  qte_fit <- object$qte$fit %||% NULL
  quantile_summary <- NULL
  if (!is.null(qte_fit) && is.data.frame(qte_fit)) {
    quantile_summary <- do.call(rbind, lapply(probs, function(tau) {
      rows <- qte_fit[qte_fit$index == tau, , drop = FALSE]
      if (nrow(rows) == 0L) return(NULL)
      est <- rows$estimate
      data.frame(
        quantile = tau,
        mean_qte = mean(est, na.rm = TRUE),
        median_qte = stats::median(est, na.rm = TRUE),
        min_qte = min(est, na.rm = TRUE),
        max_qte = max(est, na.rm = TRUE),
        sd_qte = if (length(est) > 1) stats::sd(est, na.rm = TRUE) else NA_real_
      )
    }))
  } else if (!is.null(object$fit) && is.matrix(object$fit)) {
    fit_mat <- object$fit
    quantile_summary <- do.call(rbind, lapply(seq_along(probs), function(j) {
      est <- fit_mat[, j]
      data.frame(
        quantile = probs[j],
        mean_qte = mean(est, na.rm = TRUE),
        median_qte = stats::median(est, na.rm = TRUE),
        min_qte = min(est, na.rm = TRUE),
        max_qte = max(est, na.rm = TRUE),
        sd_qte = if (length(est) > 1) stats::sd(est, na.rm = TRUE) else NA_real_
      )
    }))
  }

  # CI width summary (for both credible and HPD intervals)
  ci_summary <- NULL
  if (interval %in% c("credible", "hpd") && !is.null(object$lower) && !is.null(object$upper)) {
    widths <- as.vector(object$upper - object$lower)
    ci_summary <- list(
      mean_width = mean(widths, na.rm = TRUE),
      median_width = stats::median(widths, na.rm = TRUE),
      min_width = min(widths, na.rm = TRUE),
      max_width = max(widths, na.rm = TRUE)
    )
  }

  out <- list(
    overall = overall,
    quantile_summary = quantile_summary,
    ci_summary = ci_summary,
    meta = meta,
    object = object
  )
  class(out) <- "summary.dpmixgpd_qte"
  out
}

#' Print a QTE summary
#'
#' @param x A \code{"summary.dpmixgpd_qte"} object.
#' @param digits Number of digits to display.
#' @param ... Unused.
#' @return The object \code{x}, invisibly.
#' @export
#' @method print summary.dpmixgpd_qte
print.summary.dpmixgpd_qte <- function(x, digits = 3, ...) {
  stopifnot(inherits(x, "summary.dpmixgpd_qte"))

  ov <- x$overall
  meta <- x$meta %||% list()

  cat("QTE Summary\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat(sprintf("Prediction points: %d | Quantiles: %d\n", ov$n_pred, ov$n_quantiles))
  cat(sprintf("Quantile grid: %s\n", paste(round(ov$quantiles, digits), collapse = ", ")))
  cat(sprintf("Conditional: %s | PS used: %s\n",
              if (ov$has_covariates) "YES" else "NO",
              if (ov$ps_used) "YES" else "NO"))
  cat(sprintf("Interval: %s", ov$interval))
  if (ov$interval == "credible") {
    cat(sprintf(" (%.0f%%)", ov$level * 100))
  }
  cat("\n\n")

  # Model info
  if (!is.null(meta$backend) || !is.null(meta$kernel)) {
    cat("Model specification:\n")
    if (!is.null(meta$backend)) {
      cat(sprintf("  Backend (trt/con): %s / %s\n",
                  meta$backend$trt %||% "?", meta$backend$con %||% "?"))
    }
    if (!is.null(meta$kernel)) {
      cat(sprintf("  Kernel (trt/con): %s / %s\n",
                  meta$kernel$trt %||% "?", meta$kernel$con %||% "?"))
    }
    if (!is.null(meta$GPD)) {
      cat(sprintf("  GPD tail (trt/con): %s / %s\n",
                  if (isTRUE(meta$GPD$trt)) "YES" else "NO",
                  if (isTRUE(meta$GPD$con)) "YES" else "NO"))
    }
    cat("\n")
  }

  # Quantile summary table
  qs <- x$quantile_summary
  if (!is.null(qs) && nrow(qs) > 0) {
    cat("QTE by quantile:\n")
    qs_print <- qs
    for (col in names(qs_print)) {
      if (is.numeric(qs_print[[col]])) {
        qs_print[[col]] <- round(qs_print[[col]], digits)
      }
    }
    print(qs_print, row.names = FALSE)
    cat("\n")
  }

  # CI summary
  ci <- x$ci_summary
  if (!is.null(ci)) {
    cat("Credible interval width:\n")
    cat(sprintf("  Mean: %s | Median: %s\n",
                round(ci$mean_width, digits), round(ci$median_width, digits)))
    cat(sprintf("  Range: [%s, %s]\n",
                round(ci$min_width, digits), round(ci$max_width, digits)))
  }

  invisible(x)
}

#' Summarize an ATE object
#'
#' Returns a structured summary of ATE results for further analysis or display.
#' Includes overall statistics and metadata.
#'
#' @param object A \code{"dpmixgpd_ate"} object from \code{ate()}.
#' @param ... Unused.
#' @return An object of class \code{"summary.dpmixgpd_ate"} containing summary statistics.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", J = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' a <- ate(fit, interval = "credible")
#' summary(a)
#' }
#' @export
#' @method summary dpmixgpd_ate
summary.dpmixgpd_ate <- function(object, ...) {
  stopifnot(inherits(object, "dpmixgpd_ate"))
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  n_pred <- object$n_pred %||% length(object$fit)
  level <- object$level %||% 0.95
  interval <- object$interval %||% "none"
  nsim_mean <- object$nsim_mean %||% NA
  meta <- object$meta %||% list()

  # Overall summary
  overall <- list(
    n_pred = n_pred,
    level = level,
    interval = interval,
    nsim_mean = nsim_mean,
    has_covariates = !is.null(object$x),
    ps_used = !is.null(object$ps) && any(is.finite(object$ps))
  )

  # ATE statistics
  ate_fit <- object$ate$fit %||% NULL
  ate_stats <- NULL
  if (!is.null(ate_fit) && is.data.frame(ate_fit)) {
    est <- ate_fit$estimate
    ate_stats <- list(
      mean_ate = mean(est, na.rm = TRUE),
      median_ate = stats::median(est, na.rm = TRUE),
      min_ate = min(est, na.rm = TRUE),
      max_ate = max(est, na.rm = TRUE),
      sd_ate = if (length(est) > 1) stats::sd(est, na.rm = TRUE) else NA_real_
    )
  } else if (!is.null(object$fit)) {
    est <- object$fit
    ate_stats <- list(
      mean_ate = mean(est, na.rm = TRUE),
      median_ate = stats::median(est, na.rm = TRUE),
      min_ate = min(est, na.rm = TRUE),
      max_ate = max(est, na.rm = TRUE),
      sd_ate = if (length(est) > 1) stats::sd(est, na.rm = TRUE) else NA_real_
    )
  }

  # CI width summary (for both credible and HPD intervals)
  ci_summary <- NULL
  if (interval %in% c("credible", "hpd") && !is.null(object$lower) && !is.null(object$upper)) {
    widths <- object$upper - object$lower
    ci_summary <- list(
      mean_width = mean(widths, na.rm = TRUE),
      median_width = stats::median(widths, na.rm = TRUE),
      min_width = min(widths, na.rm = TRUE),
      max_width = max(widths, na.rm = TRUE)
    )
  }

  out <- list(
    overall = overall,
    ate_stats = ate_stats,
    ci_summary = ci_summary,
    meta = meta,
    object = object
  )
  class(out) <- "summary.dpmixgpd_ate"
  out
}

#' Print an ATE summary
#'
#' @param x A \code{"summary.dpmixgpd_ate"} object.
#' @param digits Number of digits to display.
#' @param ... Unused.
#' @return The object \code{x}, invisibly.
#' @export
#' @method print summary.dpmixgpd_ate
print.summary.dpmixgpd_ate <- function(x, digits = 3, ...) {
  stopifnot(inherits(x, "summary.dpmixgpd_ate"))

  ov <- x$overall
  meta <- x$meta %||% list()

  cat("ATE Summary\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat(sprintf("Prediction points: %d\n", ov$n_pred))
  cat(sprintf("Conditional: %s | PS used: %s\n",
              if (ov$has_covariates) "YES" else "NO",
              if (ov$ps_used) "YES" else "NO"))
  if (!is.na(ov$nsim_mean)) {
    cat(sprintf("Posterior mean draws: %d\n", ov$nsim_mean))
  }
  cat(sprintf("Interval: %s", ov$interval))
  if (ov$interval == "credible") {
    cat(sprintf(" (%.0f%%)", ov$level * 100))
  }
  cat("\n\n")

  # Model info
  if (!is.null(meta$backend) || !is.null(meta$kernel)) {
    cat("Model specification:\n")
    if (!is.null(meta$backend)) {
      cat(sprintf("  Backend (trt/con): %s / %s\n",
                  meta$backend$trt %||% "?", meta$backend$con %||% "?"))
    }
    if (!is.null(meta$kernel)) {
      cat(sprintf("  Kernel (trt/con): %s / %s\n",
                  meta$kernel$trt %||% "?", meta$kernel$con %||% "?"))
    }
    if (!is.null(meta$GPD)) {
      cat(sprintf("  GPD tail (trt/con): %s / %s\n",
                  if (isTRUE(meta$GPD$trt)) "YES" else "NO",
                  if (isTRUE(meta$GPD$con)) "YES" else "NO"))
    }
    cat("\n")
  }

  # ATE statistics
  as <- x$ate_stats
  if (!is.null(as)) {
    cat("ATE statistics:\n")
    cat(sprintf("  Mean: %s | Median: %s\n",
                round(as$mean_ate, digits), round(as$median_ate, digits)))
    cat(sprintf("  Range: [%s, %s]\n",
                round(as$min_ate, digits), round(as$max_ate, digits)))
    if (!is.na(as$sd_ate)) {
      cat(sprintf("  SD: %s\n", round(as$sd_ate, digits)))
    }
    cat("\n")
  }

  # CI summary
  ci <- x$ci_summary
  if (!is.null(ci)) {
    cat("Credible interval width:\n")
    cat(sprintf("  Mean: %s | Median: %s\n",
                round(ci$mean_width, digits), round(ci$median_width, digits)))
    cat(sprintf("  Range: [%s, %s]\n",
                round(ci$min_width, digits), round(ci$max_width, digits)))
  }

  invisible(x)
}

#' Plot QTE results
#'
#' Generates visualizations for quantile treatment effects. The \code{type} parameter
#' controls the plot style:
#' \itemize{
#'   \item \code{"both"} (default): Returns a list with both \code{trt_control} (treated vs
#'     control quantile curves) and \code{treatment_effect} (QTE curve) plots
#'   \item \code{"effect"}: QTE curve vs quantile levels (\code{probs}) with CI ribbon
#'   \item \code{"arms"}: Treated and control quantile curves vs \code{probs}, with CI ribbons
#' }
#'
#' @param x Object of class \code{dpmixgpd_qte}.
#' @param y Ignored.
#' @param type Character; plot type: \code{"both"} (default), \code{"effect"}, or \code{"arms"}.
#' @param facet_by Character; faceting strategy when multiple prediction points exist.
#'   \code{"tau"} (default) facets by quantile level, \code{"id"} facets by prediction point.
#' @param ... Additional arguments passed to ggplot2 functions.
#' @return A list of ggplot objects with elements \code{trt_control} and \code{treatment_effect}
#'   (if \code{type="both"}), or a single ggplot object (if \code{type} is \code{"effect"} or
#'   \code{"arms"}).
#' @examples
#' \dontrun{
#' qte_result <- qte(fit, probs = c(0.1, 0.5, 0.9), newdata = X_new)
#' plot(qte_result)  # default: returns list with both plots
#' plot(qte_result, type = "effect")  # single QTE plot
#' plot(qte_result, type = "arms")    # single arms plot
#' }
#' @export
plot.dpmixgpd_qte <- function(x, y = NULL, type = c("both", "effect", "arms"),
                              facet_by = c("tau", "id"), ...) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Install it first.", call. = FALSE)
  }

  type <- match.arg(type)
  facet_by <- match.arg(facet_by)

  if (!is.list(x) || is.null(x$trt) || is.null(x$con)) {
    stop("Invalid QTE object for plotting.", call. = FALSE)
  }

  pr_trt <- x$trt
  pr_con <- x$con
  probs <- x$grid %||% x$probs %||% numeric()
  fit_trt <- pr_trt$fit

  # Determine n_pred (number of prediction points)
  if (is.data.frame(fit_trt)) {
    if ("id" %in% names(fit_trt)) {
      n_pred <- max(fit_trt$id, na.rm = TRUE)
    } else if ("index" %in% names(fit_trt)) {
      n_pred <- 1L
    } else {
      n_pred <- nrow(fit_trt)
    }
  } else if (is.matrix(fit_trt)) {
    n_pred <- nrow(fit_trt)
  } else {
    n_pred <- length(fit_trt)
  }

  ps_vec <- x$ps %||% rep(NA_real_, n_pred)
  if (!length(ps_vec)) ps_vec <- rep(NA_real_, n_pred)
  ax <- if (any(is.finite(ps_vec))) list(x = ps_vec, label = "Estimated PS") else
    list(x = seq_len(n_pred), label = "Index")

  # Helper to coerce prediction fit to data.frame
  .as_df <- function(pr, n_pred, probs) {
    fit <- pr$fit
    if (!is.data.frame(fit)) {
      # Try to coerce using helper
      fit <- .coerce_fit_df(fit, n_pred = n_pred, probs = probs)
    }
    df <- fit
    if (!("id" %in% names(df))) df$id <- rep(seq_len(n_pred), times = length(probs))
    if (!("index" %in% names(df))) df$index <- rep(probs, each = n_pred)
    if (!("estimate" %in% names(df))) df$estimate <- NA_real_
    if (!("lower" %in% names(df))) df$lower <- NA_real_
    if (!("upper" %in% names(df))) df$upper <- NA_real_
    df <- df[, c("id", "index", "estimate", "lower", "upper"), drop = FALSE]
    df
  }

  pal <- .plot_palette(8L)

  # Build arms plot (treated vs control)
  .build_arms_plot <- function() {
    df_trt <- .as_df(pr_trt, n_pred, probs)
    df_con <- .as_df(pr_con, n_pred, probs)
    df_trt$group <- "Treated"
    df_con$group <- "Control"

    df_tc <- rbind(df_trt, df_con)
    df_tc$ps <- ax$x[df_tc$id]
    df_tc$tau <- factor(paste0("\u03C4 = ", df_tc$index), levels = paste0("\u03C4 = ", probs))

    # Choose faceting
    facet_formula <- if (facet_by == "tau" && length(probs) > 1) {
      ~ tau
    } else if (facet_by == "id" && n_pred > 1) {
      ~ id
    } else if (length(probs) > 1) {
      ~ tau
    } else {
      NULL
    }

    p <- ggplot2::ggplot(df_tc, ggplot2::aes(x = ps, y = estimate, color = group, fill = group)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::scale_color_manual(values = pal[1:2]) +
      ggplot2::scale_fill_manual(values = pal[1:2]) +
      .plot_theme() +
      ggplot2::labs(x = ax$label, y = "Quantile", title = "Treated vs Control Quantiles",
                    color = "Arm", fill = "Arm")

    if (!is.null(facet_formula)) {
      p <- p + ggplot2::facet_wrap(facet_formula, scales = "free_y")
    }

    if (any(is.finite(df_tc$lower)) && any(is.finite(df_tc$upper))) {
      p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                                    alpha = 0.2, color = NA)
    }
    p
  }

  # Build effect plot (QTE = treated - control)
  .build_effect_plot <- function() {
    te_mat <- x$fit
    te_lower <- x$lower
    te_upper <- x$upper

    df_te <- data.frame(
      id = rep(seq_len(n_pred), times = length(probs)),
      index = rep(probs, each = n_pred),
      estimate = as.vector(te_mat),
      lower = if (!is.null(te_lower)) as.vector(te_lower) else NA_real_,
      upper = if (!is.null(te_upper)) as.vector(te_upper) else NA_real_,
      ps = rep(ax$x, times = length(probs))
    )
    df_te$tau <- factor(paste0("\u03C4 = ", df_te$index), levels = paste0("\u03C4 = ", probs))

    # Choose faceting
    facet_formula <- if (facet_by == "tau" && length(probs) > 1) {
      ~ tau
    } else if (facet_by == "id" && n_pred > 1) {
      ~ id
    } else if (length(probs) > 1) {
      ~ tau
    } else {
      NULL
    }

    p <- ggplot2::ggplot(df_te, ggplot2::aes(x = ps, y = estimate)) +
      ggplot2::geom_line(color = pal[7], linewidth = 0.8) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
      .plot_theme() +
      ggplot2::labs(x = ax$label, y = "Quantile Treatment Effect", title = "QTE")

    if (!is.null(facet_formula)) {
      p <- p + ggplot2::facet_wrap(facet_formula, scales = "free_y")
    }

    if (any(is.finite(df_te$lower)) && any(is.finite(df_te$upper))) {
      p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                                    alpha = 0.2, fill = pal[5], color = NA)
    }
    p
  }

  # Return based on type
  if (type == "effect") {
    result <- .build_effect_plot()
    class(result) <- c("dpmixgpd_qte_plot", class(result))
    return(result)
  }

  if (type == "arms") {
    result <- .build_arms_plot()
    class(result) <- c("dpmixgpd_qte_plot", class(result))
    return(result)
  }

  # type == "both" (default) - maintain backward compatible naming
  result <- list(
    trt_control = .build_arms_plot(),
    treatment_effect = .build_effect_plot()
  )
  class(result) <- c("dpmixgpd_causal_predict_plots", "list")
  result
}

#' Plot ATE results
#'
#' Generates visualizations for average treatment effects. The \code{type} parameter
#' controls the plot style:
#' \itemize{
#'   \item \code{"both"} (default): Returns a list with both \code{trt_control} (treated vs
#'     control means) and \code{treatment_effect} (ATE curve) plots
#'   \item \code{"effect"}: ATE curve/points vs index/PS with CI ribbon/bars
#'   \item \code{"arms"}: Treated mean vs control mean, with CI ribbons
#' }
#'
#' @param x Object of class \code{dpmixgpd_ate}.
#' @param y Ignored.
#' @param type Character; plot type: \code{"both"} (default), \code{"effect"}, or \code{"arms"}.
#' @param ... Additional arguments passed to ggplot2 functions.
#' @return A list of ggplot objects with elements \code{trt_control} and \code{treatment_effect}
#'   (if \code{type="both"}), or a single ggplot object (if \code{type} is \code{"effect"} or
#'   \code{"arms"}).
#' @examples
#' \dontrun{
#' ate_result <- ate(fit, newdata = X_new, interval = "credible")
#' plot(ate_result)  # default: returns list with both plots
#' plot(ate_result, type = "effect")  # single ATE plot
#' plot(ate_result, type = "arms")    # single arms plot
#' }
#' @export
plot.dpmixgpd_ate <- function(x, y = NULL, type = c("both", "effect", "arms"), ...) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Install it first.", call. = FALSE)
  }

  type <- match.arg(type)

  if (!is.list(x) || is.null(x$trt) || is.null(x$con)) {
    stop("Invalid ATE object for plotting.", call. = FALSE)
  }

  pr_trt <- x$trt
  pr_con <- x$con
  n_pred <- length(x$fit)
  ps_vec <- x$ps %||% rep(NA_real_, n_pred)
  if (!length(ps_vec)) ps_vec <- rep(NA_real_, n_pred)
  ax <- if (any(is.finite(ps_vec))) list(x = ps_vec, label = "Estimated PS") else
    list(x = seq_len(n_pred), label = "Index")

  # Helper to extract statistics from prediction objects
  .extract_stats <- function(pr, n_pred) {
    fit <- pr$fit
    if (is.data.frame(fit)) {
      if ("id" %in% names(fit)) fit <- fit[order(fit$id), , drop = FALSE]
      est <- if ("estimate" %in% names(fit)) fit$estimate else as.numeric(fit[[1]])
      lower <- if ("lower" %in% names(fit)) fit$lower else rep(NA_real_, length(est))
      upper <- if ("upper" %in% names(fit)) fit$upper else rep(NA_real_, length(est))
    } else if (is.matrix(fit)) {
      # Try to coerce using helper
      fit_df <- .coerce_fit_df(fit, n_pred = n_pred)
      est <- fit_df$estimate
      lower <- fit_df$lower
      upper <- fit_df$upper
    } else {
      est <- as.numeric(fit)
      lower <- rep(NA_real_, length(est))
      upper <- rep(NA_real_, length(est))
    }
    if (length(est) == 1L && n_pred > 1L) {
      est <- rep(est, n_pred)
      lower <- rep(lower, n_pred)
      upper <- rep(upper, n_pred)
    }
    if (length(est) != n_pred) {
      stop("Unexpected prediction length in ATE plot.", call. = FALSE)
    }
    list(estimate = est, lower = lower, upper = upper)
  }

  pal <- .plot_palette(8L)

  # Build arms plot (treated vs control means)
  .build_arms_plot <- function() {
    trt_stats <- .extract_stats(pr_trt, n_pred)
    con_stats <- .extract_stats(pr_con, n_pred)

    df_tc <- rbind(
      data.frame(x = ax$x, group = "Treated", estimate = trt_stats$estimate,
                 lower = trt_stats$lower, upper = trt_stats$upper),
      data.frame(x = ax$x, group = "Control", estimate = con_stats$estimate,
                 lower = con_stats$lower, upper = con_stats$upper)
    )

    p <- ggplot2::ggplot(df_tc, ggplot2::aes(x = x, y = estimate, color = group, fill = group)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_color_manual(values = pal[1:2]) +
      ggplot2::scale_fill_manual(values = pal[1:2]) +
      .plot_theme() +
      ggplot2::labs(x = ax$label, y = "Mean Outcome", title = "Treated vs Control Means",
                    color = "Arm", fill = "Arm")

    if (any(is.finite(df_tc$lower)) && any(is.finite(df_tc$upper))) {
      p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                                    alpha = 0.2, color = NA)
    }
    p
  }

  # Build effect plot (ATE = treated - control)
  .build_effect_plot <- function() {
    df_te <- data.frame(
      x = ax$x,
      estimate = as.numeric(x$fit),
      lower = if (!is.null(x$lower)) as.numeric(x$lower) else NA_real_,
      upper = if (!is.null(x$upper)) as.numeric(x$upper) else NA_real_
    )

    p <- ggplot2::ggplot(df_te, ggplot2::aes(x = x, y = estimate)) +
      ggplot2::geom_line(color = pal[7], linewidth = 0.8) +
      ggplot2::geom_point(color = pal[7], size = 2) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
      .plot_theme() +
      ggplot2::labs(x = ax$label, y = "Average Treatment Effect", title = "ATE")

    if (any(is.finite(df_te$lower)) && any(is.finite(df_te$upper))) {
      p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                                    alpha = 0.2, fill = pal[5], color = NA)
    }
    p
  }

  # Return based on type
  if (type == "effect") {
    result <- .build_effect_plot()
    class(result) <- c("dpmixgpd_ate_plot", class(result))
    return(result)
  }

  if (type == "arms") {
    result <- .build_arms_plot()
    class(result) <- c("dpmixgpd_ate_plot", class(result))
    return(result)
  }

  # type == "both" (default) - maintain backward compatible naming
  result <- list(
    trt_control = .build_arms_plot(),
    treatment_effect = .build_effect_plot()
  )
  class(result) <- c("dpmixgpd_causal_predict_plots", "list")
  result
}

#' Print method for causal prediction plots
#'
#' @param x Object of class \code{dpmixgpd_causal_predict_plots}.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object.
#' @keywords internal
#' @export
print.dpmixgpd_causal_predict_plots <- function(x, ...) {
  if (is.list(x)) {
    for (nm in names(x)) {
      print(x[[nm]])
      cat("\n")
    }
  } else {
    print(x)
  }
  invisible(x)
}

#' Plot fitted values diagnostics
#'
#' S3 method for visualizing fitted values from \code{fitted.mixgpd_fit()}.
#' Produces a 2-panel figure: Q-Q plot and residuals vs fitted.
#'
#' @param x Object of class \code{mixgpd_fitted} from \code{fitted.mixgpd_fit()}.
#' @param y Ignored; included for S3 compatibility.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns a list with the two plots.
#' @keywords internal
#' @export
plot.mixgpd_fitted <- function(x, y = NULL, ...) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Install it first.", call. = FALSE)
  }

  obj <- attr(x, "object")
  y_data <- obj$data$y %||% obj$y

  # Panel 1: Observed vs Fitted (diagonal plot)
  p1_data <- data.frame(
    fitted = x$fit,
    observed = y_data
  )

  # Get axis limits for diagonal line
  axis_min <- min(c(p1_data$fitted, p1_data$observed), na.rm = TRUE)
  axis_max <- max(c(p1_data$fitted, p1_data$observed), na.rm = TRUE)

  pal <- .plot_palette(4L)
  p1 <- ggplot2::ggplot(p1_data, ggplot2::aes(x = fitted, y = observed)) +
    ggplot2::geom_point(size = 2, color = pal[1], alpha = 0.6) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                        color = pal[2], linewidth = 1) +
    .plot_theme() +
    ggplot2::labs(
      title = "Observed vs Fitted Values",
      x = "Fitted Values",
      y = "Observed Values",
      subtitle = "Red line: perfect fit (y = x)"
    ) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(axis_min, axis_max), ylim = c(axis_min, axis_max))

  # Panel 2: Residuals vs Fitted
  p2_data <- data.frame(
    fitted = x$fit,
    residuals = x$residuals
  )

  p2 <- ggplot2::ggplot(p2_data, ggplot2::aes(x = fitted, y = residuals)) +
    ggplot2::geom_point(size = 2, color = pal[3], alpha = 0.6) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = pal[2], linewidth = 1) +
    .plot_theme() +
    ggplot2::labs(
      title = "Residuals vs Fitted Values",
      x = "Fitted Values",
      y = "Residuals",
      subtitle = "Red line: zero residual"
    )

  # Return plot list - prints only if not assigned to variable
  result <- list(observed_fitted_plot = p1, residual_plot = p2)
  class(result) <- c("mixgpd_fitted_plots", "list")
  result
}

#' Print method for fitted value plots
#'
#' @param x Object of class \code{mixgpd_fitted_plots}.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object.
#' @keywords internal
#' @export
print.mixgpd_fitted_plots <- function(x, ...) {
  print(x$observed_fitted_plot)
  cat("\n")
  print(x$residual_plot)
  invisible(x)
}

#' Print method for mixgpd_fit diagnostic plots
#'
#' @param x Object of class \code{mixgpd_fit_plots}.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object.
#' @keywords internal
#' @export
print.mixgpd_fit_plots <- function(x, ...) {
  for (plot_name in names(x)) {
    cat(sprintf("\n=== %s ===\n", plot_name))
    print(x[[plot_name]])
  }
  invisible(x)
}

#' @export
print.dpmixgpd_causal_fit_plots <- function(x, ...) {
  cat("\n=== treated ===\n")
  print(x$treated, ...)
  cat("\n=== control ===\n")
  print(x$control, ...)
  invisible(x)
}

#' Print method for prediction plots
#'
#' @param x Object of class \code{mixgpd_predict_plots}.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object.
#' @keywords internal
#' @export
print.mixgpd_predict_plots <- function(x, ...) {
  # Remove custom class to call default print method for the underlying object
  cls <- class(x)
  class(x) <- setdiff(cls, "mixgpd_predict_plots")
  print(x)
  class(x) <- cls
  invisible(x)
}
