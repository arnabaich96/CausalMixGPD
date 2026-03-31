# 04-S3-Methods.R for DPMixGPD bundles and fits

# S3 methods for Bundle objects -------------------------------------------



#' Print a one-arm workflow bundle
#'
#' \code{print.causalmixgpd_bundle()} gives a compact structural summary of the
#' pre-run bundle created by \code{\link{build_nimble_bundle}}.
#'
#' @param x A \code{"causalmixgpd_bundle"} object.
#' @param code Logical; if TRUE, print the generated NIMBLE model code.
#' @param max_code_lines Integer; maximum number of code lines to print when \code{code=TRUE}.
#' @param ... Unused.
#' @return The object \code{x}, invisibly.
#' @seealso \code{\link{summary.causalmixgpd_bundle}}, \code{\link{mcmc}},
#'   \code{\link{run_mcmc_bundle_manual}}.
#' @examples
#' \dontrun{
#' y <- abs(stats::rnorm(50)) + 0.1
#' bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
#'                              GPD = FALSE, components = 6)
#' print(bundle)
#' print(bundle, code = TRUE, max_code_lines = 30)
#' }
#' @export
print.causalmixgpd_bundle <- function(x, code = FALSE, max_code_lines = 200L, ...) {
  stopifnot(inherits(x, "causalmixgpd_bundle"))
  spec <- x$spec
  meta <- spec$meta

  backend <- meta$backend
  kernel  <- meta$kernel
  K       <- meta$components
  N       <- meta$N
  P       <- meta$P %||% 0L
  has_X   <- isTRUE(meta$has_X)
  GPD     <- isTRUE(meta$GPD)
  knitr_kable <- .is_knitr_output() && isTRUE(getOption("causalmixgpd.knitr.kable", FALSE))

  if (!knitr_kable) {
    cat("CausalMixGPD bundle\n")
  }
  tbl <- data.frame(
    Field = c("Backend", "Kernel", "Components", "N", "X", "GPD", "Epsilon"),
    Value = c(.backend_label(backend),
              .kernel_label(kernel),
              as.character(K),
              as.character(N),
              if (has_X) sprintf("YES (P=%d)", P) else "NO",
              if (GPD) "TRUE" else "FALSE",
              fmt3(x$epsilon %||% 0.025)),
    stringsAsFactors = FALSE
  )
  if (knitr_kable) {
    kbl <- .kable_table(tbl, row.names = FALSE)
    pieces <- list(
      "CausalMixGPD bundle",
      kbl,
      "  contains  : code, constants, data, dimensions, inits, monitors"
    )
    if (isTRUE(code)) {
      code_lines <- NULL
      code_obj <- .extract_nimble_code(x$code)
      if (is.null(code_obj)) {
        code_lines <- "  <no code available>"
      } else {
        out <- .deparse_without_covr(code_obj)
        out <- strsplit(out, "\n", fixed = TRUE)[[1]]
        if (!is.finite(max_code_lines) || max_code_lines <= 0L) {
          code_lines <- out
        } else {
          max_code_lines <- as.integer(max_code_lines)
          show_n <- min(length(out), max_code_lines)
          if (show_n > 0L) {
            code_lines <- out[seq_len(show_n)]
          }
          if (length(out) > show_n) {
            code_lines <- c(code_lines, sprintf("... (%d more lines)", length(out) - show_n))
          }
        }
      }
      pieces <- c(pieces, list("", "Model code", c("```", code_lines, "```")))
    }
    return(do.call(.knitr_asis, pieces))
  }
  print_fmt3(tbl, row.names = FALSE)
  cat("\n  contains  : code, constants, data, dimensions, inits, monitors\n")

  if (isTRUE(code)) {
    cat("\nModel code\n")
    code_obj <- .extract_nimble_code(x$code)
    if (is.null(code_obj)) {
      cat("  <no code available>\n")
    } else {
      out <- .deparse_without_covr(code_obj)
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

#' Print a causal workflow bundle
#'
#' \code{print.causalmixgpd_causal_bundle()} gives a compact structural summary
#' of the pre-run causal bundle created by \code{\link{build_causal_bundle}}.
#'
#' @param x A \code{"causalmixgpd_causal_bundle"} object.
#' @param code Logical; if TRUE, print generated NIMBLE code for each block.
#' @param max_code_lines Integer; maximum number of code lines to print when \code{code=TRUE}.
#' @param ... Unused.
#' @importFrom utils capture.output
#' @return The input object (invisibly).
#' @seealso \code{\link{summary.causalmixgpd_causal_bundle}},
#'   \code{\link{run_mcmc_causal}}, \code{\link{ate}}, \code{\link{qte}}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal")
#' print(cb)
#' }
#' @export
print.causalmixgpd_causal_bundle <- function(x, code = FALSE, max_code_lines = 200L, ...) {
  stopifnot(inherits(x, "causalmixgpd_causal_bundle"))

  meta <- x$meta %||% list()
  knitr_kable <- .is_knitr_output() && isTRUE(getOption("causalmixgpd.knitr.kable", FALSE))
  if (!knitr_kable) {
    cat("CausalMixGPD causal bundle\n")
  }
  ps_meta <- meta$ps %||% list()
  ps_model <- ps_meta$model_type %||% FALSE
  ps_label <- if (!isTRUE(ps_meta$enabled) || isFALSE(ps_model)) {
    "PS model: disabled"
  } else {
    switch(ps_model,
           logit = "PS model: Bayesian logit (A | X)",
           probit = "PS model: Bayesian probit (A | X)",
           naive = "PS model: Gaussian naive Bayes",
           sprintf("PS model: %s", ps_model))
  }
  cat(ps_label, "\n")
  backend <- meta$backend %||% list()
  kernel <- meta$kernel %||% list()
  gpd <- meta$GPD %||% list()
  comps <- meta$components %||% list()
  eps <- meta$epsilon %||% list()

  trt_backend <- backend$trt %||% "?"
  con_backend <- backend$con %||% "?"
  trt_kernel <- kernel$trt %||% "?"
  con_kernel <- kernel$con %||% "?"

  tbl <- data.frame(
    Field = c("Backend", "Kernel", "Components", "GPD tail", "Epsilon"),
    Treated = c(
      if (trt_backend %in% c("sb", "crp")) .backend_label(trt_backend) else trt_backend,
      trt_kernel,
      as.character(comps$trt %||% "?"),
      ifelse(isTRUE(gpd$trt), "TRUE", "FALSE"),
      fmt3(eps$trt %||% NA_real_)
    ),
    Control = c(
      if (con_backend %in% c("sb", "crp")) .backend_label(con_backend) else con_backend,
      con_kernel,
      as.character(comps$con %||% "?"),
      ifelse(isTRUE(gpd$con), "TRUE", "FALSE"),
      fmt3(eps$con %||% NA_real_)
    ),
    stringsAsFactors = FALSE
  )
  if (knitr_kable) {
    kbl <- .kable_table(tbl, row.names = FALSE)
    pieces <- list(
      "CausalMixGPD causal bundle",
      ps_label,
      kbl,
      "",
      paste("Outcome PS included:", ifelse(isTRUE(meta$ps$enabled), "TRUE", "FALSE")),
      paste("n (control) =", length(x$index$con %||% integer(0)),
            "| n (treated) =", length(x$index$trt %||% integer(0)))
    )
    if (isTRUE(code)) {
      code_lines <- capture.output({
        cat("-- PS code --\n")
        print(x$design, code = TRUE, max_code_lines = max_code_lines)
        cat("\n-- Outcome code (control) --\n")
        print(x$outcome$con, code = TRUE, max_code_lines = max_code_lines)
        cat("\n-- Outcome code (treated) --\n")
        print(x$outcome$trt, code = TRUE, max_code_lines = max_code_lines)
      })
      pieces <- c(pieces, list("", "Model code", c("```", code_lines, "```")))
    }
    return(do.call(.knitr_asis, pieces))
  }
  print_fmt3(tbl, row.names = FALSE)
  cat("\n")
  cat("Outcome PS included:", ifelse(isTRUE(meta$ps$enabled), "TRUE", "FALSE"), "\n")
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

#' Summarize a causal workflow bundle
#'
#' \code{summary.causalmixgpd_causal_bundle()} is the bundle-level validation
#' checkpoint for the causal workflow.
#'
#' @param object A \code{"causalmixgpd_causal_bundle"} object.
#' @param code Logical; if TRUE, print generated NIMBLE code for each block.
#' @param max_code_lines Integer; maximum number of code lines to print when \code{code=TRUE}.
#' @param ... Unused.
#' @return The input object (invisibly).
#' @seealso \code{\link{print.causalmixgpd_causal_bundle}},
#'   \code{\link{run_mcmc_causal}}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal")
#' summary(cb)
#' }
#' @export
summary.causalmixgpd_causal_bundle <- function(object, code = FALSE, max_code_lines = 200L, ...) {
  stopifnot(inherits(object, "causalmixgpd_causal_bundle"))

  knitr_kable <- .is_knitr_output() && isTRUE(getOption("causalmixgpd.knitr.kable", FALSE))
  if (knitr_kable) {
    base_out <- print.causalmixgpd_causal_bundle(object, code = code, max_code_lines = max_code_lines)
    return(do.call(.knitr_asis, list("CausalMixGPD causal bundle summary", base_out)))
  }
  cat("CausalMixGPD causal bundle summary\n")
  print.causalmixgpd_causal_bundle(object, code = FALSE, max_code_lines = max_code_lines)
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
#' @param x A \code{"causalmixgpd_ps_bundle"} object.
#' @param code Logical; if TRUE, print generated NIMBLE code for the PS model.
#' @param max_code_lines Integer; maximum number of code lines to print when \code{code=TRUE}.
#' @param ... Unused.
#' @return The input object (invisibly).
#' @export
print.causalmixgpd_ps_bundle <- function(x, code = FALSE, max_code_lines = 200L, ...) {
  stopifnot(inherits(x, "causalmixgpd_ps_bundle"))

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
    txt <- .deparse_without_covr(x$code)
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

#' @export
summary.causalmixgpd_ps_bundle <- function(object, code = FALSE, max_code_lines = 200L, ...) {
  print.causalmixgpd_ps_bundle(object, code = isTRUE(code), max_code_lines = max_code_lines)
  invisible(object)
}

#' Print a fitted causal model
#'
#' \code{print.causalmixgpd_causal_fit()} provides a compact overview of the
#' fitted treated/control outcome blocks and the PS component when present.
#'
#' @param x A \code{"causalmixgpd_causal_fit"} object.
#' @param ... Unused.
#' @return The input object (invisibly).
#' @seealso \code{\link{summary.causalmixgpd_causal_fit}},
#'   \code{\link{predict.causalmixgpd_causal_fit}}, \code{\link{ate}},
#'   \code{\link{qte}}.
#' @export
print.causalmixgpd_causal_fit <- function(x, ...) {
  stopifnot(inherits(x, "causalmixgpd_causal_fit"))

  meta <- x$bundle$meta %||% list()
  cat("CausalMixGPD causal fit\n")
  ps_meta <- meta$ps %||% list()
  ps_model <- ps_meta$model_type %||% FALSE
  ps_label <- if (!isTRUE(ps_meta$enabled) || isFALSE(ps_model)) {
    "PS model: disabled"
  } else {
    switch(ps_model,
           logit = "PS model: Bayesian logit (A | X)",
           probit = "PS model: Bayesian probit (A | X)",
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

#' Summarize a fitted causal model
#'
#' \code{summary.causalmixgpd_causal_fit()} routes the user to the fitted PS
#' block, treated/control fits, and downstream prediction/effect methods.
#'
#' @param object A \code{"causalmixgpd_causal_fit"} object.
#' @param ... Unused.
#' @return The input object (invisibly).
#' @seealso \code{\link{print.causalmixgpd_causal_fit}},
#'   \code{\link{predict.causalmixgpd_causal_fit}}, \code{\link{ate}},
#'   \code{\link{qte}}, \code{\link{cate}}, \code{\link{cqte}}.
#' @export
summary.causalmixgpd_causal_fit <- function(object, ...) {
  stopifnot(inherits(object, "causalmixgpd_causal_fit"))
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
#' @param x A \code{"causalmixgpd_ps_fit"} object.
#' @param ... Unused.
#' @return The input object (invisibly).
#' @export
print.causalmixgpd_ps_fit <- function(x, ...) {
  stopifnot(inherits(x, "causalmixgpd_ps_fit"))
  cat("CausalMixGPD PS fit\n")
  model_type <- x$bundle$spec$meta$type %||% "ps_logit"
  model_label <- switch(model_type,
                        ps_logit = "logit",
                        ps_probit = "probit",
                        ps_naive = "naive",
                        model_type)
  cat("model:", model_label, "\n")
  invisible(x)
}

#' @export
summary.causalmixgpd_ps_fit <- function(object, ...) {
  print.causalmixgpd_ps_fit(object)
  invisible(object)
}

#' Plot the treated and control outcome fits from a causal model
#'
#' \code{plot.causalmixgpd_causal_fit()} is a convenience router to the
#' underlying one-arm diagnostic plots for the treated and control fits.
#'
#' @param x A \code{"causalmixgpd_causal_fit"} object.
#' @param arm Integer or character; \code{1} or \code{"treated"} for treatment,
#'   \code{0} or \code{"control"} for control.
#' @param ... Additional arguments forwarded to the underlying outcome plot method.
#' @return The result of the underlying plot call (invisibly).
#' @seealso \code{\link{predict.causalmixgpd_causal_fit}}, \code{\link{ate}},
#'   \code{\link{qte}}, \code{\link{plot.mixgpd_fit}}.
#' @export
plot.causalmixgpd_causal_fit <- function(x, arm = "both", ...) {
  stopifnot(inherits(x, "causalmixgpd_causal_fit"))
  if (is.null(arm)) arm <- "both"
  if (is.character(arm)) {
    arm_chr <- tolower(arm)
    # accept common aliases
    if (arm_chr %in% c("trt", "t")) arm_chr <- "treated"
    if (arm_chr %in% c("con", "c", "ctrl")) arm_chr <- "control"
    if (arm_chr %in% c("both", "all")) arm_chr <- "both"
    arm <- match.arg(arm_chr, c("treated", "control", "both"))
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
    class(out) <- c("causalmixgpd_causal_fit_plots", "list")
    return(.wrap_plotly(out))
  }
  if (identical(arm, "treated")) {
    out <- plot.mixgpd_fit(x$outcome_fit$trt, ...)
  } else if (identical(arm, "control")) {
    out <- plot.mixgpd_fit(x$outcome_fit$con, ...)
  } else {
    stop("arm must be 0/1 or 'treated'/'control'/'both'.", call. = FALSE)
  }
  .wrap_plotly(out)
}
# helper


#' Summarize a one-arm workflow bundle
#'
#' \code{summary.causalmixgpd_bundle()} prints the structural contents of a
#' bundle before MCMC is run.
#'
#' @details
#' The summary is meant for workflow validation rather than inference. It shows:
#' \itemize{
#'   \item the model metadata (backend, kernel, components, covariates, GPD flag),
#'   \item the prior/parameter table derived from \code{spec$plan},
#'   \item the nodes that will be monitored during MCMC.
#' }
#'
#' This is the recommended checkpoint after \code{\link{build_nimble_bundle}}
#' and before \code{\link{run_mcmc_bundle_manual}}.
#'
#' @param object A \code{"causalmixgpd_bundle"} object.
#' @param ... Unused.
#' @return An invisible list with elements \code{meta}, \code{priors}, and
#'   \code{monitors}.
#' @seealso \code{\link{build_nimble_bundle}}, \code{\link{print.causalmixgpd_bundle}},
#'   \code{\link{run_mcmc_bundle_manual}}.
#' @examples
#' \dontrun{
#' y <- abs(stats::rnorm(50)) + 0.1
#' bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
#'                              GPD = FALSE, components = 6)
#' summary(bundle)
#' }
#' @export
summary.causalmixgpd_bundle <- function(object, ...) {
  stopifnot(inherits(object, "causalmixgpd_bundle"))
  spec <- object$spec
  meta <- spec$meta

  backend <- meta$backend
  kernel  <- meta$kernel
  K       <- meta$components
  N       <- meta$N
  P       <- meta$P %||% 0L
  has_X   <- isTRUE(meta$has_X)
  GPD     <- isTRUE(meta$GPD)
  knitr_kable <- .is_knitr_output() && isTRUE(getOption("causalmixgpd.knitr.kable", FALSE))

  meta_tbl <- data.frame(
    Field = c("Backend", "Kernel", "Components", "N", "X", "GPD", "Epsilon"),
    Value = c(.backend_label(backend),
              .kernel_label(kernel),
              as.character(K),
              as.character(N),
              if (has_X) sprintf("YES (P=%d)", P) else "NO",
              if (GPD) "TRUE" else "FALSE",
              fmt3(object$epsilon %||% 0.025)),
    stringsAsFactors = FALSE
  )
  pri <- build_prior_table_from_spec(spec)
  mons <- object$monitors %||% character()
  if (knitr_kable) {
    meta_kbl <- .kable_table(meta_tbl, row.names = FALSE)
    pri_kbl <- .kable_table(format_df3(pri), row.names = FALSE)
    pieces <- list(
      "CausalMixGPD bundle summary",
      meta_kbl,
      "",
      "Parameter specification",
      pri_kbl,
      "",
      "Monitors",
      paste("  n =", length(mons))
    )
    if (length(mons)) {
      show_n <- min(12L, length(mons))
      pieces <- c(pieces, list(paste0("  ", paste(mons[seq_len(show_n)], collapse = ", "),
                                      if (length(mons) > show_n) ", ..." else "")))
    }
    return(do.call(.knitr_asis, pieces))
  }
  cat("CausalMixGPD bundle summary\n")
  print_fmt3(meta_tbl, row.names = FALSE)
  cat("\n")

  # Prior/parameter table
  cat("Parameter specification\n")
  print_fmt3(pri, row.names = FALSE)
  cat("\n")

  # Monitor overview (compact)
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



# S3 methods for Fit objects ----------------------------------------------

# ============================================================
# Public S3 generics (export in your NAMESPACE if packaging)
# ============================================================

#' Print a one-arm fitted model
#'
#' \code{print.mixgpd_fit()} gives a compact header for a fitted one-arm model.
#' It is meant as a quick identity check rather than a full posterior summary.
#'
#' @param x A fitted object of class \code{"mixgpd_fit"}.
#' @param ... Unused.
#' @return \code{x} invisibly.
#' @seealso \code{\link{summary.mixgpd_fit}}, \code{\link{params}},
#'   \code{\link{predict.mixgpd_fit}}.
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

#' Extract posterior mean parameters in natural shape
#'
#' \code{params()} reshapes posterior mean summaries back into the parameter
#' layout implied by the fitted model specification.
#'
#' @details
#' This extractor is intended for structural inspection of the fitted model.
#' Scalar quantities remain scalar, component-specific parameters are returned as
#' vectors, and linked regression blocks are returned as matrices.
#'
#' For a spliced model, the extractor returns posterior means of the bulk
#' mixture parameters together with component-level threshold, tail-scale, and
#' tail-shape terms. When tail terms are link-mode, the corresponding
#' component-by-covariate beta blocks are returned.
#'
#' @param object A fitted object of class \code{"mixgpd_fit"}.
#' @param ... Unused.
#' @return An object of class \code{"mixgpd_params"} (a named list). For
#'   causal fits, \code{params()} returns a treated/control pair.
#' @seealso \code{\link{summary.mixgpd_fit}}, \code{\link{predict.mixgpd_fit}},
#'   \code{\link{ess_summary}}.
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

  mat <- .extract_draws_matrix(object, drop_v = TRUE, epsilon = NULL)
  means <- colMeans(mat, na.rm = TRUE)
  means[!is.finite(means)] <- NA_real_
  cn <- names(means)

  spec <- object$spec %||% list()
  plan <- spec$plan %||% list()
  meta <- spec$meta %||% list()
  bulk <- plan$bulk %||% list()
  gpd <- plan$gpd %||% list()
  is_spliced <- identical(meta$backend %||% spec$dispatch$backend %||% "", "spliced")

  X <- object$data$X %||% object$X %||% NULL
  xnames <- if (!is.null(X) && !is.null(colnames(X))) colnames(X) else NULL

  .get_vector <- function(prefix) {
    cols <- grep(paste0("^", prefix, "\\[[0-9]+\\]$"), cn, value = TRUE)
    if (!length(cols)) return(NULL)
    idx <- as.integer(sub(paste0("^", prefix, "\\[([0-9]+)\\]$"), "\\1", cols))
    ord <- order(idx, na.last = NA)
    vec <- as.numeric(unname(means[cols][ord]))
    storage.mode(vec) <- "double"
    vec
  }

  .get_scalar <- function(name) {
    if (!(name %in% cn)) return(NULL)
    as.numeric(unname(means[name]))
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
      out[idx1[i], idx2[i]] <- as.numeric(unname(means[cols[i]]))
    }
    storage.mode(out) <- "double"
    if (!is.null(xnames) && length(xnames) == n2) colnames(out) <- xnames
    rownames(out) <- paste0("comp", seq_len(n1))
    out
  }

  out <- list()

  if ("alpha" %in% cn) out$alpha <- as.numeric(unname(means["alpha"]))

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
          storage.mode(beta) <- "double"
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
      if (is_spliced) {
        thr <- .get_vector("threshold")
        if (!is.null(thr)) out$threshold <- thr
      } else {
        thr <- .get_scalar("threshold")
        if (is.null(thr)) thr <- .get_vector("threshold")
        if (!is.null(thr)) out$threshold <- if (length(thr) == 1L) thr else as.numeric(mean(thr, na.rm = TRUE))
      }
    } else if (identical(thr_mode, "link")) {
      beta_thr <- .get_matrix("beta_threshold")
      if (is.null(beta_thr)) beta_thr <- .get_vector("beta_threshold")
      if (!is.null(beta_thr)) out$beta_threshold <- beta_thr
      if (!is.null(gpd$threshold$link_dist) &&
          identical(gpd$threshold$link_dist$dist, "lognormal") &&
          "sdlog_u" %in% cn) {
        out$sdlog_u <- as.numeric(unname(means["sdlog_u"]))
      }
    }
  }

  if (!is.null(gpd$tail_scale)) {
    ts_mode <- gpd$tail_scale$mode %||% NA_character_
    if (identical(ts_mode, "link")) {
      beta_ts <- .get_matrix("beta_tail_scale")
      if (is.null(beta_ts)) beta_ts <- .get_vector("beta_tail_scale")
      if (!is.null(beta_ts)) out$beta_tail_scale <- beta_ts
    } else if (ts_mode %in% c("fixed", "dist")) {
      if (is_spliced) {
        tail_scale <- .get_vector("tail_scale")
        if (!is.null(tail_scale)) out$tail_scale <- tail_scale
      } else {
        tail_scale <- .get_scalar("tail_scale")
        if (!is.null(tail_scale)) out$tail_scale <- tail_scale
      }
    }
  }

  if (!is.null(gpd$tail_shape)) {
    tsh_mode <- gpd$tail_shape$mode %||% NA_character_
    if (identical(tsh_mode, "link")) {
      beta_tsh <- .get_matrix("beta_tail_shape")
      if (is.null(beta_tsh)) beta_tsh <- .get_vector("beta_tail_shape")
      if (!is.null(beta_tsh)) out$beta_tail_shape <- beta_tsh
    } else if (is_spliced) {
      tail_shape <- .get_vector("tail_shape")
      if (!is.null(tail_shape)) out$tail_shape <- tail_shape
    } else {
      tail_shape <- .get_scalar("tail_shape")
      if (!is.null(tail_shape)) out$tail_shape <- tail_shape
    }
  }

  .coerce_numeric_like <- function(v) {
    if (is.null(v)) return(v)
    if (is.matrix(v) && !is.numeric(v)) {
      vv <- suppressWarnings(as.numeric(v))
      if (!any(is.na(vv) & !is.na(c(v)))) {
        outm <- matrix(vv, nrow = nrow(v), ncol = ncol(v))
        dimnames(outm) <- dimnames(v)
        return(outm)
      }
      return(v)
    }
    if (is.data.frame(v)) {
      num_like <- vapply(v, function(col) {
        if (is.numeric(col)) return(TRUE)
        if (!is.character(col) && !is.factor(col)) return(FALSE)
        col_chr <- as.character(col)
        test <- suppressWarnings(as.numeric(col_chr))
        !any(is.na(test) & !is.na(col_chr))
      }, logical(1))
      if (any(num_like)) {
        for (j in which(num_like)) {
          if (!is.numeric(v[[j]])) {
            v[[j]] <- suppressWarnings(as.numeric(as.character(v[[j]])))
          }
        }
      }
      return(v)
    }
    if (is.character(v) || is.factor(v)) {
      vv <- suppressWarnings(as.numeric(as.character(v)))
      if (!any(is.na(vv) & !is.na(v))) return(vv)
      return(v)
    }
    v
  }
  out <- lapply(out, .coerce_numeric_like)

  class(out) <- "mixgpd_params"
  out
}

#' @export
params.causalmixgpd_causal_fit <- function(object, ...) {
  stopifnot(inherits(object, "causalmixgpd_causal_fit"))
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
  .round_num <- function(v, digits = 4) {
    if (is.matrix(v) && is.numeric(v)) {
      out <- matrix(signif(as.numeric(v), digits = digits),
                    nrow = nrow(v), ncol = ncol(v))
      dimnames(out) <- dimnames(v)
      return(out)
    }
    if (is.numeric(v)) {
      return(signif(as.numeric(v), digits = digits))
    }
    v
  }
  for (nm in names(x)) {
    cat("\n$", nm, "\n", sep = "")
    val <- x[[nm]]
    if (is.null(dim(val))) {
      if (is.numeric(val)) {
        print(.round_num(val, digits = digits), ...)
      } else {
        print(val)
      }
    } else if (is.matrix(val) || is.data.frame(val)) {
      if (is.matrix(val) && is.numeric(val)) {
        print(.round_num(val, digits = digits), ...)
      } else if (is.data.frame(val)) {
        num_cols <- vapply(val, is.numeric, logical(1))
        val_out <- val
        if (any(num_cols)) {
          val_out[num_cols] <- lapply(val_out[num_cols], .round_num, digits = digits)
        }
        print(val_out, quote = FALSE, ...)
      } else {
        print(val, ...)
      }
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

#' Summarize posterior draws from a one-arm fitted model
#'
#' \code{summary.mixgpd_fit()} computes posterior summaries for monitored model
#' parameters.
#'
#' @details
#' The returned table is a parameter-level summary of the posterior draws, not a
#' predictive summary. Use \code{\link{predict.mixgpd_fit}} for posterior
#' predictive quantities such as densities, survival probabilities, quantiles,
#' and means.
#'
#' The summary respects the stored truncation metadata and reports WAIC if it
#' was requested during MCMC.
#'
#' @param object A fitted object of class \code{"mixgpd_fit"}.
#' @param pars Optional character vector of parameters to summarize. If NULL, summarize all (excluding v's).
#' @param probs Numeric vector of quantiles to report.
#' @param ... Unused.
#' @return An object of class \code{"mixgpd_summary"}.
#' @seealso \code{\link{print.mixgpd_fit}}, \code{\link{params}},
#'   \code{\link{predict.mixgpd_fit}}, \code{\link{ess_summary}}.
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
#' summary(fit)
#' }
#' @export
print.mixgpd_summary <- function(x, digits = 3, max_rows = 60, ...) {
  stopifnot(inherits(x, "mixgpd_summary"))
  model <- x$model %||% list()
  waic <- x$waic
  trunc <- model$truncation %||% list()
  knitr_kable <- .is_knitr_output() && isTRUE(getOption("causalmixgpd.knitr.kable", FALSE))

  gpd_txt <- if (isTRUE(model$gpd)) "TRUE" else if (identical(model$gpd, FALSE)) "FALSE" else "<unknown>"
  eps <- model$epsilon %||% NA_real_

  if (knitr_kable) {
    pieces <- list(
      sprintf("MixGPD summary | backend: %s | kernel: %s | GPD tail: %s | epsilon: %s",
              .backend_label(model$backend %||% "<unknown>"),
              .kernel_label(model$kernel  %||% "<unknown>"),
              gpd_txt,
              ifelse(is.na(eps), "<unknown>", fmt3(eps))),
      sprintf("n = %s | components = %s",
              ifelse(is.na(model$n), "<unknown>", model$n),
              ifelse(is.na(model$components), "<unknown>", model$components)),
      "Summary"
    )
    if (!is.na(eps) && eps > 0) {
      pieces <- c(pieces, list(sprintf("Initial components: %s | Components after truncation: %s",
                                       ifelse(is.na(model$components), "<unknown>", model$components),
                                       trunc$Kt %||% "<unknown>")))
    }
    if (!is.null(waic)) {
      wa <- waic$WAIC %||% waic$waic %||% waic[["WAIC"]] %||% NA_real_
      lp <- waic$lppd %||% waic[["lppd"]] %||% NA_real_
      pw <- waic$pWAIC %||% waic[["pWAIC"]] %||% NA_real_
      pieces <- c(pieces, list(sprintf("WAIC: %s", ifelse(is.na(wa), "<unknown>", fmt3(wa)))))
      if (is.finite(lp) || is.finite(pw)) {
        pieces <- c(pieces, list(sprintf("lppd: %s | pWAIC: %s",
                                         ifelse(is.finite(lp), fmt3(lp), "<unknown>"),
                                         ifelse(is.finite(pw), fmt3(pw), "<unknown>"))))
      }
    }
    pieces <- c(pieces, list("", "Summary table"))

    tab_print <- x$table
    num_cols <- vapply(tab_print, is.numeric, logical(1))
    tab_print[num_cols] <- lapply(tab_print[num_cols], function(v) round(v, digits))
    if (nrow(tab_print) > max_rows) {
      pieces <- c(pieces, list(sprintf("Showing first %d of %d parameters.", max_rows, nrow(tab_print)), ""))
      tab_print <- tab_print[seq_len(max_rows), , drop = FALSE]
    }
    pieces <- c(pieces, list(.kable_table(tab_print, row.names = FALSE)))
    return(do.call(.knitr_asis, pieces))
  }
  cat(sprintf("MixGPD summary | backend: %s | kernel: %s | GPD tail: %s | epsilon: %s\n",
              .backend_label(model$backend %||% "<unknown>"),
              .kernel_label(model$kernel  %||% "<unknown>"),
              gpd_txt,
              ifelse(is.na(eps), "<unknown>", fmt3(eps))))
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
                ifelse(is.na(wa), "<unknown>", fmt3(wa))))
    if (is.finite(lp) || is.finite(pw)) {
      cat(sprintf("lppd: %s | pWAIC: %s\n",
                  ifelse(is.finite(lp), fmt3(lp), "<unknown>"),
                  ifelse(is.finite(pw), fmt3(pw), "<unknown>")))
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

  print_fmt3(tab_print, row.names = FALSE)
  invisible(x)
}



#' Effective sample size summaries for fitted models
#'
#' \code{ess_summary()} reports effective sample size diagnostics for posterior
#' draws, optionally scaled by wall-clock time.
#'
#' @details
#' This is a convergence and efficiency diagnostic, not a model summary. For
#' causal fits the function evaluates each outcome arm separately and tags the
#' rows accordingly.
#'
#' @param fit A \code{"mixgpd_fit"} or \code{"causalmixgpd_causal_fit"} object.
#' @param params Optional character vector of parameter names/patterns. If
#'   \code{NULL}, a fixed canonical set is auto-resolved.
#' @param per_chain Logical; if \code{TRUE}, include per-chain ESS rows.
#' @param wall_time Optional numeric total MCMC time in seconds. If \code{NULL},
#'   uses \code{fit$timing$mcmc} when available.
#' @param robust Logical; if \code{TRUE}, skip missing parameters gracefully.
#' @param ... Unused.
#' @return Object of class \code{"mixgpd_ess_summary"} with elements
#'   \code{table}, \code{overall}, and \code{meta}.
#' @seealso \code{\link{summary.mixgpd_fit}}, \code{\link{plot.mixgpd_fit}},
#'   \code{\link{params}}.
#' @export
ess_summary <- function(fit, params = NULL, per_chain = TRUE, wall_time = NULL, robust = TRUE, ...) {
  if (!requireNamespace("coda", quietly = TRUE)) {
    stop("Package 'coda' is required for ess_summary().", call. = FALSE)
  }

  .resolve_core_params <- function(cn) {
    pick_first <- function(cands, cn) {
      for (rx in cands) {
        hit <- grep(rx, cn, value = TRUE)
        if (length(hit)) return(hit[1L])
      }
      NULL
    }
    out <- character(0)
    p1 <- pick_first(c("^alpha$", "^kappa$", "^dp_alpha$"), cn)
    if (!is.null(p1)) out <- c(out, p1)
    p2 <- pick_first(c("^threshold$", "^threshold\\[1\\]$"), cn)
    if (!is.null(p2)) out <- c(out, p2)
    p3 <- pick_first(c("^tail_scale$", "^tail_scale\\[1\\]$"), cn)
    if (!is.null(p3)) out <- c(out, p3)
    p4 <- pick_first(c("^tail_shape$", "^tail_shape\\[1\\]$"), cn)
    if (!is.null(p4)) out <- c(out, p4)
    p5 <- pick_first(c("^mu\\[1\\]$", "^meanlog\\[1\\]$", "^location\\[1\\]$", "^mu$"), cn)
    if (!is.null(p5)) out <- c(out, p5)
    p6 <- pick_first(c("^sigma\\[1\\]$", "^sdlog\\[1\\]$", "^scale\\[1\\]$", "^shape\\[1\\]$", "^sigma$"), cn)
    if (!is.null(p6)) out <- c(out, p6)
    p7 <- pick_first(c("^beta\\[1\\]$", "^beta_.*\\[1\\]$"), cn)
    if (!is.null(p7)) out <- c(out, p7)
    unique(out)
  }

  .resolve_params <- function(cn, params, robust = TRUE) {
    if (is.null(params)) return(.resolve_core_params(cn))
    hits <- unique(unlist(lapply(params, function(p) {
      if (p %in% cn) return(p)
      grep(p, cn, value = TRUE)
    })))
    if (!length(hits) && !isTRUE(robust)) {
      stop("No parameters matched 'params'.", call. = FALSE)
    }
    hits
  }

  .ess_one <- function(obj, arm = NA_character_, wall_time = NULL, params = NULL, per_chain = TRUE, robust = TRUE) {
    smp <- .get_samples_mcmclist(obj)
    chain_mats <- lapply(smp, function(ch) as.matrix(ch))
    cn <- colnames(chain_mats[[1L]])
    keep <- .resolve_params(cn, params = params, robust = robust)
    if (!length(keep)) {
      return(list(
        table = data.frame(),
        meta = list(params_used = character(0), nchains = length(chain_mats), seconds = NA_real_, seconds_source = "none")
      ))
    }
    chain_secs <- suppressWarnings(as.numeric(wall_time %||% obj$timing$mcmc %||% NA_real_))
    seconds_source <- if (!is.null(wall_time)) "wall_time" else if (is.finite(chain_secs)) "fit$timing$mcmc" else "none"
    if (identical(seconds_source, "none")) {
      warning("No wall-time available; ESS/sec reported as NA. Provide 'wall_time' or run with timing=TRUE.", call. = FALSE)
    }
    if (is.finite(chain_secs) && length(chain_mats) > 0L) chain_secs <- chain_secs / length(chain_mats)

    rows <- list()
    if (isTRUE(per_chain)) {
      for (i in seq_along(chain_mats)) {
        m <- chain_mats[[i]]
        for (p in keep) {
          v <- m[, p]
          v <- v[is.finite(v)]
          ess <- if (length(v) >= 3L) as.numeric(coda::effectiveSize(coda::mcmc(v))) else NA_real_
          rows[[length(rows) + 1L]] <- data.frame(
            arm = arm,
            param = p,
            chain = paste0("chain", i),
            ess = ess,
            seconds = chain_secs,
            ess_per_sec = if (is.finite(chain_secs) && chain_secs > 0) ess / chain_secs else NA_real_,
            stringsAsFactors = FALSE
          )
        }
      }
    }

    pooled <- as.matrix(do.call(rbind, chain_mats))
    pooled_secs <- suppressWarnings(as.numeric(wall_time %||% obj$timing$mcmc %||% NA_real_))
    for (p in keep) {
      v <- pooled[, p]
      v <- v[is.finite(v)]
      ess <- if (length(v) >= 3L) as.numeric(coda::effectiveSize(coda::mcmc(v))) else NA_real_
      rows[[length(rows) + 1L]] <- data.frame(
        arm = arm,
        param = p,
        chain = "pooled",
        ess = ess,
        seconds = pooled_secs,
        ess_per_sec = if (is.finite(pooled_secs) && pooled_secs > 0) ess / pooled_secs else NA_real_,
        stringsAsFactors = FALSE
      )
    }

    tab <- do.call(rbind, rows)
    list(
      table = tab,
      meta = list(params_used = keep, nchains = length(chain_mats), seconds = pooled_secs, seconds_source = seconds_source)
    )
  }

  if (inherits(fit, "causalmixgpd_causal_fit")) {
    con <- .ess_one(fit$outcome_fit$con, arm = "control", wall_time = wall_time, params = params, per_chain = per_chain, robust = robust)
    trt <- .ess_one(fit$outcome_fit$trt, arm = "treated", wall_time = wall_time, params = params, per_chain = per_chain, robust = robust)
    tab <- rbind(con$table, trt$table)
    meta <- list(
      nchains = unique(c(con$meta$nchains, trt$meta$nchains)),
      seconds_source = unique(c(con$meta$seconds_source, trt$meta$seconds_source)),
      params_used = unique(c(con$meta$params_used, trt$meta$params_used))
    )
  } else {
    if (!inherits(fit, "mixgpd_fit")) stop("'fit' must be mixgpd_fit or causalmixgpd_causal_fit.", call. = FALSE)
    one <- .ess_one(fit, arm = "single", wall_time = wall_time, params = params, per_chain = per_chain, robust = robust)
    tab <- one$table
    meta <- one$meta
  }

  overall <- if (nrow(tab)) {
    stats::aggregate(cbind(ess, ess_per_sec) ~ arm + param, data = tab[tab$chain == "pooled", , drop = FALSE], FUN = mean, na.rm = TRUE)
  } else {
    data.frame()
  }

  out <- list(table = tab, overall = overall, meta = meta)
  class(out) <- "mixgpd_ess_summary"
  out
}

#' @export
print.mixgpd_ess_summary <- function(x, digits = 3L, max_rows = 25L, ...) {
  stopifnot(inherits(x, "mixgpd_ess_summary"))
  tab <- x$table %||% data.frame()
  cat("ESS summary (ESS/sec)\n")
  if (!nrow(tab)) {
    cat("No matched parameters.\n")
    return(invisible(x))
  }
  if (nrow(tab) > max_rows) tab <- tab[seq_len(max_rows), , drop = FALSE]
  num_cols <- vapply(tab, is.numeric, logical(1))
  tab[num_cols] <- lapply(tab[num_cols], function(v) round(v, digits = digits))
  print_fmt3(tab, row.names = FALSE)
  invisible(x)
}

#' @export
summary.mixgpd_ess_summary <- function(object, ...) {
  object$overall %||% data.frame()
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
#'   (i) character vector of parameter patterns (exact names or partial matches),
#'   or (ii) a single regex string (e.g. \code{"alpha|threshold|tail_"}).
#'   If \code{NULL}, plots all parameters in the samples.
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

  # ---- choose params ----
  cn <- colnames(as.matrix(smp[[1]]))
  if (is.null(params)) {
    params <- cn
  }

  # params can be vector of names/patterns OR single regex
  match_params <- function(p, all_params) {
    hits <- character(0)
    for (tok in p) {
      if (tok %in% all_params) {
        hits <- c(hits, tok)
        next
      }
      m <- grep(tok, all_params, value = TRUE)
      if (!length(m)) {
        m <- agrep(tok, all_params, value = TRUE, ignore.case = TRUE, max.distance = 0.2)
      }
      if (!length(m)) {
        stop("No parameters match: ", tok, call. = FALSE)
      }
      hits <- c(hits, m)
    }
    unique(hits)
  }

  if (is.character(params) && length(params) > 1) {
    keep <- match_params(params, cn)
  } else if (is.character(params) && length(params) == 1) {
    keep <- grep(params, cn, value = TRUE)
    if (!length(keep)) {
      keep <- agrep(params, cn, value = TRUE, ignore.case = TRUE, max.distance = 0.2)
    }
    if (!length(keep)) stop("No parameters match regex: ", params, call. = FALSE)
  } else {
    stop("'params' must be NULL, a character vector of names/patterns, or a single regex string.", call. = FALSE)
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
    if (identical(f, "geweke")) {
      z_obj <- coda::geweke.diag(smp)
      z_list <- if (inherits(z_obj, "geweke.diag")) list(z_obj) else z_obj
      z_df <- do.call(rbind, lapply(seq_along(z_list), function(i) {
        z_vals <- z_list[[i]]$z %||% z_list[[i]]
        data.frame(
          Parameter = names(z_vals),
          z = as.numeric(z_vals),
          Chain = factor(i),
          stringsAsFactors = FALSE
        )
      }))
      z_df <- z_df[z_df$Parameter %in% keep, , drop = FALSE]
      z_df$Parameter <- factor(z_df$Parameter, levels = rev(unique(keep)))
      p <- ggplot2::ggplot(z_df, ggplot2::aes(x = z, y = Parameter, color = Chain)) +
        ggplot2::geom_point(position = ggplot2::position_jitter(height = 0.15, width = 0)) +
        ggplot2::geom_vline(xintercept = c(-1.96, 1.96), linetype = "dashed", color = "grey50") +
        ggplot2::labs(x = "Geweke z-score", y = NULL, title = "Geweke diagnostic")
    } else {
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
        caterpillar      = ggmcmc::ggs_caterpillar(D, family = NA, ...),
        pairs            = ggmcmc::ggs_pairs(D, family = NA, ...)
      )
    }

    fill_scale <- "manual"
    built <- tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL)
    if (!is.null(built)) {
      fill_vals <- unlist(lapply(built$data, function(df) {
        if ("fill" %in% names(df)) df$fill else NULL
      }), use.names = FALSE)
      if (length(fill_vals)) {
        non_na <- fill_vals[!is.na(fill_vals)]
        if (!length(non_na)) {
          fill_scale <- "none"
        } else if (is.numeric(non_na)) {
          fill_scale <- "continuous"
        } else {
          fill_scale <- "manual"
        }
      }
    }

    p <- p + .plot_theme()
    p <- tryCatch(p + ggplot2::scale_color_manual(values = pal), error = function(e) p)
    if (identical(fill_scale, "manual")) {
      p <- tryCatch(p + ggplot2::scale_fill_manual(values = pal), error = function(e) p)
    } else if (identical(fill_scale, "continuous")) {
      p <- tryCatch(p + ggplot2::scale_fill_viridis_c(option = "C"), error = function(e) p)
    }
    plots[[f]] <- p
  }

  class(plots) <- c("mixgpd_fit_plots", "list")
  .wrap_plotly(plots)
}




#' Posterior predictive summaries from a fitted one-arm model
#'
#' \code{predict.mixgpd_fit()} is the central distributional prediction method
#' for fitted one-arm models.
#'
#' @details
#' The method works with posterior predictive functionals rather than raw model
#' parameters. Supported output types include:
#' \itemize{
#'   \item \code{"density"} for \eqn{f(y \mid x, \mathcal{D})},
#'   \item \code{"survival"} for \eqn{S(y \mid x, \mathcal{D}) = 1 - F(y \mid x, \mathcal{D})},
#'   \item \code{"quantile"} for \eqn{Q(\tau \mid x, \mathcal{D})},
#'   \item \code{"mean"} for \eqn{E(Y \mid x, \mathcal{D})},
#'   \item \code{"rmean"} for \eqn{E\{\min(Y, c) \mid x, \mathcal{D}\}},
#'   \item \code{"sample"} and \code{"fit"} for draw-level predictive output.
#' }
#'
#' For spliced models these predictions integrate over both the DPM bulk and
#' the GPD tail using component-specific tail parameters, including link-mode
#' tail coefficients when present. When \code{type = "mean"}, the function
#' averages conditional means over posterior draws, so the result is a Bayesian
#' predictive mean.
#'
#' @details
#' The \code{type="mean"} option computes the posterior predictive mean by:
#' \enumerate{
#'   \item For each posterior draw s, computing E(Y | x, theta_s) via Monte Carlo simulation
#'   \item Averaging these conditional means over the posterior: E(Y | x, data) = mean_s(E(Y | x, theta_s))
#' }
#' This accounts for parameter uncertainty and is the Bayesian predictive mean. When the tail
#' shape parameter xi >= 1 (heavy tail), the mean is undefined and the function returns Inf
#' with a warning suggesting alternatives like median or restricted mean.
#'
#' @param object A fitted object of class \code{"mixgpd_fit"}.
#' @param x Optional new data. Alias for \code{newdata}.
#' @param newdata Optional new data. If \code{NULL}, uses training design (if stored).
#' @param id Optional identifier for prediction rows. Provide either a column name
#'   in \code{x}/\code{newdata} or a vector of length \code{nrow(x)}. The id column
#'   is excluded from analysis.
#' @param ps Optional numeric vector of propensity scores for conditional prediction.
#'   Used when the model was fit with propensity score augmentation.
#' @param type Prediction type:
#'   \itemize{
#'     \item \code{"density"}: Posterior predictive density f(y | x, data)
#'     \item \code{"survival"}: Posterior predictive survival S(y | x, data) = 1 - F(y | x, data)
#'     \item \code{"quantile"}: Posterior predictive quantiles Q(p | x, data)
#'     \item \code{"sample"}: Posterior predictive samples Y^rep ~ f(y | x, data)
#'     \item \code{"mean"}: Posterior predictive mean E(Y | x, data) (averaged over posterior parameter uncertainty)
#'     \item \code{"rmean"}: Posterior predictive restricted mean \eqn{E[\min(Y, cutoff) \mid x, data]}
#'     \item \code{"median"}: Posterior predictive median (quantile at p=0.5)
#'     \item \code{"fit"}: Per-observation posterior predictive draws
#'   }
#'   Note: \code{type="mean"} returns the posterior predictive mean, which integrates over
#'   parameter uncertainty. This differs from the mean of a single model distribution.
#' @param p Numeric vector of probabilities for quantiles (required for \code{type="quantile"}).
#' @param index Alias for \code{p}; numeric vector of quantile levels.
#' @param y Numeric vector of evaluation points (required for \code{type="density"} or \code{"survival"}).
#' @param level Credible level for credible intervals (default 0.95 for 95 percent intervals).
#' @param interval Character or NULL; type of credible interval: \code{NULL} for no interval,
#'   \code{"credible"} for equal-tailed quantile intervals (default), or \code{"hpd"} for
#'   highest posterior density intervals.
#' @param probs Quantiles for credible interval bands.
#' @param nsim Number of posterior predictive samples (for \code{type="sample"}).
#' @param store_draws Logical; whether to store all posterior draws (for \code{type="sample"}).
#' @param nsim_mean Number of posterior predictive samples to use for posterior mean estimation (for \code{type="mean"}).
#' @param cutoff Finite numeric cutoff for \code{type="rmean"} (restricted mean).
#' @param ncores Number of CPU cores to use for parallel prediction (if supported).
#' @param show_progress Logical; if TRUE, print step messages and render progress where supported.
#' @param ndraws_pred Optional integer subsample of posterior draws for prediction speed.
#'   If NULL and \code{nrow(newdata) > 20000}, defaults to 200.
#' @param chunk_size Optional row chunk size for large \code{newdata} prediction.
#'   If NULL and \code{nrow(newdata) > 20000}, defaults to 10000.
#' @param parallel Logical; if TRUE, enable parallel prediction (alias for setting \code{ncores > 1}).
#' @param workers Optional integer worker count (alias for \code{ncores}).
#' @param ... Unused.
#' @return A list with elements:
#'   \itemize{
#'     \item \code{fit}: numeric vector/matrix for \code{type = "sample"}, otherwise a data frame with
#'       \code{estimate}/\code{lower}/\code{upper} columns (posterior means over draws) plus any index
#'       columns (e.g. \code{id}, \code{y}, \code{index}).
#'     \item \code{lower}, \code{upper}: reserved for backward compatibility (typically \code{NULL}).
#'     \item \code{type}, \code{grid}: metadata.
#'   }
#' @seealso \code{\link{summary.mixgpd_fit}}, \code{\link{fitted.mixgpd_fit}},
#'   \code{\link{residuals.mixgpd_fit}}, \code{\link{predict.causalmixgpd_causal_fit}}.
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
#' # Restricted mean (finite under heavy tails)
#' pr_rmean <- predict(fit, type = "rmean", cutoff = 10, interval = "credible")
#' }
#' @export
predict.mixgpd_fit <- function(object,
                               x = NULL,
                               y = NULL,
                               ps = NULL,
                               id = NULL,
                               newdata = NULL,
                               type = c("density", "survival",
                                        "quantile", "sample", "mean", "rmean", "median", "fit"),
                               p = NULL,
                               index = NULL,
                               nsim = NULL,
                               level = 0.95,
                               interval = "credible",
                               probs = c(0.025, 0.5, 0.975),
                               store_draws = TRUE,
                               nsim_mean = 200L,
                               cutoff = NULL,
                               ncores = 1L,
                               show_progress = TRUE,
                               ndraws_pred = NULL,
                               chunk_size = NULL,
                               parallel = FALSE,
                               workers = NULL,
                               ...) {
  .validate_fit(object)
  dots <- list(...)

  type <- match.arg(type)

  # Handle interval: NULL means no interval, otherwise match to credible/hpd
  if (is.character(interval) && length(interval) == 1L && identical(tolower(interval), "none")) {
    interval <- NULL
  }
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

  if ("cred.level" %in% names(dots)) {
    stop("'cred.level' is no longer supported; use 'level' instead.", call. = FALSE)
  }

  # Construct probs from level for non-sample types
  if (type != "sample") {
    if (!is.numeric(level) || length(level) != 1 || !is.finite(level) || level <= 0 || level >= 1) {
      stop("'level' must be a numeric value between 0 and 1.", call. = FALSE)
    }
    probs <- c((1 - level) / 2, 0.5, (1 + level) / 2)
  }

  if (!is.null(workers)) {
    ncores <- workers
  } else if (isTRUE(parallel) && isTRUE(is.null(workers))) {
    ncores <- max(2L, as.integer(ncores))
  }
  ncores <- as.integer(ncores)
  if (is.na(ncores) || ncores < 1L) stop("'ncores' must be an integer >= 1.", call. = FALSE)

  .predict_mixgpd(object,
                  x = x,
                  y = y,
                  ps = ps,
                  id = id,
                  type = type,
                  p = p,
                  index = index,
                  nsim = nsim,
                  level = level,
                  interval = interval,
                  probs = probs,
                  store_draws = store_draws,
                  nsim_mean = nsim_mean,
                  cutoff = cutoff,
                  show_progress = show_progress,
                  ndraws_pred = ndraws_pred,
                  chunk_size = chunk_size,
                  ncores = ncores)
}


#' Fitted values on the training design
#'
#' \code{fitted.mixgpd_fit()} is a thin training-data wrapper around
#' \code{\link{predict.mixgpd_fit}} for conditional models.
#'
#' @details
#' The method returns posterior predictive fitted values on the observed design
#' matrix. It is available only when the fitted model stored covariates.
#'
#' @param object A fitted object of class \code{"mixgpd_fit"} (must have covariates).
#' @param type Which fitted functional to return: mean, median, or quantile.
#' @param p Quantile level used when \code{type = "quantile"}.
#' @param level Credible level for confidence intervals (default 0.95 for 95 percent credible intervals).
#' @param interval Character or NULL; type of credible interval: \code{NULL} for no interval,
#'   \code{"credible"} for equal-tailed quantile intervals (default), or \code{"hpd"} for
#'   highest posterior density intervals.
#' @param seed Random seed used for deterministic fitted values.
#' @param ... Unused.
#' @return A data frame with columns for fitted values, optional intervals, and
#'   residuals computed on the training sample.
#' @seealso \code{\link{predict.mixgpd_fit}}, \code{\link{residuals.mixgpd_fit}},
#'   \code{\link{plot.mixgpd_fitted}}.
#' @examples
#' \dontrun{
#' # Conditional model (with covariates X)
#' y <- abs(stats::rnorm(50)) + 0.1
#' X <- data.frame(x1 = stats::rnorm(50), x2 = stats::runif(50))
#' bundle <- build_nimble_bundle(y = y, X = X, backend = "sb", kernel = "normal",
#'                              GPD = TRUE, components = 6,
#'                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
#' fit <- run_mcmc_bundle_manual(bundle)
#' fitted(fit)
#' fitted(fit, level = 0.90)
#' fitted(fit, interval = "hpd")  # HPD intervals
#' fitted(fit, interval = NULL)   # No intervals
#' }
#' @export
fitted.mixgpd_fit <- function(object, type = c("mean", "median", "quantile"),
                              p = 0.5, level = 0.95,
                              interval = "credible",
                              seed = 1, ...) {

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
  if (is.null(X)) stop("fitted() is not supported for unconditional models (no covariates). Use predict() for predictions.", call. = FALSE)

  if (type == "quantile") {
    pred <- predict(object, x = X, type = "quantile",
                    index = p, level = level, interval = interval)
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
                    level = level, interval = interval)
    fit_df <- pred$fit
    if ("id" %in% names(fit_df)) fit_df <- fit_df[order(fit_df$id), , drop = FALSE]
    fit_vals <- fit_df$estimate
    lower_vals <- fit_df$lower
    upper_vals <- fit_df$upper
  } else {
    pred <- predict(object, type = type,
                    level = level, interval = interval)
    fit_df <- pred$fit
    fit_vals <- rep(fit_df$estimate[1], length(y))
    lower_vals <- rep(fit_df$lower[1], length(y))
    upper_vals <- rep(fit_df$upper[1], length(y))
  }

  result <- data.frame(fit = fit_vals,
                       lower = lower_vals,
                       upper = upper_vals,
                       residuals = y - fit_vals)
  class(result) <- c("mixgpd_fitted", "data.frame")
  attr(result, "object") <- object
  attr(result, "level") <- level
  attr(result, "interval") <- interval
  return(result)
}

#' Residual diagnostics on the training design
#'
#' \code{residuals.mixgpd_fit()} computes residual diagnostics for conditional
#' fits on the original training data.
#'
#' @details
#' Raw residuals are based on posterior predictive fitted means or medians.
#' PIT residuals instead assess calibration through the posterior predictive CDF.
#' The plug-in PIT uses a posterior mean CDF, while the Bayesian PIT variants
#' work draw by draw.
#'
#' This method is not available for unconditional models because no training
#' design matrix is stored for observation-specific fitted values.
#'
#' @param object A fitted object of class \code{"mixgpd_fit"} (must have covariates).
#' @param type Residual type: \code{"raw"} or \code{"pit"}.
#' @param fitted_type For \code{type = "raw"}, use fitted means or medians.
#' @param pit PIT mode for \code{type = "pit"}:
#'   \itemize{
#'     \item \code{"plugin"}: plug-in PIT using the posterior mean CDF.
#'     \item \code{"bayes_mean"}: Bayesian PIT using draw-wise CDFs averaged over draws.
#'     \item \code{"bayes_draw"}: Bayesian PIT using a single draw-wise CDF per observation.
#'   }
#'   Bayesian PIT modes drop invalid posterior draws using the same validation
#'   rules as prediction and attach diagnostics via \code{attr(res, "pit_diagnostics")}.
#' @param pit_seed Optional integer seed for reproducible \code{bayes_draw} sampling.
#' @param ... Unused.
#' @examples
#' \dontrun{
#' y <- abs(stats::rnorm(50)) + 0.1
#' X <- data.frame(x1 = stats::rnorm(50), x2 = stats::runif(50))
#' bundle <- build_nimble_bundle(y = y, X = X, backend = "sb", kernel = "lognormal",
#'                              GPD = FALSE, components = 4,
#'                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
#' fit <- run_mcmc_bundle_manual(bundle)
#' pit_plugin <- residuals(fit, type = "pit", pit = "plugin")
#' pit_bayes_mean <- residuals(fit, type = "pit", pit = "bayes_mean", pit_seed = 1L)
#' pit_bayes_draw <- residuals(fit, type = "pit", pit = "bayes_draw", pit_seed = 1L)
#' attr(pit_bayes_draw, "pit_diagnostics")
#' }
#' @return Numeric vector of residuals with length equal to the training sample
#'   size. PIT variants attach diagnostic metadata as attributes.
#' @seealso \code{\link{fitted.mixgpd_fit}}, \code{\link{predict.mixgpd_fit}},
#'   \code{\link{plot.mixgpd_fitted}}.
#' @export
residuals.mixgpd_fit <- function(object,
                                 type = c("raw", "pit"),
                                 fitted_type = c("mean", "median"),
                                 pit = c("plugin", "bayes_mean", "bayes_draw"),
                                 pit_seed = NULL,
                                 ...) {

  type <- match.arg(type)
  y <- object$data$y %||% object$y
  X <- object$data$X %||% object$X %||% NULL
  if (is.null(y)) stop("Could not extract y from fitted object.", call. = FALSE)
  if (is.null(X)) stop("residuals() is not supported for unconditional models (no covariates). Use predict() for predictions.", call. = FALSE)

  y <- as.numeric(y)
  X <- as.matrix(X)

  if (type == "raw") {
    fitted_type <- match.arg(fitted_type)
    fit_vals <- fitted(object, type = fitted_type)
    return(as.numeric(fit_vals$residuals))
  }

  pit <- match.arg(pit)

  # -----------------------------
  # plugin PIT: posterior-mean CDF evaluated at y_i (exact, no nearest-grid)
  # -----------------------------
  if (pit == "plugin") {
    pr_surv <- predict(object,
                       x = X,
                       y = y,
                       type = "survival",
                       interval = NULL,
                       store_draws = FALSE,
                       ncores = 1L)

    fit_df <- pr_surv$fit
    surv_col <- if ("survival" %in% names(fit_df)) "survival" else "estimate"

    if (!("id" %in% names(fit_df))) {
      cdfv <- 1 - as.numeric(fit_df[[surv_col]])
      cdfv <- pmin(pmax(cdfv, 0), 1)
      attr(cdfv, "pit_type") <- "plugin"
      return(cdfv)
    }

    n <- length(y)
    ord <- order(fit_df$id)
    surv_vec <- as.numeric(fit_df[[surv_col]][ord])
    surv_mat <- matrix(surv_vec, nrow = n, byrow = TRUE)
    surv_diag <- diag(surv_mat)

    cdfv <- 1 - surv_diag
    cdfv <- pmin(pmax(cdfv, 0), 1)
    attr(cdfv, "pit_type") <- "plugin"
    return(cdfv)
  }

  # -----------------------------
  # Bayesian PIT: use draw-wise CDF F_s(y_i | x_i)
  # - bayes_mean: average over draws
  # - bayes_draw: randomly select one draw per i
  # -----------------------------
  if (!is.null(pit_seed)) set.seed(as.integer(pit_seed))

  spec <- object$spec %||% list()
  meta <- spec$meta %||% list()

  backend <- meta$backend %||% spec$dispatch$backend %||% "<unknown>"
  kernel  <- meta$kernel  %||% spec$kernel$key %||% "<unknown>"
  GPD     <- isTRUE(meta$GPD %||% spec$dispatch$GPD)
  is_spliced <- identical(backend, "spliced")

  pred_backend <- if (backend %in% c("crp", "spliced")) "sb" else backend

  # dispatch functions
  fns <- .get_dispatch(object, backend_override = pred_backend)
  p_fun <- fns$p
  bulk_params <- fns$bulk_params
  kdef <- get_kernel_registry()[[kernel]] %||% list()
  bulk_support <- kdef$bulk_support %||% list()

  draw_mat <- .extract_draws_matrix(object)
  if (is.null(draw_mat) || !is.matrix(draw_mat) || nrow(draw_mat) < 2L) {
    stop("Posterior draws not found or malformed in fitted object.", call. = FALSE)
  }
  S <- nrow(draw_mat)
  n <- length(y)

  # weights + bulk params
  W_draws <- .extract_weights(draw_mat, backend = pred_backend)
  bulk_draws <- .extract_bulk_params(draw_mat, bulk_params = bulk_params)
  base_params <- names(bulk_draws)
  link_plan <- spec$dispatch$link_params %||% meta$link_params %||% list()
  if (!length(link_plan)) {
    bulk_plan <- spec$plan$bulk %||% list()
    for (nm in names(bulk_plan)) {
      ent <- bulk_plan[[nm]] %||% list()
      if (identical(ent$mode %||% "constant", "link")) {
        link_plan[[nm]] <- list(
          mode = "link",
          link = ent$link %||% "identity",
          link_power = ent$link_power %||% NULL
        )
      }
    }
  }
  link_params <- names(link_plan)

  # gpd pieces
  tail_shape <- NULL
  threshold_mat <- NULL
  threshold_scalar <- NULL
  tail_scale <- NULL
  spliced_gpd_draws <- list()
  spliced_gpd_link <- list()

  P <- ncol(X)

  .apply_link <- function(eta, link, link_power = NULL) {
    link <- as.character(link %||% "identity")
    if (link == "identity") return(eta)
    if (link == "exp") return(exp(eta))
    if (link == "log") return(log(eta))
    if (link == "softplus") return(log1p(exp(eta)))
    if (link == "power") {
      if (is.null(link_power) || length(link_power) != 1L || !is.finite(as.numeric(link_power))) {
        stop("power link requires numeric link_power.", call. = FALSE)
      }
      pw <- as.numeric(link_power)
      return(eta ^ pw)
    }
    stop(sprintf("Unsupported link '%s'.", link), call. = FALSE)
  }

  .compute_link_eta <- function(s) {
    if (!length(link_params)) return(list())
    out <- list()
    for (nm in link_params) {
      plan <- link_plan[[nm]] %||% list()
      mode <- plan$mode %||% "constant"
      if (identical(mode, "link")) {
        link <- plan$link %||% "identity"
        pw   <- plan$link_power %||% NULL

        beta_cols <- grep(paste0("^beta_", nm, "\\[[0-9]+,\\s*[0-9]+\\]$"), colnames(draw_mat), value = TRUE)
        if (length(beta_cols)) {
          idx1 <- as.integer(sub(paste0("^beta_", nm, "\\[([0-9]+),\\s*([0-9]+)\\]$"), "\\1", beta_cols))
          idx2 <- as.integer(sub(paste0("^beta_", nm, "\\[([0-9]+),\\s*([0-9]+)\\]$"), "\\2", beta_cols))
          Kb <- max(idx1, na.rm = TRUE)
          Pb <- max(idx2, na.rm = TRUE)
          beta_mat <- matrix(NA_real_, nrow = Kb, ncol = Pb)
          for (j in seq_along(beta_cols)) {
            beta_mat[idx1[j], idx2[j]] <- draw_mat[s, beta_cols[j]]
          }
          eta_mat <- X %*% t(beta_mat)
          out[[nm]] <- .apply_link(eta_mat, link, pw)
        } else {
          beta_cols_1d <- grep(paste0("^beta_", nm, "\\[[0-9]+\\]$"), colnames(draw_mat), value = TRUE)
          if (length(beta_cols_1d)) {
            idx <- as.integer(sub(paste0("^beta_", nm, "\\[([0-9]+)\\]$"), "\\1", beta_cols_1d))
            ord <- order(idx)
            beta_cols_1d <- beta_cols_1d[ord]
            Kb <- ncol(W_draws)
            Pb <- P
            if (length(beta_cols_1d) == Kb * Pb) {
              beta_vec <- as.numeric(draw_mat[s, beta_cols_1d])
              beta_mat <- matrix(beta_vec, nrow = Kb, ncol = Pb)
              eta_mat <- X %*% t(beta_mat)
              out[[nm]] <- .apply_link(eta_mat, link, pw)
            } else {
              beta_nm <- .indexed_block(draw_mat, paste0("beta_", nm), K = P)
              eta <- as.numeric(X %*% beta_nm[s, ])
              out[[nm]] <- matrix(as.numeric(.apply_link(eta, link, pw)), nrow = n)
            }
          } else {
            beta_nm <- .indexed_block(draw_mat, paste0("beta_", nm), K = P)
            eta <- as.numeric(X %*% beta_nm[s, ])
            out[[nm]] <- matrix(as.numeric(.apply_link(eta, link, pw)), nrow = n)
          }
        }
      } else {
        if (!(nm %in% colnames(draw_mat))) stop(sprintf("'%s' not found in posterior draws.", nm), call. = FALSE)
        out[[nm]] <- matrix(rep(as.numeric(draw_mat[s, nm]), n), nrow = n)
      }
    }
    out
  }

  if (GPD) {
    gpd_plan <- spec$dispatch$gpd %||% meta$gpd %||% list()

    if (is_spliced) {
      K_sp <- ncol(W_draws)
      for (nm in c("threshold", "tail_scale", "tail_shape")) {
        ent <- gpd_plan[[nm]] %||% list(mode = "dist")
        mode <- ent$mode %||% "dist"
        if (identical(mode, "link")) {
          beta_arr <- .indexed_block_matrix(draw_mat, paste0("beta_", nm), K = K_sp, P = P, allow_missing = TRUE)
          if (is.null(beta_arr)) stop(sprintf("beta_%s not found in posterior draws.", nm), call. = FALSE)
          spliced_gpd_link[[nm]] <- list(
            beta = beta_arr,
            link = ent$link %||% if (identical(nm, "tail_shape")) "identity" else "exp",
            link_power = ent$link_power %||% NULL
          )
        } else {
          vals <- .indexed_block(draw_mat, nm, K = K_sp, allow_missing = TRUE)
          if (is.null(vals) && identical(mode, "fixed") && !is.null(ent$value)) {
            vals <- matrix(rep(as.numeric(ent$value), S * K_sp), nrow = S, ncol = K_sp)
          }
          if (is.null(vals)) stop(sprintf("%s not found in posterior draws.", nm), call. = FALSE)
          spliced_gpd_draws[[nm]] <- vals
        }
      }
    } else {
      thr_mode <- gpd_plan$threshold$mode %||% "constant"
      if (identical(thr_mode, "link")) {
        beta_thr <- .indexed_block(draw_mat, "beta_threshold", K = P)
        threshold_mat <- matrix(NA_real_, nrow = S, ncol = n)
        thr_link <- gpd_plan$threshold$link %||% "exp"
        thr_power <- gpd_plan$threshold$link_power %||% NULL
        for (s in seq_len(S)) {
          eta <- as.numeric(X %*% beta_thr[s, ])
          threshold_mat[s, ] <- as.numeric(.apply_link(eta, thr_link, thr_power))
        }
      } else {
        thr_cols <- grep("^threshold(\\b|_)", colnames(draw_mat), value = TRUE)
        if (length(thr_cols) == 0L && "threshold" %in% colnames(draw_mat)) thr_cols <- "threshold"
        if (length(thr_cols) == 0L) stop("threshold not found in posterior draws.", call. = FALSE)
        if (length(thr_cols) == 1L) {
          threshold_scalar <- as.numeric(draw_mat[, thr_cols])
        } else {
          threshold_scalar <- rowMeans(draw_mat[, thr_cols, drop = FALSE], na.rm = TRUE)
        }
      }

      has_beta_ts <- any(grepl("^beta_tail_scale\\[", colnames(draw_mat)))
      ts_mode <- gpd_plan$tail_scale$mode %||% if (has_beta_ts) "link" else "constant"
      if (identical(ts_mode, "link")) {
        beta_ts <- .indexed_block(draw_mat, "beta_tail_scale", K = P)
        tail_scale <- matrix(NA_real_, nrow = S, ncol = n)
        ts_link <- gpd_plan$tail_scale$link %||% "exp"
        ts_power <- gpd_plan$tail_scale$link_power %||% NULL
        for (s in seq_len(S)) {
          eta <- as.numeric(X %*% beta_ts[s, ])
          tail_scale[s, ] <- as.numeric(.apply_link(eta, ts_link, ts_power))
        }
      } else if ("tail_scale" %in% colnames(draw_mat)) {
        tail_scale <- as.numeric(draw_mat[, "tail_scale"])
      } else if (!is.null(gpd_plan$tail_scale$value)) {
        tail_scale <- rep(as.numeric(gpd_plan$tail_scale$value), S)
      } else {
        stop("tail_scale not found in posterior draws.", call. = FALSE)
      }

      has_beta_tsh <- any(grepl("^beta_tail_shape\\[", colnames(draw_mat)))
      tsh_mode <- gpd_plan$tail_shape$mode %||% if (has_beta_tsh) "link" else "constant"
      if (identical(tsh_mode, "link")) {
        beta_tsh <- .indexed_block(draw_mat, "beta_tail_shape", K = P)
        tail_shape <- matrix(NA_real_, nrow = S, ncol = n)
        tsh_link <- gpd_plan$tail_shape$link %||% "identity"
        tsh_power <- gpd_plan$tail_shape$link_power %||% NULL
        for (s in seq_len(S)) {
          eta <- as.numeric(X %*% beta_tsh[s, ])
          tail_shape[s, ] <- as.numeric(.apply_link(eta, tsh_link, tsh_power))
        }
      } else if ("tail_shape" %in% colnames(draw_mat)) {
        tail_shape <- as.numeric(draw_mat[, "tail_shape"])
      } else if (!is.null(gpd_plan$tail_shape$value)) {
        tail_shape <- rep(as.numeric(gpd_plan$tail_shape$value), S)
      } else {
        stop("tail_shape not found in posterior draws.", call. = FALSE)
      }
    }
  }

  .threshold_at <- function(s, i) {
    if (!is.null(threshold_mat)) return(threshold_mat[s, i])
    threshold_scalar[s]
  }

  .tail_scale_at <- function(s, i) {
    if (is.matrix(tail_scale)) return(tail_scale[s, i])
    tail_scale[s]
  }

  .tail_shape_at <- function(s, i) {
    if (is.matrix(tail_shape)) return(tail_shape[s, i])
    tail_shape[s]
  }

  .support_ok <- function(nm, v) {
    sup <- as.character(bulk_support[[nm]] %||% "")
    if (sup %in% c("positive_sd", "positive_scale", "positive_shape", "positive_location")) {
      return(all(is.finite(v) & (v > 0)))
    }
    all(is.finite(v))
  }

  .build_args0_or_null <- function(s) {
    w_s <- as.numeric(W_draws[s, ])
    if (!all(is.finite(w_s))) return(NULL)

    args0 <- if (pred_backend == "sb") list(w = w_s) else list()

    for (nm in base_params) {
      v <- as.numeric(bulk_draws[[nm]][s, ])
      if (!.support_ok(nm, v)) return(NULL)
      args0[[nm]] <- v
    }

    if (GPD && !is_spliced && !is.matrix(tail_shape)) {
      xi <- as.numeric(tail_shape[s])
      if (!is.finite(xi)) return(NULL)
      args0$tail_shape <- xi
    }
    args0
  }

  .draw_valid <- logical(S)
  for (s in seq_len(S)) {
    ok <- !is.null(.build_args0_or_null(s))
    if (ok && GPD) {
      if (is_spliced) {
        for (nm in names(spliced_gpd_draws)) {
          vals <- as.numeric(spliced_gpd_draws[[nm]][s, ])
          if (identical(nm, "tail_scale")) {
            ok <- all(is.finite(vals) & (vals > 0))
          } else {
            ok <- all(is.finite(vals))
          }
          if (!ok) break
        }
      } else {
        if (!is.null(threshold_mat)) {
          if (!all(is.finite(threshold_mat[s, ]))) ok <- FALSE
        } else {
          if (!is.finite(threshold_scalar[s])) ok <- FALSE
        }
        if (ok) {
          if (is.matrix(tail_scale)) {
            ok <- all(is.finite(tail_scale[s, ]) & (tail_scale[s, ] > 0))
          } else {
            ok <- is.finite(tail_scale[s]) && (tail_scale[s] > 0)
          }
        }
        if (ok) {
          if (is.matrix(tail_shape)) {
            ok <- all(is.finite(tail_shape[s, ]))
          } else {
            ok <- is.finite(tail_shape[s])
          }
        }
      }
    }
    .draw_valid[s] <- ok
  }

  spliced_scalar <- if (is_spliced && GPD) .get_dispatch_scalar(object, backend_override = "crp") else NULL

  .row_component_value <- function(obj, i, k) {
    if (is.null(obj)) return(NA_real_)
    if (is.null(dim(obj))) return(as.numeric(obj[1L]))
    if (length(dim(obj)) != 2L) stop("Expected a scalar or matrix component block.", call. = FALSE)
    j <- if (ncol(obj) >= k && ncol(obj) > 1L) k else 1L
    as.numeric(obj[i, j])
  }

  .compute_spliced_gpd_eta <- function(s) {
    if (!length(spliced_gpd_link)) return(list())
    out <- list()
    for (nm in names(spliced_gpd_link)) {
      ent <- spliced_gpd_link[[nm]]
      beta_mat <- ent$beta[s, , , drop = TRUE]
      if (is.null(dim(beta_mat))) beta_mat <- matrix(beta_mat, nrow = ncol(W_draws), ncol = P)
      eta_mat <- X %*% t(beta_mat)
      out[[nm]] <- .apply_link(eta_mat, ent$link, ent$link_power)
    }
    out
  }

  .spliced_gpd_value_at <- function(nm, s, i, k, eta_cache) {
    if (!is.null(eta_cache[[nm]])) return(.row_component_value(eta_cache[[nm]], i, k))
    vals <- spliced_gpd_draws[[nm]]
    if (is.null(vals)) return(NA_real_)
    as.numeric(vals[s, k])
  }

  .spliced_component_args_or_null <- function(s, i, k, link_eta, gpd_eta) {
    args <- list()
    for (nm in bulk_params) {
      if (nm %in% link_params) {
        vv <- .row_component_value(link_eta[[nm]], i, k)
      } else if (nm %in% base_params) {
        vv <- as.numeric(bulk_draws[[nm]][s, k])
      } else {
        next
      }
      if (!.support_ok(nm, vv)) return(NULL)
      args[[nm]] <- as.numeric(vv)
    }

    args$threshold <- .spliced_gpd_value_at("threshold", s, i, k, gpd_eta)
    args$tail_scale <- .spliced_gpd_value_at("tail_scale", s, i, k, gpd_eta)
    args$tail_shape <- .spliced_gpd_value_at("tail_shape", s, i, k, gpd_eta)
    if (!is.finite(args$threshold) || !is.finite(args$tail_scale) || args$tail_scale <= 0 || !is.finite(args$tail_shape)) {
      return(NULL)
    }
    args
  }

  .spliced_component_args_list_or_null <- function(s, i, link_eta, gpd_eta) {
    K <- ncol(W_draws)
    out <- vector("list", K)
    for (k in seq_len(K)) {
      out[[k]] <- .spliced_component_args_or_null(s, i, k, link_eta = link_eta, gpd_eta = gpd_eta)
      if (is.null(out[[k]])) return(NULL)
    }
    out
  }

  .normalize_weights_or_null <- function(w) {
    w <- as.numeric(w)
    if (!all(is.finite(w))) return(NULL)
    w[w < 0] <- 0
    sw <- sum(w)
    if (!is.finite(sw) || sw <= 0) return(NULL)
    w / sw
  }

  .spliced_cdf_one <- function(s, i, yval, link_eta, gpd_eta) {
    w_s <- .normalize_weights_or_null(W_draws[s, ])
    if (is.null(w_s)) return(NA_real_)
    comp_args <- .spliced_component_args_list_or_null(s, i, link_eta = link_eta, gpd_eta = gpd_eta)
    if (is.null(comp_args)) return(NA_real_)

    vals <- vapply(seq_along(comp_args), function(k) {
      cdfv <- as.numeric(do.call(spliced_scalar$p, c(list(q = yval, lower.tail = 1L, log.p = 0L), comp_args[[k]])))[1]
      pmin(pmax(cdfv, 0), 1)
    }, numeric(1))
    sum(w_s * vals)
  }

  # compute draw-wise CDF matrix: S x n
  cdf_draws <- matrix(NA_real_, nrow = S, ncol = n)

  for (s in seq_len(S)) {
    if (!.draw_valid[s]) next
    args0 <- .build_args0_or_null(s)
    if (is.null(args0)) next
    link_eta <- .compute_link_eta(s)
    gpd_eta <- if (is_spliced && GPD) .compute_spliced_gpd_eta(s) else list()

    for (i in seq_len(n)) {
      if (is_spliced && GPD) {
        cdf_draws[s, i] <- .spliced_cdf_one(s, i, y[i], link_eta = link_eta, gpd_eta = gpd_eta)
        next
      }

      args <- args0
      if (GPD) {
        args$threshold <- .threshold_at(s, i)
        args$tail_scale <- .tail_scale_at(s, i)
        args$tail_shape <- .tail_shape_at(s, i)
        if (!is.finite(args$threshold) || !is.finite(args$tail_scale) || args$tail_scale <= 0 || !is.finite(args$tail_shape)) next
      }
      if (length(link_params)) {
        bad <- FALSE
        for (nm in link_params) {
          vv <- as.numeric(link_eta[[nm]][i, ])
          if (!all(is.finite(vv))) {
            bad <- TRUE
            break
          }
          args[[nm]] <- vv
        }
        if (bad) next
      }
      cdfv <- as.numeric(do.call(p_fun, c(list(q = y[i], lower.tail = 1L, log.p = 0L), args)))
      cdf_draws[s, i] <- cdfv[1]
    }
  }

  cdf_draws <- pmin(pmax(cdf_draws, 0), 1)

  n_used <- colSums(is.finite(cdf_draws))

  if (pit == "bayes_mean") {
    u <- colMeans(cdf_draws, na.rm = TRUE)
    u <- pmin(pmax(u, 0), 1)
    attr(u, "pit_type") <- "bayes_mean"
    attr(u, "pit_diagnostics") <- list(
      n_draws_total = S,
      n_draws_valid = sum(.draw_valid),
      n_draws_dropped = S - sum(.draw_valid),
      n_draws_used = n_used
    )
    return(u)
  }

  # bayes_draw
  u <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    ok <- which(is.finite(cdf_draws[, i]))
    if (!length(ok)) next
    s_pick <- sample(ok, size = 1L)
    u[i] <- cdf_draws[s_pick, i]
  }
  u <- pmin(pmax(u, 0), 1)
  attr(u, "pit_type") <- "bayes_draw"
  attr(u, "pit_diagnostics") <- list(
    n_draws_total = S,
    n_draws_valid = sum(.draw_valid),
    n_draws_dropped = S - sum(.draw_valid),
    n_draws_used = n_used
  )
  u
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
         fit = .plot_fit_pred(x, ...),
         mean = .plot_mean_pred(x, ...),
         density = .plot_density_pred(x, ...),
         survival = .plot_survival_pred(x, ...),
         {warning("Unknown prediction type: ", pred_type); NULL})

  if (!is.null(result)) {
    class(result) <- c("mixgpd_predict_plots", class(result))
  }
  .wrap_plotly(result)
}

#' Plot causal prediction outputs
#'
#' S3 method for visualizing causal predictions from \code{predict.causalmixgpd_causal_fit()}.
#' For mean/quantile, plots treated/control and treatment effect versus PS (or index).
#' For \code{type = "sample"}, plots arm-level posterior predictive samples alongside
#' treatment-effect samples. For density/prob, plots treated/control values versus y.
#'
#' @param x Object of class \code{causalmixgpd_causal_predict}.
#' @param y Ignored.
#' @param ... Additional arguments passed to ggplot2 functions.
#' @return A ggplot object or a list of ggplot objects.
#' @export
plot.causalmixgpd_causal_predict <- function(x, y = NULL, ...) {

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

  if (pred_type == "sample") {
    trt <- x$trt %||% attr(x, "trt")
    con <- x$con %||% attr(x, "con")
    if (is.null(trt) || is.null(con)) {
      stop("Causal sample prediction missing treated/control sample objects.", call. = FALSE)
    }

    .sample_matrix <- function(obj) {
      fit <- obj$fit
      if (is.null(dim(fit))) {
        return(matrix(as.numeric(fit), nrow = 1L))
      }
      as.matrix(fit)
    }

    trt_mat <- .sample_matrix(trt)
    con_mat <- .sample_matrix(con)
    eff_mat <- if (is.null(dim(x$fit))) matrix(as.numeric(x$fit), nrow = 1L) else as.matrix(x$fit)

    if (!identical(dim(trt_mat), dim(con_mat)) || !identical(dim(trt_mat), dim(eff_mat))) {
      stop("Causal sample prediction contains incompatible sample dimensions.", call. = FALSE)
    }

    id_vals <- x$id %||% attr(x, "id") %||% seq_len(nrow(eff_mat))
    if (length(id_vals) != nrow(eff_mat)) id_vals <- seq_len(nrow(eff_mat))
    pal <- .plot_palette(8L)

    if (nrow(eff_mat) == 1L) {
      df_tc <- rbind(
        data.frame(value = as.numeric(trt_mat[1, ]), arm = "Treated"),
        data.frame(value = as.numeric(con_mat[1, ]), arm = "Control")
      )
      p_tc <- ggplot2::ggplot(df_tc, ggplot2::aes(x = value, fill = arm, color = arm)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                bins = 30, alpha = 0.35, position = "identity") +
        ggplot2::geom_density(linewidth = 0.9) +
        ggplot2::scale_fill_manual(values = pal[1:2]) +
        ggplot2::scale_color_manual(values = pal[1:2]) +
        .plot_theme() +
        ggplot2::labs(x = "Value", y = "Density", title = "Treated vs Control Samples")

      df_te <- data.frame(value = as.numeric(eff_mat[1, ]))
      p_te <- ggplot2::ggplot(df_te, ggplot2::aes(x = value)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                bins = 30, alpha = 0.7, fill = pal[5], color = pal[7]) +
        ggplot2::geom_density(color = pal[7], linewidth = 0.9) +
        .plot_theme() +
        ggplot2::labs(x = "Treatment effect", y = "Density", title = "Treatment-Effect Samples")
    } else {
      df_tc <- rbind(
        data.frame(id = rep(id_vals, each = ncol(trt_mat)), arm = "Treated", value = as.vector(t(trt_mat))),
        data.frame(id = rep(id_vals, each = ncol(con_mat)), arm = "Control", value = as.vector(t(con_mat)))
      )
      p_tc <- ggplot2::ggplot(df_tc, ggplot2::aes(x = factor(id), y = value, fill = arm)) +
        ggplot2::geom_boxplot(position = ggplot2::position_dodge(width = 0.75), outlier.alpha = 0.2) +
        ggplot2::scale_fill_manual(values = pal[1:2]) +
        .plot_theme() +
        ggplot2::labs(x = "Prediction row", y = "Value", title = "Treated vs Control Samples")

      df_te <- data.frame(id = rep(id_vals, each = ncol(eff_mat)), value = as.vector(t(eff_mat)))
      p_te <- ggplot2::ggplot(df_te, ggplot2::aes(x = factor(id), y = value)) +
        ggplot2::geom_boxplot(fill = pal[5], color = pal[7], outlier.alpha = 0.2) +
        .plot_theme() +
        ggplot2::labs(x = "Prediction row", y = "Treatment effect", title = "Treatment-Effect Samples")
    }

    result <- list(trt_control = p_tc, treatment_effect = p_te)
    class(result) <- c("causalmixgpd_causal_predict_plots", "list")
    return(.wrap_plotly(result))
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
    class(result) <- c("causalmixgpd_causal_predict_plots", "list")
    return(.wrap_plotly(result))
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

    return(.wrap_plotly(p))
  }

  stop("Unsupported causal prediction type for plotting.", call. = FALSE)
}


# S3 methods for QTE/ATE objects -------------------------------------------

.effect_label_qte <- function(type) {
  type_chr <- if (is.null(type) || !length(type)) "qte" else tolower(as.character(type[[1]]))
  if (type_chr == "cqte") {
    return(list(short = "CQTE", long = "Conditional Quantile Treatment Effect"))
  }
  if (type_chr == "qtt") {
    return(list(short = "QTT", long = "Quantile Treatment Effect on the Treated"))
  }
  list(short = "QTE", long = "Quantile Treatment Effect")
}

.effect_label_ate <- function(type, metric = NULL) {
  type_chr <- if (is.null(type) || !length(type)) "ate" else tolower(as.character(type[[1]]))
  metric_chr <- if (is.null(metric) || !length(metric)) "mean" else tolower(as.character(metric[[1]]))
  is_rmean <- identical(metric_chr, "rmean")

  if (type_chr == "cate") {
    if (is_rmean) {
      return(list(short = "RMCATE", long = "Conditional Restricted Mean Treatment Effect"))
    }
    return(list(short = "CATE", long = "Conditional Average Treatment Effect"))
  }
  if (type_chr == "att") {
    if (is_rmean) {
      return(list(short = "RMATT", long = "Restricted Mean Treatment Effect on the Treated"))
    }
    return(list(short = "ATT", long = "Average Treatment Effect on the Treated"))
  }
  if (is_rmean) {
    return(list(short = "RMATE", long = "Restricted Mean Treatment Effect"))
  }
  list(short = "ATE", long = "Average Treatment Effect")
}

.causal_effect_is_conditional <- function(type) {
  type_chr <- if (is.null(type) || !length(type)) "" else tolower(as.character(type[[1]]))
  type_chr %in% c("cate", "cqte")
}

.causal_effect_table_for_display <- function(df, type = NULL) {
  if (!is.data.frame(df)) return(df)
  if (!.causal_effect_is_conditional(type) && "id" %in% names(df)) {
    df <- df[, setdiff(names(df), "id"), drop = FALSE]
  }
  df
}

#' Print a QTE-style effect object
#'
#' \code{print.causalmixgpd_qte()} prints a compact summary for objects produced
#' by \code{\link{qte}}, \code{\link{qtt}}, or \code{\link{cqte}}.
#'
#' @param x A \code{"causalmixgpd_qte"} object from \code{qte()}.
#' @param digits Number of digits to display.
#' @param max_rows Maximum number of estimate rows to display.
#' @param ... Unused.
#' @return The object \code{x}, invisibly.
#' @seealso \code{\link{summary.causalmixgpd_qte}},
#'   \code{\link{plot.causalmixgpd_qte}}, \code{\link{qte}},
#'   \code{\link{cqte}}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' q <- qte(fit, probs = c(0.25, 0.5, 0.75), interval = "credible")
#' print(q)
#' }
#' @export
print.causalmixgpd_qte <- function(x, digits = 3, max_rows = 6, ...) {
  stopifnot(inherits(x, "causalmixgpd_qte"))

  lbl <- .effect_label_qte(x$type %||% "qte")
  probs <- x$probs %||% x$grid %||% numeric(0)
  n_pred <- x$n_pred %||% 1L
  level <- x$level %||% 0.95
  interval <- x$interval %||% "none"
  meta <- x$meta %||% list()
  knitr_kable <- .is_knitr_output() && isTRUE(getOption("causalmixgpd.knitr.kable", FALSE))

  if (knitr_kable) {
    pieces <- list(
      sprintf("%s (%s)", lbl$short, lbl$long),
      sprintf("  Prediction points: %d", n_pred),
      sprintf("  Quantile grid: %s", fmt3_vec(probs))
    )
    has_x <- !is.null(x$x)
    ps_used <- isTRUE(meta$ps_enabled) || (!is.null(x$ps) && any(is.finite(x$ps)))
    pieces <- c(pieces, list(
      sprintf("  Conditional (covariates): %s", if (has_x) "YES" else "NO"),
      sprintf("  Propensity score used: %s", if (ps_used) "YES" else "NO")
    ))
    if (ps_used && !is.null(meta$ps_scale)) {
      pieces <- c(pieces, list(sprintf("  PS scale: %s", meta$ps_scale)))
    }
    if (interval == "credible") {
      pieces <- c(pieces, list(sprintf("  Credible interval: %s (%.0f%%)", interval, level * 100)))
    } else {
      pieces <- c(pieces, list(sprintf("  Credible interval: %s", interval)))
    }
    pieces <- c(pieces, list("", sprintf("%s estimates (treated - control):", lbl$short)))

    qte_fit <- x$qte$fit %||% NULL
    if (!is.null(qte_fit) && is.data.frame(qte_fit)) {
      show_df <- .causal_effect_table_for_display(qte_fit, type = x$type %||% "qte")
      if ("estimate" %in% names(show_df)) names(show_df)[names(show_df) == "estimate"] <- "mean"
      if (nrow(show_df) > max_rows) {
        pieces <- c(pieces, list(.kable_table(format_df3_sci(utils::head(show_df, max_rows), digits = digits), row.names = FALSE)))
        pieces <- c(pieces, list(sprintf("... (%d more rows)", nrow(show_df) - max_rows)))
      } else {
        pieces <- c(pieces, list(.kable_table(format_df3_sci(show_df, digits = digits), row.names = FALSE)))
      }
    } else if (!is.null(x$fit)) {
      fit_mat <- x$fit
      show_n <- min(nrow(fit_mat), max_rows)
      pieces <- c(pieces, list(sprintf("  (matrix: %d x %d)", nrow(fit_mat), ncol(fit_mat))))
      show_df <- as.data.frame(fit_mat[seq_len(show_n), , drop = FALSE])
      pieces <- c(pieces, list(.kable_table(format_df3_sci(show_df, digits = digits), row.names = TRUE)))
      if (nrow(fit_mat) > show_n) {
        pieces <- c(pieces, list(sprintf("... (%d more rows)", nrow(fit_mat) - show_n)))
      }
    }
    return(do.call(.knitr_asis, pieces))
  }
  cat(sprintf("%s (%s)\n", lbl$short, lbl$long))
  cat(sprintf("  Prediction points: %d\n", n_pred))
  cat(sprintf("  Quantile grid: %s\n", fmt3_vec(probs)))

  has_x <- !is.null(x$x)
  ps_used <- isTRUE(meta$ps_enabled) || (!is.null(x$ps) && any(is.finite(x$ps)))
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

  cat(sprintf("\n%s estimates (treated - control):\n", lbl$short))
  qte_fit <- x$qte$fit %||% NULL
  if (!is.null(qte_fit) && is.data.frame(qte_fit)) {
    show_df <- .causal_effect_table_for_display(qte_fit, type = x$type %||% "qte")
    if ("estimate" %in% names(show_df)) names(show_df)[names(show_df) == "estimate"] <- "mean"
    if (nrow(show_df) > max_rows) {
      print_fmt3_sci(utils::head(show_df, max_rows), row.names = FALSE, digits = digits)
      cat(sprintf("... (%d more rows)\n", nrow(show_df) - max_rows))
    } else {
      print_fmt3_sci(show_df, row.names = FALSE, digits = digits)
    }
  } else if (!is.null(x$fit)) {
    # Fallback to raw matrix
    fit_mat <- x$fit
    show_n <- min(nrow(fit_mat), max_rows)
    cat(sprintf("  (matrix: %d x %d)\n", nrow(fit_mat), ncol(fit_mat)))
    print_fmt3_sci(fit_mat[seq_len(show_n), , drop = FALSE], digits = digits)
    if (nrow(fit_mat) > show_n) {
      cat(sprintf("... (%d more rows)\n", nrow(fit_mat) - show_n))
    }
  }

  invisible(x)
}

#' Print an ATE-style effect object
#'
#' \code{print.causalmixgpd_ate()} prints a compact summary for objects
#' produced by \code{\link{ate}}, \code{\link{att}}, \code{\link{cate}}, or
#' \code{\link{ate_rmean}}.
#'
#' @param x A \code{"causalmixgpd_ate"} object from \code{ate()}.
#' @param digits Number of digits to display.
#' @param max_rows Maximum number of estimate rows to display.
#' @param ... Unused.
#' @return The object \code{x}, invisibly.
#' @seealso \code{\link{summary.causalmixgpd_ate}},
#'   \code{\link{plot.causalmixgpd_ate}}, \code{\link{ate}},
#'   \code{\link{cate}}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' a <- ate(fit, interval = "credible")
#' print(a)
#' }
#' @export
print.causalmixgpd_ate <- function(x, digits = 3, max_rows = 6, ...) {
  stopifnot(inherits(x, "causalmixgpd_ate"))

  lbl <- .effect_label_ate(x$type %||% "ate", metric = x$trt$type %||% x$con$type %||% NULL)
  n_pred <- x$n_pred %||% length(x$fit)
  level <- x$level %||% 0.95
  interval <- x$interval %||% "none"
  nsim_mean <- x$nsim_mean %||% NA
  meta <- x$meta %||% list()
  knitr_kable <- .is_knitr_output() && isTRUE(getOption("causalmixgpd.knitr.kable", FALSE))

  if (knitr_kable) {
    pieces <- list(
      sprintf("%s (%s)", lbl$short, lbl$long),
      sprintf("  Prediction points: %d", n_pred)
    )
    has_x <- !is.null(x$x)
    ps_used <- isTRUE(meta$ps_enabled) || (!is.null(x$ps) && any(is.finite(x$ps)))
    pieces <- c(pieces, list(
      sprintf("  Conditional (covariates): %s", if (has_x) "YES" else "NO"),
      sprintf("  Propensity score used: %s", if (ps_used) "YES" else "NO")
    ))
    if (ps_used && !is.null(meta$ps_scale)) {
      pieces <- c(pieces, list(sprintf("  PS scale: %s", meta$ps_scale)))
    }
    if (!is.na(nsim_mean)) {
      pieces <- c(pieces, list(sprintf("  Posterior mean draws: %d", nsim_mean)))
    }
    if (interval == "credible") {
      pieces <- c(pieces, list(sprintf("  Credible interval: %s (%.0f%%)", interval, level * 100)))
    } else {
      pieces <- c(pieces, list(sprintf("  Credible interval: %s", interval)))
    }
    pieces <- c(pieces, list("", sprintf("%s estimates (treated - control):", lbl$short)))

    ate_fit <- x$ate$fit %||% NULL
    if (!is.null(ate_fit) && is.data.frame(ate_fit)) {
      show_df <- .causal_effect_table_for_display(ate_fit, type = x$type %||% "ate")
      if ("estimate" %in% names(show_df)) names(show_df)[names(show_df) == "estimate"] <- "mean"
      if (nrow(show_df) > max_rows) {
        pieces <- c(pieces, list(.kable_table(format_df3_sci(utils::head(show_df, max_rows), digits = digits), row.names = FALSE)))
        pieces <- c(pieces, list(sprintf("... (%d more rows)", nrow(show_df) - max_rows)))
      } else {
        pieces <- c(pieces, list(.kable_table(format_df3_sci(show_df, digits = digits), row.names = FALSE)))
      }
    } else if (!is.null(x$fit)) {
      fit_vec <- x$fit
      show_n <- min(length(fit_vec), max_rows)
      pieces <- c(pieces, list(sprintf("  (vector: %d)", length(fit_vec))))
      show_df <- data.frame(estimate = fit_vec[seq_len(show_n)])
      pieces <- c(pieces, list(.kable_table(format_df3_sci(show_df, digits = digits), row.names = FALSE)))
      if (length(fit_vec) > show_n) {
        pieces <- c(pieces, list(sprintf("... (%d more)", length(fit_vec) - show_n)))
      }
    }
    return(do.call(.knitr_asis, pieces))
  }
  cat(sprintf("%s (%s)\n", lbl$short, lbl$long))
  cat(sprintf("  Prediction points: %d\n", n_pred))

  has_x <- !is.null(x$x)
  ps_used <- isTRUE(meta$ps_enabled) || (!is.null(x$ps) && any(is.finite(x$ps)))
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

  cat(sprintf("\n%s estimates (treated - control):\n", lbl$short))
  ate_fit <- x$ate$fit %||% NULL
  if (!is.null(ate_fit) && is.data.frame(ate_fit)) {
    show_df <- .causal_effect_table_for_display(ate_fit, type = x$type %||% "ate")
    if ("estimate" %in% names(show_df)) names(show_df)[names(show_df) == "estimate"] <- "mean"
    if (nrow(show_df) > max_rows) {
      print_fmt3_sci(utils::head(show_df, max_rows), row.names = FALSE, digits = digits)
      cat(sprintf("... (%d more rows)\n", nrow(show_df) - max_rows))
    } else {
      print_fmt3_sci(show_df, row.names = FALSE, digits = digits)
    }
  } else if (!is.null(x$fit)) {
    # Fallback to raw vector
    fit_vec <- x$fit
    show_n <- min(length(fit_vec), max_rows)
    cat(sprintf("  (vector: %d)\n", length(fit_vec)))
    print_fmt3_sci(fit_vec[seq_len(show_n)], digits = digits)
    if (length(fit_vec) > show_n) {
      cat(sprintf("... (%d more)\n", length(fit_vec) - show_n))
    }
  }

  invisible(x)
}

.summary_effect_table_qte <- function(object) {
  qte_fit <- object$qte$fit %||% NULL

  if (is.data.frame(qte_fit)) {
    keep <- intersect(c("id", "index", "estimate", "lower", "upper"), names(qte_fit))
    out <- qte_fit[, keep, drop = FALSE]
    if ("index" %in% names(out)) {
      names(out)[names(out) == "index"] <- "tau"
    }
    sort_cols <- intersect(c("id", "tau"), names(out))
    if (length(sort_cols)) {
      out <- out[do.call(order, out[sort_cols]), , drop = FALSE]
    }
    rownames(out) <- NULL
    return(out)
  }

  fit_mat <- object$fit %||% NULL
  probs <- object$probs %||% object$grid %||% numeric(0)
  if (is.matrix(fit_mat) && ncol(fit_mat) > 0L && length(probs) == ncol(fit_mat)) {
    out <- data.frame(
      id = rep(seq_len(nrow(fit_mat)), each = ncol(fit_mat)),
      tau = rep(probs, times = nrow(fit_mat)),
      estimate = as.vector(t(fit_mat))
    )
    if (is.matrix(object$lower) && all(dim(object$lower) == dim(fit_mat))) {
      out$lower <- as.vector(t(object$lower))
    }
    if (is.matrix(object$upper) && all(dim(object$upper) == dim(fit_mat))) {
      out$upper <- as.vector(t(object$upper))
    }
    rownames(out) <- NULL
    return(out)
  }

  NULL
}

#' Summarize a QTE-style effect object
#'
#' \code{summary.causalmixgpd_qte()} converts QTE, QTT, or CQTE output into a
#' tabular summary suitable for reporting.
#'
#' @param object A \code{"causalmixgpd_qte"} object from \code{qte()}.
#' @param ... Unused.
#' @return An object of class \code{"summary.causalmixgpd_qte"} with
#'   \code{overall}, \code{quantile_summary}, \code{effect_table},
#'   \code{ci_summary}, \code{meta}, and the original \code{object}.
#' @seealso \code{\link{print.causalmixgpd_qte}},
#'   \code{\link{plot.causalmixgpd_qte}}, \code{\link{qte}},
#'   \code{\link{cqte}}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' q <- qte(fit, probs = c(0.25, 0.5, 0.75), interval = "credible")
#' summary(q)
#' }
#' @export
summary.causalmixgpd_qte <- function(object, ...) {
  stopifnot(inherits(object, "causalmixgpd_qte"))

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
    ps_used = isTRUE(meta$ps_enabled) || (!is.null(object$ps) && any(is.finite(object$ps)))
  )

  # Per-quantile summary statistics
  qte_fit <- object$qte$fit %||% NULL
  quantile_summary <- NULL
  qte_draws <- object$qte$draws %||% NULL

  if (!is.null(qte_draws)) {
    draw_cols <- NULL
    if (is.array(qte_draws) && length(dim(qte_draws)) == 3L) {
      # Expected layout for marginal QTE: S x id x quantile
      n_q <- dim(qte_draws)[3]
      draw_cols <- lapply(seq_len(n_q), function(j) as.numeric(qte_draws[, , j]))
    } else if (is.matrix(qte_draws)) {
      # Fallback: S x quantile
      n_q <- ncol(qte_draws)
      draw_cols <- lapply(seq_len(n_q), function(j) as.numeric(qte_draws[, j]))
    }

    if (!is.null(draw_cols) && length(draw_cols) > 0L) {
      n_q <- min(length(probs), length(draw_cols))
      if (n_q > 0L) {
        quantile_summary <- do.call(rbind, lapply(seq_len(n_q), function(j) {
          draws_j <- draw_cols[[j]]
          draws_j <- draws_j[is.finite(draws_j)]
          if (!length(draws_j)) {
            return(data.frame(
              quantile = probs[j],
              estimate_qte = NA_real_,
              mean_qte = NA_real_,
              median_qte = NA_real_,
              min_qte = NA_real_,
              max_qte = NA_real_,
              sd_qte = NA_real_,
              ci_lower = NA_real_,
              ci_upper = NA_real_,
              ci_width = NA_real_
            ))
          }

          fit_row <- NULL
          if (!is.null(qte_fit) && is.data.frame(qte_fit) && "index" %in% names(qte_fit)) {
            fit_row <- qte_fit[qte_fit$index == probs[j], , drop = FALSE]
            if (nrow(fit_row) == 0L) fit_row <- NULL
          }
          est_q <- if (!is.null(fit_row) && "estimate" %in% names(fit_row)) {
            as.numeric(fit_row$estimate[1])
          } else {
            mean(draws_j, na.rm = TRUE)
          }
          ci_l <- if (!is.null(fit_row) && "lower" %in% names(fit_row)) as.numeric(fit_row$lower[1]) else NA_real_
          ci_u <- if (!is.null(fit_row) && "upper" %in% names(fit_row)) as.numeric(fit_row$upper[1]) else NA_real_
          ci_w <- if (is.finite(ci_l) && is.finite(ci_u)) (ci_u - ci_l) else NA_real_

          data.frame(
            quantile = probs[j],
            estimate_qte = est_q,
            mean_qte = mean(draws_j, na.rm = TRUE),
            median_qte = stats::median(draws_j, na.rm = TRUE),
            min_qte = min(draws_j, na.rm = TRUE),
            max_qte = max(draws_j, na.rm = TRUE),
            sd_qte = if (length(draws_j) > 1L) stats::sd(draws_j, na.rm = TRUE) else NA_real_,
            ci_lower = ci_l,
            ci_upper = ci_u,
            ci_width = ci_w
          )
        }))
      }
    }
  }

  if (is.null(quantile_summary)) {
    if (!is.null(qte_fit) && is.data.frame(qte_fit)) {
      quantile_summary <- do.call(rbind, lapply(probs, function(tau) {
        rows <- qte_fit[qte_fit$index == tau, , drop = FALSE]
        if (nrow(rows) == 0L) return(NULL)
        est <- rows$estimate
        lo <- if ("lower" %in% names(rows)) rows$lower else NA_real_
        up <- if ("upper" %in% names(rows)) rows$upper else NA_real_
        data.frame(
          quantile = tau,
          estimate_qte = est[1],
          mean_qte = mean(est, na.rm = TRUE),
          median_qte = stats::median(est, na.rm = TRUE),
          min_qte = min(est, na.rm = TRUE),
          max_qte = max(est, na.rm = TRUE),
          sd_qte = if (length(est) > 1) stats::sd(est, na.rm = TRUE) else NA_real_,
          ci_lower = lo[1],
          ci_upper = up[1],
          ci_width = if (is.finite(lo[1]) && is.finite(up[1])) up[1] - lo[1] else NA_real_
        )
      }))
    } else if (!is.null(object$fit) && is.matrix(object$fit)) {
      fit_mat <- object$fit
      quantile_summary <- do.call(rbind, lapply(seq_along(probs), function(j) {
        est <- fit_mat[, j]
        data.frame(
          quantile = probs[j],
          estimate_qte = mean(est, na.rm = TRUE),
          mean_qte = mean(est, na.rm = TRUE),
          median_qte = stats::median(est, na.rm = TRUE),
          min_qte = min(est, na.rm = TRUE),
          max_qte = max(est, na.rm = TRUE),
          sd_qte = if (length(est) > 1) stats::sd(est, na.rm = TRUE) else NA_real_,
          ci_lower = NA_real_,
          ci_upper = NA_real_,
          ci_width = NA_real_
        )
      }))
    }
  }

  ci_summary <- NULL
  effect_table <- .summary_effect_table_qte(object)

  out <- list(
    overall = overall,
    quantile_summary = quantile_summary,
    effect_table = effect_table,
    ci_summary = ci_summary,
    meta = meta,
    object = object
  )
  class(out) <- "summary.causalmixgpd_qte"
  out
}

#' Print a QTE summary
#'
#' @param x A \code{"summary.causalmixgpd_qte"} object.
#' @param digits Number of digits to display.
#' @param ... Unused.
#' @return The object \code{x}, invisibly.
#' @export
print.summary.causalmixgpd_qte <- function(x, digits = 3, ...) {
  stopifnot(inherits(x, "summary.causalmixgpd_qte"))

  lbl <- .effect_label_qte(((x$object %||% list())$type %||% "qte"))
  ov <- x$overall
  meta <- x$meta %||% list()
  knitr_kable <- .is_knitr_output() && isTRUE(getOption("causalmixgpd.knitr.kable", FALSE))
  .clean_qte_colnames <- function(df) {
    if (is.null(df) || !is.data.frame(df)) return(df)
    drop_cols <- intersect(names(df), c("estimate_qte", "estimate"))
    if (length(drop_cols)) {
      df <- df[, setdiff(names(df), drop_cols), drop = FALSE]
    }
    nms <- names(df)
    nms <- sub("^ci_", "", nms)
    nms <- sub("_qte$", "", nms)
    names(df) <- nms
    df
  }
  .clean_effect_colnames <- function(df) {
    if (is.null(df) || !is.data.frame(df)) return(df)
    df
  }

  if (knitr_kable) {
    pieces <- list(
      sprintf("%s Summary", lbl$short),
      paste(rep("=", 50), collapse = ""),
      sprintf("Prediction points: %d | Quantiles: %d", ov$n_pred, ov$n_quantiles),
      sprintf("Quantile grid: %s", fmt3_vec(ov$quantiles)),
      sprintf("Conditional: %s | PS used: %s",
              if (ov$has_covariates) "YES" else "NO",
              if (ov$ps_used) "YES" else "NO")
    )
    if (ov$interval == "credible") {
      pieces <- c(pieces, list(sprintf("Interval: %s (%.0f%%)", ov$interval, ov$level * 100)))
    } else {
      pieces <- c(pieces, list(sprintf("Interval: %s", ov$interval)))
    }
    pieces <- c(pieces, list(""))

    if (!is.null(meta$backend) || !is.null(meta$kernel)) {
      pieces <- c(pieces, list("Model specification:"))
      if (!is.null(meta$backend)) {
        pieces <- c(pieces, list(sprintf("  Backend (trt/con): %s / %s",
                                         meta$backend$trt %||% "?", meta$backend$con %||% "?")))
      }
      if (!is.null(meta$kernel)) {
        pieces <- c(pieces, list(sprintf("  Kernel (trt/con): %s / %s",
                                         meta$kernel$trt %||% "?", meta$kernel$con %||% "?")))
      }
      if (!is.null(meta$GPD)) {
        pieces <- c(pieces, list(sprintf("  GPD tail (trt/con): %s / %s",
                                         if (isTRUE(meta$GPD$trt)) "YES" else "NO",
                                         if (isTRUE(meta$GPD$con)) "YES" else "NO")))
      }
      pieces <- c(pieces, list(""))
    }

    effect_table <- x$effect_table
    if (!is.null(effect_table) && nrow(effect_table) > 0) {
      effect_table <- .clean_effect_colnames(effect_table)
      pieces <- c(pieces, list(sprintf("%s estimates:", lbl$short)))
      pieces <- c(pieces, list(.kable_table(format_df3_sci(effect_table, digits = digits), row.names = FALSE)))
      pieces <- c(pieces, list(""))
    }

    qs <- x$quantile_summary
    if (!is.null(qs) && nrow(qs) > 0) {
      qs <- .clean_qte_colnames(qs)
      pieces <- c(pieces, list(sprintf("%s by quantile:", lbl$short)))
      pieces <- c(pieces, list(.kable_table(format_df3_sci(qs, digits = digits), row.names = FALSE)))
      pieces <- c(pieces, list(""))
    }

    return(do.call(.knitr_asis, pieces))
  }

  cat(sprintf("%s Summary\n", lbl$short))
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat(sprintf("Prediction points: %d | Quantiles: %d\n", ov$n_pred, ov$n_quantiles))
  cat(sprintf("Quantile grid: %s\n", fmt3_vec(ov$quantiles)))
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
  effect_table <- x$effect_table
  if (!is.null(effect_table) && nrow(effect_table) > 0) {
    effect_table <- .clean_effect_colnames(effect_table)
    cat(sprintf("%s estimates:\n", lbl$short))
    print_fmt3_sci(effect_table, row.names = FALSE, digits = digits)
    cat("\n")
  }

  qs <- x$quantile_summary
  if (!is.null(qs) && nrow(qs) > 0) {
    qs <- .clean_qte_colnames(qs)
    cat(sprintf("%s by quantile:\n", lbl$short))
    qs_print <- qs
    print_fmt3_sci(qs_print, row.names = FALSE, digits = digits)
    cat("\n")
  }

  invisible(x)
}

.summary_effect_table_ate <- function(object) {
  ate_fit <- object$ate$fit %||% NULL

  if (is.data.frame(ate_fit)) {
    keep <- intersect(c("id", "estimate", "lower", "upper"), names(ate_fit))
    out <- ate_fit[, keep, drop = FALSE]
    if ("id" %in% names(out)) {
      out <- out[order(out$id), , drop = FALSE]
    }
    rownames(out) <- NULL
    return(out)
  }

  fit_vec <- object$fit %||% NULL
  if (is.numeric(fit_vec) && length(fit_vec) > 0L) {
    out <- data.frame(
      id = seq_along(fit_vec),
      estimate = as.numeric(fit_vec)
    )
    if (is.numeric(object$lower) && length(object$lower) == length(fit_vec)) {
      out$lower <- as.numeric(object$lower)
    }
    if (is.numeric(object$upper) && length(object$upper) == length(fit_vec)) {
      out$upper <- as.numeric(object$upper)
    }
    rownames(out) <- NULL
    return(out)
  }

  NULL
}

#' Summarize an ATE-style effect object
#'
#' \code{summary.causalmixgpd_ate()} converts ATE, ATT, CATE, or restricted-mean
#' output into a tabular summary suitable for reporting.
#'
#' @param object A \code{"causalmixgpd_ate"} object from \code{ate()}.
#' @param ... Unused.
#' @return An object of class \code{"summary.causalmixgpd_ate"} with
#'   \code{overall}, \code{ate_stats}, \code{effect_table},
#'   \code{ci_summary}, \code{meta}, and the original \code{object}.
#' @seealso \code{\link{print.causalmixgpd_ate}},
#'   \code{\link{plot.causalmixgpd_ate}}, \code{\link{ate}},
#'   \code{\link{cate}}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' a <- ate(fit, interval = "credible")
#' summary(a)
#' }
#' @export
summary.causalmixgpd_ate <- function(object, ...) {
  stopifnot(inherits(object, "causalmixgpd_ate"))

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
    ps_used = isTRUE(meta$ps_enabled) || (!is.null(object$ps) && any(is.finite(object$ps)))
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

  effect_table <- .summary_effect_table_ate(object)

  out <- list(
    overall = overall,
    ate_stats = ate_stats,
    effect_table = effect_table,
    ci_summary = ci_summary,
    meta = meta,
    object = object
  )
  class(out) <- "summary.causalmixgpd_ate"
  out
}

#' Print an ATE summary
#'
#' @param x A \code{"summary.causalmixgpd_ate"} object.
#' @param digits Number of digits to display.
#' @param ... Unused.
#' @return The object \code{x}, invisibly.
#' @export
print.summary.causalmixgpd_ate <- function(x, digits = 3, ...) {
  stopifnot(inherits(x, "summary.causalmixgpd_ate"))

  obj <- x$object %||% list()
  lbl <- .effect_label_ate(obj$type %||% "ate", metric = (obj$trt %||% list())$type %||% (obj$con %||% list())$type %||% NULL)
  ov <- x$overall
  meta <- x$meta %||% list()
  knitr_kable <- .is_knitr_output() && isTRUE(getOption("causalmixgpd.knitr.kable", FALSE))

  if (knitr_kable) {
    pieces <- list(
      sprintf("%s Summary", lbl$short),
      paste(rep("=", 50), collapse = ""),
      sprintf("Prediction points: %d", ov$n_pred),
      sprintf("Conditional: %s | PS used: %s",
              if (ov$has_covariates) "YES" else "NO",
              if (ov$ps_used) "YES" else "NO")
    )
    if (!is.na(ov$nsim_mean)) {
      pieces <- c(pieces, list(sprintf("Posterior mean draws: %d", ov$nsim_mean)))
    }
    if (ov$interval == "credible") {
      pieces <- c(pieces, list(sprintf("Interval: %s (%.0f%%)", ov$interval, ov$level * 100)))
    } else {
      pieces <- c(pieces, list(sprintf("Interval: %s", ov$interval)))
    }
    pieces <- c(pieces, list(""))

    if (!is.null(meta$backend) || !is.null(meta$kernel)) {
      pieces <- c(pieces, list("Model specification:"))
      if (!is.null(meta$backend)) {
        pieces <- c(pieces, list(sprintf("  Backend (trt/con): %s / %s",
                                         meta$backend$trt %||% "?", meta$backend$con %||% "?")))
      }
      if (!is.null(meta$kernel)) {
        pieces <- c(pieces, list(sprintf("  Kernel (trt/con): %s / %s",
                                         meta$kernel$trt %||% "?", meta$kernel$con %||% "?")))
      }
      if (!is.null(meta$GPD)) {
        pieces <- c(pieces, list(sprintf("  GPD tail (trt/con): %s / %s",
                                         if (isTRUE(meta$GPD$trt)) "YES" else "NO",
                                         if (isTRUE(meta$GPD$con)) "YES" else "NO")))
      }
      pieces <- c(pieces, list(""))
    }

    effect_table <- x$effect_table
    if (!is.null(effect_table) && nrow(effect_table) > 0) {
      pieces <- c(pieces, list(sprintf("%s estimates:", lbl$short)))
      pieces <- c(pieces, list(.kable_table(format_df3_sci(effect_table, digits = digits), row.names = FALSE)))
      pieces <- c(pieces, list(""))
    }

    as <- x$ate_stats
    if (!is.null(as)) {
      pieces <- c(pieces, list(
        sprintf("%s statistics:", lbl$short),
        sprintf("  Mean: %s | Median: %s",
                fmt3_sci(as$mean_ate, digits = digits), fmt3_sci(as$median_ate, digits = digits)),
        sprintf("  Range: [%s, %s]",
                fmt3_sci(as$min_ate, digits = digits), fmt3_sci(as$max_ate, digits = digits))
      ))
      if (!is.na(as$sd_ate)) {
        pieces <- c(pieces, list(sprintf("  SD: %s", fmt3_sci(as$sd_ate, digits = digits))))
      }
      pieces <- c(pieces, list(""))
    }

    ci <- x$ci_summary
    if (!is.null(ci)) {
      pieces <- c(pieces, list(
        "Credible interval width:",
        sprintf("  Mean: %s | Median: %s",
                fmt3_sci(ci$mean_width, digits = digits), fmt3_sci(ci$median_width, digits = digits)),
        sprintf("  Range: [%s, %s]",
                fmt3_sci(ci$min_width, digits = digits), fmt3_sci(ci$max_width, digits = digits))
      ))
    }
    return(do.call(.knitr_asis, pieces))
  }

  cat(sprintf("%s Summary\n", lbl$short))
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
  effect_table <- x$effect_table
  if (!is.null(effect_table) && nrow(effect_table) > 0) {
    cat(sprintf("%s estimates:\n", lbl$short))
    print_fmt3_sci(effect_table, row.names = FALSE, digits = digits)
    cat("\n")
  }

  as <- x$ate_stats
  if (!is.null(as)) {
    cat(sprintf("%s statistics:\n", lbl$short))
    cat(sprintf("  Mean: %s | Median: %s\n",
                fmt3_sci(as$mean_ate, digits = digits), fmt3_sci(as$median_ate, digits = digits)))
    cat(sprintf("  Range: [%s, %s]\n",
                fmt3_sci(as$min_ate, digits = digits), fmt3_sci(as$max_ate, digits = digits)))
    if (!is.na(as$sd_ate)) {
      cat(sprintf("  SD: %s\n", fmt3_sci(as$sd_ate, digits = digits)))
    }
    cat("\n")
  }

  # CI summary
  ci <- x$ci_summary
  if (!is.null(ci)) {
    cat("Credible interval width:\n")
    cat(sprintf("  Mean: %s | Median: %s\n",
                fmt3_sci(ci$mean_width, digits = digits), fmt3_sci(ci$median_width, digits = digits)))
    cat(sprintf("  Range: [%s, %s]\n",
                fmt3_sci(ci$min_width, digits = digits), fmt3_sci(ci$max_width, digits = digits)))
  }

  invisible(x)
}

#' Plot QTE-style effect summaries
#'
#' \code{plot.causalmixgpd_qte()} visualizes objects returned by
#' \code{\link{qte}}, \code{\link{qtt}}, and \code{\link{cqte}}. The
#' \code{type} parameter controls the plot style. When \code{type} is omitted,
#' \code{cqte()} objects default to \code{"effect"} and, when multiple
#' quantile levels are present, \code{facet_by = "id"}. Whenever quantile index
#' appears on the x-axis, it is shown as an ordered categorical axis with
#' equidistant spacing:
#' \itemize{
#'   \item \code{"both"} (default): Returns a list with both \code{trt_control} (treated vs
#'     control quantile curves) and \code{treatment_effect} (QTE curve) plots
#'   \item \code{"effect"}: QTE curve vs quantile levels (\code{probs}) with pointwise CI error bars
#'   \item \code{"arms"}: Treated and control quantile curves vs \code{probs}, with pointwise CI error bars
#' }
#'
#' @param x Object of class \code{causalmixgpd_qte}.
#' @param y Ignored.
#' @param type Character; plot type: \code{"both"} (default), \code{"effect"}, or \code{"arms"}.
#' @param facet_by Character; faceting strategy when multiple prediction points exist.
#'   \code{"tau"} (default) facets by quantile level, \code{"id"} facets by prediction point.
#' @param plotly Logical; if \code{TRUE}, convert the \code{ggplot2} output to a
#'   \code{plotly} / \code{htmlwidget} representation via \code{.wrap_plotly()}. Defaults
#'   to \code{getOption("CausalMixGPD.plotly", FALSE)}.
#' @param ... Additional arguments passed to ggplot2 functions.
#' @return A list of ggplot objects with elements \code{trt_control} and \code{treatment_effect}
#'   (if \code{type="both"}), or a single ggplot object (if \code{type} is \code{"effect"} or
#'   \code{"arms"}).
#' @seealso \code{\link{qte}}, \code{\link{cqte}},
#'   \code{\link{summary.causalmixgpd_qte}}.
#' @examples
#' \dontrun{
#' qte_result <- cqte(fit, probs = c(0.1, 0.5, 0.9), newdata = X_new)
#' plot(qte_result)  # CQTE default: effect plot (faceted by id when needed)
#' plot(qte_result, type = "effect")  # single QTE plot
#' plot(qte_result, type = "arms")    # single arms plot
#' }
#' @export
plot.causalmixgpd_qte <- function(x, y = NULL, type = c("both", "effect", "arms"),
                              facet_by = c("tau", "id"),
                              plotly = getOption("CausalMixGPD.plotly", FALSE), ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Install it first.", call. = FALSE)
  }

  use_plotly <- isTRUE(plotly)
  lbl <- .effect_label_qte(x$type %||% "qte")

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

  is_cqte <- identical(tolower(x$type %||% "qte"), "cqte")
  if (missing(type) && is_cqte) {
    type <- "effect"
  }
  if (missing(facet_by) && is_cqte && n_pred > 1L && length(probs) > 1L) {
    facet_by <- "id"
  }
  type <- match.arg(type)
  facet_by <- match.arg(facet_by)

  ps_vec <- x$ps %||% rep(NA_real_, n_pred)
  if (!length(ps_vec)) ps_vec <- rep(NA_real_, n_pred)
  ax <- if (any(is.finite(ps_vec))) list(x = ps_vec, label = "Estimated PS") else
    list(x = seq_len(n_pred), label = "Index")
  single_profile_curve <- (n_pred == 1L && length(probs) > 1L)
  .x_axis_spec <- function() {
    if (single_profile_curve || (facet_by == "id" && n_pred > 1L && length(probs) > 1L)) {
      return(list(var = "index", label = "Quantile level (\u03C4)", discrete = TRUE))
    }
    if (length(probs) == 1L) {
      return(list(var = "ps", label = paste0(ax$label, " at \u03C4 = ", .fmt_num(probs)), discrete = FALSE))
    }
    list(var = "ps", label = ax$label, discrete = FALSE)
  }
  .fmt_num <- function(z) {
    out <- rep("NA", length(z))
    ok <- is.finite(z)
    out[ok] <- trimws(formatC(z[ok], digits = 4, format = "fg"))
    out
  }
  point_size_large <- 4.0
  point_size_regular <- 2.6
  .hover_interval <- function(lower, upper, label = "CI") {
    ifelse(
      is.finite(lower) & is.finite(upper),
      paste0("<br>", label, ": [", .fmt_num(lower), ", ", .fmt_num(upper), "]"),
      ""
    )
  }
  .hover_qte <- function(df, axis_spec, value_label, group_col = NULL) {
    axis_value <- if (identical(axis_spec$var, "index")) {
      paste0("Tau: ", .fmt_num(df$index))
    } else {
      paste0(axis_spec$label, ": ", .fmt_num(df$ps))
    }
    tau_text <- if (!identical(axis_spec$var, "index") && length(probs) == 1L) {
      paste0("<br>Tau: ", .fmt_num(df$index))
    } else {
      ""
    }
    group_text <- if (!is.null(group_col)) {
      paste0("<br>Arm: ", df[[group_col]])
    } else {
      ""
    }
    paste0(
      "ID: ", df$id,
      group_text,
      "<br>", axis_value,
      tau_text,
      "<br>", value_label, ": ", .fmt_num(df$estimate),
      .hover_interval(df$lower, df$upper)
    )
  }

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
  .errorbar_width <- function(x_vals) {
    x_vals <- sort(unique(as.numeric(x_vals[is.finite(x_vals)])))
    if (length(x_vals) <= 1L) {
      return(0.02)
    }
    gaps <- diff(x_vals)
    gaps <- gaps[is.finite(gaps) & gaps > 0]
    if (!length(gaps)) {
      return(0.02)
    }
    min(gaps) * 0.2
  }

  # Build arms plot (treated vs control)
  .build_arms_plot <- function() {
    df_trt <- .as_df(pr_trt, n_pred, probs)
    df_con <- .as_df(pr_con, n_pred, probs)
    df_trt$group <- "Treated"
    df_con$group <- "Control"

    df_tc <- rbind(df_trt, df_con)
    df_tc$ps <- ax$x[df_tc$id]
    df_tc$tau <- factor(paste0("\u03C4 = ", df_tc$index), levels = paste0("\u03C4 = ", probs))
    axis_spec <- .x_axis_spec()
    df_tc$x_plot <- if (isTRUE(axis_spec$discrete)) {
      factor(paste0("\u03C4 = ", .fmt_num(df_tc$index)),
             levels = paste0("\u03C4 = ", .fmt_num(probs)))
    } else {
      df_tc[[axis_spec$var]]
    }
    df_tc$hover <- .hover_qte(df_tc, axis_spec = axis_spec, value_label = "Quantile", group_col = "group")
    if (nrow(df_tc)) {
      panel_key <- if (facet_by == "tau" && length(probs) > 1L) as.integer(df_tc$tau) else df_tc$id
      x_order <- if (isTRUE(axis_spec$discrete)) match(df_tc$index, probs) else as.numeric(df_tc$x_plot)
      df_tc <- df_tc[order(df_tc$group, panel_key, x_order), , drop = FALSE]
    }

    if (single_profile_curve) {
      p <- ggplot2::ggplot(df_tc, ggplot2::aes(x = x_plot, y = estimate, color = group, fill = group, text = hover)) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::geom_point(size = point_size_large) +
        ggplot2::scale_color_manual(values = pal[1:2]) +
        ggplot2::scale_fill_manual(values = pal[1:2]) +
        .plot_theme() +
        ggplot2::labs(
          x = axis_spec$label,
          y = "Quantile",
          title = "Treated vs Control Quantiles",
          color = "Arm",
          fill = "Arm"
        )
      if (any(is.finite(df_tc$lower)) && any(is.finite(df_tc$upper))) {
        err_width <- .errorbar_width(df_tc$x_plot)
        p <- p + ggplot2::geom_errorbar(
          ggplot2::aes(ymin = lower, ymax = upper),
          width = err_width,
          alpha = 0.8
        )
      }
      return(p)
    }

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

    p <- ggplot2::ggplot(df_tc, ggplot2::aes(x = x_plot, y = estimate, color = group, fill = group, text = hover)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = point_size_regular) +
      ggplot2::scale_color_manual(values = pal[1:2]) +
      ggplot2::scale_fill_manual(values = pal[1:2]) +
      .plot_theme() +
      ggplot2::labs(x = axis_spec$label, y = "Quantile", title = "Treated vs Control Quantiles",
                    color = "Arm", fill = "Arm")

    if (!is.null(facet_formula)) {
      p <- p + ggplot2::facet_wrap(facet_formula, scales = "free_y")
    }

    if (any(is.finite(df_tc$lower)) && any(is.finite(df_tc$upper))) {
      err_width <- .errorbar_width(df_tc$x_plot)
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(ymin = lower, ymax = upper),
        width = err_width,
        alpha = 0.8
      )
    }
    p
  }

  # Build effect plot (QTE = treated - control)
  .build_effect_plot <- function() {
    te_mat <- x$fit
    te_lower <- x$lower
    te_upper <- x$upper
    df_te_src <- x$qte$fit %||% NULL
    if (is.data.frame(df_te_src)) {
      df_te <- df_te_src
      if (!("estimate" %in% names(df_te))) df_te$estimate <- NA_real_
      if (!("lower" %in% names(df_te))) df_te$lower <- NA_real_
      if (!("upper" %in% names(df_te))) df_te$upper <- NA_real_
      if (!("id" %in% names(df_te))) {
        df_te$id <- if (n_pred == 1L) rep(1L, nrow(df_te)) else rep(seq_len(n_pred), each = length(probs))
      }
      if (!("index" %in% names(df_te))) {
        df_te$index <- rep(probs, times = max(1L, n_pred))
      }
    } else {
      df_te <- .coerce_fit_df(te_mat, n_pred = n_pred, probs = probs)
      if (!("id" %in% names(df_te))) df_te$id <- rep(seq_len(n_pred), each = length(probs))
      if (!("index" %in% names(df_te))) df_te$index <- rep(probs, times = n_pred)
      if (!("estimate" %in% names(df_te))) df_te$estimate <- NA_real_
      if (!("lower" %in% names(df_te))) df_te$lower <- NA_real_
      if (!("upper" %in% names(df_te))) df_te$upper <- NA_real_

      expected_n <- n_pred * length(probs)
      if (expected_n > 0L && nrow(df_te) != expected_n) {
        te_est <- rep_len(as.numeric(df_te$estimate), expected_n)
        lo_src <- if (!is.null(te_lower)) as.vector(t(te_lower)) else as.numeric(df_te$lower)
        up_src <- if (!is.null(te_upper)) as.vector(t(te_upper)) else as.numeric(df_te$upper)
        lo <- rep_len(if (length(lo_src)) lo_src else NA_real_, expected_n)
        up <- rep_len(if (length(up_src)) up_src else NA_real_, expected_n)
        df_te <- data.frame(
          id = rep(seq_len(n_pred), each = length(probs)),
          index = rep(probs, times = n_pred),
          estimate = te_est,
          lower = lo,
          upper = up
        )
      } else {
        if (!is.null(te_lower) && all(!is.finite(df_te$lower))) {
          df_te$lower <- rep_len(as.vector(t(te_lower)), nrow(df_te))
        }
        if (!is.null(te_upper) && all(!is.finite(df_te$upper))) {
          df_te$upper <- rep_len(as.vector(t(te_upper)), nrow(df_te))
        }
        if (n_pred == 1L) {
          df_te$id <- 1L
        }
      }
    }

    df_te$id <- pmax(1L, pmin(as.integer(df_te$id), length(ax$x)))
    df_te$ps <- ax$x[df_te$id]
    df_te$tau <- factor(paste0("\u03C4 = ", df_te$index), levels = paste0("\u03C4 = ", probs))
    axis_spec <- .x_axis_spec()
    df_te$x_plot <- if (isTRUE(axis_spec$discrete)) {
      factor(paste0("\u03C4 = ", .fmt_num(df_te$index)),
             levels = paste0("\u03C4 = ", .fmt_num(probs)))
    } else {
      df_te[[axis_spec$var]]
    }
    df_te$hover <- .hover_qte(df_te, axis_spec = axis_spec, value_label = lbl$short)
    if (nrow(df_te)) {
      panel_key <- if (facet_by == "tau" && length(probs) > 1L) as.integer(df_te$tau) else df_te$id
      x_order <- if (isTRUE(axis_spec$discrete)) match(df_te$index, probs) else as.numeric(df_te$x_plot)
      df_te <- df_te[order(panel_key, x_order), , drop = FALSE]
    }

    if (single_profile_curve) {
      p <- ggplot2::ggplot(df_te, ggplot2::aes(x = x_plot, y = estimate, text = hover)) +
        ggplot2::geom_line(color = pal[7], linewidth = 0.8) +
        ggplot2::geom_point(color = pal[7], size = point_size_large) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
        .plot_theme() +
        ggplot2::labs(x = axis_spec$label, y = lbl$long, title = lbl$short)
      if (any(is.finite(df_te$lower)) && any(is.finite(df_te$upper))) {
        err_width <- .errorbar_width(df_te$x_plot)
        p <- p + ggplot2::geom_errorbar(
          ggplot2::aes(ymin = lower, ymax = upper),
          width = err_width,
          color = pal[7],
          alpha = 0.8
        )
      }
      return(p)
    }

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

    p <- ggplot2::ggplot(df_te, ggplot2::aes(x = x_plot, y = estimate, text = hover)) +
      ggplot2::geom_line(color = pal[7], linewidth = 0.8) +
      ggplot2::geom_point(color = pal[7], size = point_size_regular) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
      .plot_theme() +
      ggplot2::labs(x = axis_spec$label, y = lbl$long, title = lbl$short)

    if (!is.null(facet_formula)) {
      p <- p + ggplot2::facet_wrap(facet_formula, scales = "free_y")
    }

    if (any(is.finite(df_te$lower)) && any(is.finite(df_te$upper))) {
      err_width <- .errorbar_width(df_te$x_plot)
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(ymin = lower, ymax = upper),
        width = err_width,
        color = pal[7],
        alpha = 0.8
      )
    }
    p
  }

  # Return based on type
  if (type == "effect") {
    result <- .build_effect_plot()
    class(result) <- c("causalmixgpd_qte_plot", class(result))
    if (use_plotly) return(.wrap_plotly(result))
    return(result)
  }

  if (type == "arms") {
    result <- .build_arms_plot()
    class(result) <- c("causalmixgpd_qte_plot", class(result))
    if (use_plotly) return(.wrap_plotly(result))
    return(result)
  }

  # type == "both" (default) - maintain backward compatible naming
  result <- list(
    trt_control = .build_arms_plot(),
    treatment_effect = .build_effect_plot()
  )
  class(result) <- c("causalmixgpd_causal_predict_plots", "list")
  if (use_plotly) {
    return(.wrap_plotly(result))
  }
  result
}

#' Plot ATE-style effect summaries
#'
#' \code{plot.causalmixgpd_ate()} visualizes objects returned by
#' \code{\link{ate}}, \code{\link{att}}, \code{\link{cate}}, and
#' \code{\link{ate_rmean}}. The \code{type} parameter controls the plot style.
#' When \code{type} is omitted, \code{cate()} objects default to
#' \code{"effect"}:
#' \itemize{
#'   \item \code{"both"} (default): Returns a list with both \code{trt_control} (treated vs
#'     control means) and \code{treatment_effect} (ATE curve) plots
#'   \item \code{"effect"}: ATE curve/points vs index/PS with pointwise CI error bars
#'   \item \code{"arms"}: Treated mean vs control mean, with pointwise CI error bars
#' }
#'
#' @param x Object of class \code{causalmixgpd_ate}.
#' @param y Ignored.
#' @param type Character; plot type: \code{"both"} (default), \code{"effect"}, or \code{"arms"}.
#' @param plotly Logical; if \code{TRUE}, convert the \code{ggplot2} output to a
#'   \code{plotly} / \code{htmlwidget} representation via \code{.wrap_plotly()}. Defaults
#'   to \code{getOption("CausalMixGPD.plotly", FALSE)}.
#' @param ... Additional arguments passed to ggplot2 functions.
#' @return A list of ggplot objects with elements \code{trt_control} and \code{treatment_effect}
#'   (if \code{type="both"}), or a single ggplot object (if \code{type} is \code{"effect"} or
#'   \code{"arms"}).
#' @seealso \code{\link{ate}}, \code{\link{cate}},
#'   \code{\link{summary.causalmixgpd_ate}}.
#' @examples
#' \dontrun{
#' ate_result <- cate(fit, newdata = X_new, interval = "credible")
#' plot(ate_result)  # CATE default: effect plot
#' plot(ate_result, type = "effect")  # single ATE plot
#' plot(ate_result, type = "arms")    # single arms plot
#' }
#' @export
plot.causalmixgpd_ate <- function(x, y = NULL, type = c("both", "effect", "arms"),
                              plotly = getOption("CausalMixGPD.plotly", FALSE), ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Install it first.", call. = FALSE)
  }

  use_plotly <- isTRUE(plotly)
  lbl <- .effect_label_ate(x$type %||% "ate", metric = x$trt$type %||% x$con$type %||% NULL)

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
  single_profile <- (n_pred == 1L)
  if (missing(type) && identical(tolower(x$type %||% "ate"), "cate")) {
    type <- "effect"
  }
  type <- match.arg(type)
  .fmt_num <- function(z) {
    out <- rep("NA", length(z))
    ok <- is.finite(z)
    out[ok] <- trimws(formatC(z[ok], digits = 4, format = "fg"))
    out
  }
  point_size_large <- 4.0
  point_size_regular <- 2.6
  point_size_single_effect <- 4.4
  .hover_interval <- function(lower, upper, label = "CI") {
    ifelse(
      is.finite(lower) & is.finite(upper),
      paste0("<br>", label, ": [", .fmt_num(lower), ", ", .fmt_num(upper), "]"),
      ""
    )
  }
  .hover_ate <- function(df, value_label, group_col = NULL) {
    axis_value <- paste0(ax$label, ": ", .fmt_num(df$x_plot))
    group_text <- if (!is.null(group_col)) {
      paste0("<br>Arm: ", df[[group_col]])
    } else {
      ""
    }
    paste0(
      "ID: ", df$id,
      group_text,
      "<br>", axis_value,
      "<br>", value_label, ": ", .fmt_num(df$estimate),
      .hover_interval(df$lower, df$upper)
    )
  }
  .errorbar_width <- function(x_vals) {
    x_vals <- sort(unique(as.numeric(x_vals[is.finite(x_vals)])))
    if (length(x_vals) <= 1L) {
      return(0.02)
    }
    gaps <- diff(x_vals)
    gaps <- gaps[is.finite(gaps) & gaps > 0]
    if (!length(gaps)) {
      return(0.02)
    }
    min(gaps) * 0.2
  }

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
      data.frame(id = seq_len(n_pred), x_plot = ax$x, group = "Treated", estimate = trt_stats$estimate,
                 lower = trt_stats$lower, upper = trt_stats$upper),
      data.frame(id = seq_len(n_pred), x_plot = ax$x, group = "Control", estimate = con_stats$estimate,
                 lower = con_stats$lower, upper = con_stats$upper)
    )
    df_tc$hover <- .hover_ate(df_tc, value_label = "Mean Outcome", group_col = "group")
    if (nrow(df_tc)) {
      df_tc <- df_tc[order(df_tc$group, df_tc$x_plot), , drop = FALSE]
    }

    if (single_profile) {
      p <- ggplot2::ggplot(df_tc, ggplot2::aes(x = group, y = estimate, color = group, fill = group, text = hover)) +
        ggplot2::geom_point(size = point_size_large) +
        ggplot2::scale_color_manual(values = pal[1:2]) +
        ggplot2::scale_fill_manual(values = pal[1:2]) +
        .plot_theme() +
        ggplot2::labs(x = NULL, y = "Mean Outcome", title = "Treated vs Control Means",
                      color = "Arm", fill = "Arm")

      if (any(is.finite(df_tc$lower)) && any(is.finite(df_tc$upper))) {
        p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                                        width = 0.15, alpha = 0.8)
      }
      return(p)
    }

    p <- ggplot2::ggplot(df_tc, ggplot2::aes(x = x_plot, y = estimate, color = group, fill = group, text = hover)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = point_size_regular) +
      ggplot2::scale_color_manual(values = pal[1:2]) +
      ggplot2::scale_fill_manual(values = pal[1:2]) +
      .plot_theme() +
      ggplot2::labs(x = ax$label, y = "Mean Outcome", title = "Treated vs Control Means",
                    color = "Arm", fill = "Arm")

    if (any(is.finite(df_tc$lower)) && any(is.finite(df_tc$upper))) {
      err_width <- .errorbar_width(df_tc$x_plot)
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(ymin = lower, ymax = upper),
        width = err_width,
        alpha = 0.8
      )
    }
    p
  }

  # Build effect plot (ATE = treated - control)
  .build_effect_plot <- function() {
    df_te <- data.frame(
      id = seq_len(n_pred),
      x_plot = ax$x,
      estimate = as.numeric(x$fit),
      lower = if (!is.null(x$lower)) as.numeric(x$lower) else NA_real_,
      upper = if (!is.null(x$upper)) as.numeric(x$upper) else NA_real_
    )
    df_te$hover <- .hover_ate(df_te, value_label = lbl$short)
    if (nrow(df_te)) {
      df_te <- df_te[order(df_te$x_plot), , drop = FALSE]
    }

    if (single_profile) {
      p <- ggplot2::ggplot(df_te, ggplot2::aes(x = "ATE", y = estimate, text = hover)) +
        ggplot2::geom_point(color = pal[7], size = point_size_single_effect) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
        .plot_theme() +
        ggplot2::labs(x = NULL, y = lbl$long, title = lbl$short)
      if (any(is.finite(df_te$lower)) && any(is.finite(df_te$upper))) {
        p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                                        width = 0.15, color = pal[7], alpha = 0.8)
      }
      return(p)
    }

    p <- ggplot2::ggplot(df_te, ggplot2::aes(x = x_plot, y = estimate, text = hover)) +
      ggplot2::geom_line(color = pal[7], linewidth = 0.8) +
      ggplot2::geom_point(color = pal[7], size = point_size_regular) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
      .plot_theme() +
      ggplot2::labs(x = ax$label, y = lbl$long, title = lbl$short)

    if (any(is.finite(df_te$lower)) && any(is.finite(df_te$upper))) {
      err_width <- .errorbar_width(df_te$x_plot)
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(ymin = lower, ymax = upper),
        width = err_width,
        color = pal[7],
        alpha = 0.8
      )
    }
    p
  }

  # Return based on type
  if (type == "effect") {
    result <- .build_effect_plot()
    class(result) <- c("causalmixgpd_ate_plot", class(result))
    if (use_plotly) return(.wrap_plotly(result))
    return(result)
  }

  if (type == "arms") {
    result <- .build_arms_plot()
    class(result) <- c("causalmixgpd_ate_plot", class(result))
    if (use_plotly) return(.wrap_plotly(result))
    return(result)
  }

  # type == "both" (default) - maintain backward compatible naming
  result <- list(
    trt_control = .build_arms_plot(),
    treatment_effect = .build_effect_plot()
  )
  class(result) <- c("causalmixgpd_causal_predict_plots", "list")
  if (use_plotly) {
    return(.wrap_plotly(result))
  }
  result
}

#' Print method for causal prediction plots
#'
#' @param x Object of class \code{causalmixgpd_causal_predict_plots}.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object.
#' @export
print.causalmixgpd_causal_predict_plots <- function(x, ...) {
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
#' @export
plot.mixgpd_fitted <- function(x, y = NULL, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Install it first.", call. = FALSE)
  }

  obj <- attr(x, "object")
  y_data <- obj$data$y %||% obj$y
  if (is.list(y_data) && !is.null(y_data$y)) {
    y_data <- y_data$y
  }

  # Panel 1: Observed vs Fitted (diagonal plot)
  if (is.null(x$fit) || length(x$fit) == 0L) {
    if (!is.null(obj)) {
      x_recalc <- tryCatch(fitted.mixgpd_fit(obj), error = function(e) NULL)
      if (!is.null(x_recalc) && !is.null(x_recalc$fit) && length(x_recalc$fit) > 0L) {
        x <- x_recalc
      }
    }
  }
  if (is.null(x$fit) || length(x$fit) == 0L || is.null(y_data) || length(y_data) == 0L) {
    stop("Fitted values are unavailable; ensure the model has fitted values before plotting.",
         call. = FALSE)
  }
  if (length(x$fit) != length(y_data)) {
    stop("Fitted values length does not match observed data length.",
         call. = FALSE)
  }

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
  .wrap_plotly(result)
}

#' Print method for fitted value plots
#'
#' @param x Object of class \code{mixgpd_fitted_plots}.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the input object.
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
#' @export
print.mixgpd_fit_plots <- function(x, ...) {
  for (plot_name in names(x)) {
    cat(sprintf("\n=== %s ===\n", plot_name))
    print(x[[plot_name]])
  }
  invisible(x)
}

#' @export
print.causalmixgpd_causal_fit_plots <- function(x, ...) {
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
#' @export
print.mixgpd_predict_plots <- function(x, ...) {
  # Remove custom class to call default print method for the underlying object
  cls <- class(x)
  class(x) <- setdiff(cls, "mixgpd_predict_plots")
  print(x)
  class(x) <- cls
  invisible(x)
}
