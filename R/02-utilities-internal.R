# ============================================================
# Utilities (internal)
# ============================================================



#' Null-coalescing operator
#' @keywords internal
#' @noRd
#' @rdname null_coalescing
#' @name null_coalescing
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Extract indexed parameter blocks from a draws matrix
#' @keywords internal
#' @noRd
.indexed_block <- function(mat0, base, K = NULL, allow_missing = FALSE) {
  cn0 <- colnames(mat0)
  pat <- paste0("^", base, "\\[([0-9]+)\\]$")
  hit <- grepl(pat, cn0)
  if (!any(hit)) {
    if (isTRUE(allow_missing)) return(NULL)
    stop(sprintf("No indexed columns found for '%s[i]'.", base), call. = FALSE)
  }

  idx <- as.integer(sub(pat, "\\1", cn0[hit]))
  ord <- order(idx)
  idx <- idx[ord]
  cols <- cn0[hit][ord]

  if (is.null(K)) K <- max(idx, na.rm = TRUE)
  K <- as.integer(K)

  out <- matrix(0.0, nrow = nrow(mat0), ncol = K)
  for (j in seq_along(cols)) {
    k <- idx[j]
    if (!is.na(k) && k >= 1 && k <= K) out[, k] <- mat0[, cols[j]]
  }
  out
}

#' Extract mixture weights from draws matrix
#' @keywords internal
#' @noRd
.extract_weights <- function(draw_mat, backend = c("sb", "crp")) {
  if (is.null(draw_mat) || !is.matrix(draw_mat)) {
    stop("draw_mat must be a numeric matrix.", call. = FALSE)
  }
  backend <- match.arg(backend)
  cn <- colnames(draw_mat)

  if (identical(backend, "sb")) {
    if (any(grepl("^w\\[[0-9]+\\]$", cn))) {
      return(.indexed_block(draw_mat, "w"))
    }
    if (any(grepl("^weights\\[[0-9]+\\]$", cn))) {
      return(.indexed_block(draw_mat, "weights"))
    }
    stop("Could not find component weights in posterior draws.", call. = FALSE)
  }

  if (!any(grepl("^z\\[[0-9]+\\]$", cn))) {
    stop("Backend requires z[i] in samples to derive weights.", call. = FALSE)
  }

  Z <- .indexed_block(draw_mat, "z")
  K <- max(Z, na.rm = TRUE)
  if (!is.finite(K) || K < 1L) stop("Could not infer K for CRP weights.", call. = FALSE)
  K <- as.integer(K)

  S <- nrow(draw_mat)
  W <- matrix(0.0, nrow = S, ncol = K)
  for (s in seq_len(S)) {
    z_s <- Z[s, ]
    z_s <- z_s[is.finite(z_s)]
    z_s <- z_s[z_s >= 1 & z_s <= K]
    if (length(z_s)) W[s, ] <- tabulate(z_s, nbins = K) / length(z_s)
  }
  W
}

#' Extract bulk parameter blocks from draws matrix
#' @keywords internal
#' @noRd
.extract_bulk_params <- function(draw_mat, bulk_params) {
  if (is.null(draw_mat) || !is.matrix(draw_mat)) {
    stop("draw_mat must be a numeric matrix.", call. = FALSE)
  }
  bulk_params <- as.character(bulk_params %||% character(0))
  if (!length(bulk_params)) return(list())

  cn <- colnames(draw_mat)

  infer_K_from_params <- function() {
    k_vals <- integer(0)
    for (nm in bulk_params) {
      idx <- as.integer(sub(paste0("^", nm, "\\[([0-9]+)\\]$"), "\\1",
                            cn[grepl(paste0("^", nm, "\\[[0-9]+\\]$"), cn)]))
      if (length(idx)) k_vals <- c(k_vals, idx)
    }
    if (!length(k_vals)) return(NA_integer_)
    as.integer(max(k_vals, na.rm = TRUE))
  }

  infer_K_from_weights <- function() {
    idx <- as.integer(sub("^w\\[([0-9]+)\\]$", "\\1", cn[grepl("^w\\[[0-9]+\\]$", cn)]))
    if (length(idx)) return(as.integer(max(idx, na.rm = TRUE)))
    idx <- as.integer(sub("^weights\\[([0-9]+)\\]$", "\\1",
                          cn[grepl("^weights\\[[0-9]+\\]$", cn)]))
    if (length(idx)) return(as.integer(max(idx, na.rm = TRUE)))
    NA_integer_
  }

  K <- infer_K_from_params()
  if (!is.finite(K) || K < 1L) K <- infer_K_from_weights()
  if (!is.finite(K) || K < 1L) stop("Could not infer K for bulk parameter draws.", call. = FALSE)

  out <- list()
  for (nm in bulk_params) {
    blk <- .indexed_block(draw_mat, nm, K = K, allow_missing = TRUE)
    if (!is.null(blk)) out[[nm]] <- blk
  }
  out
}

#' Reserved-name validation for NIMBLE
#' @param names Character vector of names to validate.
#' @param context Human-readable context for error messages.
#' @keywords internal
#' @noRd
.validate_nimble_reserved_names <- function(names, context = "names") {
  if (is.null(names) || !length(names)) return(invisible(TRUE))
  names <- as.character(names)
  names <- names[!is.na(names) & nzchar(names)]
  if (!length(names)) return(invisible(TRUE))

  reserved <- c(
    "if", "else", "for", "while", "repeat", "break", "next", "in",
    "function", "return",
    "true", "false", "null", "na", "nan", "inf",
    "na_integer_", "na_real_", "na_character_", "na_complex_",
    "t", "f"
  )

  bad <- unique(names[tolower(names) %in% reserved])
  if (length(bad)) {
    stop(sprintf(
      "%s include reserved NIMBLE keywords: %s. Rename columns (e.g., if -> x_if).",
      context,
      paste(bad, collapse = ", ")
    ), call. = FALSE)
  }

  invisible(TRUE)
}

#' Extract nimbleCode from bundle code
#' @keywords internal
.extract_nimble_code <- function(code) {
  if (is.list(code) && !inherits(code, "nimbleCode")) {
    if (!is.null(code$nimble)) return(code$nimble)
    if (!is.null(code$code)) return(code$code)
  }
  code
}

#' Wrap nimbleCode for bundle storage
#' @keywords internal
.wrap_nimble_code <- function(code) {
  if (is.list(code) && !inherits(code, "nimbleCode")) return(code)
  list(nimble = code)
}

# ---- Plot styling helpers (internal) ----
.plot_palette <- function(n = 8L) {
  base <- c(
    "#0072B2", # blue
    "#D55E00", # vermillion
    "#009E73", # green
    "#CC79A7", # purple
    "#56B4E9", # sky blue
    "#E69F00", # orange
    "#000000", # black
    "#999999"  # gray
  )
  n <- as.integer(n %||% length(base))
  if (n <= length(base)) return(base[seq_len(n)])
  rep_len(base, n)
}

.plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "top"
    )
}

.strip_fill_scales <- function(p) {
  if (!inherits(p, "ggplot")) return(p)
  if (is.null(p$scales) || !length(p$scales$scales)) return(p)
  keep <- vapply(p$scales$scales, function(s) {
    !("fill" %in% (s$aesthetics %||% character()))
  }, logical(1))
  p$scales$scales <- p$scales$scales[keep]
  p
}

.safe_ggplotly <- function(p) {
  tryCatch(
    plotly::ggplotly(p),
    error = function(e) {
      p2 <- .strip_fill_scales(p)
      tryCatch(plotly::ggplotly(p2), error = function(e2) p2)
    }
  )
}

.wrap_plotly <- function(p) {
  if (requireNamespace("plotly", quietly = TRUE)) {
    if (is.list(p) && !inherits(p, "ggplot")) {
      # List of plots - wrap each, preserve class
      result <- lapply(p, function(plt) {
        if (inherits(plt, "ggplot")) .safe_ggplotly(plt) else plt
      })
      # Preserve original class attributes
      class(result) <- class(p)
      result
    } else if (inherits(p, "ggplot")) {
      # Single ggplot - wrap it
      .safe_ggplotly(p)
    } else {
      # Not a ggplot, return as-is
      p
    }
  } else {
    # plotly not available - return original
    p
  }
}

#' Coerce fit object to standardized data frame
#'
#' Converts various fit object formats (vector, matrix, data.frame) to a

#' standardized data.frame with columns: estimate, lower, upper, and id.
#' Used by plot methods to ensure consistent input handling.
#'
#' @param fit A fit object: vector, matrix, or data.frame.
#' @param n_pred Optional expected number of prediction rows for validation.
#' @param probs Optional vector of probability levels (for QTE-style objects).
#' @return A data.frame with columns: estimate, lower, upper, id (and optionally index).
#' @keywords internal
#' @noRd
.coerce_fit_df <- function(fit, n_pred = NULL, probs = NULL) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # Case 1: Already a data.frame

  if (is.data.frame(fit)) {
    df <- fit
    # Ensure required columns exist
    if (!"estimate" %in% names(df)) {
      # Try to find a suitable column
      if ("fit" %in% names(df)) {
        df$estimate <- df$fit
      } else if (ncol(df) >= 1 && is.numeric(df[[1]])) {
        df$estimate <- df[[1]]
      } else {
        df$estimate <- NA_real_
      }
    }
    if (!"lower" %in% names(df)) df$lower <- NA_real_
    if (!"upper" %in% names(df)) df$upper <- NA_real_
    if (!"id" %in% names(df)) {
      df$id <- seq_len(nrow(df))
    }
    return(df)
  }

  # Case 2: Matrix (rows = observations, cols = quantiles or estimate/lower/upper)
  if (is.matrix(fit)) {
    nr <- nrow(fit)
    nc <- ncol(fit)
    cn <- colnames(fit)

    # Check if columns are named estimate/lower/upper
    if (!is.null(cn) && all(c("estimate", "lower", "upper") %in% cn)) {
      df <- as.data.frame(fit)
      df$id <- seq_len(nr)
      return(df)
    }

    # If probs provided, assume matrix is n_pred x length(probs) of estimates
    if (!is.null(probs) && nc == length(probs)) {
      # Expand to long format: id x index
      df <- data.frame(
        id = rep(seq_len(nr), times = nc),
        index = rep(probs, each = nr),
        estimate = as.vector(fit),
        lower = NA_real_,
        upper = NA_real_
      )
      return(df)
    }

    # Default: treat first column as estimate
    df <- data.frame(
      id = seq_len(nr),
      estimate = fit[, 1],
      lower = if (nc >= 2) fit[, 2] else NA_real_,
      upper = if (nc >= 3) fit[, 3] else NA_real_
    )
    return(df)
  }

  # Case 3: Numeric vector -> single-row or multi-row df
  if (is.numeric(fit)) {
    n <- length(fit)
    df <- data.frame(
      id = seq_len(n),
      estimate = fit,
      lower = NA_real_,
      upper = NA_real_
    )
    return(df)
  }

  # Fallback: error

  stop("Cannot coerce fit to data.frame: unsupported type.", call. = FALSE)
}

#' Nimble helpers
#'
#' @keywords internal
#' @noRd
#' @importFrom nimble nimNumeric
NULL

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

# Deterministic stick-breaking map used by NIMBLE code generation.
stick_breaking <- nimble::nimbleFunction(
  run = function(v = double(1)) {
    returnType(double(1))
    K <- length(v) + 1L
    w <- numeric(K)
    remainder <- 1
    for (j in 1:(K - 1L)) {
      w[j] <- v[j] * remainder
      remainder <- remainder * (1 - v[j])
    }
    w[K] <- remainder
    return(w)
  }
)


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


#'  Get epsilon value from object spec/meta or argument
#'
#' @param object A mixgpd_fit object.
#' @param epsilon Numeric; if provided, overrides object spec/meta.
#' @keywords internal
.get_epsilon <- function(object, epsilon = NULL) {
  if (!is.null(epsilon)) return(as.numeric(epsilon)[1])
  spec <- object$spec %||% list()
  meta <- spec$meta %||% list()
  as.numeric(object$epsilon %||% meta$epsilon %||% 0.025)
}

#' Truncate component draws in a draws matrix
#' @param object A mixgpd_fit object.
#' @param mat Numeric matrix of draws (iter x parameters).
#' @param epsilon Numeric in [0,1). Truncation level.
#' @return Numeric matrix with truncated components.
#' @keywords internal
.truncate_draws_matrix_components <- function(object, mat, epsilon) {
  if (!is.numeric(epsilon) || length(epsilon) != 1L || is.na(epsilon) || epsilon < 0 || epsilon >= 1) {
    stop("epsilon must be a single number in [0, 1).", call. = FALSE)
  }

  spec <- object$spec %||% list()
  meta <- spec$meta %||% list()
  backend <- meta$backend %||% spec$dispatch$backend %||% "<unknown>"
  kernel  <- meta$kernel  %||% spec$kernel$key %||% "<unknown>"

  kdef <- get_kernel_registry()[[kernel]]
  if (is.null(kdef)) stop("Kernel not found in registry: ", kernel, call. = FALSE)
  bulk_params <- kdef$bulk_params %||% character(0)

  cn <- colnames(mat)

  .indexed_block <- function(mat0, base, K = NULL, allow_missing = FALSE) {
    cn0 <- colnames(mat0)
    pat <- paste0("^", base, "\\[([0-9]+)\\]$")
    hit <- grepl(pat, cn0)
    if (!any(hit)) {
      if (isTRUE(allow_missing)) return(NULL)
      stop(sprintf("No indexed columns found for '%s[i]'.", base), call. = FALSE)
    }

    idx <- as.integer(sub(pat, "\\1", cn0[hit]))
    ord <- order(idx)
    idx <- idx[ord]
    cols <- cn0[hit][ord]

    if (is.null(K)) K <- max(idx, na.rm = TRUE)
    K <- as.integer(K)

    out <- matrix(0.0, nrow = nrow(mat0), ncol = K)
    for (j in seq_along(cols)) {
      k <- idx[j]
      if (!is.na(k) && k >= 1 && k <= K) out[, k] <- mat0[, cols[j]]
    }
    out
  }

  # ----- compute weights + component param matrices -----
  if (!("z[1]" %in% cn) && !any(grepl("^z\\[[0-9]+\\]$", cn))) {
    stop("Backend requires z[i] in samples to derive weights.", call. = FALSE)
  }
  Z <- .indexed_block(mat, "z")  # S x N
  storage.mode(Z) <- "integer"

  infer_K_from_bulk <- function() {
    if (length(bulk_params) < 1) return(NA_integer_)
    firstp <- bulk_params[1]
    idx <- as.integer(sub(paste0("^", firstp, "\\[([0-9]+)\\]$"), "\\1",
                          cn[grepl(paste0("^", firstp, "\\[[0-9]+\\]$"), cn)]))
    if (!length(idx)) return(NA_integer_)
    as.integer(max(idx, na.rm = TRUE))
  }

  infer_K_from_w <- function() {
    widx <- as.integer(sub("^w\\[([0-9]+)\\]$", "\\1", cn[grepl("^w\\[[0-9]+\\]$", cn)]))
    if (length(widx)) return(as.integer(max(widx, na.rm = TRUE)))
    widx <- as.integer(sub("^weights\\[([0-9]+)\\]$", "\\1", cn[grepl("^weights\\[[0-9]+\\]$", cn)]))
    if (length(widx)) return(as.integer(max(widx, na.rm = TRUE)))
    NA_integer_
  }

  if (identical(backend, "sb")) {
    K <- infer_K_from_w()
    if (!is.finite(K) || K < 1L) K <- infer_K_from_bulk()
    if (!is.finite(K) || K < 1L) stop("Could not infer K for SB weights.", call. = FALSE)
  } else if (identical(backend, "crp")) {
    K <- infer_K_from_bulk()
    if (!is.finite(K) || K < 1L) stop("Could not infer Kmax from component parameter draws.", call. = FALSE)
  } else {
    stop("Unknown backend: ", backend, call. = FALSE)
  }
  K <- as.integer(K)

  S <- nrow(mat)
  W <- matrix(0.0, nrow = S, ncol = K)
  for (s in 1:S) {
    z_s <- Z[s, ]
    z_s <- z_s[is.finite(z_s)]
    z_s <- z_s[z_s >= 1 & z_s <= K]
    if (length(z_s)) W[s, ] <- tabulate(z_s, nbins = K) / length(z_s)
  }

  bulk_draws <- list()
  for (nm in bulk_params) {
    blk <- .indexed_block(mat, nm, K = K, allow_missing = TRUE)
    if (!is.null(blk)) bulk_draws[[nm]] <- blk
  }
  bulk_params_present <- names(bulk_draws)

  # ----- per-draw truncation (sorted by weight; keep params aligned) -----
  S <- nrow(mat)
  ks <- integer(S)
  k_weight_vec <- integer(S)
  k_cum_vec <- integer(S)
  ords <- vector("list", S)
  w_list <- vector("list", S)
  p_list <- vector("list", S)

  for (s in 1:S) {
    params_s <- lapply(bulk_params_present, function(nm) as.numeric(bulk_draws[[nm]][s, ]))
    names(params_s) <- bulk_params_present
    tr <- .truncate_components_one_draw(w = as.numeric(W[s, ]), params = params_s, epsilon = epsilon)

    ks[s] <- tr$k
    k_weight_vec[s] <- tr$k_weight %||% tr$k
    k_cum_vec[s] <- tr$k_cum %||% tr$k
    ords[[s]] <- tr$ord
    w_list[[s]] <- tr$weights
    p_list[[s]] <- tr$params
  }

  # Fixed K across draws: keep only components selected in all draws
  Kt <- min(ks)
  if (!is.finite(Kt) || Kt < 1L) Kt <- 1L

  # ----- build new matrix: keep all NON-component columns + replace component blocks -----
  drop_pat <- c(
    "^weights\\[[0-9]+\\]$",
    "^w\\[[0-9]+\\]$",
    paste0("^", bulk_params, "\\[[0-9]+\\]$")
  )
  drop_hit <- rep(FALSE, length(cn))
  for (pp in drop_pat) drop_hit <- drop_hit | grepl(pp, cn)
  keep_cn <- cn[!drop_hit]

  out <- mat[, keep_cn, drop = FALSE]

  # add truncated weights + params (ranked by decreasing weight)
  w_out <- matrix(0.0, nrow = S, ncol = Kt)
  colnames(w_out) <- paste0("w[", seq_len(Kt), "]")

  out_params <- list()
  for (nm in bulk_params_present) {
    tmp <- matrix(NA_real_, nrow = S, ncol = Kt)
    colnames(tmp) <- paste0(nm, "[", seq_len(Kt), "]")
    out_params[[nm]] <- tmp
  }

  for (s in 1:S) {
    w_s <- w_list[[s]]
    k_s <- min(length(w_s), Kt)
    if (k_s < 1L) next
    w_out[s, seq_len(k_s)] <- w_s[seq_len(k_s)]
    for (nm in bulk_params_present) {
      out_params[[nm]][s, seq_len(k_s)] <- p_list[[s]][[nm]][seq_len(k_s)]
    }
  }

  out <- cbind(out, w_out)
  for (nm in bulk_params_present) out <- cbind(out, out_params[[nm]])

  attr(out, "truncation") <- list(
    k = ks,
    k_weight = k_weight_vec,
    k_cum = k_cum_vec,
    Kt = Kt,
    epsilon = epsilon
  )

  out
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
.extract_draws_matrix <- function(object, drop_v = TRUE, epsilon = NULL) {
  smp <- .get_samples_mcmclist(object)
  mat <- do.call(rbind, lapply(smp, function(ch) as.matrix(ch)))
  if (is.null(colnames(mat))) stop("Draw matrix has no column names.", call. = FALSE)

  if (isTRUE(drop_v)) {
    cn <- colnames(mat)
    keep <- !(grepl("^v\\[", cn) | cn == "v")
    mat <- mat[, keep, drop = FALSE]
  }

  eps <- .get_epsilon(object, epsilon)
  mat <- .truncate_draws_matrix_components(object, mat, eps)

  mat
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
.extract_draws <- function(object, pars = NULL, chains = c("stack", "first"), epsilon = NULL) {
  .validate_fit(object)
  chains <- match.arg(chains)

  smp <- .get_samples_mcmclist(object)

  mats <- lapply(smp, function(ch) {
    m <- as.matrix(ch)
    storage.mode(m) <- "double"
    m
  })

  if (chains == "first") {
    mat <- mats[[1]]
  } else {
    # common columns only
    cn <- Reduce(intersect, lapply(mats, colnames))
    mats <- lapply(mats, function(m) m[, cn, drop = FALSE])
    mat <- do.call(rbind, mats)
  }

  # drop v's always
  cn0 <- colnames(mat)
  keep0 <- !(grepl("^v\\[", cn0) | cn0 == "v")
  mat <- mat[, keep0, drop = FALSE]

  eps <- .get_epsilon(object, epsilon)
  mat <- .truncate_draws_matrix_components(object, mat, eps)

  if (!is.null(pars)) {
    miss <- setdiff(pars, colnames(mat))
    if (length(miss)) stop("Unknown params: ", paste(miss, collapse = ", "), call. = FALSE)
    mat <- mat[, pars, drop = FALSE]
  }

  mat
}

#' Summarize truncation results from draws
#' @param object A mixgpd_fit.
#' @param epsilon Numeric; optional override.
#' @return List with k summary.
#' @keywords internal
.truncation_info <- function(object, epsilon = NULL) {
  mat <- .extract_draws(object, pars = NULL, chains = "stack", epsilon = epsilon)
  tr <- attr(mat, "truncation") %||% list()
  k <- tr$k %||% integer(0)
  k_weight <- tr$k_weight %||% integer(0)
  k_cum <- tr$k_cum %||% integer(0)
  if (!length(k)) {
    return(list(
      k_min = NA_integer_, k_median = NA_integer_, k_max = NA_integer_,
      k_weight_min = NA_integer_, k_weight_median = NA_integer_, k_weight_max = NA_integer_,
      k_cum_min = NA_integer_, k_cum_median = NA_integer_, k_cum_max = NA_integer_,
      Kt = NA_integer_
    ))
  }
  list(
    k_min = min(k),
    k_median = as.integer(stats::median(k)),
    k_max = max(k),
    k_weight_min = if (length(k_weight)) min(k_weight) else NA_integer_,
    k_weight_median = if (length(k_weight)) as.integer(stats::median(k_weight)) else NA_integer_,
    k_weight_max = if (length(k_weight)) max(k_weight) else NA_integer_,
    k_cum_min = if (length(k_cum)) min(k_cum) else NA_integer_,
    k_cum_median = if (length(k_cum)) as.integer(stats::median(k_cum)) else NA_integer_,
    k_cum_max = if (length(k_cum)) max(k_cum) else NA_integer_,
    Kt = tr$Kt %||% max(k)
  )
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

  eps <- .get_epsilon(x, epsilon = NULL)
  lines <- c(
    sprintf("MixGPD fit | backend: %s | kernel: %s | GPD tail: %s",
            .backend_label(backend), .kernel_label(kernel), gpd_txt),
    sprintf("n = %s | components = %s | epsilon = %s",
            ifelse(is.na(n), "<unknown>", n),
            ifelse(is.na(meta$components %||% NA_integer_), "<unknown>", meta$components),
            ifelse(is.na(eps), "<unknown>", eps))
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

#' Summarize posterior draws for selected parameters
#'
#' @param object mixgpd_fit
#' @param pars character vector; if NULL uses all non-v parameters
#' @param probs quantiles to report
#' @return data.frame with mean/sd/quantiles + ess/rhat where available
#' @keywords internal
.summarize_posterior <- function(object, pars = NULL, probs = c(0.025, 0.5, 0.975)) {
  stopifnot(inherits(object, "mixgpd_fit"))

  if (!requireNamespace("coda", quietly = TRUE)) stop("Need 'coda'.", call. = FALSE)

  mat <- .extract_draws(object, pars = NULL, chains = "stack", epsilon = NULL)
  eps <- .get_epsilon(object, epsilon = NULL)

  if (is.null(pars)) {
    pars <- colnames(mat)

    spec <- object$spec %||% list()
    plan <- spec$plan %||% list()
    bulk <- plan$bulk %||% list()
    gpd <- plan$gpd %||% list()

    cn <- pars
    keep <- cn %in% "alpha"
    keep <- keep | grepl("^w\\[[0-9]+\\]$", cn)

    for (nm in names(bulk)) {
      ent <- bulk[[nm]] %||% list()
      mode <- ent$mode %||% NA_character_
      if (identical(mode, "link")) {
        keep <- keep | grepl(paste0("^beta_", nm, "\\["), cn)
      } else {
        keep <- keep | grepl(paste0("^", nm, "\\[[0-9]+\\]$"), cn)
      }
    }

    if (!is.null(gpd$threshold)) {
      thr_mode <- gpd$threshold$mode %||% NA_character_
      if (identical(thr_mode, "link")) {
        keep <- keep | grepl("^beta_threshold\\[", cn)
        if (!is.null(gpd$threshold$link_dist) &&
            identical(gpd$threshold$link_dist$dist, "lognormal")) {
          keep <- keep | cn == "sdlog_u"
        }
      } else {
        keep <- keep | cn == "threshold" | grepl("^threshold\\[[0-9]+\\]$", cn)
      }
    }

    if (!is.null(gpd$tail_scale)) {
      ts_mode <- gpd$tail_scale$mode %||% NA_character_
      if (identical(ts_mode, "link")) {
        keep <- keep | grepl("^beta_tail_scale\\[", cn)
      } else if (ts_mode %in% c("dist", "fixed")) {
        keep <- keep | cn == "tail_scale"
      }
    }

    if (!is.null(gpd$tail_shape)) {
      keep <- keep | cn == "tail_shape"
    }

    pars <- cn[keep]
    mat <- mat[, pars, drop = FALSE]
  } else {
    pars <- gsub("^weight\\[", "w[", pars)
    miss <- setdiff(pars, colnames(mat))
    if (length(miss)) stop("Unknown params: ", paste(miss, collapse = ", "), call. = FALSE)
    mat <- mat[, pars, drop = FALSE]
  }
  wpars <- pars[grepl("^w\\[[0-9]+\\]$", pars)]
  if (length(wpars)) {
    pars <- c(wpars, setdiff(pars, wpars))
    mat <- mat[, pars, drop = FALSE]
  }
  if (length(wpars) && is.finite(eps) && eps > 0) {
    wmat <- mat[, wpars, drop = FALSE]
    wmat[wmat < eps] <- NA_real_
    keep_w <- apply(wmat, 2, function(v) any(is.finite(v)))
    w_keep <- wpars[keep_w]
    if (length(w_keep)) {
      mat[, w_keep] <- wmat[, w_keep, drop = FALSE]
      pars <- c(w_keep, setdiff(pars, wpars))
      mat <- mat[, pars, drop = FALSE]
    } else {
      pars <- setdiff(pars, wpars)
      mat <- mat[, pars, drop = FALSE]
    }
  }

  thr_cols <- grep("^threshold\\[[0-9]+\\]$", colnames(mat), value = TRUE)
  if (length(thr_cols) >= 1) {
    thr_vec <- if (length(thr_cols) == 1) {
      as.numeric(mat[, thr_cols[1]])
    } else {
      rowMeans(mat[, thr_cols, drop = FALSE], na.rm = TRUE)
    }
    mat <- mat[, setdiff(colnames(mat), thr_cols), drop = FALSE]
    mat <- cbind(mat, threshold = thr_vec)
    pars <- c(setdiff(pars, thr_cols), "threshold")
  }

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

  ess_vec <- rep(NA_real_, ncol(mat))
  for (j in seq_len(ncol(mat))) {
    v <- mat[, j]
    v <- v[is.finite(v)]
    if (length(v) >= 3L) {
      ess_vec[j] <- as.numeric(coda::effectiveSize(coda::mcmc(v)))
    }
  }
  names(ess_vec) <- colnames(mat)
  out$ess <- as.numeric(ess_vec[out$parameter])
  out$parameter <- sub("^w\\[", "weights[", out$parameter)

  rownames(out) <- NULL
  out
}

#' Resolve kernel dispatch functions (scalar)
#' Dispatch returns raw scalar nimbleFunctions for codegen; do not wrap.
#' @param spec_or_fit mixgpd_fit or spec list
#' @return List with d/p/q/r functions and bulk_params.
#' @keywords internal
.get_dispatch_scalar <- function(spec_or_fit, backend_override = NULL) {
  spec <- spec_or_fit
  if (inherits(spec_or_fit, "mixgpd_fit")) {
    spec <- spec_or_fit$spec %||% list()
  }

  meta <- spec$meta %||% list()
  backend <- meta$backend %||% spec$dispatch$backend %||% "<unknown>"
  if (!is.null(backend_override)) backend <- backend_override
  kernel <- meta$kernel %||% spec$kernel$key %||% "<unknown>"
  GPD <- isTRUE(meta$GPD %||% spec$dispatch$GPD)

  kdef <- get_kernel_registry()[[kernel]]
  if (is.null(kdef)) stop(sprintf("Kernel '%s' not found in registry.", kernel), call. = FALSE)
  if (isTRUE(GPD) && isFALSE(kdef$allow_gpd)) stop(sprintf("Kernel '%s' does not allow GPD.", kernel), call. = FALSE)

  backend_key <- match.arg(backend, choices = c("sb", "crp"))
  dispatch <- kdef[[backend_key]]
  if (is.null(dispatch)) {
    stop(sprintf("Missing %s dispatch in kernel registry.", backend_key), call. = FALSE)
  }

  d_name <- if (isTRUE(GPD)) {
    dispatch$d_gpd
  } else {
    dispatch$d %||% dispatch$d_base
  }
  if (is.na(d_name) || !nzchar(d_name)) {
    stop(sprintf("Missing %s dispatch for kernel '%s'.", backend_key, kernel), call. = FALSE)
  }

  p_name <- sub("^d", "p", d_name)
  q_name <- sub("^d", "q", d_name)
  r_name <- sub("^d", "r", d_name)

  ns_pkg <- asNamespace("DPmixGPD")
  ns_stats <- asNamespace("stats")
  ns_nimble <- asNamespace("nimble")

  .resolve_fun <- function(fname, kernel) {
    # Try PascalCase NIMBLE function first (for NIMBLE model code)
    if (exists(fname, envir = ns_pkg, inherits = FALSE)) {
      return(get(fname, envir = ns_pkg))
    }
    if (exists(fname, envir = ns_stats, inherits = FALSE)) {
      return(get(fname, envir = ns_stats))
    }
    if (exists(fname, envir = ns_nimble, inherits = FALSE)) {
      return(get(fname, envir = ns_nimble))
    }
    # Fallback for predictions: use lowercase R wrappers if PascalCase NIMBLE function
    # not found (e.g., in some build contexts). Lowercase wrappers are vectorized
    # and work for predictions but should not be used in NIMBLE model code.
    fname_lower <- tolower(fname)
    if (exists(fname_lower, envir = ns_pkg, inherits = FALSE)) {
      return(get(fname_lower, envir = ns_pkg))
    }
    stop(sprintf("Missing function '%s' for kernel '%s'.", fname, kernel), call. = FALSE)
  }

  d_fun <- .wrap_density_fun(.resolve_fun(d_name, kernel))
  p_fun <- .wrap_cdf_fun(.resolve_fun(p_name, kernel))
  q_fun <- .wrap_quantile_fun(.resolve_fun(q_name, kernel))
  r_fun <- .wrap_rng_fun(.resolve_fun(r_name, kernel))

  if (isTRUE(attr(d_fun, "vectorized_wrapper")) ||
      isTRUE(attr(p_fun, "vectorized_wrapper")) ||
      isTRUE(attr(q_fun, "vectorized_wrapper")) ||
      isTRUE(attr(r_fun, "vectorized_wrapper"))) {
    stop("Scalar dispatch unexpectedly received vectorized wrappers.", call. = FALSE)
  }

  list(d = d_fun, p = p_fun, q = q_fun, r = r_fun, bulk_params = kdef$bulk_params)
}

#' Resolve kernel dispatch functions
#' Dispatch returns vector-aware d/p/q and n-aware r via wrappers; do not mutate namespace.
#' @param spec_or_fit mixgpd_fit or spec list
#' @return List with d/p/q/r functions and bulk_params.
#' @keywords internal
.get_dispatch <- function(spec_or_fit, backend_override = NULL) {
  scalar <- .get_dispatch_scalar(spec_or_fit, backend_override = backend_override)
  list(
    d = .wrap_scalar_first_arg(scalar$d, "x"),
    p = .wrap_scalar_p(scalar$p),
    q = .wrap_scalar_first_arg(scalar$q, "p"),
    r = .wrap_scalar_r(scalar$r),
    bulk_params = scalar$bulk_params
  )
}

#' Compute credible or HPD interval from posterior draws
#'
#' Dispatches to either equal-tailed quantile intervals or highest posterior
#' density (HPD) intervals using \code{coda::HPDinterval()}.
#'
#' @param draws Numeric vector of posterior draws.
#' @param level Numeric; credible level (e.g., 0.95 for 95 percent interval).
#' @param type Character; \code{"credible"} for equal-tailed quantile intervals,
#'   \code{"hpd"} for highest posterior density intervals.
#' @return Named numeric vector with \code{lower} and \code{upper}.
#' @keywords internal
#' @noRd
.compute_interval <- function(draws, level = 0.95, type = c("credible", "hpd")) {
  type <- match.arg(type)
  draws <- draws[is.finite(draws)]
  if (length(draws) < 2L) {
    return(c(lower = NA_real_, upper = NA_real_))
  }

  if (type == "credible") {
    probs <- c((1 - level) / 2, (1 + level) / 2)
    q <- stats::quantile(draws, probs = probs, na.rm = TRUE)
    c(lower = unname(q[1]), upper = unname(q[2]))
  } else {
    # HPD via coda
    if (!requireNamespace("coda", quietly = TRUE)) {
      stop("Package 'coda' is required for HPD intervals.", call. = FALSE)
    }
    hpd <- coda::HPDinterval(coda::as.mcmc(draws), prob = level)
    c(lower = hpd[1, "lower"], upper = hpd[1, "upper"])
  }
}

#' Summarize posterior draws (mean + quantiles)
#' @param draws Numeric vector, matrix, or array with draws in last dimension.
#' @param probs Numeric quantile probs.
#' @param interval Character or NULL; \code{NULL} for no interval,
#'   \code{"credible"} for equal-tailed quantile intervals (default),
#'   \code{"hpd"} for highest posterior density intervals.
#' @return List with estimate, lower, upper, and q.
#' @keywords internal
.posterior_summarize <- function(draws, probs = c(0.025, 0.5, 0.975),
                                 interval = "credible") {
  # Handle NULL interval (no interval computation)
  if (is.null(interval)) {
    interval <- "none"
  } else {
    interval <- match.arg(interval, choices = c("credible", "hpd"))
  }
  probs <- as.numeric(probs)

  # Compute credible level from probs (for HPD)
  level <- probs[length(probs)] - probs[1]
  if (!is.finite(level) || level <= 0 || level >= 1) level <- 0.95

  # Helper to compute intervals for a row of draws
  .row_interval <- function(row) {
    if (interval == "none") {
      c(NA_real_, NA_real_)
    } else if (interval == "credible") {
      q <- stats::quantile(row, probs = probs, na.rm = TRUE, names = FALSE)
      c(q[1], q[length(probs)])
    } else {
      iv <- .compute_interval(row, level = level, type = "hpd")
      c(iv["lower"], iv["upper"])
    }
  }

  if (is.null(dim(draws))) {
    mat <- matrix(as.numeric(draws), nrow = 1)
    qmat <- t(apply(mat, 1, stats::quantile, probs = probs, na.rm = TRUE, names = FALSE))
    iv <- .row_interval(as.numeric(draws))
    return(list(
      estimate = rowMeans(mat, na.rm = TRUE),
      lower = iv[1],
      upper = iv[2],
      q = qmat
    ))
  }

  dims <- dim(draws)
  if (length(dims) == 2L) {
    qmat <- t(apply(draws, 1, stats::quantile, probs = probs, na.rm = TRUE, names = FALSE))
    ivmat <- t(apply(draws, 1, .row_interval))
    return(list(
      estimate = rowMeans(draws, na.rm = TRUE),
      lower = ivmat[, 1],
      upper = ivmat[, 2],
      q = qmat
    ))
  }

  Sdim <- dims[length(dims)]
  mat <- matrix(draws, nrow = prod(dims[-length(dims)]), ncol = Sdim)
  qmat <- t(apply(mat, 1, stats::quantile, probs = probs, na.rm = TRUE, names = FALSE))
  ivmat <- t(apply(mat, 1, .row_interval))
  estimate <- rowMeans(mat, na.rm = TRUE)
  lower <- ivmat[, 1]
  upper <- ivmat[, 2]
  dim(estimate) <- dims[-length(dims)]
  dim(lower) <- dims[-length(dims)]
  dim(upper) <- dims[-length(dims)]
  list(estimate = estimate, lower = lower, upper = upper, q = qmat)
}

#' Detect the first present argument name in dots.
#' @keywords internal
.detect_first_present <- function(dots, candidates = c("q", "x")) {
  for (nm in candidates) {
    if (!is.null(dots[[nm]])) return(nm)
  }
  stop("Expected one of: ", paste(candidates, collapse = ", "), call. = FALSE)
}

#' Wrap scalar first-argument functions to handle vector inputs.
#' @keywords internal
.wrap_scalar_first_arg <- function(fun, first_arg_name) {
  if (isTRUE(attr(fun, "vectorized_wrapper"))) return(fun)
  force(fun)
  force(first_arg_name)
  wrapper <- function(...) {
    dots <- list(...)
    if (!first_arg_name %in% names(dots)) {
      stop("Missing required argument: ", first_arg_name, call. = FALSE)
    }
    vec <- dots[[first_arg_name]]
    if (length(vec) <= 1L) return(do.call(fun, dots))

    dots[[first_arg_name]] <- vec[1]
    one <- do.call(fun, dots)
    if (length(one) <= 1L) {
      return(vapply(vec, function(v) {
        dots[[first_arg_name]] <- v
        do.call(fun, dots)
      }, numeric(1)))
    }

    mat <- vapply(vec, function(v) {
      dots[[first_arg_name]] <- v
      as.numeric(do.call(fun, dots))
    }, numeric(length(one)))
    t(mat)
  }
  attr(wrapper, "vectorized_wrapper") <- TRUE
  wrapper
}

#' Wrap scalar CDF to handle q/x naming and vector inputs.
#' @keywords internal
.wrap_scalar_p <- function(fun) {
  if (isTRUE(attr(fun, "vectorized_wrapper"))) return(fun)
  force(fun)
  wrapper <- function(...) {
    dots <- list(...)
    given <- .detect_first_present(dots, candidates = c("q", "x"))

    formal_names <- names(formals(fun)) %||% character()
    target <- if ("q" %in% formal_names && !"x" %in% formal_names) {
      "q"
    } else if ("x" %in% formal_names && !"q" %in% formal_names) {
      "x"
    } else {
      given
    }

    if (!identical(given, target)) {
      dots[[target]] <- dots[[given]]
      dots[[given]] <- NULL
    }

    vec <- dots[[target]]
    if (length(vec) <= 1L) return(do.call(fun, dots))

    dots[[target]] <- vec[1]
    one <- do.call(fun, dots)
    if (length(one) <= 1L) {
      return(vapply(vec, function(v) {
        dots[[target]] <- v
        do.call(fun, dots)
      }, numeric(1)))
    }

    mat <- vapply(vec, function(v) {
      dots[[target]] <- v
      as.numeric(do.call(fun, dots))
    }, numeric(length(one)))
    t(mat)
  }
  attr(wrapper, "vectorized_wrapper") <- TRUE
  wrapper
}

#' Wrap scalar RNG to handle n > 1.
#' @keywords internal
.wrap_scalar_r <- function(fun) {
  if (isTRUE(attr(fun, "vectorized_wrapper"))) return(fun)
  force(fun)
  wrapper <- function(...) {
    dots <- list(...)
    if (!("n" %in% names(dots))) stop("Missing required argument: n", call. = FALSE)
    n <- as.integer(dots$n)
    dots$n <- NULL
    if (is.na(n) || n < 0L) stop("n must be a non-negative integer.", call. = FALSE)
    if (n == 0L) {
      one <- do.call(fun, c(list(n = 1L), dots))
      if (length(one) <= 1L) return(numeric(0))
      return(matrix(numeric(0), nrow = 0, ncol = length(one)))
    }
    if (n == 1L) return(do.call(fun, c(list(n = 1L), dots)))

    one <- do.call(fun, c(list(n = 1L), dots))
    if (length(one) <= 1L) {
      return(vapply(seq_len(n), function(i) {
        do.call(fun, c(list(n = 1L), dots))
      }, numeric(1)))
    }

    mat <- vapply(seq_len(n), function(i) {
      as.numeric(do.call(fun, c(list(n = 1L), dots)))
    }, numeric(length(one)))
    t(mat)
  }
  attr(wrapper, "vectorized_wrapper") <- TRUE
  wrapper
}

#' Truncate and reorder mixture components by cumulative weight mass
#'
#' @param w Numeric vector of component weights (length K).
#' @param params Named list of numeric vectors, each length K (component-specific params).
#' @param epsilon Numeric in [0,1). Keep the smallest k s.t. cumweight >= 1-epsilon.
#' @return A list with reordered+truncated weights/params and bookkeeping.
#' @keywords internal
.truncate_components_one_draw <- function(w, params, epsilon = 0.01) {
  stopifnot(is.numeric(w), length(w) >= 1L)
  if (!is.numeric(epsilon) || length(epsilon) != 1L || is.na(epsilon) || epsilon < 0 || epsilon >= 1) {
    stop("epsilon must be a single number in [0, 1).", call. = FALSE)
  }
  if (!is.list(params) || length(params) == 0L) params <- list()

  K <- length(w)

  # Validate params lengths
  for (nm in names(params)) {
    v <- params[[nm]]
    if (!is.numeric(v) || length(v) != K) {
      stop("params[['", nm, "']] must be numeric and length K (= length(w)).", call. = FALSE)
    }
  }

  # Sort by decreasing weight
  ord <- order(w, decreasing = TRUE)
  w_sorted <- w[ord]
  params_sorted <- lapply(params, function(v) v[ord])

  # Two criteria:
  # (1) per-component minimum weight >= epsilon
  # (2) cumulative mass >= 1 - epsilon
  # Keep the smaller k that satisfies either criterion.
  keep_idx <- which(w_sorted >= epsilon)
  k_weight <- if (length(keep_idx)) max(keep_idx) else 0L

  cw <- cumsum(w_sorted)
  k_cum <- which(cw >= (1 - epsilon))[1]
  if (is.na(k_cum)) k_cum <- K

  k_keep <- min(k_weight, k_cum)
  if (!is.finite(k_keep) || k_keep < 1L) k_keep <- 1L

  keep <- seq_len(k_keep)
  w_keep <- w_sorted[keep]

  # Adjust the smallest kept weight to make the kept weights sum to 1
  if (length(w_keep) > 1L) {
    min_idx <- which.min(w_keep)
    w_keep[min_idx] <- w_keep[min_idx] + (1 - sum(w_keep))
  } else {
    w_keep[1] <- 1
  }

  s <- sum(w_keep)
  if (!is.finite(s) || s <= 0) stop("Invalid weight sum after truncation.", call. = FALSE)

  params_keep <- lapply(params_sorted, function(v) v[keep])

  list(
    k = length(keep),
    k_weight = k_weight,
    k_cum = k_cum,
    ord = ord,
    weights = w_keep,
    params = params_keep
  )
}


#' Internal prediction engine: evaluate per posterior draw, then summarize.
#'
#' Project rules:
#' - density/survival: either provide both (x,y) or neither (defaults to training X and training y).
#' - quantile/sample/mean: y must be NULL; x may be provided (new X) or NULL (defaults to training X).
#' - CRP predictions use posterior weights derived from z for each draw.
#' - Stores per-draw results in object$cache$predict (environment) for reuse in treatment effects.
#'
#' @keywords internal
.predict_mixgpd <- function(object,
                            x = NULL, y = NULL, ps = NULL,
                            type = c("density", "survival", "quantile", "sample", "mean", "rmean", "median", "fit"),
                            p = NULL, index = NULL, nsim = NULL,
                            cred.level = 0.95,
                            interval = "credible",
                            probs = c(0.025, 0.5, 0.975),
                            store_draws = TRUE,
                            nsim_mean = 200L,
                            cutoff = NULL,
                            ncores = 1L) {

  .validate_fit(object)
  type <- match.arg(type)

  # Handle interval: NULL means no interval
  compute_interval <- TRUE
  if (is.null(interval)) {
    compute_interval <- FALSE
    interval <- "credible"
  } else {
    interval <- match.arg(interval, choices = c("credible", "hpd"))
  }

  ncores <- as.integer(ncores)
  if (is.na(ncores) || ncores < 1L) stop("'ncores' must be an integer >= 1.", call. = FALSE)

  # Spec / meta
  spec <- object$spec %||% list()
  meta <- spec$meta %||% list()

  backend <- meta$backend %||% spec$dispatch$backend %||% "<unknown>"
  kernel  <- meta$kernel  %||% spec$kernel$key %||% "<unknown>"
  GPD     <- isTRUE(meta$GPD %||% spec$dispatch$GPD)

  # Use mixture dispatch for prediction even when backend is CRP
  pred_backend <- if (identical(backend, "crp")) "sb" else backend

  # Training data
  Xtrain <- object$data$X %||% object$X %||% NULL
  ytrain <- object$data$y %||% object$y %||% NULL
  ps_train <- object$data$ps %||% NULL

  has_X <- isTRUE(meta$has_X %||% (!is.null(Xtrain)))

  # Validate X helper
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

  # Resolve MIX functions for kernel
  fns <- .get_dispatch(object, backend_override = pred_backend)
  bulk_params <- fns$bulk_params
  d_fun <- fns$d
  p_fun <- fns$p
  q_fun <- fns$q
  r_fun <- fns$r

  kdef <- get_kernel_registry()[[kernel]] %||% list()
  bulk_support <- kdef$bulk_support %||% list()

  # Link helper
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

  # -----------------------------
  # Resolve inputs by type (contract)
  # -----------------------------
  Xpred <- NULL
  ygrid <- NULL
  pgrid <- NULL

  if (type %in% c("density", "survival")) {
    if (has_X) {
      if (is.null(x) && is.null(y)) {
        if (is.null(Xtrain)) stop("Training X not found in fit object.", call. = FALSE)
        if (is.null(ytrain)) stop("Training y not found in fit object.", call. = FALSE)
        Xpred <- Xtrain
        ygrid <- ytrain
      } else if (!is.null(x) && !is.null(y)) {
        Xpred <- x
        ygrid <- y
      } else {
        stop("For type='density'/'survival' with X, provide BOTH 'x' and 'y', or provide NEITHER to use training defaults.",
             call. = FALSE)
      }
    } else {
      if (!is.null(x)) stop("Unconditional model: 'x' is not allowed.", call. = FALSE)
      ygrid <- y %||% ytrain
      if (is.null(ygrid)) stop("No 'y' provided and training y not found.", call. = FALSE)
    }

    if (!is.null(Xpred)) Xpred <- .validate_X_pred(Xpred, Xtrain)
    ygrid <- as.numeric(ygrid)
    if (anyNA(ygrid)) stop("Missing values (NA) found in 'y'.", call. = FALSE)
  }

  if (type %in% c("quantile", "median")) {
    if (!is.null(p)) index <- p
    if (type == "median" && is.null(index)) index <- 0.5
    if (is.null(index)) stop("For type='quantile'/'median', provide 'index' (or 'p').", call. = FALSE)
    pgrid <- as.numeric(index)
    if (anyNA(pgrid) || any(pgrid <= 0 | pgrid >= 1)) stop("'index' must be in (0,1).", call. = FALSE)

    if (has_X) {
      if (is.null(x)) {
        if (is.null(Xtrain)) stop("Training X not found in fit object.", call. = FALSE)
        Xpred <- Xtrain
      } else {
        Xpred <- .validate_X_pred(x, Xtrain)
      }
    } else {
      if (!is.null(x)) stop("Unconditional model: 'x' is not allowed for quantiles.", call. = FALSE)
    }
  }

  if (type %in% c("sample", "fit", "mean")) {
    if (has_X) {
      if (is.null(x)) {
        if (is.null(Xtrain)) stop("Training X not found in fit object.", call. = FALSE)
        Xpred <- Xtrain
      } else {
        Xpred <- .validate_X_pred(x, Xtrain)
      }
    } else {
      if (!is.null(x)) stop("Unconditional model: 'x' is not allowed.", call. = FALSE)
    }
  }

  # -----------------------------
  # Posterior draws extraction
  # -----------------------------
  draw_mat <- .extract_draws_matrix(object)
  if (is.null(draw_mat) || !is.matrix(draw_mat) || nrow(draw_mat) < 2L) {
    stop("Posterior draws not found or malformed in fitted object.", call. = FALSE)
  }
  S <- nrow(draw_mat)

  # mixture weights + bulk parameter blocks
  # - W_draws: S x K
  # - bulk_draws: list(param -> S x K or S x 1 or S x ?)
  # - base_params: names of bulk params required by kernel
  W_draws <- .extract_weights(draw_mat, backend = pred_backend)
  bulk_draws <- .extract_bulk_params(draw_mat, bulk_params = bulk_params)
  base_params <- names(bulk_draws)

  # Prediction dimensions
  n_pred <- if (!is.null(Xpred)) nrow(Xpred) else 1L

  # -----------------------------
  # Optional PS covariate (used only if model expects it)
  # -----------------------------
  if (!is.null(ps)) {
    ps <- as.numeric(ps)
    if (anyNA(ps)) stop("Missing values (NA) found in 'ps'.", call. = FALSE)
    if (has_X && length(ps) != n_pred) stop("Length of 'ps' must equal nrow(x).", call. = FALSE)
    if (!has_X && length(ps) != 1L) stop("Unconditional model: 'ps' must be scalar if provided.", call. = FALSE)
  }

  # -----------------------------
  # Link-mode parameter handling (conditional)
  # -----------------------------
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
  P <- if (!is.null(Xpred)) ncol(Xpred) else 0L

  .compute_link_eta <- function(s) {
    if (!length(link_params)) return(list())
    out <- list()
    for (nm in link_params) {
      plan <- link_plan[[nm]] %||% list()
      mode <- plan$mode %||% "constant"
      if (identical(mode, "link")) {
        if (!has_X) stop("link-mode requires X.", call. = FALSE)
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
          eta_mat <- Xpred %*% t(beta_mat)
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
              eta_mat <- Xpred %*% t(beta_mat)
              out[[nm]] <- .apply_link(eta_mat, link, pw)
            } else {
              beta_nm <- .indexed_block(draw_mat, paste0("beta_", nm), K = P) # S x P
              eta <- as.numeric(Xpred %*% beta_nm[s, ])
              out[[nm]] <- matrix(as.numeric(.apply_link(eta, link, pw)), nrow = n_pred)
            }
          } else {
            beta_nm <- .indexed_block(draw_mat, paste0("beta_", nm), K = P) # S x P
            eta <- as.numeric(Xpred %*% beta_nm[s, ])
            out[[nm]] <- matrix(as.numeric(.apply_link(eta, link, pw)), nrow = n_pred)
          }
        }
      } else {
        # constant (scalar per draw)
        if (!(nm %in% colnames(draw_mat))) stop(sprintf("'%s' not found in posterior draws.", nm), call. = FALSE)
        out[[nm]] <- matrix(rep(as.numeric(draw_mat[s, nm]), n_pred), nrow = n_pred)
      }
    }
    out
  }

  # -----------------------------
  # GPD tail plan extraction (if enabled)
  # -----------------------------
  tail_shape <- NULL
  threshold_mat <- NULL
  threshold_scalar <- NULL
  tail_scale <- NULL

  if (GPD) {
    if (!("tail_shape" %in% colnames(draw_mat))) stop("tail_shape not found in posterior draws.", call. = FALSE)
    tail_shape <- as.numeric(draw_mat[, "tail_shape"])

    gpd_plan <- spec$dispatch$gpd %||% meta$gpd %||% list()

    # threshold
    thr_mode <- gpd_plan$threshold$mode %||% "constant"
    if (identical(thr_mode, "link")) {
      if (!has_X) stop("threshold link-mode requires X.", call. = FALSE)
      beta_thr <- .indexed_block(draw_mat, "beta_threshold", K = P)  # S x P
      threshold_mat <- matrix(NA_real_, nrow = S, ncol = n_pred)
      thr_link <- gpd_plan$threshold$link %||% "exp"
      thr_power <- gpd_plan$threshold$link_power %||% NULL
      for (s in 1:S) {
        eta <- as.numeric(Xpred %*% beta_thr[s, ])
        threshold_mat[s, ] <- as.numeric(.apply_link(eta, thr_link, thr_power))
      }
    } else {
      # constant threshold
      thr_cols <- grep("^threshold(\\b|_)", colnames(draw_mat), value = TRUE)
      if (length(thr_cols) == 0L && "threshold" %in% colnames(draw_mat)) thr_cols <- "threshold"
      if (length(thr_cols) == 0L) stop("threshold not found in posterior draws.", call. = FALSE)
      if (length(thr_cols) == 1L) {
        threshold_scalar <- as.numeric(draw_mat[, thr_cols])
      } else {
        threshold_scalar <- rowMeans(draw_mat[, thr_cols, drop = FALSE], na.rm = TRUE)
      }
    }

    # tail scale
    has_beta_ts <- any(grepl("^beta_tail_scale\\[", colnames(draw_mat)))
    ts_mode <- gpd_plan$tail_scale$mode %||% if (has_beta_ts) "link" else "constant"
    if (identical(ts_mode, "link")) {
      if (!has_X) stop("tail_scale link-mode requires X.", call. = FALSE)
      beta_ts <- .indexed_block(draw_mat, "beta_tail_scale", K = P)  # S x P
      tail_scale <- matrix(NA_real_, nrow = S, ncol = n_pred)
      ts_link <- gpd_plan$tail_scale$link %||% "exp"
      ts_power <- gpd_plan$tail_scale$link_power %||% NULL
      for (s in 1:S) {
        eta <- as.numeric(Xpred %*% beta_ts[s, ])
        tail_scale[s, ] <- as.numeric(.apply_link(eta, ts_link, ts_power))
      }
    } else {
      if ("tail_scale" %in% colnames(draw_mat)) {
        tail_scale <- as.numeric(draw_mat[, "tail_scale"])
      } else if (!is.null(gpd_plan$tail_scale$value)) {
        tail_scale <- rep(as.numeric(gpd_plan$tail_scale$value), S)
      } else {
        stop("tail_scale not found in posterior draws.", call. = FALSE)
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

  # -----------------------------
  # Draw validation (NO silent repair)
  # -----------------------------
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

    if (GPD) {
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
      # validate threshold + scale (support)
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
    }
    .draw_valid[s] <- ok
  }

  n_valid <- sum(.draw_valid)
  if (n_valid == 0L) stop("All posterior draws are invalid for prediction (non-finite or out-of-support parameters).", call. = FALSE)

  # Parallel helper
  .lapply_draws <- function(FUN) {
    idx <- seq_len(S)
    if (ncores == 1L) return(lapply(idx, FUN))

    if (!requireNamespace("future.apply", quietly = TRUE) ||
        !requireNamespace("future", quietly = TRUE)) {
      stop("Packages 'future' and 'future.apply' are required for ncores > 1.", call. = FALSE)
    }

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = ncores)

    old_max <- getOption("future.globals.maxSize")
    on.exit(options(future.globals.maxSize = old_max), add = TRUE)
    options(future.globals.maxSize = Inf)

    future.apply::future_lapply(idx, FUN)
  }

  # -----------------------------
  # density / survival
  # -----------------------------
  if (type %in% c("density", "survival")) {
    G <- length(ygrid)
    ygrid_num <- as.numeric(ygrid)

    .one_draw <- function(s) {
      if (!.draw_valid[s]) return(list(valid = FALSE, out = matrix(NA_real_, nrow = n_pred, ncol = G)))

      args0 <- .build_args0_or_null(s)
      if (is.null(args0)) return(list(valid = FALSE, out = matrix(NA_real_, nrow = n_pred, ncol = G)))

      link_eta <- .compute_link_eta(s)

      out <- matrix(NA_real_, nrow = n_pred, ncol = G)
      for (i in seq_len(n_pred)) {
        args <- args0
        if (GPD) {
          args$threshold <- .threshold_at(s, i)
          args$tail_scale <- .tail_scale_at(s, i)
          if (!is.finite(args$threshold) || !is.finite(args$tail_scale) || args$tail_scale <= 0) {
            out[i, ] <- NA_real_
            next
          }
        }
        if (length(link_params)) {
          for (nm in link_params) {
            vv <- as.numeric(link_eta[[nm]][i, ])
            if (!all(is.finite(vv))) { out[i, ] <- NA_real_; next }
            args[[nm]] <- vv
          }
        }

        if (type == "density") {
          v <- as.numeric(do.call(d_fun, c(list(x = ygrid_num, log = 0L), args)))
          out[i, ] <- v
        } else {
          cdfv <- as.numeric(do.call(p_fun, c(list(q = ygrid_num, lower.tail = 1L, log.p = 0L), args)))
          # Clamp to [0,1] before survival transform
          cdfv <- pmin(pmax(cdfv, 0), 1)
          surv <- 1 - cdfv
          surv <- pmin(pmax(surv, 0), 1)
          out[i, ] <- surv
        }
      }
      list(valid = TRUE, out = out)
    }

    res_list <- .lapply_draws(.one_draw)

    draws_arr <- array(NA_real_, dim = c(S, n_pred, G))
    valid_vec <- logical(S)
    for (s in seq_len(S)) {
      valid_vec[s] <- isTRUE(res_list[[s]]$valid)
      draws_arr[s, , ] <- res_list[[s]]$out
    }

    fit <- apply(draws_arr, c(2, 3), mean, na.rm = TRUE)

    lower <- upper <- NULL
    if (compute_interval) {
      lower <- matrix(NA_real_, nrow = n_pred, ncol = G)
      upper <- matrix(NA_real_, nrow = n_pred, ncol = G)
      for (i in seq_len(n_pred)) {
        for (j in seq_len(G)) {
          iv <- .compute_interval(draws_arr[, i, j], level = cred.level, type = interval)
          lower[i, j] <- iv["lower"]
          upper[i, j] <- iv["upper"]
        }
      }
    }

    # Build DF
    result_list <- vector("list", n_pred * G)
    k <- 1L
    for (i in seq_len(n_pred)) {
      for (j in seq_len(G)) {
        result_list[[k]] <- list(
          id = i,
          y = ygrid_num[j],
          estimate = fit[i, j],
          lower = if (!is.null(lower)) lower[i, j] else NA_real_,
          upper = if (!is.null(upper)) upper[i, j] else NA_real_
        )
        k <- k + 1L
      }
    }
    fit_df <- do.call(rbind, lapply(result_list, as.data.frame))
    colnames(fit_df) <- c("id", "y", ifelse(type == "density", "density", "survival"), "lower", "upper")

    out <- list(
      fit = fit_df,
      type = type,
      grid = ygrid_num,
      diagnostics = list(
        n_draws_total = S,
        n_draws_valid = sum(valid_vec),
        n_draws_dropped = S - sum(valid_vec)
      )
    )
    class(out) <- "mixgpd_predict"
    return(out)
  }

  # -----------------------------
  # quantile / median
  # -----------------------------
  if (type %in% c("quantile", "median")) {
    M <- length(pgrid)

    if (!has_X) {
      draws_mat <- matrix(NA_real_, nrow = M, ncol = S)

      for (s in seq_len(S)) {
        if (!.draw_valid[s]) next
        args0 <- .build_args0_or_null(s)
        if (is.null(args0)) next

        if (GPD) {
          args0$threshold <- threshold_scalar[s]
          args0$tail_scale <- tail_scale[s]
          if (!is.finite(args0$threshold) || !is.finite(args0$tail_scale) || args0$tail_scale <= 0) next
        }

        draws_mat[, s] <- as.numeric(do.call(q_fun, c(list(p = pgrid), args0)))
      }

      summ <- .posterior_summarize(draws_mat, probs = probs, interval = if (compute_interval) interval else NULL)

      fit_df <- data.frame(
        estimate = as.numeric(summ$estimate),
        index    = pgrid,
        lower    = if (compute_interval) as.numeric(summ$lower) else NA_real_,
        upper    = if (compute_interval) as.numeric(summ$upper) else NA_real_,
        row.names = NULL
      )

      out <- list(
        fit = fit_df,
        type = type,
        grid = pgrid,
        draws = if (isTRUE(store_draws)) draws_mat else NULL,
        diagnostics = list(
          n_draws_total = S,
          n_draws_valid = sum(.draw_valid),
          n_draws_dropped = S - sum(.draw_valid)
        )
      )
      class(out) <- "mixgpd_predict"
      return(out)
    }

    # Conditional quantiles
    draws_arr <- array(NA_real_, dim = c(n_pred, M, S))

    for (s in seq_len(S)) {
      if (!.draw_valid[s]) next
      args0 <- .build_args0_or_null(s)
      if (is.null(args0)) next

      link_eta <- .compute_link_eta(s)

      for (i in seq_len(n_pred)) {
        args <- args0
        if (GPD) {
          args$threshold <- .threshold_at(s, i)
          args$tail_scale <- .tail_scale_at(s, i)
          if (!is.finite(args$threshold) || !is.finite(args$tail_scale) || args$tail_scale <= 0) {
            draws_arr[i, , s] <- NA_real_
            next
          }
        }
        if (length(link_params)) {
          for (nm in link_params) {
            vv <- as.numeric(link_eta[[nm]][i, ])
            if (!all(is.finite(vv))) { draws_arr[i, , s] <- NA_real_; next }
            args[[nm]] <- vv
          }
        }
        draws_arr[i, , s] <- as.numeric(do.call(q_fun, c(list(p = pgrid), args)))
      }
    }

    summ <- .posterior_summarize(draws_arr, probs = probs, interval = if (compute_interval) interval else NULL)
    estimate <- summ$estimate
    lower <- summ$lower
    upper <- summ$upper

    fit_df <- data.frame(
      estimate = as.vector(estimate),
      index    = rep(pgrid, each = n_pred),
      id       = rep(seq_len(n_pred), times = M),
      lower    = if (compute_interval) as.vector(lower) else NA_real_,
      upper    = if (compute_interval) as.vector(upper) else NA_real_,
      row.names = NULL
    )

    out <- list(
      fit = fit_df,
      type = type,
      grid = pgrid,
      draws = if (isTRUE(store_draws)) aperm(draws_arr, c(3, 1, 2)) else NULL, # S x n_pred x M
      diagnostics = list(
        n_draws_total = S,
        n_draws_valid = sum(.draw_valid),
        n_draws_dropped = S - sum(.draw_valid)
      )
    )
    class(out) <- "mixgpd_predict"
    return(out)
  }

  # -----------------------------
  # sample (posterior predictive)
  # -----------------------------
  if (type == "sample") {
    if (!has_X) {
      if (is.na(nsim) || nsim < 1L) nsim <- length(ytrain)
      idx <- sample.int(S, size = nsim, replace = TRUE)
      u_samples <- runif(nsim)
      outv <- numeric(nsim)

      for (t in seq_len(nsim)) {
        s <- idx[t]
        if (!.draw_valid[s]) { outv[t] <- NA_real_; next }
        args0 <- .build_args0_or_null(s)
        if (is.null(args0)) { outv[t] <- NA_real_; next }

        if (GPD) {
          args0$threshold <- threshold_scalar[s]
          args0$tail_scale <- tail_scale[s]
          if (!is.finite(args0$threshold) || !is.finite(args0$tail_scale) || args0$tail_scale <= 0) {
            outv[t] <- NA_real_
            next
          }
        }
        outv[t] <- as.numeric(do.call(q_fun, c(list(p = u_samples[t]), args0)))
      }

      res <- list(
        fit = outv,
        type = type,
        grid = NULL,
        diagnostics = list(
          n_draws_total = S,
          n_draws_valid = sum(.draw_valid),
          n_draws_dropped = S - sum(.draw_valid)
        )
      )
      class(res) <- "mixgpd_predict"
      return(res)
    }

    if (is.na(nsim) || nsim < 1L) nsim <- n_pred
    idx <- sample.int(S, size = nsim, replace = TRUE)
    u_samples <- runif(nsim)
    outm <- matrix(NA_real_, nrow = n_pred, ncol = nsim)

    for (t in seq_len(nsim)) {
      s <- idx[t]
      if (!.draw_valid[s]) next
      args0 <- .build_args0_or_null(s)
      if (is.null(args0)) next

      link_eta <- .compute_link_eta(s)

      for (i in seq_len(n_pred)) {
        args <- args0
        if (GPD) {
          args$threshold <- .threshold_at(s, i)
          args$tail_scale <- .tail_scale_at(s, i)
          if (!is.finite(args$threshold) || !is.finite(args$tail_scale) || args$tail_scale <= 0) {
            outm[i, t] <- NA_real_
            next
          }
        }
        if (length(link_params)) {
          for (nm in link_params) {
            vv <- as.numeric(link_eta[[nm]][i, ])
            if (!all(is.finite(vv))) { outm[i, t] <- NA_real_; next }
            args[[nm]] <- vv
          }
        }
        outm[i, t] <- as.numeric(do.call(q_fun, c(list(p = u_samples[t]), args)))
      }
    }

    res <- list(
      fit = outm,
      type = type,
      grid = NULL,
      diagnostics = list(
        n_draws_total = S,
        n_draws_valid = sum(.draw_valid),
        n_draws_dropped = S - sum(.draw_valid)
      )
    )
    class(res) <- "mixgpd_predict"
    return(res)
  }

  # -----------------------------
  # fit (one posterior predictive draw per observation per posterior draw)
  # -----------------------------
  if (type == "fit") {
    n_obs <- if (!has_X) length(ytrain) else n_pred
    if (!is.numeric(n_obs) || n_obs < 1L) stop("Could not determine n_obs for type='fit'.", call. = FALSE)

    samples_mat <- matrix(NA_real_, nrow = S, ncol = n_obs)

    for (s in seq_len(S)) {
      if (!.draw_valid[s]) next
      args0 <- .build_args0_or_null(s)
      if (is.null(args0)) next

      # Conditional: one sample per observation
      if (has_X) {
        link_eta <- .compute_link_eta(s)
        for (i in seq_len(n_obs)) {
          args <- args0
          if (GPD) {
            args$threshold <- .threshold_at(s, i)
            args$tail_scale <- .tail_scale_at(s, i)
            if (!is.finite(args$threshold) || !is.finite(args$tail_scale) || args$tail_scale <= 0) {
              samples_mat[s, i] <- NA_real_
              next
            }
          }
          if (length(link_params)) {
            for (nm in link_params) {
              vv <- as.numeric(link_eta[[nm]][i, ])
              if (!all(is.finite(vv))) { samples_mat[s, i] <- NA_real_; next }
              args[[nm]] <- vv
            }
          }
          samples_mat[s, i] <- as.numeric(do.call(r_fun, c(list(n = 1L), args)))[1]
        }
      } else {
        # Unconditional: draw n_obs from marginal predictive for draw s
        if (GPD) {
          args0$threshold <- threshold_scalar[s]
          args0$tail_scale <- tail_scale[s]
          if (!is.finite(args0$threshold) || !is.finite(args0$tail_scale) || args0$tail_scale <= 0) next
        }
        samples_mat[s, ] <- as.numeric(do.call(r_fun, c(list(n = n_obs), args0)))
      }
    }

    estimate <- as.numeric(colMeans(samples_mat, na.rm = TRUE))

    if (compute_interval) {
      lower <- apply(samples_mat, 2, stats::quantile, probs = probs[1], na.rm = TRUE)
      upper <- apply(samples_mat, 2, stats::quantile, probs = probs[length(probs)], na.rm = TRUE)
    } else {
      lower <- rep(NA_real_, n_obs)
      upper <- rep(NA_real_, n_obs)
    }

    fit_df <- data.frame(
      id = seq_len(n_obs),
      estimate = estimate,
      lower = as.numeric(lower),
      upper = as.numeric(upper),
      row.names = NULL
    )

    out <- list(
      fit = fit_df,
      type = type,
      draws = if (isTRUE(store_draws)) samples_mat else NULL,
      diagnostics = list(
        n_draws_total = S,
        n_draws_valid = sum(.draw_valid),
        n_draws_dropped = S - sum(.draw_valid)
      )
    )
    class(out) <- "mixgpd_predict"
    return(out)
  }

  # -----------------------------
  # mean (posterior mean of predictive distribution)
  # -----------------------------
  if (type == "mean") {
    nsim_inner <- as.integer(nsim_mean)
    if (is.na(nsim_inner) || nsim_inner < 10L) nsim_inner <- 200L

    # If GPD and any valid draw has xi >= 1, posterior mean is infinite
    if (GPD) {
      xi_valid <- tail_shape[.draw_valid]
      if (any(is.finite(xi_valid) & xi_valid >= 1)) {
        warning("Posterior mean is infinite because there is posterior mass with tail_shape (xi) >= 1. Use type='median'/'quantile' or a restricted-mean target.", call. = FALSE)
        inf_df <- data.frame(
          id = if (has_X) seq_len(n_pred) else 1L,
          estimate = Inf,
          lower = if (compute_interval) Inf else NA_real_,
          upper = if (compute_interval) Inf else NA_real_,
          row.names = NULL
        )
        out <- list(
          fit = inf_df,
          type = type,
          grid = NULL,
          draws = if (isTRUE(store_draws)) rep(Inf, sum(.draw_valid)) else NULL,
          diagnostics = list(
            n_draws_total = S,
            n_draws_valid = sum(.draw_valid),
            n_draws_dropped = S - sum(.draw_valid),
            mean_infinite = TRUE
          )
        )
        class(out) <- "mixgpd_predict"
        return(out)
      }
    }

    # Monte Carlo approximation to E[Y | params] per draw, then summarize over posterior draws
    if (!has_X) {
      draw_means <- rep(NA_real_, S)
      for (s in seq_len(S)) {
        if (!.draw_valid[s]) next
        args0 <- .build_args0_or_null(s)
        if (is.null(args0)) next
        if (GPD) {
          args0$threshold <- threshold_scalar[s]
          args0$tail_scale <- tail_scale[s]
          if (!is.finite(args0$threshold) || !is.finite(args0$tail_scale) || args0$tail_scale <= 0) next
        }
        yy <- as.numeric(do.call(r_fun, c(list(n = nsim_inner), args0)))
        draw_means[s] <- mean(yy, na.rm = TRUE)
      }

      summ <- .posterior_summarize(draw_means, probs = probs, interval = if (compute_interval) interval else NULL)

      fit_df <- data.frame(
        id = 1L,
        estimate = as.numeric(summ$estimate)[1],
        lower = if (compute_interval) as.numeric(summ$lower)[1] else NA_real_,
        upper = if (compute_interval) as.numeric(summ$upper)[1] else NA_real_,
        row.names = NULL
      )

      out <- list(
        fit = fit_df,
        type = type,
        grid = NULL,
        draws = if (isTRUE(store_draws)) draw_means else NULL,
        diagnostics = list(
          n_draws_total = S,
          n_draws_valid = sum(.draw_valid),
          n_draws_dropped = S - sum(.draw_valid),
          nsim_mean = nsim_inner
        )
      )
      class(out) <- "mixgpd_predict"
      return(out)
    }

    # Conditional mean: compute mean per x-row per posterior draw by simulation
    draw_means_mat <- matrix(NA_real_, nrow = S, ncol = n_pred)

    for (s in seq_len(S)) {
      if (!.draw_valid[s]) next
      args0 <- .build_args0_or_null(s)
      if (is.null(args0)) next

      link_eta <- .compute_link_eta(s)

      for (i in seq_len(n_pred)) {
        args <- args0
        if (GPD) {
          args$threshold <- .threshold_at(s, i)
          args$tail_scale <- .tail_scale_at(s, i)
          if (!is.finite(args$threshold) || !is.finite(args$tail_scale) || args$tail_scale <= 0) {
            draw_means_mat[s, i] <- NA_real_
            next
          }
        }
        if (length(link_params)) {
          for (nm in link_params) {
            vv <- as.numeric(link_eta[[nm]][i, ])
            if (!all(is.finite(vv))) { draw_means_mat[s, i] <- NA_real_; next }
            args[[nm]] <- vv
          }
        }

        yy <- as.numeric(do.call(r_fun, c(list(n = nsim_inner), args)))
        draw_means_mat[s, i] <- mean(yy, na.rm = TRUE)
      }
    }

    # Summarize across draws for each i
    estimate <- apply(draw_means_mat, 2, mean, na.rm = TRUE)

    lower <- upper <- NULL
    if (compute_interval) {
      lower <- upper <- rep(NA_real_, n_pred)
      for (i in seq_len(n_pred)) {
        iv <- .compute_interval(draw_means_mat[, i], level = cred.level, type = interval)
        lower[i] <- iv["lower"]
        upper[i] <- iv["upper"]
      }
    }

    fit_df <- data.frame(
      id = seq_len(n_pred),
      estimate = as.numeric(estimate),
      lower = if (compute_interval) as.numeric(lower) else NA_real_,
      upper = if (compute_interval) as.numeric(upper) else NA_real_,
      row.names = NULL
    )

    out <- list(
      fit = fit_df,
      type = type,
      grid = NULL,
      draws = if (isTRUE(store_draws)) draw_means_mat else NULL,
      diagnostics = list(
        n_draws_total = S,
        n_draws_valid = sum(.draw_valid),
        n_draws_dropped = S - sum(.draw_valid),
        nsim_mean = nsim_inner
      )
    )
    class(out) <- "mixgpd_predict"
    return(out)
  }


  # -----------------------------
  # rmean (restricted mean E[min(Y, cutoff)])
  # -----------------------------
  if (type == "rmean") {
    if (is.null(cutoff) || length(cutoff) != 1L || !is.finite(as.numeric(cutoff))) {
      stop("For type='rmean', provide a finite numeric 'cutoff'.", call. = FALSE)
    }
    cutoff <- as.numeric(cutoff)

    nsim_inner <- as.integer(nsim_mean)
    if (is.na(nsim_inner) || nsim_inner < 10L) nsim_inner <- 200L

    if (!has_X) {
      draw_rmeans <- rep(NA_real_, S)
      for (s in seq_len(S)) {
        if (!.draw_valid[s]) next
        args0 <- .build_args0_or_null(s)
        if (is.null(args0)) next
        if (GPD) {
          args0$threshold <- threshold_scalar[s]
          args0$tail_scale <- tail_scale[s]
          if (!is.finite(args0$threshold) || !is.finite(args0$tail_scale) || args0$tail_scale <= 0) next
        }
        yy <- as.numeric(do.call(r_fun, c(list(n = nsim_inner), args0)))
        draw_rmeans[s] <- mean(pmin(yy, cutoff), na.rm = TRUE)
      }

      summ <- .posterior_summarize(draw_rmeans, probs = probs, interval = if (compute_interval) interval else NULL)

      fit_df <- data.frame(
        id = 1L,
        estimate = as.numeric(summ$estimate)[1],
        lower = if (compute_interval) as.numeric(summ$lower)[1] else NA_real_,
        upper = if (compute_interval) as.numeric(summ$upper)[1] else NA_real_,
        row.names = NULL
      )

      out <- list(
        fit = fit_df,
        type = type,
        grid = NULL,
        cutoff = cutoff,
        draws = if (isTRUE(store_draws)) draw_rmeans else NULL,
        diagnostics = list(
          n_draws_total = S,
          n_draws_valid = sum(.draw_valid),
          n_draws_dropped = S - sum(.draw_valid),
          nsim_mean = nsim_inner
        )
      )
      class(out) <- "mixgpd_predict"
      return(out)
    }

    draw_rmeans_mat <- matrix(NA_real_, nrow = S, ncol = n_pred)

    for (s in seq_len(S)) {
      if (!.draw_valid[s]) next
      args0 <- .build_args0_or_null(s)
      if (is.null(args0)) next

      link_eta <- .compute_link_eta(s)

      for (i in seq_len(n_pred)) {
        args <- args0
        if (GPD) {
          args$threshold <- .threshold_at(s, i)
          args$tail_scale <- .tail_scale_at(s, i)
          if (!is.finite(args$threshold) || !is.finite(args$tail_scale) || args$tail_scale <= 0) {
            draw_rmeans_mat[s, i] <- NA_real_
            next
          }
        }
        if (length(link_params)) {
          for (nm in link_params) {
            vv <- as.numeric(link_eta[[nm]][i, ])
            if (!all(is.finite(vv))) { draw_rmeans_mat[s, i] <- NA_real_; next }
            args[[nm]] <- vv
          }
        }

        yy <- as.numeric(do.call(r_fun, c(list(n = nsim_inner), args)))
        draw_rmeans_mat[s, i] <- mean(pmin(yy, cutoff), na.rm = TRUE)
      }
    }

    estimate <- apply(draw_rmeans_mat, 2, mean, na.rm = TRUE)

    lower <- upper <- NULL
    if (compute_interval) {
      lower <- upper <- rep(NA_real_, n_pred)
      for (i in seq_len(n_pred)) {
        iv <- .compute_interval(draw_rmeans_mat[, i], level = cred.level, type = interval)
        lower[i] <- iv["lower"]
        upper[i] <- iv["upper"]
      }
    }

    fit_df <- data.frame(
      id = seq_len(n_pred),
      estimate = as.numeric(estimate),
      lower = if (compute_interval) as.numeric(lower) else NA_real_,
      upper = if (compute_interval) as.numeric(upper) else NA_real_,
      row.names = NULL
    )

    out <- list(
      fit = fit_df,
      type = type,
      grid = NULL,
      cutoff = cutoff,
      draws = if (isTRUE(store_draws)) draw_rmeans_mat else NULL,
      diagnostics = list(
        n_draws_total = S,
        n_draws_valid = sum(.draw_valid),
        n_draws_dropped = S - sum(.draw_valid),
        nsim_mean = nsim_inner
      )
    )
    class(out) <- "mixgpd_predict"
    return(out)
  }

stop(sprintf("Unsupported prediction type '%s'.", type), call. = FALSE)
}





.wrap_density_fun <- function(fun) {
  function(x, ...) {
    if (length(x) == 0) return(numeric(0))
    if (length(x) == 1) return(as.numeric(fun(x, ...)))
    vapply(x, function(xx) as.numeric(fun(xx, ...)), numeric(1))
  }
}

.wrap_cdf_fun <- function(fun) {
  function(q, ...) {
    if (length(q) == 0) return(numeric(0))
    if (length(q) == 1) return(as.numeric(fun(q, ...)))
    vapply(q, function(qq) as.numeric(fun(qq, ...)), numeric(1))
  }
}

.wrap_quantile_fun <- function(fun) {
  function(p, ...) {
    if (length(p) == 0) return(numeric(0))
    fun(p, ...)
  }
}

.wrap_rng_fun <- function(fun) {
  function(n, ...) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(as.numeric(fun(1, ...)))
    vapply(seq_len(n), function(i) as.numeric(fun(1, ...)), numeric(1))
  }
}
