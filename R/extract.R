#' MCMC draw extraction and manipulation (internal)
#'
#' Internal helper functions for extracting and processing posterior draws
#' from MCMC samples. These functions handle validation, matrix extraction,
#' component truncation, and parameter block organization.
#'
#' @name mcmc-extract
#' @keywords internal
#' @noRd
NULL

#' Extract indexed parameter blocks from draws matrix
#'
#' Extracts columns matching a pattern like `name[1]`, `name[2]`, etc.,
#' and returns them as an S x K matrix where S is iterations and K is
#' the maximum index found. Fills missing components with zeros.
#'
#' @param mat0 Numeric matrix of draws.
#' @param base Character, the parameter base name (e.g., `"w"` for `"w[1]"`, `"w[2]"`).
#' @param K Optional integer; the number of components. If NULL, inferred as maximum index.
#' @param allow_missing Logical; if TRUE, returns NULL if no matches found; if FALSE, errors.
#'
#' @return Numeric matrix (S x K) or NULL if no matches and allow_missing=TRUE.
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

#' Extract mixture weights from posterior draws
#'
#' Extracts mixture component weights from draw matrix. For stick-breaking (SB)
#' backend, looks for explicit weight columns (`w[k]` or `weights[k]`). For
#' Chinese Restaurant Process (CRP) backend, derives weights from cluster
#' assignments (`z[i]`).
#'
#' @param draw_mat Numeric matrix (iterations x parameters) of posterior draws.
#' @param backend Character; either "sb" (stick-breaking) or "crp" (CRP).
#'
#' @return Numeric matrix (iterations x K) of weights. Row sums = 1.
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
#'
#' For each bulk parameter name (e.g., "mu", "sigma"), extracts indexed
#' blocks (`"mu[1]"`, `"mu[2]"`, ...) and returns them as a list of matrices.
#' Automatically infers K from parameter indices or weight columns.
#'
#' @param draw_mat Numeric matrix of draws.
#' @param bulk_params Character vector of parameter base names to extract.
#'
#' @return List of numeric matrices (S x K), named by parameter name.
#'   Empty list if no parameters found.
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

#' Safely coerce MCMC samples to coda::mcmc.list
#'
#' Validates that object is a fitted mixgpd_fit and coerces its samples
#' to the standard coda::mcmc.list format, required for summary and plotting.
#'
#' @param object A mixgpd_fit object.
#' @return A coda::mcmc.list object.
#' @keywords internal
#' @noRd
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

#' Extract posterior draws as a numeric matrix
#'
#' Combines all chains' draws, optionally drops stick-breaking v parameters,
#' and applies component truncation based on model epsilon.
#'
#' @param object A mixgpd_fit object.
#' @param drop_v Logical; if TRUE, drop stick-breaking `"v[k]"` columns.
#' @param epsilon Optional numeric; override truncation threshold.
#'
#' @return Numeric matrix (iterations x parameters) with truncation metadata
#'   attached as "truncation" attribute.
#' @keywords internal
#' @noRd
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

#' Get number of observations in fitted object
#'
#' Extracts the sample size n from different possible storage locations
#' in the fitted object.
#'
#' @param object A mixgpd_fit object.
#' @return Integer n, or NA_integer_ if not found.
#' @keywords internal
#' @noRd
.get_nobs <- function(object) {
  if (!is.null(object$data) && !is.null(object$data$y)) return(length(object$data$y))
  if (!is.null(object$y)) return(length(object$y))
  NA_integer_
}

#' Extract posterior draws with optional parameter selection
#'
#' Combines all chains, drops v parameters, applies truncation, and
#' optionally filters to specific parameter names.
#'
#' @param object A mixgpd_fit object.
#' @param pars Optional; character vector of exact parameter names to keep.
#' @param chains Character; "stack" (combine all chains) or "first" (first chain only).
#' @param epsilon Optional numeric; override truncation threshold.
#'
#' @return Numeric matrix (iterations x parameters).
#' @keywords internal
#' @noRd
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

#' Get epsilon truncation threshold
#'
#' Resolves the truncation epsilon from object specification, metadata,
#' or user override. Defaults to 0.025 if not specified.
#'
#' @param object A mixgpd_fit object.
#' @param epsilon Optional numeric override.
#'
#' @return Numeric epsilon value in [0, 1).
#' @keywords internal
#' @noRd
.get_epsilon <- function(object, epsilon = NULL) {
  if (!is.null(epsilon)) return(as.numeric(epsilon)[1])
  spec <- object$spec %||% list()
  meta <- spec$meta %||% list()
  as.numeric(object$epsilon %||% meta$epsilon %||% 0.025)
}

#' Truncate components in draw matrix by posterior weight
#'
#' For each posterior draw, keeps only components above epsilon threshold
#' and ensures retained components explain at least 1-epsilon probability mass.
#' Rebuilds weight and parameter matrices with consistent component ordering
#' (largest to smallest) across all draws.
#'
#' @param object A mixgpd_fit object.
#' @param mat Numeric matrix of draws (iter x parameters).
#' @param epsilon Numeric in [0, 1); truncation threshold.
#'
#' @return Matrix with truncated components and truncation metadata.
#' @keywords internal
#' @noRd
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

  .indexed_block_local <- function(mat0, base, K = NULL, allow_missing = FALSE) {
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

  # Extract cluster assignments and weights
  if (!("z[1]" %in% cn) && !any(grepl("^z\\[[0-9]+\\]$", cn))) {
    stop("Backend requires z[i] in samples to derive weights.", call. = FALSE)
  }
  Z <- .indexed_block_local(mat, "z")
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
    blk <- .indexed_block_local(mat, nm, K = K, allow_missing = TRUE)
    if (!is.null(blk)) bulk_draws[[nm]] <- blk
  }
  bulk_params_present <- names(bulk_draws)

  # Per-draw truncation
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

  # Fixed K across draws
  Kt <- min(ks)
  if (!is.finite(Kt) || Kt < 1L) Kt <- 1L

  # Build output matrix: keep non-component columns, replace blocks
  drop_pat <- c(
    "^weights\\[[0-9]+\\]$",
    "^w\\[[0-9]+\\]$",
    paste0("^", bulk_params, "\\[[0-9]+\\]$")
  )
  drop_hit <- rep(FALSE, length(cn))
  for (pp in drop_pat) drop_hit <- drop_hit | grepl(pp, cn)
  keep_cn <- cn[!drop_hit]

  out <- mat[, keep_cn, drop = FALSE]

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

#' Truncate components in a single posterior draw
#'
#' For one draw, keeps components above epsilon and ensures cumulative weight
#' at least `1 - epsilon`. Returns truncated weights and parameters sorted by decreasing weight.
#'
#' @param w Numeric vector of component weights (must sum to 1).
#' @param params List of numeric vectors, each length K; e.g., list(mu=c(...), sigma=c(...)).
#' @param epsilon Numeric in [0, 1); truncation threshold.
#'
#' @return List with: k (number kept), k_weight, k_cum, ord (reorder indices),
#'   weights, params (truncated and reordered).
#' @keywords internal
#' @noRd
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

  # Two criteria: (1) per-component weight >= epsilon, (2) cumulative >= 1-epsilon
  keep_idx <- which(w_sorted >= epsilon)
  k_weight <- if (length(keep_idx)) max(keep_idx) else 0L

  cw <- cumsum(w_sorted)
  k_cum <- which(cw >= (1 - epsilon))[1]
  if (is.na(k_cum)) k_cum <- K

  k_keep <- min(k_weight, k_cum)
  if (!is.finite(k_keep) || k_keep < 1L) k_keep <- 1L

  keep <- seq_len(k_keep)
  w_keep <- w_sorted[keep]

  # Adjust smallest kept weight to ensure sum = 1
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

#' Summarize component truncation across draws
#'
#' Extracts truncation metadata from draw matrix and returns summary statistics.
#'
#' @param object A mixgpd_fit object.
#' @param epsilon Optional numeric; override truncation threshold.
#'
#' @return List with min/median/max for k, k_weight, k_cum, and Kt (fixed K).
#' @keywords internal
#' @noRd
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
