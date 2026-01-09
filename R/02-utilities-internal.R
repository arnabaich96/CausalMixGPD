

# ============================================================
# Utilities (internal)
# ============================================================



#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a)) a else b

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

  Kt <- max(ks)
  if (!is.finite(Kt) || Kt < 1) Kt <- 1L

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
    k_s <- length(w_s)
    w_out[s, seq_len(k_s)] <- w_s
    for (nm in bulk_params_present) {
      out_params[[nm]][s, seq_len(k_s)] <- p_list[[s]][[nm]]
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
        keep <- keep | grepl("^threshold\\[[0-9]+\\]$", cn)
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

#' Compute CRP weights from a vector of allocations z
#'
#' @param z Integer vector of length N with values in 1:Kmax.
#' @param Kmax Integer.
#' @return Numeric vector of length Kmax summing to 1.
#' @keywords internal
.crp_weights_from_z <- function(z, Kmax) {
  Kmax <- as.integer(Kmax)
  if (Kmax < 1L) stop("Kmax must be >= 1.", call. = FALSE)
  if (!is.numeric(z)) stop("z must be numeric/integer.", call. = FALSE)

  z <- as.integer(z)
  if (any(is.na(z))) stop("z contains NA.", call. = FALSE)
  if (any(z < 1L | z > Kmax)) stop("z must be in 1:Kmax.", call. = FALSE)

  tab <- tabulate(z, nbins = Kmax)
  w <- tab / sum(tab)
  as.numeric(w)
}



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
                            x = NULL, y = NULL,
                            type = c("density", "survival", "quantile", "sample", "mean"),
                            p = NULL, nsim = NULL,
                            interval = c("none", "credible"),
                            probs = c(0.025, 0.5, 0.975),
                            store_draws = TRUE,
                            nsim_mean = 200L,
                            ncores = 1L) {


  .validate_fit(object)
  type <- match.arg(type)
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
  d_fun <- fns$d
  p_fun <- fns$p
  q_fun <- fns$q
  r_fun <- fns$r
  kdef <- get_kernel_registry()[[kernel]] %||% list()
  bulk_support <- kdef$bulk_support %||% list()

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

  .fill_param_na <- function(nm, vec) {
    if (!any(!is.finite(vec))) return(vec)
    sup <- bulk_support[[nm]] %||% ""
    def <- if (sup %in% c("positive_sd", "positive_scale", "positive_shape", "positive_location")) 1 else 0
    vec[!is.finite(vec)] <- def
    vec
  }

  # -----------------------------
  # Resolve inputs by type (YOUR CONTRACT)
  # -----------------------------
  Xpred <- NULL
  ygrid <- NULL
  pgrid <- NULL

  if (type %in% c("density", "survival")) {
    if (has_X) {
      # with X: either BOTH (x,y) provided or NEITHER (defaults to training X and training y)
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
      # without X: y can be provided alone, otherwise default to training y
      if (is.null(y)) {
        if (is.null(ytrain)) stop("Training y not found in fit object.", call. = FALSE)
        ygrid <- ytrain
      } else {
        ygrid <- y
      }
      Xpred <- NULL
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

  # Determine K and weights per draw (truncated/reordered weights)
  cn <- colnames(draw_mat)
  widx <- as.integer(sub("^w\\[([0-9]+)\\]$", "\\1", cn[grepl("^w\\[[0-9]+\\]$", cn)]))
  K <- max(widx, na.rm = TRUE)
  if (!is.finite(K) || K < 1) stop("Could not infer K from w[] in posterior draws.", call. = FALSE)
  K <- as.integer(K)
  W_draws <- .indexed_block(draw_mat, "w", K = K)

  # Component parameter matrices (S x K) and link-mode betas (S x K x P)
  plan <- spec$plan %||% list()
  bulk_plan <- plan$bulk %||% list()
  bulk_modes <- vapply(bulk_params, function(nm) {
    ent <- bulk_plan[[nm]] %||% list()
    ent$mode %||% NA_character_
  }, character(1))

  link_params <- bulk_params[bulk_modes == "link"]
  base_params <- bulk_params[bulk_modes != "link"]

  bulk_draws <- list()
  for (nm in base_params) bulk_draws[[nm]] <- .indexed_block(draw_mat, nm, K = K)

  .indexed_block2 <- function(mat, base, K, P) {
    cn <- colnames(mat)
    pat <- paste0("^", base, "\\[([0-9]+),\\s*([0-9]+)\\]$")
    hit <- grepl(pat, cn)
    if (!any(hit)) return(NULL)

    idx1 <- as.integer(sub(pat, "\\1", cn[hit]))
    idx2 <- as.integer(sub(pat, "\\2", cn[hit]))
    cols <- cn[hit]

    out <- array(0.0, dim = c(nrow(mat), K, P))
    for (j in seq_along(cols)) {
      k <- idx1[j]
      p <- idx2[j]
      if (!is.na(k) && !is.na(p) && k >= 1 && k <= K && p >= 1 && p <= P) {
        out[, k, p] <- mat[, cols[j]]
      }
    }
    out
  }

  .indexed_block_beta <- function(mat, base, K, P) {
    out2 <- .indexed_block2(mat, base, K = K, P = P)
    if (!is.null(out2)) return(out2)

    # Fallback for single-column P=1 stored as base[i]
    out1 <- .indexed_block(mat, base, K = K)
    array(out1, dim = c(nrow(mat), K, 1L))
  }

  link_betas <- list()
  link_specs <- list()
  if (length(link_params)) {
    if (!has_X) stop("Link-mode bulk parameters require X.", call. = FALSE)
    P <- ncol(Xpred)
    for (nm in link_params) {
      link_betas[[nm]] <- .indexed_block_beta(draw_mat, paste0("beta_", nm), K = K, P = P)
      ent <- bulk_plan[[nm]] %||% list()
      link_specs[[nm]] <- list(link = ent$link %||% "identity", link_power = ent$link_power %||% NULL)
    }
  }

  # Tail / threshold draws
  tail_scale <- tail_shape <- NULL
  threshold_scalar <- NULL
  threshold_mat <- NULL

  if (GPD) {
    gpd_plan <- plan$gpd %||% list()
    ts_mode <- gpd_plan$tail_scale$mode %||% NA_character_

    if (!("tail_shape" %in% colnames(draw_mat))) stop("tail_shape not found in posterior draws.", call. = FALSE)
    tail_shape <- as.numeric(draw_mat[, "tail_shape"])

    if (has_X) {
      P <- ncol(Xpred)
      beta_mat <- .indexed_block(draw_mat, "beta_threshold", K = P)  # S x P
      threshold_mat <- matrix(NA_real_, nrow = S, ncol = n_pred)
      thr_link <- gpd_plan$threshold$link %||% "identity"
      thr_power <- gpd_plan$threshold$link_power %||% NULL
      for (s in 1:S) {
        eta <- as.numeric(Xpred %*% beta_mat[s, ])
        threshold_mat[s, ] <- as.numeric(.apply_link(eta, thr_link, thr_power))
      }
    } else {
      if ("threshold" %in% colnames(draw_mat)) {
        threshold_scalar <- as.numeric(draw_mat[, "threshold"])
      } else {
        thr_cols <- grep("^threshold\\[[0-9]+\\]$", colnames(draw_mat), value = TRUE)
        if (!length(thr_cols)) {
          stop("threshold not found in posterior draws.", call. = FALSE)
        }
        threshold_scalar <- rowMeans(draw_mat[, thr_cols, drop = FALSE], na.rm = TRUE)
      }
    }

    if (identical(ts_mode, "link")) {
      if (!has_X) stop("tail_scale is link-mode but X is missing.", call. = FALSE)
      beta_ts <- .indexed_block(draw_mat, "beta_tail_scale", K = P)  # S x P
      tail_scale <- matrix(NA_real_, nrow = S, ncol = n_pred)
      ts_link <- gpd_plan$tail_scale$link %||% "exp"
      ts_power <- gpd_plan$tail_scale$link_power %||% NULL
      for (s in 1:S) {
        eta <- as.numeric(Xpred %*% beta_ts[s, ])
        tail_scale[s, ] <- as.numeric(.apply_link(eta, ts_link, ts_power))
      }
    } else {
      if (!("tail_scale" %in% colnames(draw_mat))) stop("tail_scale not found in posterior draws.", call. = FALSE)
      tail_scale <- as.numeric(draw_mat[, "tail_scale"])
    }
  }

  .tail_scale_at <- function(s, i) {
    if (is.matrix(tail_scale)) return(tail_scale[s, i])
    tail_scale[s]
  }

  # -----------------------------
  # Parallel helper
  # -----------------------------
  .lapply_draws <- function(FUN) {
    idx <- seq_len(S)
    if (ncores == 1L) return(lapply(idx, FUN))

    if (!requireNamespace("future.apply", quietly = TRUE) ||
        !requireNamespace("future", quietly = TRUE)) {
      stop("Packages 'future' and 'future.apply' are required for ncores > 1.", call. = FALSE)
    }

    # Export function objects explicitly to keep closures stable across workers.
    d_fun <- fns$d
    p_fun <- fns$p
    q_fun <- fns$q
    r_fun <- fns$r

    if (!is.function(d_fun) || !is.function(p_fun) || !is.function(q_fun) || !is.function(r_fun)) {
      stop("Internal error: kernel dispatch functions not resolved.", call. = FALSE)
    }

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = ncores)

    future.apply::future_lapply(idx, FUN)
  }


  # -----------------------------
  # density / survival
  # -----------------------------
  if (type %in% c("density", "survival")) {
    G <- length(ygrid)

    .one_draw <- function(s) {
      w_s <- as.numeric(W_draws[s, ])
      args0 <- list(w = w_s)
      for (nm in base_params) {
        args0[[nm]] <- .fill_param_na(nm, as.numeric(bulk_draws[[nm]][s, ]))
      }
      if (GPD) { args0$tail_shape <- tail_shape[s] }

      link_eta <- list()
      if (length(link_params)) {
        for (nm in link_params) {
          beta_mat <- link_betas[[nm]][s, , , drop = FALSE]
          dim(beta_mat) <- c(K, ncol(Xpred))
          eta <- beta_mat %*% t(Xpred)
          spec <- link_specs[[nm]] %||% list()
          link_eta[[nm]] <- .apply_link(eta, spec$link %||% "identity", spec$link_power %||% NULL)
        }
      }

      out <- matrix(NA_real_, nrow = n_pred, ncol = G)
      for (i in 1:n_pred) {
        args <- args0
        if (GPD) {
          args$threshold <- if (has_X) threshold_mat[s, i] else threshold_scalar[s]
          args$tail_scale <- .tail_scale_at(s, i)
        }
        if (length(link_params)) {
          for (nm in link_params) args[[nm]] <- as.numeric(link_eta[[nm]][, i])
        }

        if (type == "density") {
          out[i, ] <- vapply(ygrid, function(yy) {
            do.call(d_fun, c(list(x = yy, log = 0L), args))
          }, numeric(1))
        } else {
          cdfv <- vapply(ygrid, function(yy) {
            do.call(p_fun, c(list(q = yy, lower.tail = 1L, log.p = 0L), args))
          }, numeric(1))
          if (type == "survival") cdfv <- 1 - cdfv
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
      for (nm in base_params) {
        args0[[nm]] <- .fill_param_na(nm, as.numeric(bulk_draws[[nm]][s, ]))
      }
      if (GPD) { args0$tail_shape <- tail_shape[s] }

      link_eta <- list()
      if (length(link_params)) {
        for (nm in link_params) {
          beta_mat <- link_betas[[nm]][s, , , drop = FALSE]
          dim(beta_mat) <- c(K, ncol(Xpred))
          eta <- beta_mat %*% t(Xpred)
          spec <- link_specs[[nm]] %||% list()
          link_eta[[nm]] <- .apply_link(eta, spec$link %||% "identity", spec$link_power %||% NULL)
        }
      }

      out <- matrix(NA_real_, nrow = n_pred, ncol = M)
      for (i in 1:n_pred) {
        args <- args0
        if (GPD) {
          args$threshold <- if (has_X) threshold_mat[s, i] else threshold_scalar[s]
          args$tail_scale <- .tail_scale_at(s, i)
        }
        if (length(link_params)) {
          for (nm in link_params) args[[nm]] <- as.numeric(link_eta[[nm]][, i])
        }
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
      for (nm in base_params) {
        args0[[nm]] <- .fill_param_na(nm, as.numeric(bulk_draws[[nm]][s, ]))
      }
      if (GPD) { args0$tail_shape <- tail_shape[s] }

      link_eta <- list()
      if (length(link_params)) {
        for (nm in link_params) {
          beta_mat <- link_betas[[nm]][s, , , drop = FALSE]
          dim(beta_mat) <- c(K, ncol(Xpred))
          eta <- beta_mat %*% t(Xpred)
          spec <- link_specs[[nm]] %||% list()
          link_eta[[nm]] <- .apply_link(eta, spec$link %||% "identity", spec$link_power %||% NULL)
        }
      }

      for (i in 1:n_pred) {
        args <- args0
        if (GPD) {
          args$threshold <- if (has_X) threshold_mat[s, i] else threshold_scalar[s]
          args$tail_scale <- .tail_scale_at(s, i)
        }
        if (length(link_params)) {
          for (nm in link_params) args[[nm]] <- as.numeric(link_eta[[nm]][, i])
        }
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
      for (nm in base_params) {
        args0[[nm]] <- .fill_param_na(nm, as.numeric(bulk_draws[[nm]][s, ]))
      }
      if (GPD) { args0$tail_shape <- tail_shape[s] }

      link_eta <- list()
      if (length(link_params)) {
        for (nm in link_params) {
          beta_mat <- link_betas[[nm]][s, , , drop = FALSE]
          dim(beta_mat) <- c(K, ncol(Xpred))
          eta <- beta_mat %*% t(Xpred)
          spec <- link_specs[[nm]] %||% list()
          link_eta[[nm]] <- .apply_link(eta, spec$link %||% "identity", spec$link_power %||% NULL)
        }
      }

      for (i in 1:n_pred) {
        args <- args0
        if (GPD) {
          args$threshold <- if (has_X) threshold_mat[s, i] else threshold_scalar[s]
          args$tail_scale <- .tail_scale_at(s, i)
        }
        if (length(link_params)) {
          for (nm in link_params) args[[nm]] <- as.numeric(link_eta[[nm]][, i])
        }
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

