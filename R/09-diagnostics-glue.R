#' Validate bulk+tail glue for MixGPD predictive distribution
#'
#' This diagnostic checks whether the implied predictive distribution behaves like
#' a valid distribution (monotone CDF in \eqn{[0,1]}, nonnegative density, and
#' sensible behavior around the threshold when a GPD tail is enabled).
#'
#' The check is performed draw-by-draw on a user-specified grid. It is intended
#' for development, debugging, and CI (not for routine large-scale use).
#'
#' @param fit A \code{mixgpd_fit} object.
#' @param x Optional design matrix for conditional models. If \code{NULL}, uses training \code{X}.
#' @param grid Numeric evaluation grid. If \code{NULL}, defaults to a grid based on training \code{y}.
#' @param n_draws Number of posterior draws to check (sampled without replacement when possible).
#' @param tol Numerical tolerance for monotonicity/range checks.
#' @param check_continuity Logical; if \code{TRUE} and GPD is enabled, checks continuity at the threshold.
#' @param eps Small offset used for threshold continuity check.
#' @return A list with per-check pass/fail flags and summaries of violations.
#' @export
check_glue_validity <- function(fit,
                                x = NULL,
                                grid = NULL,
                                n_draws = 50L,
                                tol = 1e-8,
                                check_continuity = TRUE,
                                eps = 1e-6) {
  stopifnot(inherits(fit, "mixgpd_fit"))

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  spec <- fit$spec %||% list()
  meta <- spec$meta %||% list()

  backend <- meta$backend %||% spec$dispatch$backend %||% "<unknown>"
  GPD <- isTRUE(meta$GPD %||% spec$dispatch$GPD)
  pred_backend <- if (identical(backend, "crp")) "sb" else backend

  Xtrain <- fit$data$X %||% fit$X %||% NULL
  ytrain <- fit$data$y %||% fit$y %||% NULL

  has_X <- isTRUE(meta$has_X %||% (!is.null(Xtrain)))

  if (has_X) {
    if (is.null(x)) {
      if (is.null(Xtrain)) stop("Training X not found; provide 'x'.", call. = FALSE)
      X <- as.matrix(Xtrain)
    } else {
      X <- as.matrix(x)
    }
  } else {
    if (!is.null(x)) stop("Unconditional model: 'x' not allowed.", call. = FALSE)
    X <- NULL
  }

  if (is.null(grid)) {
    if (is.null(ytrain)) stop("Training y not found; provide 'grid'.", call. = FALSE)
    ytrain <- as.numeric(ytrain)
    lo <- stats::quantile(ytrain, probs = 0.001, na.rm = TRUE)
    hi <- stats::quantile(ytrain, probs = 0.999, na.rm = TRUE)
    grid <- seq(from = as.numeric(lo), to = as.numeric(hi), length.out = 200L)
  }
  grid <- as.numeric(grid)
  if (any(!is.finite(grid))) stop("'grid' must be finite numeric.", call. = FALSE)
  grid <- sort(unique(grid))
  G <- length(grid)

  fns <- .get_dispatch(fit, backend_override = pred_backend)
  d_fun <- fns$d
  p_fun <- fns$p
  bulk_params <- fns$bulk_params

  draw_mat <- .extract_draws_matrix(fit)
  if (is.null(draw_mat) || !is.matrix(draw_mat) || nrow(draw_mat) < 2L) {
    stop("Posterior draws not found or malformed in fitted object.", call. = FALSE)
  }
  S <- nrow(draw_mat)

  n_draws <- as.integer(n_draws)
  if (is.na(n_draws) || n_draws < 1L) stop("'n_draws' must be >= 1.", call. = FALSE)
  idx <- if (S <= n_draws) seq_len(S) else sample.int(S, size = n_draws, replace = FALSE)

  W_draws <- .extract_weights(draw_mat, backend = pred_backend)
  bulk_draws <- .extract_bulk_params(draw_mat, bulk_params = bulk_params)
  base_params <- names(bulk_draws)

  # GPD pieces (constant or link)
  tail_shape <- NULL
  threshold_mat <- NULL
  threshold_scalar <- NULL
  tail_scale <- NULL
  P <- if (!is.null(X)) ncol(X) else 0L
  n_x <- if (!is.null(X)) nrow(X) else 1L

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

  if (GPD) {
    if (!("tail_shape" %in% colnames(draw_mat))) stop("tail_shape not found in posterior draws.", call. = FALSE)
    tail_shape <- as.numeric(draw_mat[, "tail_shape"])

    gpd_plan <- spec$dispatch$gpd %||% meta$gpd %||% list()

    thr_mode <- gpd_plan$threshold$mode %||% "constant"
    if (identical(thr_mode, "link")) {
      if (is.null(X)) stop("threshold link-mode requires X.", call. = FALSE)
      beta_thr <- .indexed_block(draw_mat, "beta_threshold", K = P)
      threshold_mat <- array(NA_real_, dim = c(S, n_x))
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
      if (length(thr_cols) == 1L) threshold_scalar <- as.numeric(draw_mat[, thr_cols])
      else threshold_scalar <- rowMeans(draw_mat[, thr_cols, drop = FALSE], na.rm = TRUE)
    }

    ts_mode <- gpd_plan$tail_scale$mode %||% "constant"
    if (identical(ts_mode, "link")) {
      if (is.null(X)) stop("tail_scale link-mode requires X.", call. = FALSE)
      beta_ts <- .indexed_block(draw_mat, "beta_tail_scale", K = P)
      tail_scale <- array(NA_real_, dim = c(S, n_x))
      ts_link <- gpd_plan$tail_scale$link %||% "exp"
      ts_power <- gpd_plan$tail_scale$link_power %||% NULL
      for (s in seq_len(S)) {
        eta <- as.numeric(X %*% beta_ts[s, ])
        tail_scale[s, ] <- as.numeric(.apply_link(eta, ts_link, ts_power))
      }
    } else {
      if (!("tail_scale" %in% colnames(draw_mat))) stop("tail_scale not found in posterior draws.", call. = FALSE)
      tail_scale <- as.numeric(draw_mat[, "tail_scale"])
    }
  }

  .threshold_at <- function(s, i) {
    if (!is.null(threshold_mat)) return(threshold_mat[s, i])
    threshold_scalar[s]
  }

  .tail_scale_at <- function(s, i) {
    if (is.array(tail_scale) && length(dim(tail_scale)) == 2L) return(tail_scale[s, i])
    tail_scale[s]
  }

  .build_args0_or_null <- function(s) {
    w_s <- as.numeric(W_draws[s, ])
    if (!all(is.finite(w_s))) return(NULL)
    args0 <- if (pred_backend == "sb") list(w = w_s) else list()
    for (nm in base_params) {
      v <- as.numeric(bulk_draws[[nm]][s, ])
      if (!all(is.finite(v))) return(NULL)
      args0[[nm]] <- v
    }
    if (GPD) {
      xi <- as.numeric(tail_shape[s])
      if (!is.finite(xi)) return(NULL)
      args0$tail_shape <- xi
    }
    args0
  }

  violations <- list(
    cdf_range = 0L,
    cdf_monotone = 0L,
    density_nonneg = 0L,
    continuity = 0L
  )

  details <- list(
    bad_draws = integer(0),
    examples = list()
  )

  for (s in idx) {
    args0 <- .build_args0_or_null(s)
    if (is.null(args0)) {
      violations$cdf_range <- violations$cdf_range + 1L
      details$bad_draws <- c(details$bad_draws, s)
      next
    }

    for (i in seq_len(n_x)) {
      args <- args0
      if (GPD) {
        args$threshold <- .threshold_at(s, i)
        args$tail_scale <- .tail_scale_at(s, i)
        if (!is.finite(args$threshold) || !is.finite(args$tail_scale) || args$tail_scale <= 0) {
          violations$cdf_range <- violations$cdf_range + 1L
          next
        }
      }

      cdfv <- as.numeric(do.call(p_fun, c(list(q = grid, lower.tail = 1L, log.p = 0L), args)))
      dens <- as.numeric(do.call(d_fun, c(list(x = grid, log = 0L), args)))

      if (any(!is.finite(cdfv)) || min(cdfv, na.rm = TRUE) < -tol || max(cdfv, na.rm = TRUE) > 1 + tol) {
        violations$cdf_range <- violations$cdf_range + 1L
        if (length(details$examples) < 5L) details$examples <- c(details$examples, list(list(draw = s, row = i, check = "cdf_range")))
      }

      if (any(diff(cdfv) < -tol, na.rm = TRUE)) {
        violations$cdf_monotone <- violations$cdf_monotone + 1L
        if (length(details$examples) < 5L) details$examples <- c(details$examples, list(list(draw = s, row = i, check = "cdf_monotone")))
      }

      if (any(!is.finite(dens)) || min(dens, na.rm = TRUE) < -tol) {
        violations$density_nonneg <- violations$density_nonneg + 1L
        if (length(details$examples) < 5L) details$examples <- c(details$examples, list(list(draw = s, row = i, check = "density_nonneg")))
      }

      if (check_continuity && GPD) {
        u <- as.numeric(args$threshold)
        if (is.finite(u)) {
          left <- as.numeric(do.call(p_fun, c(list(q = u - eps, lower.tail = 1L, log.p = 0L), args)))
          right <- as.numeric(do.call(p_fun, c(list(q = u + eps, lower.tail = 1L, log.p = 0L), args)))
          if (is.finite(left) && is.finite(right) && abs(right - left) > 1e-4) {
            violations$continuity <- violations$continuity + 1L
            if (length(details$examples) < 5L) details$examples <- c(details$examples, list(list(draw = s, row = i, check = "continuity")))
          }
        }
      }
    }
  }

  pass <- list(
    cdf_range = (violations$cdf_range == 0L),
    cdf_monotone = (violations$cdf_monotone == 0L),
    density_nonneg = (violations$density_nonneg == 0L),
    continuity = if (GPD && check_continuity) (violations$continuity == 0L) else NA
  )

  list(
    pass = pass,
    violations = violations,
    n_checked_draws = length(idx),
    n_x = n_x,
    grid_n = G,
    details = details
  )
}
