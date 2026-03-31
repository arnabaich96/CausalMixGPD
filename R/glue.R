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


  spec <- fit$spec %||% list()
  meta <- spec$meta %||% list()

  backend <- meta$backend %||% spec$dispatch$backend %||% "<unknown>"
  GPD <- isTRUE(meta$GPD %||% spec$dispatch$GPD)
  is_spliced <- identical(backend, "spliced")
  pred_backend <- if (backend %in% c("crp", "spliced")) "sb" else backend

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

  # GPD pieces (constant or link)
  tail_shape <- NULL
  threshold_mat <- NULL
  threshold_scalar <- NULL
  tail_scale <- NULL
  spliced_gpd_draws <- list()
  spliced_gpd_link <- list()
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

  .compute_link_eta <- function(s) {
    if (!length(link_params)) return(list())
    out <- list()
    for (nm in link_params) {
      plan <- link_plan[[nm]] %||% list()
      mode <- plan$mode %||% "constant"
      if (identical(mode, "link")) {
        if (is.null(X)) stop("link-mode requires X.", call. = FALSE)
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
              out[[nm]] <- matrix(as.numeric(.apply_link(eta, link, pw)), nrow = n_x)
            }
          } else {
            beta_nm <- .indexed_block(draw_mat, paste0("beta_", nm), K = P)
            eta <- as.numeric(X %*% beta_nm[s, ])
            out[[nm]] <- matrix(as.numeric(.apply_link(eta, link, pw)), nrow = n_x)
          }
        }
      } else {
        if (!(nm %in% colnames(draw_mat))) stop(sprintf("'%s' not found in posterior draws.", nm), call. = FALSE)
        out[[nm]] <- matrix(rep(as.numeric(draw_mat[s, nm]), n_x), nrow = n_x)
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
          if (is.null(X)) stop(sprintf("%s link-mode requires X.", nm), call. = FALSE)
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
        if (is.null(X)) stop("threshold link-mode requires X.", call. = FALSE)
        beta_thr <- .indexed_block(draw_mat, "beta_threshold", K = P)
        threshold_mat <- matrix(NA_real_, nrow = S, ncol = n_x)
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
        if (is.null(X)) stop("tail_scale link-mode requires X.", call. = FALSE)
        beta_ts <- .indexed_block(draw_mat, "beta_tail_scale", K = P)
        tail_scale <- matrix(NA_real_, nrow = S, ncol = n_x)
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
        if (is.null(X)) stop("tail_shape link-mode requires X.", call. = FALSE)
        beta_tsh <- .indexed_block(draw_mat, "beta_tail_shape", K = P)
        tail_shape <- matrix(NA_real_, nrow = S, ncol = n_x)
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

  .build_args0_or_null <- function(s) {
    w_s <- as.numeric(W_draws[s, ])
    if (!all(is.finite(w_s))) return(NULL)
    args0 <- if (pred_backend == "sb") list(w = w_s) else list()
    for (nm in base_params) {
      v <- as.numeric(bulk_draws[[nm]][s, ])
      sup <- as.character((get_kernel_registry()[[meta$kernel %||% spec$kernel$key %||% "<unknown>"]] %||% list())$bulk_support[[nm]] %||% "")
      if (sup %in% c("positive_sd", "positive_scale", "positive_shape", "positive_location")) {
        if (!all(is.finite(v) & (v > 0))) return(NULL)
      } else if (!all(is.finite(v))) {
        return(NULL)
      }
      args0[[nm]] <- v
    }
    if (GPD && !is_spliced && !is.matrix(tail_shape)) {
      xi <- as.numeric(tail_shape[s])
      if (!is.finite(xi)) return(NULL)
      args0$tail_shape <- xi
    }
    args0
  }

  .support_ok <- function(nm, v) {
    sup <- as.character((get_kernel_registry()[[meta$kernel %||% spec$kernel$key %||% "<unknown>"]] %||% list())$bulk_support[[nm]] %||% "")
    if (sup %in% c("positive_sd", "positive_scale", "positive_shape", "positive_location")) {
      return(all(is.finite(v) & (v > 0)))
    }
    all(is.finite(v))
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

  spliced_scalar <- if (is_spliced && GPD) .get_dispatch_scalar(fit, backend_override = "crp") else NULL

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

  .spliced_density_cdf_row <- function(s, i, yvals, quantity, link_eta, gpd_eta) {
    w_s <- .normalize_weights_or_null(W_draws[s, ])
    if (is.null(w_s)) return(rep(NA_real_, length(yvals)))
    comp_args <- .spliced_component_args_list_or_null(s, i, link_eta = link_eta, gpd_eta = gpd_eta)
    if (is.null(comp_args)) return(rep(NA_real_, length(yvals)))

    out <- numeric(length(yvals))
    for (j in seq_along(yvals)) {
      yj <- yvals[j]
      vals <- vapply(seq_along(comp_args), function(k) {
        args <- comp_args[[k]]
        if (quantity == "density") {
          as.numeric(do.call(spliced_scalar$d, c(list(x = yj, log = 0L), args)))[1]
        } else {
          cdfv <- as.numeric(do.call(spliced_scalar$p, c(list(q = yj, lower.tail = 1L, log.p = 0L), args)))[1]
          pmin(pmax(cdfv, 0), 1)
        }
      }, numeric(1))
      out[j] <- sum(w_s * vals)
    }
    out
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
    if (!.draw_valid[s]) {
      violations$cdf_range <- violations$cdf_range + 1L
      details$bad_draws <- c(details$bad_draws, s)
      next
    }

    args0 <- .build_args0_or_null(s)
    if (is.null(args0)) {
      violations$cdf_range <- violations$cdf_range + 1L
      details$bad_draws <- c(details$bad_draws, s)
      next
    }
    link_eta <- .compute_link_eta(s)
    gpd_eta <- if (is_spliced && GPD) .compute_spliced_gpd_eta(s) else list()

    for (i in seq_len(n_x)) {
      if (is_spliced && GPD) {
        cdfv <- .spliced_density_cdf_row(s, i, grid, quantity = "cdf", link_eta = link_eta, gpd_eta = gpd_eta)
        dens <- .spliced_density_cdf_row(s, i, grid, quantity = "density", link_eta = link_eta, gpd_eta = gpd_eta)
      } else {
        args <- args0
        if (GPD) {
          args$threshold <- .threshold_at(s, i)
          args$tail_scale <- .tail_scale_at(s, i)
          args$tail_shape <- .tail_shape_at(s, i)
          if (!is.finite(args$threshold) || !is.finite(args$tail_scale) || args$tail_scale <= 0 || !is.finite(args$tail_shape)) {
            violations$cdf_range <- violations$cdf_range + 1L
            next
          }
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
          if (bad) {
            violations$cdf_range <- violations$cdf_range + 1L
            next
          }
        }
        cdfv <- as.numeric(do.call(p_fun, c(list(q = grid, lower.tail = 1L, log.p = 0L), args)))
        dens <- as.numeric(do.call(d_fun, c(list(x = grid, log = 0L), args)))
      }

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
        if (is_spliced) {
          comp_args <- .spliced_component_args_list_or_null(s, i, link_eta = link_eta, gpd_eta = gpd_eta)
          if (!is.null(comp_args)) {
            u_vals <- unique(vapply(comp_args, function(args) as.numeric(args$threshold), numeric(1)))
            u_vals <- u_vals[is.finite(u_vals)]
            bad_continuity <- FALSE
            for (u in u_vals) {
              lr <- .spliced_density_cdf_row(s, i, c(u - eps, u + eps), quantity = "cdf", link_eta = link_eta, gpd_eta = gpd_eta)
              if (all(is.finite(lr)) && abs(lr[2] - lr[1]) > 1e-4) {
                bad_continuity <- TRUE
                break
              }
            }
            if (bad_continuity) {
              violations$continuity <- violations$continuity + 1L
              if (length(details$examples) < 5L) details$examples <- c(details$examples, list(list(draw = s, row = i, check = "continuity")))
            }
          }
        } else {
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
