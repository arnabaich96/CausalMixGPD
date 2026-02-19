#' Internal Prediction Engine (internal)
#'
#' Evaluate posterior predictions per draw, then summarize.
#' This is the core prediction workhorse implementing all prediction types.
#'
#' **Prediction Types:**
#' - **density/survival**: Either provide both (x,y) or neither (defaults to training X and training y).
#' - **quantile/median**: y must be NULL; x may be provided (new X) or NULL (defaults to training X).
#' - **sample/fit/mean/rmean**: Posterior predictive draws or summaries.
#' - **CRP predictions**: Use posterior weights derived from z for each draw.
#'
#' **Storage:** Per-draw results are stored in object$cache$predict (environment) for reuse in treatment effects.
#'
#' @name prediction-engine
#' @keywords internal
#' @noRd
NULL

#' Internal prediction engine: evaluate per posterior draw, then summarize
#'
#' @param object mixgpd_fit object
#' @param x Optional new predictor matrix
#' @param y Optional response vector for density/survival types
#' @param ps Optional propensity scores
#' @param id Optional id vector for predictions
#' @param type Character; prediction type (density, survival, quantile, sample, mean, rmean, median, fit)
#' @param p Numeric; quantile index (alias for 'index')
#' @param index Numeric; quantile/median index in (0,1)
#' @param nsim Integer; number of posterior draws to sample (for type='sample')
#' @param level Numeric in (0,1); credible level for intervals
#' @param interval Character or NULL; interval type ("credible", "hpd", or NULL)
#' @param probs Numeric vector; quantiles to report in summary
#' @param store_draws Logical; whether to cache per-draw results
#' @param nsim_mean Integer; MC samples for posterior mean estimation
#' @param cutoff Numeric; cutoff for restricted mean (rmean)
#' @param ncores Integer; number of cores for parallel prediction
#' @return List with class 'mixgpd_predict' containing fit dataframe, grid, draws, diagnostics
#' @keywords internal
#' @noRd
.predict_mixgpd <- function(object,
                            x = NULL, y = NULL, ps = NULL, id = NULL,
                            type = c("density", "survival", "quantile", "sample", "mean", "rmean", "median", "fit"),
                            p = NULL, index = NULL, nsim = NULL,
                            level = 0.95,
                            interval = "credible",
                            probs = c(0.025, 0.5, 0.975),
                            store_draws = TRUE,
                            nsim_mean = 200L,
                            cutoff = NULL,
                            ndraws_pred = NULL,
                            chunk_size = NULL,
                            ncores = 1L) {

  .validate_fit(object)
  type <- match.arg(type)

  id_info <- .resolve_predict_id(x, id = id)
  x <- id_info$x
  id_vec <- id_info$id

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
  if (!is.null(ndraws_pred)) {
    ndraws_pred <- as.integer(ndraws_pred)[1L]
    if (!is.finite(ndraws_pred) || ndraws_pred < 1L) {
      stop("'ndraws_pred' must be NULL or a positive integer.", call. = FALSE)
    }
  }
  if (!is.null(chunk_size)) {
    chunk_size <- as.integer(chunk_size)[1L]
    if (!is.finite(chunk_size) || chunk_size < 1L) {
      stop("'chunk_size' must be NULL or a positive integer.", call. = FALSE)
    }
  }

  .vec_sig <- function(v) {
    v <- as.numeric(v)
    if (!length(v)) return("0:0:0")
    take <- v[seq_len(min(16L, length(v)))]
    paste(length(v), sprintf("%.10f", sum(take, na.rm = TRUE)), sprintf("%.10f", sum(v, na.rm = TRUE)), sep = ":")
  }

  # Spec / meta
  spec <- object$spec %||% list()
  meta <- spec$meta %||% list()

  backend <- meta$backend %||% spec$dispatch$backend %||% "<unknown>"
  kernel  <- meta$kernel  %||% spec$kernel$key %||% "<unknown>"
  GPD     <- isTRUE(meta$GPD %||% spec$dispatch$GPD)

  # Use mixture dispatch for prediction even when backend is CRP/spliced
  # For spliced backend with link-mode GPD params, component-level parameters
  # must be reconstructed from beta coefficients and newdata X values.
  pred_backend <- if (backend %in% c("crp", "spliced")) "sb" else backend
  is_spliced <- identical(backend, "spliced")

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

  if (type %in% c("sample", "fit", "mean", "rmean")) {
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

  # Prediction dimensions
  n_pred <- if (!is.null(Xpred)) nrow(Xpred) else 1L
  if (!is.null(id_vec) && length(id_vec) != n_pred) {
    stop("Length of 'id' must match the number of prediction rows.", call. = FALSE)
  }
  id_vals <- if (!is.null(id_vec)) id_vec else seq_len(n_pred)

  # Fast defaults for large prediction jobs.
  if (n_pred > 20000L) {
    if (is.null(ndraws_pred)) {
      ndraws_pred <- 200L
      message("Large prediction request detected (N > 20000): using ndraws_pred=200 by default.")
    }
    if (is.null(chunk_size)) {
      chunk_size <- 10000L
      message("Large prediction request detected (N > 20000): using chunk_size=10000 by default.")
    }
  }

  pred_cache <- NULL
  if (is.list(object$cache) && !is.null(object$cache$predict_env) && is.environment(object$cache$predict_env)) {
    pred_cache <- object$cache$predict_env
  }
  cache_key <- NULL
  if (!is.null(pred_cache) && type %in% c("density", "survival", "quantile", "median") && ncores == 1L) {
    x_sig <- if (is.null(Xpred)) "x:none" else paste0("x:", nrow(Xpred), "x", ncol(Xpred))
    y_sig <- if (!is.null(ygrid)) paste0("y:", .vec_sig(ygrid)) else "y:none"
    p_sig <- if (!is.null(pgrid)) paste0("p:", .vec_sig(pgrid)) else "p:none"
    cache_key <- paste(type, kernel %||% "", x_sig, y_sig, p_sig, ndraws_pred %||% NA_integer_, sep = "|")
    if (exists(cache_key, envir = pred_cache, inherits = FALSE)) {
      cached <- get(cache_key, envir = pred_cache, inherits = FALSE)
      if (is.list(cached) && inherits(cached, "mixgpd_predict")) {
        cached$diagnostics <- utils::modifyList(cached$diagnostics %||% list(), list(cache_hit = TRUE))
        return(cached)
      }
    }
  }

  .cache_store <- function(obj) {
    if (!is.null(pred_cache) && is.character(cache_key) && nzchar(cache_key)) {
      assign(cache_key, obj, envir = pred_cache)
    }
    obj
  }

  # -----------------------------
  # Posterior draws extraction
  # -----------------------------
  draw_mat <- .extract_draws_matrix(object)
  if (is.null(draw_mat) || !is.matrix(draw_mat) || nrow(draw_mat) < 2L) {
    stop("Posterior draws not found or malformed in fitted object.", call. = FALSE)
  }
  if (!is.null(ndraws_pred) && ndraws_pred < nrow(draw_mat)) {
    set.seed(1L)
    keep_idx <- sort(sample.int(nrow(draw_mat), size = ndraws_pred, replace = FALSE))
    draw_mat <- draw_mat[keep_idx, , drop = FALSE]
  }
  S <- nrow(draw_mat)

  # mixture weights + bulk parameter blocks
  # - W_draws: S x K
  # - bulk_draws: list(param -> S x K or S x 1 or S x ?)
  # - base_params: names of bulk params required by kernel
  W_draws <- .extract_weights(draw_mat, backend = pred_backend)
  bulk_draws <- .extract_bulk_params(draw_mat, bulk_params = bulk_params)
  base_params <- names(bulk_draws)

  if (!is.null(chunk_size) && !is.null(Xpred) && n_pred > chunk_size) {
    starts <- seq.int(1L, n_pred, by = chunk_size)
    parts <- vector("list", length(starts))
    for (ii in seq_along(starts)) {
      i0 <- starts[ii]
      i1 <- min(n_pred, i0 + chunk_size - 1L)
      idx <- i0:i1
      x_chunk <- Xpred[idx, , drop = FALSE]
      y_chunk <- NULL
      if (type %in% c("density", "survival") && has_X) y_chunk <- ygrid[idx]
      ps_chunk <- if (!is.null(ps)) ps[idx] else NULL
      id_chunk <- if (!is.null(id_vals)) id_vals[idx] else NULL
      parts[[ii]] <- .predict_mixgpd(
        object = object, x = x_chunk, y = y_chunk, ps = ps_chunk, id = id_chunk,
        type = type, p = p, index = index, nsim = nsim, level = level, interval = interval,
        probs = probs, store_draws = store_draws, nsim_mean = nsim_mean, cutoff = cutoff,
        ndraws_pred = ndraws_pred, chunk_size = NULL, ncores = ncores
      )
    }
    out <- parts[[1L]]
    if (is.data.frame(out$fit)) {
      out$fit <- do.call(rbind, lapply(parts, function(z) z$fit))
      rownames(out$fit) <- NULL
    }
    if (!is.null(out$diagnostics)) out$diagnostics$n_chunks <- length(parts)
    return(.cache_store(out))
  }

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
    gpd_plan <- spec$dispatch$gpd %||% meta$gpd %||% list()
    
    # Check for unsupported spliced + GPD + link mode combination
    if (is_spliced) {
      thr_mode <- gpd_plan$threshold$mode %||% "dist"
      ts_mode <- gpd_plan$tail_scale$mode %||% "dist"
      tsh_mode <- gpd_plan$tail_shape$mode %||% "dist"
      
      if (any(c(thr_mode, ts_mode, tsh_mode) == "link")) {
        stop(
          "Spliced backend with link-mode GPD parameters is not yet fully implemented in prediction.\n",
          "For now, use fixed or dist modes for GPD parameters with spliced backend.\n",
          "Full link-mode support for component-specific tail covariate effects is planned.",
          call. = FALSE
        )
      }
    }
    
    if (!("tail_shape" %in% colnames(draw_mat))) stop("tail_shape not found in posterior draws.", call. = FALSE)
    tail_shape <- as.numeric(draw_mat[, "tail_shape"])

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
      warning("ncores > 1 requested but 'future'/'future.apply' are unavailable; running sequentially.",
              call. = FALSE)
      return(lapply(idx, FUN))
    }

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = ncores)

    old_max <- getOption("future.globals.maxSize")
    on.exit(options(future.globals.maxSize = old_max), add = TRUE)
    options(future.globals.maxSize = Inf)

    future.apply::future_lapply(idx, FUN)
  }

  # [Continued in next sections...]
  # The full function is split below into type-specific handlers
  # for readability. Each type calls appropriate (.one_draw, .row_interval functions.


  # density / survival
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
          iv <- .compute_interval(draws_arr[, i, j], level = level, type = interval)
          lower[i, j] <- iv["lower"]
          upper[i, j] <- iv["upper"]
        }
      }
    }

    # Build DF (id varies slowest, y varies fastest)
    fit_df <- data.frame(
      id = rep(id_vals, each = G),
      y = rep(ygrid_num, times = n_pred),
      estimate = as.vector(t(fit)),
      lower = if (!is.null(lower)) as.vector(t(lower)) else NA_real_,
      upper = if (!is.null(upper)) as.vector(t(upper)) else NA_real_,
      row.names = NULL
    )
    colnames(fit_df)[colnames(fit_df) == "estimate"] <- ifelse(type == "density", "density", "survival")
    fit_df <- .reorder_predict_cols(fit_df)

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
    return(.cache_store(out))
  }

  # quantile / median
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
        id       = rep(id_vals, each = length(pgrid)),
        index    = pgrid,
        estimate = as.numeric(summ$estimate),
        lower    = if (compute_interval) as.numeric(summ$lower) else NA_real_,
        upper    = if (compute_interval) as.numeric(summ$upper) else NA_real_,
        row.names = NULL
      )
      fit_df <- .reorder_predict_cols(fit_df)

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
      return(.cache_store(out))
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
      id       = rep(id_vals, each = M),
      index    = rep(pgrid, times = n_pred),
      estimate = as.vector(t(estimate)),
      lower    = if (compute_interval) as.vector(t(lower)) else NA_real_,
      upper    = if (compute_interval) as.vector(t(upper)) else NA_real_,
      row.names = NULL
    )
    fit_df <- .reorder_predict_cols(fit_df)

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
    return(.cache_store(out))
  }

  # sample (posterior predictive)
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
      return(.cache_store(res))
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
    return(.cache_store(res))
  }

  # fit (one posterior predictive draw per observation per posterior draw)
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

    id_use <- if (length(id_vals) == n_obs) id_vals else seq_len(n_obs)
    fit_df <- data.frame(
      id = id_use,
      estimate = estimate,
      lower = as.numeric(lower),
      upper = as.numeric(upper),
      row.names = NULL
    )
    fit_df <- .reorder_predict_cols(fit_df)

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
    return(.cache_store(out))
  }

  # mean (posterior mean of predictive distribution)
  if (type == "mean") {
    nsim_inner <- as.integer(nsim_mean)
    if (is.na(nsim_inner) || nsim_inner < 10L) nsim_inner <- 200L

    # If GPD and any valid draw has xi >= 1, posterior mean is infinite
    if (GPD) {
      xi_valid <- tail_shape[.draw_valid]
      if (any(is.finite(xi_valid) & xi_valid >= 1)) {
        warning("Posterior mean is infinite because there is posterior mass with tail_shape (xi) >= 1. Use type='median'/'quantile' or a restricted-mean target.", call. = FALSE)
        inf_df <- data.frame(
          id = if (has_X) id_vals else 1L,
          estimate = Inf,
          lower = if (compute_interval) Inf else NA_real_,
          upper = if (compute_interval) Inf else NA_real_,
          row.names = NULL
        )
        inf_df <- .reorder_predict_cols(inf_df)
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
        return(.cache_store(out))
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
        id = if (length(id_vals) >= 1L) id_vals[1] else 1L,
        estimate = as.numeric(summ$estimate)[1],
        lower = if (compute_interval) as.numeric(summ$lower)[1] else NA_real_,
        upper = if (compute_interval) as.numeric(summ$upper)[1] else NA_real_,
        row.names = NULL
      )
      fit_df <- .reorder_predict_cols(fit_df)

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
      return(.cache_store(out))
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
        iv <- .compute_interval(draw_means_mat[, i], level = level, type = interval)
        lower[i] <- iv["lower"]
        upper[i] <- iv["upper"]
      }
    }

    fit_df <- data.frame(
      id = id_vals,
      estimate = as.numeric(estimate),
      lower = if (compute_interval) as.numeric(lower) else NA_real_,
      upper = if (compute_interval) as.numeric(upper) else NA_real_,
      row.names = NULL
    )
    fit_df <- .reorder_predict_cols(fit_df)

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
    return(.cache_store(out))
  }


  # rmean (restricted mean E[min(Y, cutoff)])
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
        id = if (length(id_vals) >= 1L) id_vals[1] else 1L,
        estimate = as.numeric(summ$estimate)[1],
        lower = if (compute_interval) as.numeric(summ$lower)[1] else NA_real_,
        upper = if (compute_interval) as.numeric(summ$upper)[1] else NA_real_,
        row.names = NULL
      )
      fit_df <- .reorder_predict_cols(fit_df)

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
      return(.cache_store(out))
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
        iv <- .compute_interval(draw_rmeans_mat[, i], level = level, type = interval)
        lower[i] <- iv["lower"]
        upper[i] <- iv["upper"]
      }
    }

    fit_df <- data.frame(
      id = id_vals,
      estimate = as.numeric(estimate),
      lower = if (compute_interval) as.numeric(lower) else NA_real_,
      upper = if (compute_interval) as.numeric(upper) else NA_real_,
      row.names = NULL
    )
    fit_df <- .reorder_predict_cols(fit_df)

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
    return(.cache_store(out))
  }

  stop(sprintf("Unsupported prediction type '%s'.", type), call. = FALSE)
}
