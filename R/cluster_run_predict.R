`%||%` <- function(a, b) if (!is.null(a)) a else b

.cluster_draw_indices <- function(n_draws, burnin = NULL, thin = NULL) {
  burnin <- as.integer(burnin %||% 0L)
  thin <- as.integer(thin %||% 1L)
  if (!is.finite(burnin) || burnin < 0L) stop("'burnin' must be >= 0.", call. = FALSE)
  if (!is.finite(thin) || thin < 1L) stop("'thin' must be >= 1.", call. = FALSE)
  if (burnin >= n_draws) stop("'burnin' is too large for available draws.", call. = FALSE)
  seq.int(from = burnin + 1L, to = n_draws, by = thin)
}

.cluster_samples_to_matrix <- function(samples) {
  if (!requireNamespace("coda", quietly = TRUE)) {
    stop("Package 'coda' is required.", call. = FALSE)
  }
  smp <- samples
  if (inherits(smp, "mcmc")) smp <- coda::mcmc.list(smp)
  if (!inherits(smp, "mcmc.list")) stop("Expected 'mcmc' or 'mcmc.list' samples.", call. = FALSE)
  do.call(rbind, lapply(smp, as.matrix))
}

.cluster_extract_z_from_matrix <- function(draw_mat) {
  z_cols <- grep("^z\\[[0-9]+\\]$", colnames(draw_mat))
  if (!length(z_cols)) stop("No z[i] columns found in samples.", call. = FALSE)
  z_names <- colnames(draw_mat)[z_cols]
  z_idx <- as.integer(sub("^z\\[([0-9]+)\\]$", "\\1", z_names))
  z_cols <- z_cols[order(z_idx)]
  z <- draw_mat[, z_cols, drop = FALSE]
  mode(z) <- "integer"
  z
}

extract_z_draws <- function(samples, burnin = NULL, thin = NULL) {
  draw_mat <- .cluster_samples_to_matrix(samples)
  idx <- .cluster_draw_indices(nrow(draw_mat), burnin = burnin, thin = thin)
  z <- .cluster_extract_z_from_matrix(draw_mat[idx, , drop = FALSE])
  z
}

compute_psm <- function(z_draws) {
  if (exists(".compute_psm", mode = "function")) return(.compute_psm(z_draws))
  n_iter <- nrow(z_draws)
  n_obs <- ncol(z_draws)
  psm <- matrix(0, n_obs, n_obs)
  for (s in seq_len(n_iter)) {
    z <- z_draws[s, ]
    psm <- psm + outer(z, z, "==") * 1.0
  }
  psm / n_iter
}

dahl_labels <- function(z_draws, psm) {
  if (exists(".dahl_representative", mode = "function")) return(.dahl_representative(z_draws, psm))

  n_iter <- nrow(z_draws)
  ssq <- numeric(n_iter)
  for (s in seq_len(n_iter)) {
    z <- z_draws[s, ]
    A <- outer(z, z, "==") * 1.0
    ssq[s] <- sum((A - psm)^2)
  }
  s_star <- which.min(ssq)
  z_hat <- as.integer(z_draws[s_star, ])
  labels <- match(z_hat, unique(z_hat))
  list(draw_index = s_star, labels = labels, K = length(unique(labels)))
}

.cluster_compute_scores <- function(z_draws, labels, psm) {
  if (exists(".compute_cluster_probs", mode = "function")) {
    return(.compute_cluster_probs(z_draws, labels, psm))
  }
  n_obs <- length(labels)
  K <- length(unique(labels))
  probs <- matrix(0, nrow = n_obs, ncol = K)
  for (k in seq_len(K)) {
    idx <- which(labels == k)
    for (i in seq_len(n_obs)) probs[i, k] <- mean(psm[i, idx])
  }
  rs <- rowSums(probs)
  rs[rs <= 0] <- 1
  probs / rs
}

.cluster_compute_probs <- .cluster_compute_scores

.cluster_build_design <- function(meta, newdata) {
  if (!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
  response_name <- meta$response %||% "y"
  if (!(response_name %in% names(newdata))) {
    stop(sprintf("newdata must include response column '%s'.", response_name), call. = FALSE)
  }

  y_new <- as.numeric(newdata[[response_name]])
  if (anyNA(y_new)) stop("newdata response contains NA.", call. = FALSE)

  trm <- meta$terms
  has_X <- length(meta$X_cols %||% character(0)) > 0L
  if (!has_X) {
    return(list(y = y_new, X = NULL))
  }

  rhs <- stats::delete.response(trm)
  mf_new <- stats::model.frame(
    rhs,
    data = newdata,
    xlev = meta$xlevels %||% NULL,
    na.action = stats::na.fail
  )
  mm <- stats::model.matrix(rhs, data = mf_new, contrasts.arg = meta$contrasts %||% NULL)
  if ("(Intercept)" %in% colnames(mm)) {
    mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  }
  X_cols <- meta$X_cols %||% character(0)
  miss <- setdiff(X_cols, colnames(mm))
  if (length(miss)) {
    stop(sprintf("newdata is missing required predictors: %s", paste(miss, collapse = ", ")), call. = FALSE)
  }
  mm <- mm[, X_cols, drop = FALSE]
  storage.mode(mm) <- "double"
  list(y = y_new, X = mm)
}

.cluster_link_apply <- function(eta, link = "identity", link_power = NULL) {
  link <- as.character(link %||% "identity")
  if (identical(link, "identity")) return(eta)
  if (identical(link, "exp")) return(exp(eta))
  if (identical(link, "log")) return(log(eta))
  if (identical(link, "softplus")) return(log1p(exp(eta)))
  if (identical(link, "power")) {
    pw <- as.numeric(link_power)
    if (!is.finite(pw) || length(pw) != 1L) stop("Invalid power link exponent.", call. = FALSE)
    return(eta ^ pw)
  }
  stop(sprintf("Unsupported link '%s'.", link), call. = FALSE)
}

.cluster_indexed_block <- function(draw_mat, base, K = NULL, allow_missing = FALSE) {
  cn <- colnames(draw_mat)
  pat <- paste0("^", base, "\\[([0-9]+)\\]$")
  hit <- grepl(pat, cn)
  if (!any(hit)) {
    if (isTRUE(allow_missing)) return(NULL)
    stop(sprintf("No indexed columns found for '%s[i]'.", base), call. = FALSE)
  }
  idx <- as.integer(sub(pat, "\\1", cn[hit]))
  ord <- order(idx)
  idx <- idx[ord]
  cols <- cn[hit][ord]
  if (is.null(K)) K <- max(idx, na.rm = TRUE)
  K <- as.integer(K)
  out <- matrix(0.0, nrow = nrow(draw_mat), ncol = K)
  for (j in seq_along(cols)) {
    k <- idx[j]
    if (!is.na(k) && k >= 1L && k <= K) out[, k] <- draw_mat[, cols[j]]
  }
  out
}

.cluster_extract_beta_component <- function(draw_row, base, comp, P) {
  out <- rep(0, P)
  for (p in seq_len(P)) {
    nm <- sprintf("%s[%d,%d]", base, comp, p)
    if (!(nm %in% names(draw_row))) {
      nm <- sprintf("%s[%d, %d]", base, comp, p)
    }
    if (nm %in% names(draw_row)) out[p] <- as.numeric(draw_row[[nm]])
  }
  out
}

.cluster_extract_beta_global <- function(draw_row, base, P) {
  out <- rep(0, P)
  for (p in seq_len(P)) {
    nm <- sprintf("%s[%d]", base, p)
    if (nm %in% names(draw_row)) out[p] <- as.numeric(draw_row[[nm]])
  }
  out
}

.cluster_extract_beta_auto <- function(draw_row, base, comp, P) {
  if (P < 1L) return(numeric(0))
  nn <- names(draw_row)
  has_component <- any(grepl(sprintf("^%s\\[%d,\\s*[0-9]+\\]$", base, comp), nn))
  if (isTRUE(has_component)) {
    return(.cluster_extract_beta_component(draw_row, base = base, comp = comp, P = P))
  }
  .cluster_extract_beta_global(draw_row, base = base, P = P)
}

.cluster_softmax <- function(logits) {
  logits <- as.numeric(logits)
  if (!length(logits)) return(numeric(0))
  shift <- max(logits)
  ex <- exp(logits - shift)
  s <- sum(ex)
  if (!is.finite(s) || s <= 0) return(rep(1 / length(logits), length(logits)))
  ex / s
}

.cluster_extract_gating_draw <- function(draw_row, K, P) {
  K <- as.integer(K)
  P <- as.integer(P)
  if (K < 2L || P < 1L) return(NULL)
  eta <- rep(0, K - 1L)
  B <- matrix(0, nrow = K - 1L, ncol = P)
  nn <- names(draw_row)

  for (j in seq_len(K - 1L)) {
    nm_eta <- sprintf("eta[%d]", j)
    if (nm_eta %in% nn) eta[j] <- as.numeric(draw_row[[nm_eta]])
    for (p in seq_len(P)) {
      nm <- sprintf("B[%d,%d]", j, p)
      if (!(nm %in% nn)) nm <- sprintf("B[%d, %d]", j, p)
      if (nm %in% nn) B[j, p] <- as.numeric(draw_row[[nm]])
    }
  }

  if (!any(grepl("^eta\\[[0-9]+\\]$", nn)) || !any(grepl("^B\\[[0-9]+,\\s*[0-9]+\\]$", nn))) {
    return(NULL)
  }
  list(eta = eta, B = B)
}

.cluster_gating_weights <- function(gating_draw, x_row) {
  if (is.null(gating_draw)) return(NULL)
  eta <- gating_draw$eta
  B <- gating_draw$B
  K <- length(eta) + 1L
  x_row <- as.numeric(x_row)
  lin <- as.numeric(B %*% x_row)
  logits <- c(eta + lin, 0)
  out <- .cluster_softmax(logits)
  if (length(out) != K) return(NULL)
  out
}

.cluster_resolve_density_fun <- function(spec) {
  meta <- spec$meta %||% list()
  kernel <- meta$kernel
  GPD <- isTRUE(meta$GPD)
  kdef <- get_kernel_registry()[[kernel]]
  if (is.null(kdef)) stop(sprintf("Kernel '%s' not found.", kernel), call. = FALSE)
  d_name <- if (GPD) kdef$crp$d_gpd else kdef$crp$d_base
  if (is.null(d_name) || isTRUE(is.na(d_name))) stop("Could not resolve density function.", call. = FALSE)

  ns_pkg <- getNamespace("CausalMixGPD")
  ns_stats <- getNamespace("stats")
  ns_nimble <- getNamespace("nimble")

  if (exists(d_name, envir = ns_pkg, inherits = FALSE)) return(get(d_name, envir = ns_pkg))
  if (exists(d_name, envir = ns_stats, inherits = FALSE)) return(get(d_name, envir = ns_stats))
  if (exists(d_name, envir = ns_nimble, inherits = FALSE)) return(get(d_name, envir = ns_nimble))
  stop(sprintf("Density function '%s' is unavailable.", d_name), call. = FALSE)
}

.cluster_component_density <- function(spec, draw_row, k, x_row, y_val, density_fun) {
  meta <- spec$meta %||% list()
  plan <- spec$plan %||% list()
  bulk <- plan$bulk %||% list()
  arg_order <- if (isTRUE(meta$GPD)) spec$signatures$gpd$args else spec$signatures$bulk$args
  P <- as.integer(meta$P %||% 0L)

  args <- list(x = as.numeric(y_val))

  for (nm in arg_order) {
    if (nm %in% names(bulk)) {
      ent <- bulk[[nm]]
      mode <- ent$mode %||% "dist"
      if (identical(mode, "link")) {
        b <- .cluster_extract_beta_component(draw_row, paste0("beta_", nm), comp = k, P = P)
        eta <- if (P > 0L) sum(x_row * b) else 0
        args[[nm]] <- .cluster_link_apply(eta, ent$link, ent$link_power)
      } else {
        col_nm <- sprintf("%s[%d]", nm, k)
        if (!(col_nm %in% names(draw_row))) {
          args[[nm]] <- as.numeric(ent$value %||% NA_real_)
        } else {
          args[[nm]] <- as.numeric(draw_row[[col_nm]])
        }
      }
      next
    }

    if (nm %in% c("threshold", "tail_scale", "tail_shape")) {
      gpd <- plan$gpd %||% list()
      ent <- gpd[[nm]] %||% list(mode = "dist")
      if (identical(ent$mode, "link")) {
        b <- .cluster_extract_beta_auto(
          draw_row = draw_row,
          base = paste0("beta_", nm),
          comp = k,
          P = P
        )
        eta <- if (P > 0L) sum(x_row * b) else 0
        args[[nm]] <- .cluster_link_apply(eta, ent$link, ent$link_power)
      } else if (identical(ent$mode, "fixed")) {
        args[[nm]] <- as.numeric(ent$value)
      } else {
        # dist mode: prefer scalar draw, fallback to first indexed value
        if (nm %in% names(draw_row)) {
          args[[nm]] <- as.numeric(draw_row[[nm]])
        } else {
          col_nm <- sprintf("%s[%d]", nm, k)
          if (col_nm %in% names(draw_row)) {
            args[[nm]] <- as.numeric(draw_row[[col_nm]])
          } else {
            args[[nm]] <- NA_real_
          }
        }
      }
      next
    }
  }

  fm <- names(formals(density_fun))
  if ("log" %in% fm && is.null(args$log)) args$log <- FALSE

  val <- suppressWarnings(tryCatch(as.numeric(do.call(density_fun, args))[1], error = function(e) NA_real_))
  if (!is.finite(val) || val < 0) val <- 0
  val
}

predict_labels_newdata <- function(fit, newdata, burnin = NULL, thin = NULL) {
  stopifnot(inherits(fit, "dpmixgpd_cluster_fit"))
  spec <- fit$spec
  meta <- spec$meta %||% list()
  formula_meta <- (spec$cluster %||% list())$formula_meta %||% list()
  newdat <- .cluster_build_design(formula_meta, newdata = newdata)

  draw_mat <- .cluster_samples_to_matrix(fit$samples)
  idx <- .cluster_draw_indices(nrow(draw_mat), burnin = burnin, thin = thin)
  draw_sub <- draw_mat[idx, , drop = FALSE]
  z_draws <- .cluster_extract_z_from_matrix(draw_sub)

  cache_key <- paste("cache", length(idx), burnin %||% 0L, thin %||% 1L, sep = "_")
  cache_env <- fit$cache_env %||% new.env(parent = emptyenv())
  if (exists(cache_key, envir = cache_env, inherits = FALSE)) {
    tr <- get(cache_key, envir = cache_env, inherits = FALSE)
    if (is.null(tr$scores_train) && !is.null(tr$probs_train)) tr$scores_train <- tr$probs_train
  } else {
    psm <- compute_psm(z_draws)
    dahl <- dahl_labels(z_draws, psm)
    scores_train <- .cluster_compute_scores(z_draws, dahl$labels, psm)
    tr <- list(psm = psm, dahl = dahl, scores_train = scores_train)
    assign(cache_key, tr, envir = cache_env)
  }

  dahl_labels_train <- as.integer(tr$dahl$labels)
  Kd <- as.integer(tr$dahl$K)
  density_fun <- .cluster_resolve_density_fun(spec)
  n_new <- length(newdat$y)
  out_scores <- matrix(0, nrow = n_new, ncol = Kd)

  K <- as.integer(meta$components %||% max(z_draws, na.rm = TRUE))
  type <- (spec$cluster %||% list())$type %||% "weights"
  use_gating <- type %in% c("weights", "both")

  for (s in seq_len(nrow(draw_sub))) {
    draw_row <- draw_sub[s, ]
    z_s <- as.integer(z_draws[s, ])
    comp_sizes <- pmax(tabulate(z_s, nbins = K), 0)
    if (sum(comp_sizes) <= 0) comp_sizes <- rep(1, K)
    gating_draw <- if (isTRUE(use_gating) && !is.null(newdat$X)) {
      .cluster_extract_gating_draw(draw_row = draw_row, K = K, P = ncol(newdat$X))
    } else {
      NULL
    }
    w_nm <- paste0("w[", seq_len(K), "]")

    map_comp_to_dahl <- matrix(0, nrow = K, ncol = Kd)
    for (k in seq_len(K)) {
      idx_k <- which(z_s == k)
      if (!length(idx_k)) {
        map_comp_to_dahl[k, ] <- rep(1 / Kd, Kd)
      } else {
        map_comp_to_dahl[k, ] <- tabulate(dahl_labels_train[idx_k], nbins = Kd) / length(idx_k)
      }
    }

    for (i in seq_len(n_new)) {
      x_row <- if (is.null(newdat$X)) numeric(0) else as.numeric(newdat$X[i, ])
      weight_factor <- comp_sizes
      if (isTRUE(use_gating)) {
        w_new <- .cluster_gating_weights(gating_draw = gating_draw, x_row = x_row)
        if (!is.null(w_new)) {
          weight_factor <- pmax(w_new, 0)
        } else if (all(w_nm %in% names(draw_row))) {
          weight_factor <- pmax(as.numeric(draw_row[w_nm]), 0)
        }
      }
      if (sum(weight_factor) <= 0) weight_factor <- rep(1 / K, K)
      like_k <- numeric(K)
      for (k in seq_len(K)) {
        like_k[k] <- .cluster_component_density(
          spec = spec,
          draw_row = draw_row,
          k = k,
          x_row = x_row,
          y_val = newdat$y[i],
          density_fun = density_fun
        )
      }
      post_k <- weight_factor * pmax(like_k, 0)
      if (!any(is.finite(post_k)) || sum(post_k) <= 0) {
        post_k <- weight_factor
      } else {
        post_k <- post_k / sum(post_k)
      }
      out_scores[i, ] <- out_scores[i, ] + as.numeric(post_k %*% map_comp_to_dahl)
    }
  }

  out_scores <- out_scores / nrow(draw_sub)
  rs <- rowSums(out_scores)
  rs[rs <= 0] <- 1
  out_scores <- out_scores / rs
  labels <- max.col(out_scores, ties.method = "first")

  list(
    labels = as.integer(labels),
    scores = out_scores,
    K = Kd,
    cache = tr
  )
}

run_cluster_mcmc <- function(bundle, ...) {
  stopifnot(inherits(bundle, "dpmixgpd_cluster_bundle"))
  base_fit <- run_mcmc_bundle_manual(bundle, ...)

  out <- list(
    call = match.call(),
    spec = bundle$spec,
    bundle = bundle,
    base_fit = base_fit,
    samples = base_fit$samples %||% (base_fit$mcmc %||% list())$samples,
    mcmc = base_fit$mcmc %||% list(),
    cache_env = new.env(parent = emptyenv()),
    psm = NULL,
    dahl = NULL
  )
  class(out) <- c("dpmixgpd_cluster_fit", "list")
  out
}

## S3 -------------------------------------------------------------------------

#' Predict cluster outputs from a cluster fit
#'
#' @param object A fitted cluster object.
#' @param newdata Optional new data containing response and predictors from the fit formula.
#' @param type Prediction type: `"label"` or `"psm"`.
#' @param burnin Number of initial posterior draws to discard.
#' @param thin Keep every `thin`-th posterior draw.
#' @param return_scores Logical; if `TRUE` and `type="label"`, include Dahl-cluster
#'   score matrix in the output.
#' @param psm_max_n Maximum training sample size allowed for `type="psm"`.
#' @param ... Unused.
#' @return A `dpmixgpd_cluster_labels` or `dpmixgpd_cluster_psm` object.
#' @export
predict.dpmixgpd_cluster_fit <- function(object,
                                         newdata = NULL,
                                         type = c("label", "psm"),
                                         burnin = NULL,
                                         thin = NULL,
                                         return_scores = FALSE,
                                         psm_max_n = 2000L,
                                         ...) {
  stopifnot(inherits(object, "dpmixgpd_cluster_fit"))
  type <- match.arg(type)
  psm_max_n <- as.integer(psm_max_n)[1]
  if (!is.finite(psm_max_n) || psm_max_n < 1L) {
    stop("'psm_max_n' must be an integer >= 1.", call. = FALSE)
  }

  if (!is.null(newdata) && identical(type, "psm")) {
    stop("type='psm' is only available for training data (newdata=NULL).", call. = FALSE)
  }

  draw_mat <- .cluster_samples_to_matrix(object$samples)
  idx <- .cluster_draw_indices(nrow(draw_mat), burnin = burnin, thin = thin)
  draw_sub <- draw_mat[idx, , drop = FALSE]
  z_draws <- .cluster_extract_z_from_matrix(draw_sub)

  cache_key <- paste("cache", length(idx), burnin %||% 0L, thin %||% 1L, sep = "_")
  cache_env <- object$cache_env %||% new.env(parent = emptyenv())
  if (exists(cache_key, envir = cache_env, inherits = FALSE)) {
    tr <- get(cache_key, envir = cache_env, inherits = FALSE)
    if (is.null(tr$scores_train) && !is.null(tr$probs_train)) tr$scores_train <- tr$probs_train
  } else {
    n_train <- ncol(z_draws)
    if (identical(type, "psm") && n_train > psm_max_n) {
      stop(
        sprintf(
          "PSM is O(n^2): n=%d exceeds psm_max_n=%d. Increase 'psm_max_n' or use type='label'.",
          n_train,
          psm_max_n
        ),
        call. = FALSE
      )
    }
    psm <- compute_psm(z_draws)
    dahl <- dahl_labels(z_draws, psm)
    scores_train <- .cluster_compute_scores(z_draws, dahl$labels, psm)
    tr <- list(psm = psm, dahl = dahl, scores_train = scores_train)
    assign(cache_key, tr, envir = cache_env)
  }

  if (identical(type, "psm")) {
    n_train <- ncol(z_draws)
    if (n_train > psm_max_n) {
      stop(
        sprintf(
          "PSM is O(n^2): n=%d exceeds psm_max_n=%d. Increase 'psm_max_n' or use type='label'.",
          n_train,
          psm_max_n
        ),
        call. = FALSE
      )
    }
    out <- list(
      psm = tr$psm,
      labels = as.integer(tr$dahl$labels),
      components = as.integer(tr$dahl$K),
      draw_index = as.integer(tr$dahl$draw_index),
      burnin = as.integer(burnin %||% 0L),
      thin = as.integer(thin %||% 1L),
      psm_max_n = psm_max_n
    )
    class(out) <- c("dpmixgpd_cluster_psm", "list")
    return(out)
  }

  if (is.null(newdata)) {
    out <- list(
      labels = as.integer(tr$dahl$labels),
      components = as.integer(tr$dahl$K),
      source = "train",
      burnin = as.integer(burnin %||% 0L),
      thin = as.integer(thin %||% 1L)
    )
    if (isTRUE(return_scores)) out$scores <- tr$scores_train
    class(out) <- c("dpmixgpd_cluster_labels", "list")
    return(out)
  }

  pred <- predict_labels_newdata(fit = object, newdata = newdata, burnin = burnin, thin = thin)
  out <- list(
    labels = as.integer(pred$labels),
    components = as.integer(pred$K),
    source = "newdata",
    burnin = as.integer(burnin %||% 0L),
    thin = as.integer(thin %||% 1L)
  )
  if (isTRUE(return_scores)) out$scores <- pred$scores
  class(out) <- c("dpmixgpd_cluster_labels", "list")
  out
}
