#' Posterior Summary Utilities (internal)
#'
#' Helper functions for summarizing posterior draws: compute credible/HPD intervals,
#' format fit headers, and create summary tables with mean/sd/quantiles.
#'
#' @name summary-utils
#' @keywords internal
#' @noRd
NULL

#' Compute credible or HPD interval for draws
#'
#' @param draws Numeric vector of posterior draws.
#' @param level Numeric in (0, 1); credible level (default 0.95).
#' @param type Character; interval type:
#'   \itemize{
#'     \item \code{"credible"}: equal-tailed quantile intervals
#'     \item \code{"hpd"}: highest posterior density intervals
#'   }
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
#'
#' @param draws Numeric vector, matrix, or array with draws in last dimension.
#' @param probs Numeric quantile probs.
#' @param interval Character or NULL; interval type:
#'   \itemize{
#'     \item \code{NULL}: no interval
#'     \item \code{"credible"} (default): equal-tailed quantile
#'       intervals
#'     \item \code{"hpd"}: highest posterior density intervals
#'   }
#' @return List with estimate, lower, upper, and q.
#' @keywords internal
#' @noRd
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

#' Format a short header for printing
#'
#' @param x A mixgpd_fit.
#' @return Character vector lines.
#' @keywords internal
#' @noRd
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
#' @noRd
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
    .match_summary_pars <- function(tokens, all_params) {
      hits <- character(0)
      for (tok in tokens) {
        if (tok %in% all_params) {
          hits <- c(hits, tok)
          next
        }
        tok_esc <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", tok)
        pref <- grep(paste0("^", tok_esc, "(\\[|$)"), all_params, value = TRUE)
        if (!length(pref) && identical(tok, "weights")) {
          pref <- grep("^w\\[", all_params, value = TRUE)
        }
        if (!length(pref)) {
          stop("Unknown params: ", tok, call. = FALSE)
        }
        hits <- c(hits, pref)
      }
      unique(hits)
    }
    pars <- .match_summary_pars(pars, colnames(mat))
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
