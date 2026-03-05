`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Print a cluster bundle
#'
#' @param x A cluster bundle.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.dpmixgpd_cluster_bundle <- function(x, ...) {
  stopifnot(inherits(x, "dpmixgpd_cluster_bundle"))
  meta <- x$spec$meta %||% list()
  cl <- x$spec$cluster %||% list()
  cat("Cluster bundle\n")
  cat("type      :", cl$type %||% "weights", "\n")
  cat("link mode :", if (isTRUE(cl$param_link)) "param|x" else "none", "\n")
  cat("kernel    :", meta$kernel %||% "?", "\n")
  cat("GPD       :", isTRUE(meta$GPD), "\n")
  cat("n         :", as.integer(meta$N %||% NA_integer_), "\n")
  cat("components:", as.integer(meta$components %||% NA_integer_), "\n")
  invisible(x)
}

#' Summarize a cluster bundle
#'
#' @param object A cluster bundle.
#' @param ... Unused.
#' @return A summary list.
#' @export
summary.dpmixgpd_cluster_bundle <- function(object, ...) {
  stopifnot(inherits(object, "dpmixgpd_cluster_bundle"))
  meta <- object$spec$meta %||% list()
  cl <- object$spec$cluster %||% list()
  out <- list(
    type = cl$type %||% "weights",
    link_mode = if (isTRUE(cl$param_link)) "param|x" else "none",
    kernel = meta$kernel %||% NA_character_,
    GPD = isTRUE(meta$GPD),
    N = as.integer(meta$N %||% NA_integer_),
    P = as.integer(meta$P %||% 0L),
    components = as.integer(meta$components %||% NA_integer_),
    monitors = object$monitors %||% character(0)
  )
  class(out) <- c("summary.dpmixgpd_cluster_bundle", "list")
  out
}

#' Plot a cluster bundle
#'
#' @param x A cluster bundle.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
plot.dpmixgpd_cluster_bundle <- function(x, ...) {
  stopifnot(inherits(x, "dpmixgpd_cluster_bundle"))
  sm <- summary(x)
  graphics::plot.new()
  txt <- c(
    "Cluster Bundle",
    sprintf("Type: %s", sm$type),
    sprintf("Link mode: %s", sm$link_mode),
    sprintf("Kernel: %s", sm$kernel),
    sprintf("GPD: %s", sm$GPD),
    sprintf("N: %d  P: %d", sm$N, sm$P),
    sprintf("Components: %d", sm$components)
  )
  graphics::text(0.02, 0.98, labels = paste(txt, collapse = "\n"), adj = c(0, 1))
  invisible(x)
}

#' Print a cluster fit
#'
#' @param x A cluster fit.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.dpmixgpd_cluster_fit <- function(x, ...) {
  stopifnot(inherits(x, "dpmixgpd_cluster_fit"))
  meta <- x$spec$meta %||% list()
  cl <- x$spec$cluster %||% list()
  cat("Cluster fit\n")
  cat("type      :", cl$type %||% "weights", "\n")
  cat("link mode :", if (isTRUE(cl$param_link)) "param|x" else "none", "\n")
  cat("kernel    :", meta$kernel %||% "?", "\n")
  cat("GPD       :", isTRUE(meta$GPD), "\n")
  cat("n         :", as.integer(meta$N %||% NA_integer_), "\n")
  cat("components:", as.integer(meta$components %||% NA_integer_), "\n")
  invisible(x)
}

#' Summarize a cluster fit
#'
#' @param object A cluster fit.
#' @param burnin Number of initial posterior draws to discard.
#' @param thin Keep every `thin`-th posterior draw.
#' @param ... Unused.
#' @return A summary list.
#' @export
summary.dpmixgpd_cluster_fit <- function(object, burnin = NULL, thin = NULL, ...) {
  stopifnot(inherits(object, "dpmixgpd_cluster_fit"))
  lbl <- predict(object, type = "label", burnin = burnin, thin = thin)
  tab <- table(lbl$labels)
  out <- list(
    K_star = length(tab),
    cluster_sizes = tab,
    source = lbl$source,
    burnin = lbl$burnin,
    thin = lbl$thin
  )
  class(out) <- c("summary.dpmixgpd_cluster_fit", "list")
  out
}

#' Plot a cluster fit
#'
#' @param x A cluster fit.
#' @param which Plot type.
#' @param burnin Number of initial posterior draws to discard.
#' @param thin Keep every `thin`-th posterior draw.
#' @param psm_max_n Maximum training sample size allowed for PSM plotting.
#' @param ... Unused.
#' @return Plot output, invisibly.
#' @export
plot.dpmixgpd_cluster_fit <- function(x, which = c("psm", "k", "sizes"), burnin = NULL, thin = NULL, psm_max_n = 2000L, ...) {
  stopifnot(inherits(x, "dpmixgpd_cluster_fit"))
  which <- match.arg(which)

  if (identical(which, "psm")) {
    psm <- predict(x, type = "psm", burnin = burnin, thin = thin, psm_max_n = psm_max_n)
    graphics::image(psm$psm, main = "Posterior Similarity Matrix", xlab = "", ylab = "")
    return(invisible(psm))
  }

  if (identical(which, "k")) {
    z <- extract_z_draws(x$samples, burnin = burnin, thin = thin)
    k_draw <- apply(z, 1, function(v) length(unique(as.integer(v))))
    graphics::plot(seq_along(k_draw), k_draw, type = "l", xlab = "Draw", ylab = "K", main = "Clusters per draw")
    return(invisible(k_draw))
  }

  lbl <- predict(x, type = "label", burnin = burnin, thin = thin)
  tab <- table(lbl$labels)
  graphics::barplot(as.numeric(tab), names.arg = names(tab), xlab = "Cluster", ylab = "Size", main = "Dahl cluster sizes")
  invisible(lbl)
}

#' Print cluster labels
#'
#' @param x Cluster labels object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.dpmixgpd_cluster_labels <- function(x, ...) {
  stopifnot(inherits(x, "dpmixgpd_cluster_labels"))
  tab <- table(x$labels)
  cat("Cluster labels (", x$source %||% "train", ")\n", sep = "")
  cat("n         :", length(x$labels), "\n")
  cat("components:", x$components %||% length(tab), "\n")
  cat("sizes     :", paste(sprintf("%s:%s", names(tab), as.integer(tab)), collapse = ", "), "\n")
  invisible(x)
}

#' Summarize cluster labels
#'
#' @param object Cluster labels object.
#' @param ... Unused.
#' @return A summary list.
#' @export
summary.dpmixgpd_cluster_labels <- function(object, ...) {
  stopifnot(inherits(object, "dpmixgpd_cluster_labels"))
  tab <- table(object$labels)
  score_mat <- object$scores %||% object$probs
  max_prob <- if (is.matrix(score_mat)) apply(score_mat, 1, max) else NA_real_
  out <- list(
    source = object$source %||% "train",
    n = length(object$labels),
    components = object$components %||% length(tab),
    cluster_sizes = tab,
    certainty = if (all(is.na(max_prob))) NULL else summary(max_prob)
  )
  class(out) <- c("summary.dpmixgpd_cluster_labels", "list")
  out
}

#' Plot cluster labels
#'
#' @param x Cluster labels object.
#' @param type Plot type.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
plot.dpmixgpd_cluster_labels <- function(x, type = c("sizes", "certainty"), ...) {
  stopifnot(inherits(x, "dpmixgpd_cluster_labels"))
  type <- match.arg(type)
  if (identical(type, "sizes")) {
    tab <- table(x$labels)
    graphics::barplot(as.numeric(tab), names.arg = names(tab), xlab = "Cluster", ylab = "Size", main = "Cluster sizes")
    return(invisible(x))
  }
  score_mat <- x$scores %||% x$probs
  if (!is.matrix(score_mat)) {
    warning("No score matrix available for certainty plot.", call. = FALSE)
    return(invisible(x))
  }
  max_prob <- apply(score_mat, 1, max)
  graphics::hist(max_prob, breaks = 20, xlab = "Max assignment probability", main = "Label certainty")
  invisible(x)
}

#' Print a cluster posterior similarity matrix
#'
#' @param x Cluster PSM object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.dpmixgpd_cluster_psm <- function(x, ...) {
  stopifnot(inherits(x, "dpmixgpd_cluster_psm"))
  n <- nrow(x$psm %||% matrix(0, 0, 0))
  cat("Cluster PSM\n")
  cat("n         :", n, "\n")
  cat("components:", x$components %||% NA_integer_, "\n")
  cat("draw_index:", x$draw_index %||% NA_integer_, "\n")
  invisible(x)
}

#' Summarize a cluster posterior similarity matrix
#'
#' @param object Cluster PSM object.
#' @param ... Unused.
#' @return A summary list.
#' @export
summary.dpmixgpd_cluster_psm <- function(object, ...) {
  stopifnot(inherits(object, "dpmixgpd_cluster_psm"))
  p <- object$psm
  out <- list(
    n = nrow(p),
    components = object$components %||% NA_integer_,
    min = min(p, na.rm = TRUE),
    mean = mean(p, na.rm = TRUE),
    max = max(p, na.rm = TRUE),
    diagonal_mean = mean(diag(p), na.rm = TRUE)
  )
  class(out) <- c("summary.dpmixgpd_cluster_psm", "list")
  out
}

#' Plot a cluster posterior similarity matrix
#'
#' @param x Cluster PSM object.
#' @param psm_max_n Maximum allowed matrix size for plotting.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
plot.dpmixgpd_cluster_psm <- function(x, psm_max_n = x$psm_max_n %||% 2000L, ...) {
  stopifnot(inherits(x, "dpmixgpd_cluster_psm"))
  psm_max_n <- as.integer(psm_max_n)[1]
  if (!is.finite(psm_max_n) || psm_max_n < 1L) {
    stop("'psm_max_n' must be an integer >= 1.", call. = FALSE)
  }
  n <- nrow(x$psm %||% matrix(0, 0, 0))
  if (n > psm_max_n) {
    stop(
      sprintf(
        "PSM plot blocked: n=%d exceeds psm_max_n=%d. Increase 'psm_max_n' to plot.",
        n,
        psm_max_n
      ),
      call. = FALSE
    )
  }
  graphics::image(x$psm, main = "Posterior Similarity Matrix", xlab = "", ylab = "")
  invisible(x)
}
