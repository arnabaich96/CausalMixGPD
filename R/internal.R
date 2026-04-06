# ============================================================
# Utilities (internal)
# ============================================================



#' Null-coalescing operator
#' @keywords internal
#' @noRd
#' @rdname null_coalescing
#' @name null_coalescing
`%||%` <- function(a, b) if (!is.null(a)) a else b

.cmgpd_message <- function(...) {
  msg <- paste0(..., collapse = "")
  old <- getOption("CausalMixGPD._allow_message", FALSE)
  options(CausalMixGPD._allow_message = TRUE)
  on.exit(options(CausalMixGPD._allow_message = old), add = TRUE)
  message(msg)
  invisible(msg)
}

.cmgpd_progress_colorize <- function(text, step_index, enabled = TRUE) {
  if (!isTRUE(enabled) || !nzchar(text)) return(text)
  if (!requireNamespace("cli", quietly = TRUE)) return(text)
  ncols <- get0("num_ansi_colors", envir = asNamespace("cli"), mode = "function", inherits = FALSE)
  ncols <- if (is.function(ncols)) {
    tryCatch(as.integer(ncols()), error = function(...) 0L)
  } else {
    0L
  }
  idx <- max(1L, as.integer(step_index)[1L])

  if (isTRUE(ncols >= 256L)) {
    # Broad hue spread in 256-color ANSI to keep adjacent steps visually distinct.
    palette_256 <- c(39L, 45L, 51L, 50L, 49L, 48L, 47L, 46L, 82L, 118L,
                     154L, 190L, 226L, 220L, 214L, 208L, 202L, 196L,
                     197L, 198L, 199L, 165L, 129L, 93L, 57L, 63L, 69L, 75L)
    code <- palette_256[((idx - 1L) %% length(palette_256)) + 1L]
    return(paste0("\033[38;5;", code, "m", text, "\033[39m"))
  }

  palette_named <- c(
    "col_blue", "col_cyan", "col_green", "col_yellow", "col_magenta", "col_red",
    "col_br_blue", "col_br_cyan", "col_br_green", "col_br_yellow", "col_br_magenta", "col_br_red",
    "col_silver"
  )
  fn <- get0(
    palette_named[((idx - 1L) %% length(palette_named)) + 1L],
    envir = asNamespace("cli"),
    mode = "function",
    inherits = FALSE
  )
  if (is.null(fn)) return(text)
  tryCatch(fn(text), error = function(...) text)
}

.cmgpd_progress_format <- function(current, total, step_label, label = NULL, width = 12L, color = FALSE) {
  step_label <- trimws(as.character(step_label %||% ""))
  if (!nzchar(step_label)) step_label <- "Working..."
  family_label <- trimws(as.character(label %||% ""))
  msg <- if (nzchar(family_label)) {
    paste0("[", family_label, "] ", step_label)
  } else {
    step_label
  }
  .cmgpd_progress_colorize(msg, step_index = current, enabled = color)
}

.cmgpd_progress_live_format <- function(current, total, step_label, label = NULL, color = FALSE) {
  .cmgpd_progress_format(
    current = current,
    total = total,
    step_label = step_label,
    label = label,
    color = color
  )
}

.cmgpd_progress_bar_line <- function(current, total, width = 28L) {
  current <- max(0L, as.integer(current)[1L])
  total <- max(1L, as.integer(total)[1L])
  width <- max(10L, as.integer(width)[1L])
  current <- min(current, total)
  pct <- as.integer(round(100 * current / total))
  filled <- if (current >= total) width else as.integer(floor(width * current / total))
  empty <- max(0L, width - filled)
  paste0("  [", strrep("=", filled), strrep("-", empty), "] ", sprintf("%3d%%", pct))
}

.cmgpd_progress_write <- function(text) {
  cat(text)
  utils::flush.console()
  invisible(text)
}

.cmgpd_progress_visible_nchar <- function(text) {
  text <- gsub("\033\\[[0-9;]*[A-Za-z]", "", as.character(text), perl = TRUE)
  nchar(text, type = "width")
}

.cmgpd_progress_pad_line <- function(text, width) {
  width <- max(0L, as.integer(width)[1L])
  pad <- max(0L, width - .cmgpd_progress_visible_nchar(text))
  paste0(text, strrep(" ", pad))
}

.cmgpd_progress_render <- function(ctx, status = ctx$status, final = FALSE) {
  if (!is.environment(ctx) || !identical(ctx$live_backend, "inline")) return(invisible(ctx))
  bar <- .cmgpd_progress_bar_line(ctx$current, ctx$total, width = ctx$bar_width)
  ctx$last_status_width <- max(ctx$last_status_width %||% 0L, .cmgpd_progress_visible_nchar(status))
  ctx$last_bar_width <- max(ctx$last_bar_width %||% 0L, .cmgpd_progress_visible_nchar(bar))
  status_out <- .cmgpd_progress_pad_line(status, ctx$last_status_width)
  bar_out <- .cmgpd_progress_pad_line(bar, ctx$last_bar_width)
  prefix <- if (isTRUE(ctx$rendered)) "\r\033[F" else ""
  line2_prefix <- "\n"
  suffix <- if (isTRUE(final)) "\n" else ""
  .cmgpd_progress_write(paste0(prefix, status_out, line2_prefix, bar_out, suffix))
  ctx$rendered <- TRUE
  invisible(ctx)
}

.cmgpd_progress_start <- function(total_steps, enabled = TRUE, quiet = FALSE, label = NULL) {
  total_steps <- as.integer(total_steps)[1L]
  if (!is.finite(total_steps) || total_steps < 1L) total_steps <- 1L
  enabled <- isTRUE(enabled) && !isTRUE(quiet)

  ctx <- new.env(parent = emptyenv())
  ctx$enabled <- enabled
  ctx$total <- total_steps
  ctx$current <- 0L
  ctx$label <- as.character(label %||% "")
  ctx$inline_width <- 12L
  ctx$bar_width <- 28L
  ctx$live_backend <- "none"
  ctx$rendered <- FALSE
  ctx$last_status_width <- 0L
  ctx$last_bar_width <- 0L
  ctx$step_label <- "Starting..."
  ctx$status <- .cmgpd_progress_live_format(
    current = 0L,
    total = total_steps,
    step_label = ctx$step_label,
    label = ctx$label,
    color = FALSE
  )

  has_cli <- requireNamespace("cli", quietly = TRUE)
  ctx$has_cli <- has_cli
  ctx$color_enabled <- FALSE
  if (has_cli) {
    ncols <- get0("num_ansi_colors", envir = asNamespace("cli"), mode = "function", inherits = FALSE)
    if (is.function(ncols)) {
      cols <- tryCatch(as.integer(ncols()), error = function(...) 0L)
      ctx$color_enabled <- isTRUE(cols > 1L)
    }
  }

  # Live 2-line renderer only in interactive terminals; step messages still emitted elsewhere.
  can_bar <- enabled && interactive() && !isTRUE(getOption("knitr.in.progress"))
  if (can_bar) {
    ctx$live_backend <- "inline"
    .cmgpd_progress_render(ctx, status = ctx$status, final = FALSE)
  }
  ctx
}

.cmgpd_progress_step <- function(ctx, step_label) {
  if (!is.environment(ctx) || !isTRUE(ctx$enabled)) return(invisible(ctx))
  prev <- as.integer(ctx$current)
  ctx$current <- min(prev + 1L, ctx$total)
  inc <- ctx$current - prev

  ctx$step_label <- trimws(as.character(step_label %||% ""))
  if (!nzchar(ctx$step_label)) ctx$step_label <- "Working..."
  status <- if (identical(ctx$live_backend, "none")) {
    .cmgpd_progress_format(
      current = ctx$current,
      total = ctx$total,
      step_label = ctx$step_label,
      label = ctx$label,
      width = ctx$inline_width,
      color = ctx$color_enabled
    )
  } else {
    .cmgpd_progress_live_format(
      current = ctx$current,
      total = ctx$total,
      step_label = ctx$step_label,
      label = ctx$label,
      color = ctx$color_enabled
    )
  }
  ctx$status <- status

  if (identical(ctx$live_backend, "inline")) {
    .cmgpd_progress_render(ctx, status = status, final = FALSE)
  } else {
    .cmgpd_message(status)
  }
  invisible(ctx)
}

.cmgpd_progress_done <- function(ctx, final_label = NULL) {
  if (!is.environment(ctx) || !isTRUE(ctx$enabled)) return(invisible(ctx))
  remain <- max(0L, as.integer(ctx$total) - as.integer(ctx$current))
  if (remain > 0L) ctx$current <- as.integer(ctx$total)
  final_status <- if (is.null(final_label)) {
    if (identical(ctx$live_backend, "none")) {
      .cmgpd_progress_format(
        current = ctx$total,
        total = ctx$total,
        step_label = ctx$step_label,
        label = ctx$label,
        width = ctx$inline_width,
        color = ctx$color_enabled
      )
    } else {
      .cmgpd_progress_live_format(
        current = ctx$total,
        total = ctx$total,
        step_label = ctx$step_label,
        label = ctx$label,
        color = ctx$color_enabled
      )
    }
  } else if (identical(ctx$live_backend, "none")) {
    .cmgpd_progress_format(
      current = ctx$total,
      total = ctx$total,
      step_label = as.character(final_label),
      label = ctx$label,
      width = ctx$inline_width,
      color = ctx$color_enabled
    )
  } else {
    .cmgpd_progress_live_format(
      current = ctx$total,
      total = ctx$total,
      step_label = as.character(final_label),
      label = ctx$label,
      color = ctx$color_enabled
    )
  }

  ctx$current <- as.integer(ctx$total)
  ctx$status <- final_status
  if (!is.null(final_label)) {
    ctx$step_label <- as.character(final_label)
  }
  if (identical(ctx$live_backend, "inline")) {
    .cmgpd_progress_render(ctx, status = final_status, final = TRUE)
  } else if (!is.null(final_label)) {
    .cmgpd_message(final_status)
  }
  invisible(ctx)
}

.cmgpd_capture_nimble <- function(expr, suppress = FALSE) {
  if (!isTRUE(suppress)) {
    return(eval.parent(substitute(expr)))
  }
  result <- NULL
  utils::capture.output(
    withCallingHandlers(
      result <- eval.parent(substitute(expr)),
      warning = function(w) invokeRestart("muffleWarning"),
      message = function(m) invokeRestart("muffleMessage")
    ),
    file = if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  )
  result
}


.silent_wrapper <- function(fun_name, fun, opt_name) {
  wrapper <- function(...) {
    if (isFALSE(getOption(opt_name, TRUE))) {
      return(fun(...))
    }
    call <- match.call(expand.dots = FALSE)
    args <- as.list(call)[-1]
    if (length(args) && !is.null(args$...)) {
      dot_args <- eval(args$..., parent.frame())
      args$... <- NULL
      args <- c(args, dot_args)
    }
    call_env <- new.env(parent = parent.frame())
    assign(fun_name, fun, envir = call_env)
    run <- function() do.call(fun_name, args, envir = call_env)
    vis <- withCallingHandlers(
      withVisible(run()),
      warning = function(w) invokeRestart("muffleWarning"),
      message = function(m) {
        if (isTRUE(getOption("CausalMixGPD._allow_message", FALSE))) return()
        invokeRestart("muffleMessage")
      }
    )
    if (isTRUE(vis$visible)) return(vis$value)
    invisible(vis$value)
  }
  attr(wrapper, "CausalMixGPD.silent_wrapper") <- TRUE
  wrapper
}

.wrap_exported_silent <- function(pkgname, opt_name = "CausalMixGPD.silent") {
  if (isFALSE(getOption(opt_name, TRUE))) return(invisible(FALSE))
  ns <- asNamespace(pkgname)
  exports <- getNamespaceExports(pkgname)
  for (nm in exports) {
    obj <- get(nm, envir = ns, inherits = FALSE)
    if (!is.function(obj)) next
    if (isTRUE(attr(obj, "CausalMixGPD.silent_wrapper"))) next
    assign(nm, .silent_wrapper(nm, obj, opt_name), envir = ns)
  }
  invisible(TRUE)
}

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

#' Extract indexed component-by-column parameter blocks from a draws matrix
#' @keywords internal
#' @noRd
.indexed_block_matrix <- function(mat0, base, K = NULL, P = NULL, allow_missing = FALSE) {
  cn0 <- colnames(mat0)
  pat <- paste0("^", base, "\\[([0-9]+),\\s*([0-9]+)\\]$")
  hit <- grepl(pat, cn0)
  if (!any(hit)) {
    if (isTRUE(allow_missing)) return(NULL)
    stop(sprintf("No indexed columns found for '%s[i,j]'.", base), call. = FALSE)
  }

  idx1 <- as.integer(sub(pat, "\\1", cn0[hit]))
  idx2 <- as.integer(sub(pat, "\\2", cn0[hit]))
  cols <- cn0[hit]

  if (is.null(K)) K <- max(idx1, na.rm = TRUE)
  if (is.null(P)) P <- max(idx2, na.rm = TRUE)
  K <- as.integer(K)
  P <- as.integer(P)

  out <- array(0.0, dim = c(nrow(mat0), K, P))
  for (j in seq_along(cols)) {
    k <- idx1[j]
    p <- idx2[j]
    if (!is.na(k) && !is.na(p) && k >= 1L && k <= K && p >= 1L && p <= P) {
      out[, k, p] <- mat0[, cols[j]]
    }
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
#'
#' @details
#' Bundle objects may store generated NIMBLE code either directly as a
#' `nimbleCode` object or inside a small wrapper list used for package storage.
#' This helper normalizes those storage conventions so downstream code can work
#' with the underlying code object without repeatedly checking both cases.
#' @keywords internal
.extract_nimble_code <- function(code) {
  if (is.list(code) && !inherits(code, "nimbleCode")) {
    if (!is.null(code$nimble)) return(code$nimble)
    if (!is.null(code$code)) return(code$code)
  }
  code
}

#' Wrap nimbleCode for bundle storage
#'
#' @details
#' This helper is the inverse of `.extract_nimble_code()`. It stores a raw
#' `nimbleCode` object inside a lightweight list so bundle objects can carry code
#' alongside other metadata without ambiguity about the field layout.
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
  # Default to static output; opt in via options(CausalMixGPD.plotly = TRUE).
  if (isTRUE(getOption("CausalMixGPD.plotly", FALSE)) &&
      requireNamespace("plotly", quietly = TRUE)) {
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

#' Prediction helpers
#'
#' @keywords internal
#' @noRd
.resolve_predict_id <- function(x, id = NULL) {
  id_vec <- NULL
  x_out <- x

  if (is.null(x_out)) {
    if (!is.null(id)) {
      stop("'id' requires 'x'/'newdata' to supply prediction rows.", call. = FALSE)
    }
    return(list(x = x_out, id = id_vec))
  }

  if (is.data.frame(x_out)) {
    if (is.character(id) && length(id) == 1L) {
      if (!id %in% names(x_out)) {
        stop(sprintf("Column '%s' not found in 'x'.", id), call. = FALSE)
      }
      id_vec <- x_out[[id]]
      x_out <- x_out[, names(x_out) != id, drop = FALSE]
    } else {
      if ("id" %in% names(x_out)) {
        x_out <- x_out[, names(x_out) != "id", drop = FALSE]
      }
    }
  } else if (is.character(id) && length(id) == 1L) {
    stop("'id' must reference a column in data.frame 'x'.", call. = FALSE)
  }

  if (!is.null(id) && !is.character(id)) {
    n_x <- nrow(as.matrix(x_out))
    if (length(id) != n_x) {
      stop("Length of 'id' must match nrow(x).", call. = FALSE)
    }
    id_vec <- id
  }

  list(x = x_out, id = id_vec)
}

.reorder_predict_cols <- function(df) {
  if (!is.data.frame(df)) return(df)
  cols <- names(df)
  profile_col <- if ("profile" %in% cols) "profile" else NULL
  id_col <- if ("id" %in% cols) "id" else NULL
  idx_col <- if ("index" %in% cols) "index" else if ("y" %in% cols) "y" else NULL
  est_col <- if ("estimate" %in% cols) {
    "estimate"
  } else if ("density" %in% cols) {
    "density"
  } else if ("survival" %in% cols) {
    "survival"
  } else {
    NULL
  }
  base <- c(profile_col, id_col, idx_col, est_col, intersect(c("lower", "upper"), cols))
  base <- base[!is.na(base) & nzchar(base)]
  rest <- setdiff(cols, base)
  df[, c(base, rest), drop = FALSE]
}

.values_to_long_df <- function(x, id = NULL, value_name = "value") {
  if (is.data.frame(x)) {
    return(.reorder_predict_cols(x))
  }

  if (is.null(dim(x))) {
    out <- data.frame(
      draw = seq_along(x),
      row.names = NULL
    )
    out[[value_name]] <- as.numeric(x)
    return(out)
  }

  mat <- as.matrix(x)
  n_row <- nrow(mat)
  n_col <- ncol(mat)
  id_use <- if (!is.null(id) && length(id) == n_row) id else seq_len(n_row)

  out <- data.frame(
    id = rep(id_use, each = n_col),
    draw = rep(seq_len(n_col), times = n_row),
    row.names = NULL
  )
  out[[value_name]] <- as.vector(t(mat))
  .reorder_predict_cols(out)
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

  # Case 1: Already a data.frame

  if (is.data.frame(fit)) {
    df <- fit
    # Ensure required columns exist
    if (!"estimate" %in% names(df)) {
      if ("fit" %in% names(df)) {
        df$estimate <- df$fit
      } else if ("density" %in% names(df)) {
        df$estimate <- df$density
      } else if ("survival" %in% names(df)) {
        df$estimate <- df$survival
      } else {
        num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        num_cols <- setdiff(num_cols, c("id", "index", "y", "ps", "lower", "upper"))
        if (length(num_cols)) {
          df$estimate <- df[[num_cols[1]]]
        } else {
          df$estimate <- NA_real_
        }
      }
    }
    if (!"lower" %in% names(df)) df$lower <- NA_real_
    if (!"upper" %in% names(df)) df$upper <- NA_real_
    if (!"id" %in% names(df)) {
      df$id <- seq_len(nrow(df))
    }
    return(.reorder_predict_cols(df))
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
      return(.reorder_predict_cols(df))
    }

    # If probs provided, assume matrix is n_pred x length(probs) of estimates
    if (!is.null(probs) && nc == length(probs)) {
      # Expand to long format: id x index
      df <- data.frame(
        id = rep(seq_len(nr), each = nc),
        index = rep(probs, times = nr),
        estimate = as.vector(t(fit)),
        lower = NA_real_,
        upper = NA_real_
      )
      return(.reorder_predict_cols(df))
    }

    # Default: treat first column as estimate
    df <- data.frame(
      id = seq_len(nr),
      estimate = fit[, 1],
      lower = if (nc >= 2) fit[, 2] else NA_real_,
      upper = if (nc >= 3) fit[, 3] else NA_real_
    )
    return(.reorder_predict_cols(df))
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
    return(.reorder_predict_cols(df))
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
#' @details
#' Many downstream summaries truncate mixture components according to the bundle
#' or fit-level `epsilon` setting. This helper centralizes that lookup so an
#' explicit function argument overrides the stored fit metadata, and the package
#' fallback is used only when neither source is present.
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
#'
#' @details
#' Posterior draw matrices often contain more components than are effectively
#' needed for reporting. This helper applies the package truncation rule
#' draw-by-draw, keeping the retained component blocks, associated weights, and
#' any linked coefficient matrices aligned after reordering and truncation.
#'
#' The bookkeeping attached to the returned matrix records both the cumulative
#' mass rule and the per-component weight rule so later summaries can report how
#' many components were effectively retained.
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
  plan <- spec$plan %||% list()
  bulk_plan <- plan$bulk %||% list()
  gpd_plan <- plan$gpd %||% list()
  has_ps <- !is.null(plan$ps)
  backend_raw <- meta$backend %||% spec$dispatch$backend %||% "<unknown>"
  is_spliced <- identical(backend_raw, "spliced")
  backend <- if (is_spliced) "crp" else backend_raw
  kernel  <- meta$kernel  %||% spec$kernel$key %||% "<unknown>"

  kdef <- get_kernel_registry()[[kernel]]
  if (is.null(kdef)) stop("Kernel not found in registry: ", kernel, call. = FALSE)
  bulk_params <- kdef$bulk_params %||% character(0)

  cn <- colnames(mat)
  has_z <- any(grepl("^z\\[[0-9]+\\]$", cn))
  has_w <- any(grepl("^w\\[[0-9]+\\]$", cn))
  has_weights <- any(grepl("^weights\\[[0-9]+\\]$", cn))

  component_matrix_bases <- unique(c(
    unlist(lapply(names(bulk_plan), function(nm) {
      ent <- bulk_plan[[nm]] %||% list()
      if (identical(ent$mode %||% "", "link")) paste0("beta_", nm) else character(0)
    }), use.names = FALSE),
    if (is_spliced) {
      unlist(lapply(c("threshold", "tail_scale", "tail_shape"), function(nm) {
        ent <- gpd_plan[[nm]] %||% list()
        if (identical(ent$mode %||% "", "link")) paste0("beta_", nm) else character(0)
      }), use.names = FALSE)
    } else {
      character(0)
    }
  ))
  component_aux_vector_bases <- unique(c(
    if (isTRUE(has_ps)) {
      unlist(lapply(names(bulk_plan), function(nm) {
        ent <- bulk_plan[[nm]] %||% list()
        if (identical(ent$mode %||% "", "link")) paste0("beta_ps_", nm) else character(0)
      }), use.names = FALSE)
    } else {
      character(0)
    }
  ))

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

  infer_K_from_z <- function() {
    if (!has_z) return(NA_integer_)
    Z <- .indexed_block(mat, "z")
    zmax <- suppressWarnings(max(Z, na.rm = TRUE))
    if (!is.finite(zmax) || zmax < 1L) return(NA_integer_)
    as.integer(zmax)
  }

  infer_K_from_component_vectors <- function() {
    bases <- unique(c(
      bulk_params,
      if (is_spliced) c("threshold", "tail_scale", "tail_shape") else character(0)
    ))
    idx_all <- integer(0)
    for (nm in bases) {
      idx <- as.integer(sub(paste0("^", nm, "\\[([0-9]+)\\]$"), "\\1",
                            cn[grepl(paste0("^", nm, "\\[[0-9]+\\]$"), cn)]))
      if (length(idx)) idx_all <- c(idx_all, idx)
    }
    if (!length(idx_all)) return(NA_integer_)
    as.integer(max(idx_all, na.rm = TRUE))
  }

  infer_K_from_component_matrices <- function() {
    if (!length(component_matrix_bases)) return(NA_integer_)
    idx_all <- integer(0)
    for (nm in component_matrix_bases) {
      idx <- as.integer(sub(paste0("^", nm, "\\[([0-9]+),\\s*[0-9]+\\]$"), "\\1",
                            cn[grepl(paste0("^", nm, "\\[[0-9]+,\\s*[0-9]+\\]$"), cn)]))
      if (length(idx)) idx_all <- c(idx_all, idx)
    }
    if (!length(idx_all)) return(NA_integer_)
    as.integer(max(idx_all, na.rm = TRUE))
  }

  if (identical(backend, "sb")) {
    K <- infer_K_from_w()
    if (!is.finite(K) || K < 1L) K <- infer_K_from_bulk()
    if (!is.finite(K) || K < 1L) stop("Could not infer K for SB weights.", call. = FALSE)
  } else if (identical(backend, "crp")) {
    K <- infer_K_from_bulk()
    if (!is.finite(K) || K < 1L) K <- infer_K_from_component_vectors()
    if (!is.finite(K) || K < 1L) K <- infer_K_from_component_matrices()
    if (!is.finite(K) || K < 1L) K <- infer_K_from_z()
    if (!is.finite(K) || K < 1L) K <- infer_K_from_w()
    if (!is.finite(K) || K < 1L) stop("Could not infer Kmax from component parameter draws.", call. = FALSE)
  } else {
    stop("Unknown backend: ", backend, call. = FALSE)
  }
  K <- as.integer(K)

  S <- nrow(mat)
  W <- matrix(0.0, nrow = S, ncol = K)
  if (identical(backend, "sb")) {
    if (has_w) {
      W <- .indexed_block(mat, "w", K = K)
    } else if (has_weights) {
      W <- .indexed_block(mat, "weights", K = K)
    } else if (has_z) {
      Z <- .indexed_block(mat, "z")
      storage.mode(Z) <- "integer"
      for (s in 1:S) {
        z_s <- Z[s, ]
        z_s <- z_s[is.finite(z_s)]
        z_s <- z_s[z_s >= 1 & z_s <= K]
        if (length(z_s)) W[s, ] <- tabulate(z_s, nbins = K) / length(z_s)
      }
    } else {
      stop("Could not derive SB weights: expected w[i]/weights[i] or z[i] in samples.", call. = FALSE)
    }
  } else {
    if (has_z) {
      Z <- .indexed_block(mat, "z")
      storage.mode(Z) <- "integer"
      for (s in 1:S) {
        z_s <- Z[s, ]
        z_s <- z_s[is.finite(z_s)]
        z_s <- z_s[z_s >= 1 & z_s <= K]
        if (length(z_s)) W[s, ] <- tabulate(z_s, nbins = K) / length(z_s)
      }
    } else if (has_w) {
      W <- .indexed_block(mat, "w", K = K)
    } else if (has_weights) {
      W <- .indexed_block(mat, "weights", K = K)
    } else {
      stop("Backend requires z[i] in samples to derive weights.", call. = FALSE)
    }
  }

  component_vector_draws <- list()
  for (nm in bulk_params) {
    blk <- .indexed_block(mat, nm, K = K, allow_missing = TRUE)
    if (!is.null(blk)) component_vector_draws[[nm]] <- blk
  }
  if (is_spliced) {
    for (nm in c("threshold", "tail_scale", "tail_shape")) {
      blk <- .indexed_block(mat, nm, K = K, allow_missing = TRUE)
      if (!is.null(blk)) component_vector_draws[[nm]] <- blk
    }
  }
  for (nm in component_aux_vector_bases) {
    blk <- .indexed_block(mat, nm, K = K, allow_missing = TRUE)
    if (!is.null(blk)) component_vector_draws[[nm]] <- blk
  }
  component_matrix_draws <- list()
  for (base in component_matrix_bases) {
    blk <- .indexed_block_matrix(mat, base, K = K, allow_missing = TRUE)
    if (!is.null(blk)) component_matrix_draws[[base]] <- blk
  }
  component_vector_bases <- names(component_vector_draws)

  # ----- per-draw truncation (sorted by weight; keep params aligned) -----
  S <- nrow(mat)
  ks <- integer(S)
  k_weight_vec <- integer(S)
  k_cum_vec <- integer(S)
  ords <- vector("list", S)
  w_list <- vector("list", S)
  p_list <- vector("list", S)

  for (s in 1:S) {
    params_s <- lapply(component_vector_bases, function(nm) as.numeric(component_vector_draws[[nm]][s, ]))
    names(params_s) <- component_vector_bases
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
    paste0("^", unique(component_vector_bases), "\\[[0-9]+\\]$"),
    paste0("^", unique(names(component_matrix_draws)), "\\[[0-9]+,\\s*[0-9]+\\]$")
  )
  drop_hit <- rep(FALSE, length(cn))
  for (pp in drop_pat) drop_hit <- drop_hit | grepl(pp, cn)
  keep_cn <- cn[!drop_hit]

  out <- mat[, keep_cn, drop = FALSE]

  # add truncated weights + params (ranked by decreasing weight)
  w_out <- matrix(0.0, nrow = S, ncol = Kt)
  colnames(w_out) <- paste0("w[", seq_len(Kt), "]")

  out_params <- list()
  for (nm in component_vector_bases) {
    tmp <- matrix(NA_real_, nrow = S, ncol = Kt)
    colnames(tmp) <- paste0(nm, "[", seq_len(Kt), "]")
    out_params[[nm]] <- tmp
  }
  out_mats <- list()
  for (base in names(component_matrix_draws)) {
    Pj <- dim(component_matrix_draws[[base]])[3L]
    tmp <- matrix(NA_real_, nrow = S, ncol = Kt * Pj)
    colnames(tmp) <- unlist(lapply(seq_len(Kt), function(k) {
      paste0(base, "[", k, ",", seq_len(Pj), "]")
    }), use.names = FALSE)
    out_mats[[base]] <- tmp
  }

  for (s in 1:S) {
    w_s <- w_list[[s]]
    k_s <- min(length(w_s), Kt)
    if (k_s < 1L) next
    w_out[s, seq_len(k_s)] <- w_s[seq_len(k_s)]
    for (nm in component_vector_bases) {
      out_params[[nm]][s, seq_len(k_s)] <- p_list[[s]][[nm]][seq_len(k_s)]
    }
    if (length(out_mats)) {
      ord_s <- ords[[s]]
      for (base in names(out_mats)) {
        mat_s <- component_matrix_draws[[base]][s, , , drop = TRUE]
        if (is.null(dim(mat_s))) {
          mat_s <- matrix(mat_s, nrow = K, ncol = 1L)
        }
        mat_ord <- mat_s[ord_s, , drop = FALSE]
        Pj <- ncol(mat_ord)
        out_mats[[base]][s, seq_len(k_s * Pj)] <- as.vector(t(mat_ord[seq_len(k_s), , drop = FALSE]))
      }
    }
  }

  out <- cbind(out, w_out)
  for (nm in component_vector_bases) out <- cbind(out, out_params[[nm]])
  for (base in names(out_mats)) out <- cbind(out, out_mats[[base]])

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
#'
#' @details
#' This is a lightweight structural check used by multiple internal helpers. It
#' verifies that the object inherits from `mixgpd_fit` and that posterior draws
#' are available in one of the expected storage locations before later code tries
#' to summarize or predict from the fit.
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
#'
#' @details
#' Downstream summary and plotting code relies on the `coda` interface. This
#' helper validates the fit, locates the stored posterior draws, and converts a
#' single-chain `mcmc` object into an `mcmc.list` so later code can treat the
#' one-chain and multi-chain cases uniformly.
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
#'
#' @details
#' This helper stacks the posterior chains into one numeric matrix, optionally
#' removes stick-breaking `v` variables, and then applies the component
#' truncation rule controlled by `epsilon`. The result is the standardized draw
#' representation used by most internal summary and prediction helpers.
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
#'
#' @details
#' The fitted object may carry the response in slightly different storage slots
#' depending on how it was built. This helper centralizes the lookup and returns
#' the effective training sample size used by summaries and print methods.
#' @param object A mixgpd_fit.
#' @return Integer n.
#' @keywords internal
.get_nobs <- function(object) {
  if (!is.null(object$data) && !is.null(object$data$y)) return(length(object$data$y))
  if (!is.null(object$y)) return(length(object$y))
  NA_integer_
}

#' Safely coerce MCMC samples to a numeric matrix
#'
#' @details
#' This is the matrix-oriented companion to `.get_samples_mcmclist()`. It can
#' either stack all chains or keep only the first chain, always removes
#' stick-breaking `v` variables, applies the standard component truncation rule,
#' and optionally filters to an exact set of parameter names.
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
#'
#' @details
#' After `.extract_draws()` applies draw-level component truncation, the chosen
#' number of retained components is stored as an attribute. This helper condenses
#' that bookkeeping into min, median, and max summaries for the effective
#' retained component count under both the cumulative-mass and per-weight
#' criteria.
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
#'
#' @details
#' This helper builds the short header used by `print.mixgpd_fit()`. It extracts
#' model identity, training size, truncation size, epsilon, and stored MCMC
#' settings into a compact character vector so higher-level print methods do not
#' duplicate formatting logic.
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
#' @details
#' This helper powers the one-arm summary methods. It extracts the retained draw
#' matrix, chooses a default set of non-redundant parameters when `pars` is not
#' supplied, and computes posterior means, standard deviations, quantiles,
#' effective sample sizes, and Gelman diagnostics when available.
#'
#' The resulting table is parameter oriented rather than prediction oriented. It
#' is the internal workhorse behind `summary.mixgpd_fit()`.
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
    meta <- spec$meta %||% list()
    is_spliced <- identical(meta$backend %||% spec$dispatch$backend %||% "", "spliced")
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
        if (is_spliced) {
          keep <- keep | grepl("^tail_scale\\[[0-9]+\\]$", cn)
        } else {
          keep <- keep | cn == "tail_scale"
        }
      }
    }

    if (!is.null(gpd$tail_shape)) {
      tsh_mode <- gpd$tail_shape$mode %||% NA_character_
      if (identical(tsh_mode, "link")) {
        keep <- keep | grepl("^beta_tail_shape\\[", cn)
      } else if (is_spliced) {
        keep <- keep | grepl("^tail_shape\\[[0-9]+\\]$", cn)
      } else {
        keep <- keep | cn == "tail_shape"
      }
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

  backend <- object$spec$meta$backend %||% object$spec$dispatch$backend %||% ""
  thr_cols <- grep("^threshold\\[[0-9]+\\]$", colnames(mat), value = TRUE)
  if (!identical(backend, "spliced") && length(thr_cols) >= 1) {
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
#'
#' @details
#' This helper resolves the density, distribution, quantile, random-generation,
#' and mean functions implied by a kernel, backend, and GPD setting. The result
#' is intentionally scalar and wrapper-free because it is used in code-generation
#' contexts where NIMBLE expects raw function objects rather than vectorized R
#' adapters.
#' @param spec_or_fit mixgpd_fit or spec list
#' @return List with d/p/q/r/mean/mean_trunc functions and bulk_params.
#' @keywords internal
.get_dispatch_scalar <- function(spec_or_fit, backend_override = NULL, gpd_override = NULL) {
  spec <- spec_or_fit
  if (inherits(spec_or_fit, "mixgpd_fit")) {
    spec <- spec_or_fit$spec %||% list()
  }

  meta <- spec$meta %||% list()
  backend <- meta$backend %||% spec$dispatch$backend %||% "<unknown>"
  if (!is.null(backend_override)) backend <- backend_override
  kernel <- meta$kernel %||% spec$kernel$key %||% "<unknown>"
  GPD <- isTRUE(meta$GPD %||% spec$dispatch$GPD)
  if (!is.null(gpd_override)) GPD <- isTRUE(gpd_override)

  kdef <- get_kernel_registry()[[kernel]]
  if (is.null(kdef)) stop(sprintf("Kernel '%s' not found in registry.", kernel), call. = FALSE)
  if (isTRUE(GPD) && isFALSE(kdef$allow_gpd)) stop(sprintf("Kernel '%s' does not allow GPD.", kernel), call. = FALSE)

  backend_key <- match.arg(backend, choices = allowed_backends)
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
  mean_name <- if (!isTRUE(GPD)) dispatch$mean %||% dispatch$mean_base %||% NULL else NULL
  mean_trunc_name <- if (!isTRUE(GPD)) dispatch$mean_trunc %||% dispatch$mean_trunc_base %||% NULL else NULL

  ns_pkg <- getNamespace("CausalMixGPD")
  ns_stats <- getNamespace("stats")
  ns_nimble <- getNamespace("nimble")

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
  mean_fun <- if (!is.null(mean_name) && nzchar(mean_name)) .resolve_fun(mean_name, kernel) else NULL
  mean_trunc_fun <- if (!is.null(mean_trunc_name) && nzchar(mean_trunc_name)) .resolve_fun(mean_trunc_name, kernel) else NULL

  if (isTRUE(attr(d_fun, "vectorized_wrapper")) ||
      isTRUE(attr(p_fun, "vectorized_wrapper")) ||
      isTRUE(attr(q_fun, "vectorized_wrapper")) ||
      isTRUE(attr(r_fun, "vectorized_wrapper"))) {
    stop("Scalar dispatch unexpectedly received vectorized wrappers.", call. = FALSE)
  }

  list(d = d_fun, p = p_fun, q = q_fun, r = r_fun, mean = mean_fun, mean_trunc = mean_trunc_fun, bulk_params = kdef$bulk_params)
}

#' Resolve kernel dispatch functions
#' Dispatch returns vector-aware d/p/q and n-aware r via wrappers; do not mutate namespace.
#'
#' @details
#' This is the prediction-oriented companion to `.get_dispatch_scalar()`. It
#' starts from the same kernel dispatch lookup, then wraps the scalar functions
#' so they can accept vector inputs and the package's preferred argument naming
#' conventions in ordinary R evaluation.
#' @param spec_or_fit mixgpd_fit or spec list
#' @return List with d/p/q/r/mean/mean_trunc functions and bulk_params.
#' @keywords internal
.get_dispatch <- function(spec_or_fit, backend_override = NULL, gpd_override = NULL) {
  scalar <- .get_dispatch_scalar(spec_or_fit, backend_override = backend_override, gpd_override = gpd_override)
  list(
    d = .wrap_scalar_first_arg(scalar$d, "x"),
    p = .wrap_scalar_p(scalar$p),
    q = .wrap_scalar_first_arg(scalar$q, "p"),
    r = .wrap_scalar_r(scalar$r),
    mean = scalar$mean,
    mean_trunc = scalar$mean_trunc,
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
#' @details
#' The last dimension of `draws` is interpreted as the posterior-draw dimension.
#' This helper collapses that dimension to posterior means and interval summaries,
#' while preserving the leading dimensions of the input object. It is used
#' throughout prediction and treatment-effect code to turn per-draw evaluations
#' into reported posterior summaries.
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
#'
#' @details
#' Some wrapped scalar functions accept either `q` or `x` as their first numeric
#' argument depending on the original API. This helper inspects `...` and returns
#' the first candidate name that is actually present so wrapper code can map user
#' input onto the target function signature.
#' @keywords internal
.detect_first_present <- function(dots, candidates = c("q", "x")) {
  for (nm in candidates) {
    if (!is.null(dots[[nm]])) return(nm)
  }
  stop("Expected one of: ", paste(candidates, collapse = ", "), call. = FALSE)
}

#' Wrap scalar first-argument functions to handle vector inputs.
#'
#' @details
#' Many low-level distribution helpers are scalar in their first argument. This
#' wrapper lifts such functions to vector inputs by evaluating the scalar
#' function repeatedly and combining the results into either a numeric vector or
#' a matrix, depending on the length of the original return value.
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
#'
#' @details
#' Different scalar CDF helpers use either `q` or `x` for their evaluation
#' argument. This wrapper normalizes those naming differences and then applies the
#' same vector-lifting strategy used elsewhere so prediction code can call the
#' resulting function consistently.
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
#'
#' @details
#' Random-generation helpers in the package are scalar-at-a-time. This wrapper
#' promotes them to the standard `n` interface by repeating the scalar generator
#' and returning either a numeric vector or a matrix of generated values,
#' depending on the length of one draw.
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
#' @details
#' This helper operates on one posterior draw at a time. It first orders mixture
#' components by decreasing weight, then keeps the smallest effective subset of
#' components implied by the package truncation rule, and finally renormalizes
#' the retained weights so they sum to one.
#'
#' The same permutation is applied to every component-specific parameter vector in
#' `params`, which keeps the retained parameter blocks aligned with the retained
#' weights.
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
#' @details
#' This is the main internal workhorse behind `predict.mixgpd_fit()` and the
#' causal effect helpers. It evaluates the requested predictive functional
#' separately for each retained posterior draw, using either explicit SB weights
#' or CRP weights reconstructed from latent cluster labels, and only then
#' collapses the draw-level results into posterior summaries.
#'
#' The helper also manages caching of per-draw predictive quantities because
#' treatment-effect functions repeatedly reuse the same arm-specific predictive
#' draws. That cache avoids recomputation while keeping the public prediction
#' interface simple.
#'
#' @keywords internal
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
                            show_progress = TRUE,
                            ncores = 1L,
                            sample_draw_idx = NULL) {

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
  mean_fun <- fns$mean %||% NULL
  bulk_scalar <- .get_dispatch_scalar(object, backend_override = pred_backend, gpd_override = FALSE)
  bulk_p_fun <- bulk_scalar$p
  bulk_mean_fun <- bulk_scalar$mean %||% NULL
  bulk_mean_trunc_fun <- bulk_scalar$mean_trunc %||% NULL

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

  chunk_starts <- NULL
  if (!is.null(chunk_size) && !is.null(Xpred) && n_pred > chunk_size) {
    chunk_starts <- seq.int(1L, n_pred, by = chunk_size)
  }
  progress_total <- 4L + if (!is.null(chunk_starts)) length(chunk_starts) else 0L
  progress_ctx <- .cmgpd_progress_start(
    total_steps = progress_total,
    enabled = isTRUE(show_progress),
    quiet = FALSE,
    label = "predict_mixgpd"
  )
  on.exit(.cmgpd_progress_done(progress_ctx, final_label = NULL), add = TRUE)
  .cmgpd_progress_step(progress_ctx, "Validating prediction inputs")

  .return_out <- function(obj) {
    .cmgpd_progress_step(progress_ctx, "Assembling prediction output")
    obj
  }

  # -----------------------------
  # Posterior draws extraction
  # -----------------------------
  .cmgpd_progress_step(progress_ctx, "Extracting posterior draws")
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

  if (!is.null(chunk_starts)) {
    .cmgpd_progress_step(progress_ctx, "Dispatching chunked prediction")
    parts <- vector("list", length(chunk_starts))
    for (ii in seq_along(chunk_starts)) {
      .cmgpd_progress_step(progress_ctx, sprintf("Computing chunk %d/%d", ii, length(chunk_starts)))
      i0 <- chunk_starts[ii]
      i1 <- min(n_pred, i0 + chunk_size - 1L)
      idx <- i0:i1
      x_chunk <- Xpred[idx, , drop = FALSE]
      y_chunk <- NULL
      if (type %in% c("density", "survival") && has_X) y_chunk <- ygrid[idx]
      ps_chunk <- if (!is.null(ps)) ps[idx] else NULL
      id_chunk <- if (!is.null(id_vals)) id_vals[idx] else NULL
      parts[[ii]] <- .predict_mixgpd(
        object = object,
        x = x_chunk,
        y = y_chunk,
        ps = ps_chunk,
        id = id_chunk,
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
        ndraws_pred = ndraws_pred,
        chunk_size = NULL,
        show_progress = FALSE,
        ncores = ncores,
        sample_draw_idx = sample_draw_idx
      )
    }
    out <- parts[[1L]]
    if (is.data.frame(out$fit)) {
      out$fit <- do.call(rbind, lapply(parts, function(z) z$fit))
      rownames(out$fit) <- NULL
    }
    if (!is.null(out$diagnostics)) {
      out$diagnostics$n_chunks <- length(parts)
    }
    return(.return_out(out))
  }

  .cmgpd_progress_step(progress_ctx, "Computing posterior summaries")

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
  spliced_gpd_draws <- list()
  spliced_gpd_link <- list()

  if (GPD) {
    gpd_plan <- spec$dispatch$gpd %||% meta$gpd %||% list()

    if (is_spliced) {
      K_sp <- ncol(W_draws)
      for (nm in c("threshold", "tail_scale", "tail_shape")) {
        ent <- gpd_plan[[nm]] %||% list(mode = "dist")
        mode <- ent$mode %||% "dist"
        if (identical(mode, "link")) {
          if (!has_X) stop(sprintf("%s link-mode requires X.", nm), call. = FALSE)
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
      # threshold
      thr_mode <- gpd_plan$threshold$mode %||% "constant"
      if (identical(thr_mode, "link")) {
        if (!has_X) stop("threshold link-mode requires X.", call. = FALSE)
        beta_thr <- .indexed_block(draw_mat, "beta_threshold", K = P)
        threshold_mat <- matrix(NA_real_, nrow = S, ncol = n_pred)
        thr_link <- gpd_plan$threshold$link %||% "exp"
        thr_power <- gpd_plan$threshold$link_power %||% NULL
        for (s in seq_len(S)) {
          eta <- as.numeric(Xpred %*% beta_thr[s, ])
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
        if (!has_X) stop("tail_scale link-mode requires X.", call. = FALSE)
        beta_ts <- .indexed_block(draw_mat, "beta_tail_scale", K = P)
        tail_scale <- matrix(NA_real_, nrow = S, ncol = n_pred)
        ts_link <- gpd_plan$tail_scale$link %||% "exp"
        ts_power <- gpd_plan$tail_scale$link_power %||% NULL
        for (s in seq_len(S)) {
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

      has_beta_tsh <- any(grepl("^beta_tail_shape\\[", colnames(draw_mat)))
      tsh_mode <- gpd_plan$tail_shape$mode %||% if (has_beta_tsh) "link" else "constant"
      if (identical(tsh_mode, "link")) {
        if (!has_X) stop("tail_shape link-mode requires X.", call. = FALSE)
        beta_tsh <- .indexed_block(draw_mat, "beta_tail_shape", K = P)
        tail_shape <- matrix(NA_real_, nrow = S, ncol = n_pred)
        tsh_link <- gpd_plan$tail_shape$link %||% "identity"
        tsh_power <- gpd_plan$tail_shape$link_power %||% NULL
        for (s in seq_len(S)) {
          eta <- as.numeric(Xpred %*% beta_tsh[s, ])
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
      eta_mat <- Xpred %*% t(beta_mat)
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

  .spliced_density_survival_row <- function(s, i, yvals, type, link_eta, gpd_eta) {
    w_s <- .normalize_weights_or_null(W_draws[s, ])
    if (is.null(w_s)) return(rep(NA_real_, length(yvals)))
    comp_args <- .spliced_component_args_list_or_null(s, i, link_eta = link_eta, gpd_eta = gpd_eta)
    if (is.null(comp_args)) return(rep(NA_real_, length(yvals)))

    out <- numeric(length(yvals))
    for (j in seq_along(yvals)) {
      yj <- yvals[j]
      vals <- vapply(seq_along(comp_args), function(k) {
        fun_args <- comp_args[[k]]
        if (type == "density") {
          as.numeric(do.call(spliced_scalar$d, c(list(x = yj, log = 0L), fun_args)))[1]
        } else {
          cdfv <- as.numeric(do.call(spliced_scalar$p, c(list(q = yj, lower.tail = 1L, log.p = 0L), fun_args)))[1]
          cdfv <- pmin(pmax(cdfv, 0), 1)
          1 - cdfv
        }
      }, numeric(1))
      out[j] <- sum(w_s * vals)
    }
    out
  }

  .spliced_quantile_one <- function(s, i, p0, link_eta, gpd_eta) {
    w_s <- .normalize_weights_or_null(W_draws[s, ])
    if (is.null(w_s)) return(NA_real_)
    comp_args <- .spliced_component_args_list_or_null(s, i, link_eta = link_eta, gpd_eta = gpd_eta)
    if (is.null(comp_args)) return(NA_real_)

    q_lo <- vapply(comp_args, function(args) {
      suppressWarnings(tryCatch(as.numeric(do.call(spliced_scalar$q, c(list(p = 1e-8), args)))[1], error = function(e) NA_real_))
    }, numeric(1))
    q_hi <- vapply(comp_args, function(args) {
      suppressWarnings(tryCatch(as.numeric(do.call(spliced_scalar$q, c(list(p = 1 - 1e-8), args)))[1], error = function(e) NA_real_))
    }, numeric(1))
    q_mid <- vapply(comp_args, function(args) {
      suppressWarnings(tryCatch(as.numeric(do.call(spliced_scalar$q, c(list(p = p0), args)))[1], error = function(e) NA_real_))
    }, numeric(1))

    finite_seed <- c(q_lo[is.finite(q_lo)], q_hi[is.finite(q_hi)], q_mid[is.finite(q_mid)])
    if (!length(finite_seed)) return(NA_real_)

    cdf_mix <- function(y) {
      vals <- vapply(comp_args, function(args) {
        as.numeric(do.call(spliced_scalar$p, c(list(q = y, lower.tail = 1L, log.p = 0L), args)))[1]
      }, numeric(1))
      sum(w_s * pmin(pmax(vals, 0), 1))
    }

    lower <- min(finite_seed, na.rm = TRUE)
    upper <- max(finite_seed, na.rm = TRUE)
    step0 <- max(1, diff(range(finite_seed)), abs(lower), abs(upper), na.rm = TRUE)
    if (!is.finite(step0) || step0 <= 0) step0 <- 1

    f_lower <- cdf_mix(lower) - p0
    f_upper <- cdf_mix(upper) - p0

    step <- step0
    iter <- 0L
    while (is.finite(f_lower) && f_lower > 0 && iter < 60L) {
      lower <- lower - step
      f_lower <- cdf_mix(lower) - p0
      step <- step * 2
      iter <- iter + 1L
    }

    step <- step0
    iter <- 0L
    while (is.finite(f_upper) && f_upper < 0 && iter < 60L) {
      upper <- upper + step
      f_upper <- cdf_mix(upper) - p0
      step <- step * 2
      iter <- iter + 1L
    }

    if (!is.finite(f_lower) || !is.finite(f_upper) || f_lower > 0 || f_upper < 0) {
      return(NA_real_)
    }

    suppressWarnings(tryCatch(
      stats::uniroot(function(y) cdf_mix(y) - p0, interval = c(lower, upper), tol = .Machine$double.eps^0.5)$root,
      error = function(e) NA_real_
    ))
  }

  .spliced_sample_values <- function(s, i, n, link_eta, gpd_eta) {
    w_s <- .normalize_weights_or_null(W_draws[s, ])
    if (is.null(w_s)) return(rep(NA_real_, n))
    comp_args <- .spliced_component_args_list_or_null(s, i, link_eta = link_eta, gpd_eta = gpd_eta)
    if (is.null(comp_args)) return(rep(NA_real_, n))

    kk <- sample.int(length(w_s), size = n, replace = TRUE, prob = w_s)
    out <- rep(NA_real_, n)
    for (ii in seq_len(n)) {
      out[ii] <- as.numeric(do.call(spliced_scalar$r, c(list(n = 1L), comp_args[[kk[ii]]])))[1]
    }
    out
  }

  .spliced_mean_infinite <- function(s, i, gpd_eta) {
    w_s <- as.numeric(W_draws[s, ])
    if (!all(is.finite(w_s))) return(FALSE)
    xi <- vapply(seq_len(ncol(W_draws)), function(k) {
      .spliced_gpd_value_at("tail_shape", s, i, k, gpd_eta)
    }, numeric(1))
    any((w_s > 0) & is.finite(xi) & (xi >= 1))
  }

  .clamp_prob <- function(x) {
    pmin(pmax(as.numeric(x), 0), 1)
  }

  .gpd_tail_mean_or_error <- function(threshold, tail_scale, tail_shape) {
    xi <- as.numeric(tail_shape)[1]
    u <- as.numeric(threshold)[1]
    sigma_u <- as.numeric(tail_scale)[1]
    if (!is.finite(u) || !is.finite(sigma_u) || sigma_u <= 0 || !is.finite(xi)) return(NA_real_)
    if (xi >= 1) {
      stop("Mean is not supported when the GPD tail has tail_shape (xi) >= 1; use type='rmean'.", call. = FALSE)
    }
    u + sigma_u / (1 - xi)
  }

  .analytic_spliced_mean_row <- function(s, i, link_eta, gpd_eta) {
    w_s <- .normalize_weights_or_null(W_draws[s, ])
    if (is.null(w_s)) return(NA_real_)
    comp_args <- .spliced_component_args_list_or_null(s, i, link_eta = link_eta, gpd_eta = gpd_eta)
    if (is.null(comp_args) || is.null(bulk_mean_trunc_fun) || !is.function(bulk_mean_trunc_fun)) {
      return(NA_real_)
    }

    comp_mean <- vapply(seq_along(comp_args), function(k) {
      if (!is.finite(w_s[k]) || w_s[k] <= 0) return(0)
      args_k <- comp_args[[k]]
      threshold_k <- as.numeric(args_k$threshold)[1]
      tail_scale_k <- as.numeric(args_k$tail_scale)[1]
      tail_shape_k <- as.numeric(args_k$tail_shape)[1]
      bulk_args_k <- c(list(w = 1), args_k[bulk_params])
      bulk_trunc_k <- as.numeric(do.call(bulk_mean_trunc_fun, c(bulk_args_k, list(threshold = threshold_k))))[1]
      Fu_k <- as.numeric(do.call(bulk_p_fun, c(list(q = threshold_k, lower.tail = 1L, log.p = 0L), bulk_args_k)))[1]
      bulk_trunc_k + (1 - .clamp_prob(Fu_k)) * .gpd_tail_mean_or_error(threshold_k, tail_scale_k, tail_shape_k)
    }, numeric(1))

    sum(w_s * comp_mean)
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
      gpd_eta <- if (is_spliced && GPD) .compute_spliced_gpd_eta(s) else list()

      out <- matrix(NA_real_, nrow = n_pred, ncol = G)
      for (i in seq_len(n_pred)) {
        if (is_spliced && GPD) {
          out[i, ] <- .spliced_density_survival_row(s, i, ygrid_num, type = type, link_eta = link_eta, gpd_eta = gpd_eta)
          next
        }

        args <- args0
        if (GPD) {
          args$threshold <- .threshold_at(s, i)
          args$tail_scale <- .tail_scale_at(s, i)
          args$tail_shape <- .tail_shape_at(s, i)
          if (!is.finite(args$threshold) || !is.finite(args$tail_scale) || args$tail_scale <= 0 || !is.finite(args$tail_shape)) {
            out[i, ] <- NA_real_
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
            out[i, ] <- NA_real_
            next
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
      fit_df = fit_df,
      type = type,
      grid = ygrid_num,
      diagnostics = list(
        n_draws_total = S,
        n_draws_valid = sum(valid_vec),
        n_draws_dropped = S - sum(valid_vec)
      )
    )
    class(out) <- "mixgpd_predict"
    return(.return_out(out))
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

        if (is_spliced && GPD) {
          draws_mat[, s] <- vapply(pgrid, function(pp) {
            .spliced_quantile_one(s, 1L, pp, link_eta = list(), gpd_eta = list())
          }, numeric(1))
          next
        }

        if (GPD) {
          args0$threshold <- threshold_scalar[s]
          args0$tail_scale <- tail_scale[s]
          args0$tail_shape <- .tail_shape_at(s, 1L)
          if (!is.finite(args0$threshold) || !is.finite(args0$tail_scale) || args0$tail_scale <= 0 || !is.finite(args0$tail_shape)) next
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
        fit_df = fit_df,
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
      return(.return_out(out))
    }

    # Conditional quantiles
    draws_arr <- array(NA_real_, dim = c(n_pred, M, S))

    for (s in seq_len(S)) {
      if (!.draw_valid[s]) next
      args0 <- .build_args0_or_null(s)
      if (is.null(args0)) next

      link_eta <- .compute_link_eta(s)
      gpd_eta <- if (is_spliced && GPD) .compute_spliced_gpd_eta(s) else list()

      for (i in seq_len(n_pred)) {
        if (is_spliced && GPD) {
          draws_arr[i, , s] <- vapply(pgrid, function(pp) {
            .spliced_quantile_one(s, i, pp, link_eta = link_eta, gpd_eta = gpd_eta)
          }, numeric(1))
          next
        }

        args <- args0
        if (GPD) {
          args$threshold <- .threshold_at(s, i)
          args$tail_scale <- .tail_scale_at(s, i)
          args$tail_shape <- .tail_shape_at(s, i)
          if (!is.finite(args$threshold) || !is.finite(args$tail_scale) || args$tail_scale <= 0 || !is.finite(args$tail_shape)) {
            draws_arr[i, , s] <- NA_real_
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
            draws_arr[i, , s] <- NA_real_
            next
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
      fit_df = fit_df,
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
    return(.return_out(out))
  }

  # -----------------------------
  # sample (posterior predictive)
  # -----------------------------
  if (type == "sample") {
    if (!is.null(sample_draw_idx)) {
      idx <- as.integer(sample_draw_idx)
      if (!length(idx) || any(!is.finite(idx)) || any(idx < 1L) || any(idx > S)) {
        stop("'sample_draw_idx' must be a non-empty integer vector indexing posterior draws.", call. = FALSE)
      }
      nsim <- length(idx)
    }

    if (!has_X) {
      if (is.na(nsim) || nsim < 1L) nsim <- length(ytrain)
      if (is.null(sample_draw_idx)) idx <- sample.int(S, size = nsim, replace = TRUE)
      outv <- numeric(nsim)

      for (t in seq_len(nsim)) {
        s <- idx[t]
        if (!.draw_valid[s]) { outv[t] <- NA_real_; next }
        args0 <- .build_args0_or_null(s)
        if (is.null(args0)) { outv[t] <- NA_real_; next }

        if (is_spliced && GPD) {
          outv[t] <- .spliced_sample_values(s, 1L, n = 1L, link_eta = list(), gpd_eta = list())[1]
          next
        }

        if (GPD) {
          args0$threshold <- threshold_scalar[s]
          args0$tail_scale <- tail_scale[s]
          args0$tail_shape <- .tail_shape_at(s, 1L)
          if (!is.finite(args0$threshold) || !is.finite(args0$tail_scale) || args0$tail_scale <= 0 || !is.finite(args0$tail_shape)) {
            outv[t] <- NA_real_
            next
          }
        }
        outv[t] <- as.numeric(do.call(r_fun, c(list(n = 1L), args0)))[1]
      }

      res <- list(
        fit = outv,
        fit_df = .values_to_long_df(outv, value_name = "sample"),
        type = type,
        grid = NULL,
        diagnostics = list(
          n_draws_total = S,
          n_draws_valid = sum(.draw_valid),
          n_draws_dropped = S - sum(.draw_valid)
        )
      )
      class(res) <- "mixgpd_predict"
      return(.return_out(res))
    }

    if (is.na(nsim) || nsim < 1L) nsim <- n_pred
    if (is.null(sample_draw_idx)) idx <- sample.int(S, size = nsim, replace = TRUE)
    outm <- matrix(NA_real_, nrow = n_pred, ncol = nsim)

    for (t in seq_len(nsim)) {
      s <- idx[t]
      if (!.draw_valid[s]) next
      args0 <- .build_args0_or_null(s)
      if (is.null(args0)) next

      link_eta <- .compute_link_eta(s)
      gpd_eta <- if (is_spliced && GPD) .compute_spliced_gpd_eta(s) else list()

      for (i in seq_len(n_pred)) {
        if (is_spliced && GPD) {
          outm[i, t] <- .spliced_sample_values(s, i, n = 1L, link_eta = link_eta, gpd_eta = gpd_eta)[1]
          next
        }

        args <- args0
        if (GPD) {
          args$threshold <- .threshold_at(s, i)
          args$tail_scale <- .tail_scale_at(s, i)
          args$tail_shape <- .tail_shape_at(s, i)
          if (!is.finite(args$threshold) || !is.finite(args$tail_scale) || args$tail_scale <= 0 || !is.finite(args$tail_shape)) {
            outm[i, t] <- NA_real_
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
            outm[i, t] <- NA_real_
            next
          }
        }
        outm[i, t] <- as.numeric(do.call(r_fun, c(list(n = 1L), args)))[1]
      }
    }

    res <- list(
      fit = outm,
      fit_df = .values_to_long_df(outm, id = id_vals, value_name = "sample"),
      type = type,
      grid = NULL,
      diagnostics = list(
        n_draws_total = S,
        n_draws_valid = sum(.draw_valid),
        n_draws_dropped = S - sum(.draw_valid)
      )
    )
    class(res) <- "mixgpd_predict"
    return(.return_out(res))
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
        gpd_eta <- if (is_spliced && GPD) .compute_spliced_gpd_eta(s) else list()
        for (i in seq_len(n_obs)) {
          if (is_spliced && GPD) {
            samples_mat[s, i] <- .spliced_sample_values(s, i, n = 1L, link_eta = link_eta, gpd_eta = gpd_eta)[1]
            next
          }

          args <- args0
          if (GPD) {
            args$threshold <- .threshold_at(s, i)
            args$tail_scale <- .tail_scale_at(s, i)
            args$tail_shape <- .tail_shape_at(s, i)
            if (!is.finite(args$threshold) || !is.finite(args$tail_scale) || args$tail_scale <= 0 || !is.finite(args$tail_shape)) {
              samples_mat[s, i] <- NA_real_
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
              samples_mat[s, i] <- NA_real_
              next
            }
          }
          samples_mat[s, i] <- as.numeric(do.call(r_fun, c(list(n = 1L), args)))[1]
        }
      } else {
        # Unconditional: draw n_obs from marginal predictive for draw s
        if (is_spliced && GPD) {
          samples_mat[s, ] <- .spliced_sample_values(s, 1L, n = n_obs, link_eta = list(), gpd_eta = list())
          next
        }

        if (GPD) {
          args0$threshold <- threshold_scalar[s]
          args0$tail_scale <- tail_scale[s]
          args0$tail_shape <- .tail_shape_at(s, 1L)
          if (!is.finite(args0$threshold) || !is.finite(args0$tail_scale) || args0$tail_scale <= 0 || !is.finite(args0$tail_shape)) next
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
      fit_df = fit_df,
      type = type,
      draws = if (isTRUE(store_draws)) samples_mat else NULL,
      diagnostics = list(
        n_draws_total = S,
        n_draws_valid = sum(.draw_valid),
        n_draws_dropped = S - sum(.draw_valid)
      )
    )
    class(out) <- "mixgpd_predict"
    return(.return_out(out))
  }

  # -----------------------------
  # mean (posterior mean of predictive distribution)
  # -----------------------------
  if (type == "mean") {
    if (is.null(bulk_mean_fun) || !is.function(bulk_mean_fun)) {
      stop(sprintf("Analytical mean is not implemented for kernel '%s'.", kernel), call. = FALSE)
    }
    if (GPD && (is.null(bulk_mean_trunc_fun) || !is.function(bulk_mean_trunc_fun))) {
      stop(sprintf("Analytical GPD mean is not implemented for kernel '%s'.", kernel), call. = FALSE)
    }

    if (!has_X) {
      draw_means <- rep(NA_real_, S)
      for (s in seq_len(S)) {
        if (!.draw_valid[s]) next
        args0 <- .build_args0_or_null(s)
        if (is.null(args0)) next

        if (!GPD) {
          draw_means[s] <- as.numeric(do.call(bulk_mean_fun, args0))[1]
          next
        }

        if (is_spliced) {
          draw_means[s] <- .analytic_spliced_mean_row(s, 1L, link_eta = list(), gpd_eta = list())
          next
        }

        threshold_s <- .threshold_at(s, 1L)
        tail_scale_s <- .tail_scale_at(s, 1L)
        tail_shape_s <- .tail_shape_at(s, 1L)
        if (!is.finite(threshold_s) || !is.finite(tail_scale_s) || tail_scale_s <= 0 || !is.finite(tail_shape_s)) next
        bulk_args_s <- c(if ("w" %in% names(args0)) list(w = args0$w) else list(), args0[bulk_params])
        bulk_trunc_s <- as.numeric(do.call(bulk_mean_trunc_fun, c(bulk_args_s, list(threshold = threshold_s))))[1]
        Fu_s <- as.numeric(do.call(bulk_p_fun, c(list(q = threshold_s, lower.tail = 1L, log.p = 0L), bulk_args_s)))[1]
        draw_means[s] <- bulk_trunc_s + (1 - .clamp_prob(Fu_s)) *
          .gpd_tail_mean_or_error(threshold_s, tail_scale_s, tail_shape_s)
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
        fit_df = fit_df,
        type = type,
        grid = NULL,
        draws = if (isTRUE(store_draws)) draw_means else NULL,
        diagnostics = list(
          n_draws_total = S,
          n_draws_valid = sum(.draw_valid),
          n_draws_dropped = S - sum(.draw_valid),
          mean_method = "analytic"
        )
      )
      class(out) <- "mixgpd_predict"
      return(.return_out(out))
    }

    draw_means_mat <- matrix(NA_real_, nrow = S, ncol = n_pred)
    for (s in seq_len(S)) {
      if (!.draw_valid[s]) next
      args0 <- .build_args0_or_null(s)
      if (is.null(args0)) next

      link_eta <- .compute_link_eta(s)
      gpd_eta <- if (is_spliced && GPD) .compute_spliced_gpd_eta(s) else list()

      for (i in seq_len(n_pred)) {
        if (is_spliced && GPD) {
          draw_means_mat[s, i] <- .analytic_spliced_mean_row(s, i, link_eta = link_eta, gpd_eta = gpd_eta)
          next
        }

        args <- args0
        if (length(link_params)) {
          bad <- FALSE
          for (nm in link_params) {
            vv <- as.numeric(link_eta[[nm]][i, ])
            if (!.support_ok(nm, vv)) {
              bad <- TRUE
              break
            }
            args[[nm]] <- vv
          }
          if (bad) {
            draw_means_mat[s, i] <- NA_real_
            next
          }
        }

        if (!GPD) {
          draw_means_mat[s, i] <- as.numeric(do.call(bulk_mean_fun, args))[1]
          next
        }

        threshold_i <- .threshold_at(s, i)
        tail_scale_i <- .tail_scale_at(s, i)
        tail_shape_i <- .tail_shape_at(s, i)
        if (!is.finite(threshold_i) || !is.finite(tail_scale_i) || tail_scale_i <= 0 || !is.finite(tail_shape_i)) {
          draw_means_mat[s, i] <- NA_real_
          next
        }
        bulk_args_i <- c(if ("w" %in% names(args)) list(w = args$w) else list(), args[bulk_params])
        bulk_trunc_i <- as.numeric(do.call(bulk_mean_trunc_fun, c(bulk_args_i, list(threshold = threshold_i))))[1]
        Fu_i <- as.numeric(do.call(bulk_p_fun, c(list(q = threshold_i, lower.tail = 1L, log.p = 0L), bulk_args_i)))[1]
        draw_means_mat[s, i] <- bulk_trunc_i + (1 - .clamp_prob(Fu_i)) *
          .gpd_tail_mean_or_error(threshold_i, tail_scale_i, tail_shape_i)
      }
    }

    summ <- .posterior_summarize(t(draw_means_mat), probs = probs, interval = if (compute_interval) interval else NULL)
    fit_df <- data.frame(
      id = id_vals,
      estimate = as.numeric(summ$estimate),
      lower = if (compute_interval) as.numeric(summ$lower) else NA_real_,
      upper = if (compute_interval) as.numeric(summ$upper) else NA_real_,
      row.names = NULL
    )
    fit_df <- .reorder_predict_cols(fit_df)

    out <- list(
      fit = fit_df,
      fit_df = fit_df,
      type = type,
      grid = NULL,
      draws = if (isTRUE(store_draws)) draw_means_mat else NULL,
      diagnostics = list(
        n_draws_total = S,
        n_draws_valid = sum(.draw_valid),
        n_draws_dropped = S - sum(.draw_valid),
        mean_method = "analytic"
      )
    )
    class(out) <- "mixgpd_predict"
    return(.return_out(out))
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
        if (is_spliced && GPD) {
          yy <- .spliced_sample_values(s, 1L, n = nsim_inner, link_eta = list(), gpd_eta = list())
          draw_rmeans[s] <- mean(pmin(yy, cutoff), na.rm = TRUE)
          next
        }
        if (GPD) {
          args0$threshold <- threshold_scalar[s]
          args0$tail_scale <- tail_scale[s]
          args0$tail_shape <- .tail_shape_at(s, 1L)
          if (!is.finite(args0$threshold) || !is.finite(args0$tail_scale) || args0$tail_scale <= 0 || !is.finite(args0$tail_shape)) next
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
        fit_df = fit_df,
        type = type,
        grid = NULL,
        cutoff = cutoff,
        draws = if (isTRUE(store_draws)) draw_rmeans else NULL,
        diagnostics = list(
          n_draws_total = S,
          n_draws_valid = sum(.draw_valid),
          n_draws_dropped = S - sum(.draw_valid),
          nsim_mean = nsim_inner,
          mean_infinite = any(!is.finite(draw_rmeans))
        )
      )
      class(out) <- "mixgpd_predict"
      return(.return_out(out))
    }

    draw_rmeans_mat <- matrix(NA_real_, nrow = S, ncol = n_pred)

    for (s in seq_len(S)) {
      if (!.draw_valid[s]) next
      args0 <- .build_args0_or_null(s)
      if (is.null(args0)) next

      link_eta <- .compute_link_eta(s)
      gpd_eta <- if (is_spliced && GPD) .compute_spliced_gpd_eta(s) else list()

      for (i in seq_len(n_pred)) {
        if (is_spliced && GPD) {
          yy <- .spliced_sample_values(s, i, n = nsim_inner, link_eta = link_eta, gpd_eta = gpd_eta)
          draw_rmeans_mat[s, i] <- mean(pmin(yy, cutoff), na.rm = TRUE)
          next
        }

        args <- args0
        if (GPD) {
          args$threshold <- .threshold_at(s, i)
          args$tail_scale <- .tail_scale_at(s, i)
          args$tail_shape <- .tail_shape_at(s, i)
          if (!is.finite(args$threshold) || !is.finite(args$tail_scale) || args$tail_scale <= 0 || !is.finite(args$tail_shape)) {
            draw_rmeans_mat[s, i] <- NA_real_
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
            draw_rmeans_mat[s, i] <- NA_real_
            next
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
      fit_df = fit_df,
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
    return(.return_out(out))
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


# ==============================================================================
# Internal helpers for cluster-label summaries (PSM, Dahl's method)
# ==============================================================================

#' Extract cluster assignment matrix from MCMC samples
#'
#' Extracts the posterior draws of cluster assignments \code{z[1:N]} from a fitted
#' mixgpd_fit object and returns them as an integer matrix (iterations x N).
#'
#' @details
#' Cluster samplers store latent labels as separate monitored nodes `z[i]`. This
#' helper locates those nodes in every retained chain, orders them by observation
#' index, stacks the chains, and returns the result as one integer matrix that is
#' ready for PSM and representative-partition calculations.
#'
#' @param object A \code{mixgpd_fit} object.
#' @return Integer matrix with rows = posterior draws, cols = observations.
#' @keywords internal
.extract_z_matrix <- function(object) {
  smp <- .get_samples_mcmclist(object)
  if (is.null(smp)) {
    stop("No MCMC samples found in object.", call. = FALSE)
  }

  # Stack all chains
  zmat_list <- vector("list", length(smp))
  for (i in seq_along(smp)) {
    M <- as.matrix(smp[[i]])
    z_cols <- grep("^z\\[[0-9]+\\]$", colnames(M))
    if (length(z_cols) == 0) {
      stop("No cluster assignment variables 'z[i]' found in MCMC samples.", call. = FALSE)
    }
    # Sort z columns numerically by index
    z_colnames <- colnames(M)[z_cols]
    z_indices <- as.integer(sub("^z\\[([0-9]+)\\]$", "\\1", z_colnames))
    z_cols <- z_cols[order(z_indices)]
    zmat_list[[i]] <- M[, z_cols, drop = FALSE]
  }

  zmat <- do.call(rbind, zmat_list)
  # Convert to integer matrix
  mode(zmat) <- "integer"
  return(zmat)
}

#' Compute posterior similarity matrix
#'
#' Computes the posterior similarity matrix (PSM) from a matrix of cluster
#' assignments. \code{PSM[i,j]} = probability that observations i and j are in the
#' same cluster.
#'
#' @details
#' If \eqn{z_i^{(s)}} denotes the cluster label of observation \eqn{i} at draw
#' \eqn{s}, then this helper computes
#' \deqn{\mathrm{PSM}_{ij} \approx \frac{1}{S} \sum_{s=1}^S I(z_i^{(s)} = z_j^{(s)}).}
#' The resulting matrix is the basic posterior co-clustering summary used by the
#' Dahl representative partition and several cluster diagnostics.
#'
#' @param z_matrix Integer matrix (iterations x N) of cluster assignments.
#' @return Symmetric N x N matrix of co-clustering probabilities.
#' @keywords internal
.compute_psm <- function(z_matrix) {
  n_iter <- nrow(z_matrix)
  n_obs <- ncol(z_matrix)
  PSM <- matrix(0, n_obs, n_obs)

  for (s in seq_len(n_iter)) {
    z <- z_matrix[s, ]
    # Indicator matrix: same cluster
    A <- outer(z, z, "==") * 1.0
    PSM <- PSM + A
  }
  PSM <- PSM / n_iter
  return(PSM)
}

#' Find Dahl representative clustering
#'
#' Identifies the posterior draw that minimizes squared distance to the
#' posterior similarity matrix, following Dahl (2006). Returns relabeled
#' cluster assignments as consecutive integers 1, 2, ..., K.
#'
#' @details
#' For each posterior draw, the helper forms its adjacency matrix and computes
#' the squared Frobenius distance to the PSM. The selected representative draw is
#' the one that minimizes that loss, which is Dahl's least-squares rule for
#' choosing one clustering from the posterior sample.
#'
#' @param z_matrix Integer matrix (iterations x N) of cluster assignments.
#' @param PSM Posterior similarity matrix (N x N).
#' @return List with components: draw_index (integer), labels (integer vector),
#'   K (number of clusters).
#' @references Dahl, D. B. (2006). Model-based clustering for expression data
#'   via a Dirichlet process mixture model. In M. Vannucci, et al. (Eds.),
#'   Bayesian Inference for Gene Expression and Proteomics (pp. 201-218).
#'   Cambridge University Press.
#' @keywords internal
.dahl_representative <- function(z_matrix, PSM) {
  n_iter <- nrow(z_matrix)
  ssq <- numeric(n_iter)

  for (s in seq_len(n_iter)) {
    z <- z_matrix[s, ]
    A <- outer(z, z, "==") * 1.0
    ssq[s] <- sum((A - PSM)^2)
  }

  s_star <- which.min(ssq)
  z_hat <- as.integer(z_matrix[s_star, ])

  # Relabel to consecutive integers
  labels <- match(z_hat, unique(z_hat))
  K <- length(unique(labels))

  return(list(
    draw_index = s_star,
    labels = labels,
    K = K
  ))
}

#' Compute cluster membership probabilities from PSM
#'
#' For each observation, computes the probability of membership in each cluster
#' defined by the representative clustering, derived from the posterior
#' similarity matrix.
#'
#' @details
#' The representative labels define a reference partition with clusters
#' \eqn{C_1, \dots, C_K}. For each observation \eqn{i}, this helper averages the
#' posterior similarity scores \eqn{\mathrm{PSM}_{ij}} over members
#' \eqn{j \in C_k} to obtain a cluster-membership score for cluster \eqn{k}, and
#' then normalizes those scores to sum to one across clusters.
#'
#' @param z_matrix Integer matrix (iterations x N) of cluster assignments.
#' @param labels_representative Integer vector of representative cluster labels.
#' @param PSM Posterior similarity matrix (N x N).
#' @return N x K matrix of cluster membership probabilities.
#' @keywords internal
.compute_cluster_probs <- function(z_matrix, labels_representative, PSM) {
  n_obs <- length(labels_representative)
  K <- length(unique(labels_representative))

  probs <- matrix(0, nrow = n_obs, ncol = K)

  for (k in seq_len(K)) {
    # Observations in cluster k in the representative partition
    idx_k <- which(labels_representative == k)
    # For each observation i, average PSM with all members of cluster k
    for (i in seq_len(n_obs)) {
      probs[i, k] <- mean(PSM[i, idx_k])
    }
  }

  # Normalize rows to sum to 1
  row_sums <- rowSums(probs)
  for (i in seq_len(n_obs)) {
    if (row_sums[i] > 0) {
      probs[i, ] <- probs[i, ] / row_sums[i]
    } else {
      # Fallback: uniform distribution
      probs[i, ] <- 1 / K
    }
  }

  return(probs)
}
