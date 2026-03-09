# Formatting helpers for consistent numeric printing
# These are internal (not exported).

#' Format numbers to 3 decimals without trailing zeros
#' @param x numeric vector
#' @return character vector
#' @noRd
fmt3 <- function(x) {
  formatC(x, digits = 3, format = "f", drop0trailing = TRUE)
}

#' Format numbers to 3 decimals, switching to scientific for large values
#' @param x numeric vector
#' @param digits number of digits after decimal
#' @param big absolute value threshold to switch to scientific notation
#' @return character vector
#' @noRd
fmt3_sci <- function(x, digits = 3, big = 1e4) {
  out <- formatC(x, digits = digits, format = "f", drop0trailing = TRUE)
  if (!length(x)) return(out)
  big_idx <- is.finite(x) & abs(x) >= big
  if (any(big_idx)) {
    out[big_idx] <- formatC(x[big_idx], digits = digits, format = "e")
  }
  out
}

#' @noRd
fmt3_vec <- function(x) {
  if (!length(x)) return("")
  paste(fmt3(x), collapse = ", ")
}

#' @noRd
format_df3 <- function(df) {
  if (!is.data.frame(df)) return(df)
  num_cols <- vapply(df, is.numeric, logical(1))
  df[num_cols] <- lapply(df[num_cols], fmt3)
  df
}

#' @noRd
format_df3_sci <- function(df, digits = 3, big = 1e4) {
  if (!is.data.frame(df)) return(df)
  num_cols <- vapply(df, is.numeric, logical(1))
  df[num_cols] <- lapply(df[num_cols], fmt3_sci, digits = digits, big = big)
  df
}

#' @noRd
format_mat3 <- function(mat) {
  if (!is.matrix(mat)) return(mat)
  out <- apply(mat, 2, fmt3)
  dim(out) <- dim(mat)
  dimnames(out) <- dimnames(mat)
  out
}

#' @noRd
format_mat3_sci <- function(mat, digits = 3, big = 1e4) {
  if (!is.matrix(mat)) return(mat)
  out <- apply(mat, 2, fmt3_sci, digits = digits, big = big)
  dim(out) <- dim(mat)
  dimnames(out) <- dimnames(mat)
  out
}

#' @noRd
.is_knitr_output <- function() {
  isTRUE(getOption("knitr.in.progress")) &&
    requireNamespace("knitr", quietly = TRUE)
}

#' @noRd
.knitr_asis <- function(...) {
  if (!requireNamespace("knitr", quietly = TRUE)) return(NULL)
  pieces <- list(...)
  flat <- unlist(lapply(pieces, function(x) {
    if (is.null(x)) return(character(0))
    if (inherits(x, "knitr_kable")) return(as.character(x))
    if (is.character(x)) return(x)
    as.character(x)
  }), use.names = FALSE)
  knitr::asis_output(paste(flat, collapse = "\n"))
}

#' @noRd
.kable_fmt <- function() {
  if (!requireNamespace("knitr", quietly = TRUE)) return("markdown")
  if (knitr::is_latex_output()) return("latex")
  if (knitr::is_html_output()) return("html")
  "markdown"
}

#' @noRd
.kable_table <- function(df, row.names = TRUE) {
  if (!requireNamespace("knitr", quietly = TRUE)) return(NULL)
  kbl <- knitr::kable(df, align = "c", row.names = row.names, format = .kable_fmt())
  if (requireNamespace("kableExtra", quietly = TRUE)) {
    kbl <- kableExtra::kable_styling(kbl, full_width = FALSE, position = "center")
  }
  kbl
}

#' @noRd
.dt_view_table <- function(df, row.names = TRUE, digits = 3, min_rows = 10L, min_cols = 8L) {
  # Goal: still print a regular data.frame to the console, but if DT is installed
  # and we're in an interactive session (not knitr), also open an interactive view.
  if (!interactive()) return(invisible(NULL))
  if (isTRUE(getOption("knitr.in.progress"))) return(invisible(NULL))
  if (!requireNamespace("DT", quietly = TRUE)) return(invisible(NULL))

  df <- as.data.frame(df)
  if (NROW(df) < as.integer(min_rows) || NCOL(df) < as.integer(min_cols)) {
    return(invisible(NULL))
  }

  num_cols <- vapply(df, is.numeric, logical(1))
  dt <- DT::datatable(
    df,
    rownames = isTRUE(row.names),
    class = "compact stripe hover",
    options = list(
      pageLength = min(25L, NROW(df)),
      scrollX = TRUE,
      autoWidth = TRUE,
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    )
  )
  if (any(num_cols)) {
    dt <- DT::formatRound(dt, columns = names(df)[num_cols], digits = digits)
  }

  # In RStudio this typically opens the Viewer; otherwise it may open a browser tab.
  withCallingHandlers(
    print(dt),
    warning = function(w) {
      # Log suppressed warnings for debugging without showing them as warnings
      .cmgpd_message("DT view warning (suppressed): ", conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  invisible(NULL)
}

#' @noRd
print_fmt3 <- function(x, ...) {
  args <- list(...)
  row_names <- if (!is.null(args$row.names)) args$row.names else TRUE
  if (is.data.frame(x)) {
    df_raw <- x
    df <- format_df3(x)
    if (.is_knitr_output() && isTRUE(getOption("causalmixgpd.knitr.kable", FALSE))) {
      kbl <- .kable_table(df, row.names = row_names)
      if (!is.null(kbl)) return(print(kbl))
    }
    .dt_view_table(df_raw, row.names = row_names, digits = 3)
    return(print(df, quote = FALSE, ...))
  }
  if (is.matrix(x)) {
    mat_raw <- x
    mat <- format_mat3(x)
    if (.is_knitr_output() && isTRUE(getOption("causalmixgpd.knitr.kable", FALSE))) {
      kbl <- .kable_table(as.data.frame(mat), row.names = row_names)
      if (!is.null(kbl)) return(print(kbl))
    }
    .dt_view_table(as.data.frame(mat_raw), row.names = row_names, digits = 3)
    return(print(noquote(mat), ...))
  }
  if (is.numeric(x)) {
    return(print(noquote(fmt3(x)), ...))
  }
  print(x, ...)
}

#' @noRd
print_fmt3_sci <- function(x, digits = 3, big = 1e4, ...) {
  args <- list(...)
  row_names <- if (!is.null(args$row.names)) args$row.names else TRUE
  if (is.data.frame(x)) {
    df_raw <- x
    df <- format_df3_sci(x, digits = digits, big = big)
    if (.is_knitr_output() && isTRUE(getOption("causalmixgpd.knitr.kable", FALSE))) {
      kbl <- .kable_table(df, row.names = row_names)
      if (!is.null(kbl)) return(print(kbl))
    }
    .dt_view_table(df_raw, row.names = row_names, digits = digits)
    return(print(df, quote = FALSE, ...))
  }
  if (is.matrix(x)) {
    mat_raw <- x
    mat <- format_mat3_sci(x, digits = digits, big = big)
    if (.is_knitr_output() && isTRUE(getOption("causalmixgpd.knitr.kable", FALSE))) {
      kbl <- .kable_table(as.data.frame(mat), row.names = row_names)
      if (!is.null(kbl)) return(print(kbl))
    }
    .dt_view_table(as.data.frame(mat_raw), row.names = row_names, digits = digits)
    return(print(noquote(mat), ...))
  }
  if (is.numeric(x)) {
    return(print(noquote(fmt3_sci(x, digits = digits, big = big)), ...))
  }
  print(x, ...)
}


#' Plot styling and theme helpers (internal)
#'
#' Internal helper functions for consistent plot styling across the package.
#' These functions provide default color palettes, themes, and plotly conversion
#' utilities.
#'
#' @name viz-theme
#' @keywords internal
#' @noRd
NULL

#' Get default color palette
#'
#' Returns a consistent color palette for plotting. Uses colorblind-friendly
#' Wong palette as base.
#'
#' @param n Integer, number of colors needed. If NULL or greater than 8,
#'   repeats the base palette.
#'
#' @return Character vector of hex colors.
#' @keywords internal
#' @noRd
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

#' Get default ggplot2 theme
#'
#' Returns a minimal theme with package-specific styling.
#'
#' @return A ggplot2 theme object.
#' @keywords internal
#' @noRd
.plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 20),
      axis.title = ggplot2::element_text(size = 17),
      axis.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 15),
      legend.text = ggplot2::element_text(size = 13),
      strip.text = ggplot2::element_text(size = 15),
      legend.position = "top"
    )
}

#' Strip fill scales from ggplot object
#'
#' Removes fill aesthetics scales from a ggplot object. Used to work around
#' plotly conversion issues with certain fill scales.
#'
#' @param p A ggplot object.
#' @return The modified ggplot object.
#' @keywords internal
#' @noRd
.strip_fill_scales <- function(p) {
  if (!inherits(p, "ggplot")) return(p)
  if (is.null(p$scales) || !length(p$scales$scales)) return(p)
  keep <- vapply(p$scales$scales, function(s) {
    !("fill" %in% (s$aesthetics %||% character()))
  }, logical(1))
  p$scales$scales <- p$scales$scales[keep]
  p
}

#' Safely convert ggplot to plotly
#'
#' Attempts to convert a ggplot to plotly, with fallback to stripping
#' fill scales if the initial conversion fails.
#'
#' @param p A ggplot object.
#' @return A plotly object or the original ggplot if conversion fails.
#' @keywords internal
#' @noRd
.safe_ggplotly <- function(p) {
  tryCatch(
    plotly::ggplotly(p),
    error = function(e) {
      p2 <- .strip_fill_scales(p)
      tryCatch(plotly::ggplotly(p2), error = function(e2) p2)
    }
  )
}

#' Wrap plots in plotly if requested
#'
#' Conditionally converts ggplot objects to plotly based on user option.
#' Respects `options(CausalMixGPD.plotly = TRUE)` setting. Handles both
#' single ggplot objects and lists of plots.
#'
#' @param p A ggplot object or list of ggplot objects.
#' @return Either plotly object(s) or the original plot(s).
#' @keywords internal
#' @noRd
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

