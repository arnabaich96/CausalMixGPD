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
      message("DT view warning (suppressed): ", conditionMessage(w))
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
    return(print(df, ...))
  }
  if (is.matrix(x)) {
    mat_raw <- x
    mat <- format_mat3(x)
    if (.is_knitr_output() && isTRUE(getOption("causalmixgpd.knitr.kable", FALSE))) {
      kbl <- .kable_table(as.data.frame(mat), row.names = row_names)
      if (!is.null(kbl)) return(print(kbl))
    }
    .dt_view_table(as.data.frame(mat_raw), row.names = row_names, digits = 3)
    return(print(mat, ...))
  }
  if (is.numeric(x)) {
    return(print(fmt3(x), ...))
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
    return(print(df, ...))
  }
  if (is.matrix(x)) {
    mat_raw <- x
    mat <- format_mat3_sci(x, digits = digits, big = big)
    if (.is_knitr_output() && isTRUE(getOption("causalmixgpd.knitr.kable", FALSE))) {
      kbl <- .kable_table(as.data.frame(mat), row.names = row_names)
      if (!is.null(kbl)) return(print(kbl))
    }
    .dt_view_table(as.data.frame(mat_raw), row.names = row_names, digits = digits)
    return(print(mat, ...))
  }
  if (is.numeric(x)) {
    return(print(fmt3_sci(x, digits = digits, big = big), ...))
  }
  print(x, ...)
}
