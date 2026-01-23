# Formatting helpers for consistent numeric printing
# These are internal (not exported).

#' Format numbers to 3 decimals without trailing zeros
#' @param x numeric vector
#' @return character vector
#' @noRd
fmt3 <- function(x) {
  formatC(x, digits = 3, format = "f", drop0trailing = TRUE)
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
format_mat3 <- function(mat) {
  if (!is.matrix(mat)) return(mat)
  out <- apply(mat, 2, fmt3)
  dim(out) <- dim(mat)
  dimnames(out) <- dimnames(mat)
  out
}

#' @noRd
print_fmt3 <- function(x, ...) {
  if (is.data.frame(x)) {
    return(print(format_df3(x), ...))
  }
  if (is.matrix(x)) {
    return(print(format_mat3(x), ...))
  }
  if (is.numeric(x)) {
    return(print(fmt3(x), ...))
  }
  print(x, ...)
}
