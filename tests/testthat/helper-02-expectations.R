.expect_cols <- function(draws, cols) {
  missing <- setdiff(cols, colnames(draws))
  testthat::expect_true(
    length(missing) == 0L,
    info = paste("Missing cols:", paste(missing, collapse = ", "))
  )
  invisible(TRUE)
}

.expect_ci_order <- function(x) {
  # Accept a few common return formats from DPmixGPD summary helpers.
  # - numeric named vector: c(mean, sd, lower, upper)
  # - numeric unnamed vector: c(lower, mean, upper) or similar
  # - 1-row matrix/data.frame with lower/upper columns

  if (is.matrix(x) || is.data.frame(x)) {
    cols <- colnames(x)
    testthat::expect_false(is.null(cols))
    testthat::expect_true(all(c("lower", "upper") %in% cols))
    lo <- as.numeric(x[1, "lower"])
    hi <- as.numeric(x[1, "upper"])
    testthat::expect_true(is.finite(lo) && is.finite(hi))
    testthat::expect_lte(lo, hi)
    return(invisible(TRUE))
  }

  testthat::expect_true(is.numeric(x))
  nms <- names(x)

  if (!is.null(nms) && all(c("lower", "upper") %in% nms)) {
    testthat::expect_lte(x[["lower"]], x[["upper"]])
    return(invisible(TRUE))
  }

  if (length(x) %in% c(3L, 4L)) {
    xs <- sort(as.numeric(x))
    testthat::expect_true(all(is.finite(xs)))
    testthat::expect_true(isTRUE(all(diff(xs) >= 0)))
    return(invisible(TRUE))
  }

  testthat::fail("Unrecognized CI format")
}

# Some older tests rely on this tiny helper.
.has_dimnames <- function(x) {
  dn <- dimnames(x)
  !is.null(dn) && length(dn) == 2L && !is.null(dn[[1L]]) && !is.null(dn[[2L]])
}
