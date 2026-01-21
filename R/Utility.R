NULL

#' Test coverage status helper
#'
#' @description
#' `coverage_status()` runs `covr::package_coverage()` against the package tests
#' and returns a small summary that can be stored for docs or dashboards. Use
#' `read_coverage_status()` to read previously saved results.
#'
#' @param type Coverage type, passed to `covr::package_coverage()`. Currently
#'   supports `tests` (the default) and `all`.
#' @param path Path to the package root. Defaults to the current working directory.
#' @param data_file Optional file path to write the status JSON (useful for
#'   documentation or pkgdown badges). When `NULL` no file is written.
#' @param quiet Passed to `covr::package_coverage()` to control console output.
#' @return A named list with `percent`, `type`, `timestamp`, `statements`, and
#'   `files`. Invisibly returns the same list when writing to disk.
#' @export
coverage_status <- function(type = c("tests", "all"),
                            path = ".",
                            data_file = NULL,
                            quiet = TRUE) {
  type <- match.arg(type)
  if (!requireNamespace("covr", quietly = TRUE)) {
    stop("Package 'covr' is required to compute coverage.", call. = FALSE)
  }

  coverage <- covr::package_coverage(path = path, type = type, quiet = quiet)
  percent <- covr::percent_coverage(coverage)
  filenames <- vapply(coverage, function(x) {
    sr <- x$srcref
    srcfile <- attr(sr, "srcfile")
    if (!is.null(srcfile) && !is.null(srcfile$filename)) {
      srcfile$filename
    } else {
      "<unknown>"
    }
  }, character(1))
  info <- list(
    percent = percent,
    type = type,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    statements = length(coverage),
    files = length(unique(filenames))
  )

  if (!is.null(data_file)) {
    dir <- dirname(data_file)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(info, data_file, auto_unbox = TRUE, pretty = TRUE)
  }

  info
}

#' Read stored coverage status
#'
#' @param file Path to a JSON status file produced by
#'   `coverage_status(..., data_file = ...)`. Defaults to `inst/extdata/coverage_status.json`.
#' @return Named list as saved by `coverage_status()`.
#' @export
read_coverage_status <- function(file = system.file("extdata/coverage_status.json", package = "DPmixGPD")) {
  if (!file.exists(file)) {
    stop("Coverage status file not found: ", file, call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to read coverage status files.", call. = FALSE)
  }
  jsonlite::read_json(file)
}
