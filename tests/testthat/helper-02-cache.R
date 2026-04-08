.cache_enabled <- function() {
  val <- tolower(Sys.getenv("DPMIXGPD_USE_CACHE", "0"))
  val %in% c("1", "true", "yes")
}

.cache_dir <- function() {
  d <- Sys.getenv("DPMIXGPD_CACHE_DIR", "")
  if (!nzchar(d)) {
    is_check <- nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) ||
      nzchar(Sys.getenv("RCMDCHECK")) ||
      identical(tolower(Sys.getenv("NOT_CRAN", "")), "false")
    d <- if (is_check) {
      file.path(tempdir(), "CausalMixGPD_test_cache")
    } else {
      file.path("tests", "testthat", "_cache")
    }
  }
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

.cache_hash <- function(x) {
  tf <- tempfile()
  writeLines(x, tf)
  h <- tools::md5sum(tf)
  unlink(tf)
  as.character(h)
}

.cache_path <- function(key) {
  file.path(.cache_dir(), paste0(key, ".rds"))
}

.cache_get <- function(key) {
  path <- .cache_path(key)
  if (!file.exists(path)) return(NULL)
  out <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(out)) {
    unlink(path)
    return(NULL)
  }
  out
}

.cache_set <- function(key, value) {
  path <- .cache_path(key)
  saveRDS(value, path)
  invisible(path)
}
