# Shared vignette helper to load DPmixGPD in multiple contexts.
.find_pkg_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in 0:6) {
    candidate <- if (i == 0) path else dirname(path)
    if (file.exists(file.path(candidate, "DESCRIPTION"))) {
      return(candidate)
    }
    path <- candidate
  }
  NULL
}

pkg <- "DPmixGPD"

# Prefer installed package when available (e.g., during R CMD check).
if (requireNamespace(pkg, quietly = TRUE)) {
  library(pkg, character.only = TRUE)
} else {
  pkg_root <- .find_pkg_root()
  if (is.null(pkg_root)) {
    stop("Could not locate package root with DESCRIPTION; cannot load ", pkg, ".")
  }
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(pkg_root, export_all = FALSE)
  } else if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(pkg_root, export_all = FALSE)
  } else {
    stop(
      "Please install 'pkgload' (preferred) or 'devtools' to load ", pkg,
      " from source."
    )
  }
}
