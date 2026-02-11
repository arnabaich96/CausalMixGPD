if (requireNamespace("pkgload", quietly = TRUE)) {
  root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (!file.exists(file.path(root, "DESCRIPTION"))) {
    root <- normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE)
  }
  if (file.exists(file.path(root, "DESCRIPTION"))) {
    try(pkgload::load_all(path = root, quiet = TRUE), silent = TRUE)
  }
}
