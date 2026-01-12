`%||%` <- function(a, b) if (!is.null(a)) a else b

print_dataset_header <- function(obj, name = NULL) {
  if (!is.null(name)) cat("\nDataset:", name, "\n")
  if (!is.null(obj$y)) {
    cat("head(y):\n")
    print(utils::head(obj$y))
  } else if (!is.null(obj$X)) {
    cat("head(X):\n")
    print(utils::head(obj$X))
  }

  meta <- obj$meta %||% list()
  n <- meta$n %||% meta$N %||% NA_integer_
  support <- meta$support %||% "unknown"
  p <- meta$p %||% NA_integer_
  K <- meta$K_true %||% paste0(meta$K0 %||% NA, "/", meta$K1 %||% NA)
  tail <- meta$tail %||% FALSE
  cat(sprintf("meta: n=%s | support=%s | p=%s | K_true=%s | tail=%s\n",
              n, support, p, K, tail))
}

default_mcmc <- function() {
  list(niter = 1000, nburnin = 250, thin = 1, nchains = 2, seed = 1)
}

# Resolve a data file path robustly when knitting via pkgdown/callr.
# Tries: (vignette_dir/../data), (cwd/data), (vignette_dir/data)
resolve_data_file <- function(filename) {
  vdir <- tryCatch(dirname(knitr::current_input(dir = TRUE)), error = function(e) NULL)
  candidates <- c(
    if (!is.null(vdir)) file.path(vdir, "..", "data", filename) else NULL,
    file.path(getwd(), "data", filename),
    if (!is.null(vdir)) file.path(vdir, "data", filename) else NULL
  )
  for (p in candidates) {
    if (!is.null(p) && file.exists(p)) return(p)
  }
  NULL
}
