if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

if (requireNamespace("renv", quietly = TRUE)) {
  renv::load(project = ".")
}

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
} else if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(quiet = TRUE)
}
