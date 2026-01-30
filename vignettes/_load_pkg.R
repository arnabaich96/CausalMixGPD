if (requireNamespace("DPmixGPD", quietly = TRUE)) {
  library(DPmixGPD)
} else if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".")
} else {
  stop("DPmixGPD is not installed and pkgload is unavailable.", call. = FALSE)
}
