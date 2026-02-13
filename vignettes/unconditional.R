## ----setup, include=FALSE-----------------------------------------------------
options(dpmixgpd.knitr.kable = TRUE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  echo = FALSE
)

.extdata <- function(fname) {
  # Approach 1: Try system.file() which works for installed packages
  path <- system.file("extdata", fname, package = "DPmixGPD")
  if (nzchar(path) && file.exists(path)) {
    return(path)
  }
  
  # Approach 2: Use find.package() to locate package root
  tryCatch({
    pkg_path <- find.package("DPmixGPD")
    path <- file.path(pkg_path, "extdata", fname)
    if (file.exists(path)) {
      return(normalizePath(path, winslash = "/", mustWork = TRUE))
    }
  }, error = function(e) {})
  
  # Approach 3: Try relative path from vignette location
  rel_path <- file.path("..", "inst", "extdata", fname)
  if (file.exists(rel_path)) {
    return(rel_path)
  }
  
  # Approach 4: Try from current working directory
  if (dir.exists("inst/extdata")) {
    path <- file.path(getwd(), "inst", "extdata", fname)
    if (file.exists(path)) {
      return(path)
    }
  }
  
  stop("Cannot find artifact '", fname, "' in package extdata.")
}

## -----------------------------------------------------------------------------
library(DPmixGPD)
data("nc_pos_tail200_k4")
y <- nc_pos_tail200_k4$y

## -----------------------------------------------------------------------------
tbl <- utils::read.csv(.extdata("unconditional_quantiles.csv"))
knitr::kable(tbl, digits = 3)

