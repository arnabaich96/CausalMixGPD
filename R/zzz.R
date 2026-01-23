#' @importFrom methods is
#' @noRd
.onLoad <- function(libname, pkgname) {
  options(digits = 3)
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::opts_chunk$set(
      digits = 3,
      render = function(x) {
        if (is.numeric(x)) fmt3(x) else x
      }
    )
  }
}
