#' Package hooks
#'
#' Internal package initialization.
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  init_kernel_registry()
  invisible()
}
