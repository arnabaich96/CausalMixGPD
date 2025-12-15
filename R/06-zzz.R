#' OnLoad
#'
#' OnLoad.
#'
#' @param libname libname.
#' @param pkgname pkgname.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".onLoad", "DPmixGPD")
#' f
#'
#' @keywords internal
#' On load hook
#'
#' Initializes internal registries and declares variables used by generated code
#' to satisfy `R CMD check`.
#'
#' @param libname Character; library path.
#' @param pkgname Character; package name.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' getFromNamespace(".onLoad", "DPmixGPD")
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # silence notes for generated / non-standard evaluation symbols
  utils::globalVariables(c(".valid_trans_labels", "v", "z", "beta_scale",
    ".iter", "value", "x", "y",
    "ehat", "term", "lower", "upper"
  ))
  # kernel registry
  init_kernel_registry()

  # load component-level kernel definitions (inst/kernels/*.R)
  kern_dir <- system.file("kernels", package = pkgname)
  if (nzchar(kern_dir) && dir.exists(kern_dir)) {
    kern_files <- sort(list.files(kern_dir, pattern = "\\.R$", full.names = TRUE))
    for (ff in kern_files) source(ff, local = FALSE)
  }

  # ensure nimble is available but DO NOT attach it
  if (!requireNamespace("nimble", quietly = TRUE)) {
    stop("Package 'nimble' is required by DPmixGPD but could not be loaded.",
         call. = FALSE)
  }

  invisible(NULL)
}