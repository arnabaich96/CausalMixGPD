.onLoad <- function(libname, pkgname) {
  # kernel registry
  init_kernel_registry()

  # ensure nimble is available but DO NOT attach it
  if (!requireNamespace("nimble", quietly = TRUE)) {
    stop("Package 'nimble' is required by DPmixGPD but could not be loaded.",
         call. = FALSE)
  }
}
