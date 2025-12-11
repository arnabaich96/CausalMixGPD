# zzz.R

.onLoad <- function(libname, pkgname) {
  # 1) your existing kernel registry setup
  init_kernel_registry()

  # 2) make sure nimble namespace is loaded
  if (!"nimble" %in% loadedNamespaces()) {
    requireNamespace("nimble", quietly = TRUE)
  }

  # 3) ATTACH nimble so its global helpers (like nimbleInternalFunctions)
  #    are on the search path where setInits() expects them.
  if (!"package:nimble" %in% search()) {
    suppressPackageStartupMessages(
      require("nimble", quietly = TRUE, character.only = TRUE)
    )
  }
}


# Small shim so that unqualified calls to getNimbleOption() work
# even inside the DPmixGPD namespace.
#' @keywords internal
getNimbleOption <- function(...) {
  if (!requireNamespace("nimble", quietly = TRUE)) {
    stop("Package 'nimble' is required but not installed.")
  }
  nimble:::getNimbleOption(...)
}
