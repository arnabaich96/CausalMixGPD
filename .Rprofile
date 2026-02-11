# Resolve the package project root from any working directory.
.dpmixgpd_find_root <- function(start = getwd()) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(12L)) {
    desc <- file.path(d, "DESCRIPTION")
    if (file.exists(desc)) {
      pkg <- tryCatch(read.dcf(desc, fields = "Package")[1L], error = function(e) "")
      if (identical(pkg, "DPmixGPD")) return(d)
    }
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  ""
}

.dpmixgpd_root <- .dpmixgpd_find_root()

# Suppress Windows OneDrive owner-resolution warnings (harmless file.info messages)
# when project lives in OneDrive. Safe to keep repo in OneDrive as a backup to GitHub.
if (nzchar(.dpmixgpd_root) &&
    Sys.info()[["sysname"]] == "Windows" &&
    getRversion() >= "4.0.0") {
  msg1 <- "cannot resolve owner of file"
  msg2 <- "No mapping between account names and security IDs"
  globalCallingHandlers(warning = function(w) {
    m <- conditionMessage(w)
    if (grepl(msg1, m, fixed = TRUE) || grepl(msg2, m, fixed = TRUE))
      invokeRestart("muffleWarning")
  })
}

if (nzchar(.dpmixgpd_root)) {
  if (!nzchar(Sys.getenv("RENV_PROJECT", unset = ""))) {
    Sys.setenv(RENV_PROJECT = .dpmixgpd_root)
  }

  tryCatch(
    source(file.path(.dpmixgpd_root, "renv", "activate.R")),
    error = function(e) {
      message("renv autoloader failed: ", conditionMessage(e))
    }
  )

  # Keep the development package available in knit/dev sessions without install().
  if (isTRUE(getOption("dpmixgpd.autoload", TRUE)) &&
      requireNamespace("pkgload", quietly = TRUE)) {
      tryCatch(
        pkgload::load_all(
          path = .dpmixgpd_root,
          export_all = FALSE,
          helpers = FALSE,
          attach_testthat = FALSE,
          quiet = TRUE
        ),
        error = function(e) {
          message("DPmixGPD autoload failed: ", conditionMessage(e))
        }
      )
  }

  # Keep knit root stable when rendering files from subdirectories.
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::opts_knit$set(root.dir = .dpmixgpd_root)
  }
}
