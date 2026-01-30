source("renv/activate.R")
# Suppress Windows OneDrive owner-resolution warnings (harmless file.info messages)
# when project lives in OneDrive. Safe to keep repo in OneDrive as a backup to GitHub.
if (Sys.info()[["sysname"]] == "Windows" && getRversion() >= "4.0.0" &&
    file.exists("DESCRIPTION")) {
  pkg <- tryCatch(read.dcf("DESCRIPTION", fields = "Package")[1L], error = function(e) "")
  if (identical(pkg, "DPmixGPD")) {
    msg1 <- "cannot resolve owner of file"
    msg2 <- "No mapping between account names and security IDs"
    globalCallingHandlers(warning = function(w) {
      m <- conditionMessage(w)
      if (grepl(msg1, m, fixed = TRUE) || grepl(msg2, m, fixed = TRUE))
        invokeRestart("muffleWarning")
    })
  }
}

tryCatch(
  source("renv/activate.R"),
  error = function(e) {
    message("renv autoloader failed: ", conditionMessage(e))
  }
)

