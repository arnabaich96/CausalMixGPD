#!/usr/bin/env Rscript

# Test pkgdown build
library(pkgdown)

setwd("D:/Git-Repos/DPMGPD_package/DPmixGPD")

message("Step 1: Initializing site...")
tryCatch({
  pkgdown::init_site(".")
  message("init_site completed")
}, error = function(e) {
  message("init_site error: ", e$message)
})

message("\nStep 2: Building site with lazy=TRUE...")
tryCatch({
  pkgdown::build_site(lazy = TRUE, devel = FALSE, preview = FALSE)
  message("build_site completed")
}, error = function(e) {
  message("build_site error: ", e$message)
  traceback()
})
