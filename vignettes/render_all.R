#!/usr/bin/env Rscript

# Render all Rmd files in this directory
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) > 0) {
  script_dir <- normalizePath(dirname(sub("^--file=", "", file_arg[1])), winslash = "/", mustWork = TRUE)
  setwd(script_dir)
}

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("The rmarkdown package is required to render vignettes.")
}

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
} else {
  suppressPackageStartupMessages(library(DPmixGPD))
}

rmd_files <- list.files(pattern = "\\.Rmd$", full.names = TRUE)
rmd_files <- rmd_files[!grepl("\\.bak$", rmd_files)]

if (length(rmd_files) == 0) {
  stop("No Rmd files found to render.")
}

dir.create("cache", showWarnings = FALSE, recursive = TRUE)

results <- vector("list", length(rmd_files))

for (i in seq_along(rmd_files)) {
  f <- rmd_files[[i]]
  message("Rendering ", f)
  results[[i]] <- tryCatch(
    {
      rmarkdown::render(f, output_dir = getwd(), clean = FALSE, envir = new.env(parent = globalenv()))
      list(file = f, status = "ok", error = NULL)
    },
    error = function(e) {
      message("Failed to render ", f, ": ", e$message)
      list(file = f, status = "failed", error = e$message)
    }
  )
}

failed <- vapply(results, function(x) identical(x$status, "failed"), logical(1))
if (any(failed)) {
  message("Render completed with failures:")
  for (x in results[failed]) message(" - ", x$file, ": ", x$error)
  stop(sum(failed), " document(s) failed; see messages above.")
}

message("Render completed successfully for ", length(results), " document(s).")
