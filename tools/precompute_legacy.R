# tools/precompute_legacy.R

dir.create("D:/Rtmp", showWarnings = FALSE)
Sys.setenv(TMPDIR="D:/Rtmp", TEMP="D:/Rtmp", TMP="D:/Rtmp")

Sys.setenv(LEGACY_FAST = "FALSE")  # force heavy compute

legacy_dir <- file.path("vignettes", "legacy")
rmd_files <- list.files(legacy_dir, pattern = "\\.Rmd$", full.names = TRUE)

cache_dir <- file.path("inst", "extdata", "legacy-precomputed")
log_dir   <- file.path(cache_dir, "logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

failures <- list()

render_one <- function(f) {
  out_log <- file.path(log_dir, paste0(basename(f), ".log"))

  zz <- file(out_log, open = "wt")
  sink(zz, type = "output")
  sink(zz, type = "message")
  on.exit({
    sink(type = "message"); sink(type = "output")
    close(zz)
  }, add = TRUE)

  message("=== Rendering: ", f, " ===")

  rmarkdown::render(
    input = f,
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )

  TRUE
}

for (f in rmd_files) {
  ok <- tryCatch(render_one(f), error = function(e) {
    failures[[f]] <<- conditionMessage(e)
    FALSE
  })
  if (!ok) message("FAILED: ", f)
}

summary_path <- file.path(cache_dir, "precompute-summary.txt")
cat("Legacy precompute summary\n", file = summary_path)
cat("Total files: ", length(rmd_files), "\n", file = summary_path, append = TRUE)
cat("Failed: ", length(failures), "\n\n", file = summary_path, append = TRUE)

if (length(failures)) {
  cat("Failures:\n", file = summary_path, append = TRUE)
  for (nm in names(failures)) {
    cat("- ", nm, "\n  ", failures[[nm]], "\n\n", file = summary_path, append = TRUE)
  }
}

message("Wrote: ", summary_path)

if (length(failures)) {
  stop(length(failures), " legacy articles failed. See: ", summary_path, call. = FALSE)
} else {
  message("All legacy articles rendered successfully.")
}
