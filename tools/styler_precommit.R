skip <- Sys.getenv("DPMIXGPD_SKIP_STYLER", "0")
if (skip == "1") quit(status = 0)

if (!requireNamespace("styler", quietly = TRUE)) {
  message("styler is required for the pre-commit hook. Install with: install.packages('styler')")
  quit(status = 1)
}

files <- tryCatch(
  system("git diff --cached --name-only --diff-filter=ACM", intern = TRUE),
  error = function(e) character(0)
)

if (!length(files)) quit(status = 0)

is_r <- grepl("\\.(R|Rmd|Rnw)$", files, ignore.case = TRUE)
files <- files[is_r]
if (!length(files)) quit(status = 0)

files <- files[file.exists(files)]
if (!length(files)) quit(status = 0)

styler::style_file(files)

status <- system2("git", c("add", files))
if (!identical(status, 0L)) {
  message("Failed to re-add styled files to git index.")
  quit(status = 1)
}
