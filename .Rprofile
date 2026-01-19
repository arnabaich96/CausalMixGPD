source("renv/activate.R")

if (interactive()) {
	# Keep temp files off C: during local dev
	tmp_root <- file.path(getwd(), ".tmp")
	if (!dir.exists(tmp_root)) dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)
	tmp_root <- normalizePath(tmp_root, winslash = "/", mustWork = FALSE)
	Sys.setenv(TMPDIR = tmp_root, TEMP = tmp_root, TMP = tmp_root)

	# Skip vignettes/manuals during dev checks for speed/stability
	options(
		devtools.check.args = c("--no-build-vignettes", "--no-manual"),
		devtools.check.build_args = c("--no-build-vignettes", "--no-manual"),
		roxygen2.load = "source"
	)
}
