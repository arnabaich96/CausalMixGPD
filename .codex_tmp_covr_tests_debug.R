options(covr.install = c('--no-build-vignettes', '--no-manual'))
Sys.setenv(DPMIXGPD_TEST_LEVEL = 'full', COVERAGE = '1', DPMIXGPD_CI_COVERAGE_ONLY = '1')
install_path <- normalizePath('covr/debug-lib-tests', winslash = '/', mustWork = FALSE)
if (dir.exists(install_path)) unlink(install_path, recursive = TRUE, force = TRUE)
dir.create(install_path, recursive = TRUE, showWarnings = FALSE)
err <- NULL
cov <- tryCatch(
  covr::package_coverage(type = 'tests', quiet = FALSE, pre_clean = TRUE, clean = FALSE, install_path = install_path, line_exclusions = stats::setNames(as.list(rep(Inf, 3)), c('R/1-registry.R', 'R/01-registry.R', 'R/zzz.R'))),
  error = function(e) { err <<- e; NULL }
)
trace_files <- list.files(path = install_path, pattern = '^covr_trace_[^/]+$', full.names = TRUE)
cat('TRACE_COUNT=', length(trace_files), '\n', sep = '')
if (length(trace_files)) {
  info <- file.info(trace_files)
  print(data.frame(file = basename(trace_files), size = info$size, row.names = NULL))
  bad <- character()
  for (f in trace_files) {
    ok <- TRUE
    msg <- ''
    tryCatch({ readRDS(f) }, error = function(e) { ok <<- FALSE; msg <<- conditionMessage(e) })
    cat(basename(f), ' OK=', ok, if (!ok) paste0(' MSG=', msg) else '', '\n', sep = '')
    if (!ok) bad <- c(bad, f)
  }
  cat('BAD_COUNT=', length(bad), '\n', sep = '')
}
if (!is.null(err)) {
  cat('ERROR_CLASS=', paste(class(err), collapse = ','), '\n', sep = '')
  cat('ERROR_MSG=', conditionMessage(err), '\n', sep = '')
  quit(save = 'no', status = 1)
}
cat('OK percent=', covr::percent_coverage(cov), '\n', sep = '')
