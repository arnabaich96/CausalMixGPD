# run_all.R
# Sources all 4 replication scripts in sequence.
# Saves each script's R environment to cache/<script>.rds
# and wall-clock times (in minutes) to timing.csv.
#
# Directory-independent: locates all files relative to this script's
# own location, so it can be run from any working directory.

# Resolve this script's directory (works with Rscript and source())
.get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  flag <- args[startsWith(args, "--file=")]
  if (length(flag) > 0)
    return(dirname(normalizePath(sub("--file=", "", flag[1]))))
  for (i in seq_along(sys.frames())) {
    f <- sys.frame(i)$ofile
    if (!is.null(f)) return(dirname(normalizePath(f)))
  }
  getwd()
}
script_dir <- .get_script_dir()
rm(.get_script_dir)

cache_dir <- file.path(script_dir, "cache")
dir.create(cache_dir, showWarnings = FALSE)

scripts <- c(
  "overview_cluster.R",
  "overview_causal.R",
  "data_analysis_cluster.R",
  "data_analysis_causal.R"
)

timing <- data.frame(script = scripts, time_min = NA_real_)

for (i in seq_along(scripts)) {
  cat(sprintf("\n[run_all] Starting %s\n", scripts[i]))
  env <- new.env(parent = globalenv())
  t0  <- proc.time()[["elapsed"]]
  source(file.path(script_dir, "Rscripts", scripts[i]), local = env)
  timing$time_min[i] <- round((proc.time()[["elapsed"]] - t0) / 60, 2)
  saveRDS(
    as.list(env),
    file.path(cache_dir, sub("\\.R$", ".rds", scripts[i]))
  )
  cat(sprintf("[run_all] Done %s — %.2f min\n",
              scripts[i], timing$time_min[i]))
}

write.csv(timing, file.path(script_dir, "timing.csv"), row.names = FALSE)
cat("\nAll scripts complete. Timing saved to timing.csv\n")
print(timing)
