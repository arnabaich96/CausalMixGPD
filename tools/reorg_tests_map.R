# tools/reorg_tests_map.R
#
# Map + move testthat files from the current flat layout to the new layout:
#   tests/testthat/{unit,coverage,integration,ci}/
#
# Usage (from package root):
#   source("tools/reorg_tests_map.R")                 # dry-run (prints plan)
#   reorg_testthat(dry_run = FALSE, use_git = TRUE)   # execute moves (git mv)
#
# Notes:
# - This script is written for the current CausalMixGPD_0.2.0 tarball layout.
# - It only moves files; it does NOT rewrite tests/testthat.R routing.
# - You can edit the MAP below if you want different categorization.

reorg_testthat <- function(root = ".", dry_run = TRUE, use_git = TRUE) {

  old_base <- file.path(root, "tests", "testthat")

  # ---------
  # 1) MAPPING
  # ---------
  # Keys are OLD filenames (relative to tests/testthat/)
  # Values are NEW relative paths (still under tests/testthat/)
  MAP <- c(
    # helpers
    "helper-test-levels.R"           = "helper-00-levels.R",
    "helper-fixtures.R"              = "helper-01-fixtures.R",
    "helper-cache.R"                 = "helper-02-cache.R",
    "helper-predict-distribution.R"  = "helper-03-predict-helpers.R",
    "setup.R"                        = "setup.R",

    # unit (fast / deterministic)
    "test-distributions.R"           = file.path("unit", "test-distributions.R"),
    "test-kernels.R"                 = file.path("unit", "test-kernels.R"),
    "test-internal-utils.R"          = file.path("unit", "test-internal-utils.R"),
    "test-hpd-intervals.R"           = file.path("unit", "test-hpd-intervals.R"),
    "test-vectorization.R"           = file.path("unit", "test-vectorization.R"),
    "test-vectorization-contract.R"  = file.path("unit", "test-vectorization-contract.R"),
    "test-theory.R"                  = file.path("unit", "test-theory.R"),
    "test-glue-validity.R"           = file.path("unit", "test-glue-validity.R"),
    "test-dispatch-separation.R"     = file.path("unit", "test-dispatch-separation.R"),
    "test-global-contracts.R"        = file.path("unit", "test-global-contracts.R"),
    "test-s3-methods.R"              = file.path("unit", "test-s3-methods.R"),
    "test-visualization-helpers.R"   = file.path("unit", "test-visualization-helpers.R"),

    # coverage (minimal, avoid duplicates; this is what covr should run)
    "test-smoke-core-workflows.R"    = file.path("coverage", "test-smoke-core-workflows.R"),
    "test-spliced-backend.R"         = file.path("coverage", "test-spliced-backend.R"),
    "test-causal.R"                  = file.path("coverage", "test-causal.R"),
    "test-predict.R"                 = file.path("coverage", "test-predict.R"),
    "test-ate-rmean.R"               = file.path("coverage", "test-ate-rmean.R"),
    "test-wrapper-bundle-gpd-policy.R" = file.path("coverage", "test-wrapper-bundle-gpd-policy.R"),

    # integration (full CI / heavier / more combinatorial)
    "test-bundle.R"                  = file.path("integration", "test-bundle.R"),
    "test-bundle-validation.R"       = file.path("integration", "test-bundle-validation.R"),
    "test-simulated-data.R"          = file.path("integration", "test-simulated-data.R"),
    "test-pit-residuals.R"           = file.path("integration", "test-pit-residuals.R"),
    "test-fitted.R"                  = file.path("integration", "test-fitted.R"),
    "test-predict-rmean.R"           = file.path("integration", "test-predict-rmean.R"),

    # ci-only gates (ideally move OUT of package later; kept here for now)
    "test-vignette-coverage.R"       = file.path("ci", "test-vignette-coverage.R"),
    "test-site-map-tools.R"          = file.path("ci", "test-site-map-tools.R")
  )

  # -------------------
  # 2) SANITY CHECKS
  # -------------------
  old_files <- names(MAP)
  old_paths <- file.path(old_base, old_files)
  missing <- old_files[!file.exists(old_paths)]
  if (length(missing)) {
    warning("These mapped files do not exist under tests/testthat/: ",
            paste(missing, collapse = ", "))
  }

  dup_new <- names(which(table(unname(MAP)) > 1))
  if (length(dup_new)) {
    stop("Duplicate NEW targets in MAP: ", paste(dup_new, collapse = ", "))
  }

  # -----------------------------
  # 3) CREATE DEST DIRS (new tree)
  # -----------------------------
  dest_dirs <- unique(dirname(unname(MAP)))
  dest_dirs <- dest_dirs[dest_dirs != "."]
  for (d in dest_dirs) {
    dir.create(file.path(old_base, d), recursive = TRUE, showWarnings = FALSE)
  }

  # --------------------
  # 4) MOVE (or dry run)
  # --------------------
  move_one <- function(from, to) {
    # Skip if source and destination are identical
    if (normalizePath(from, mustWork = FALSE) == normalizePath(to, mustWork = FALSE)) {
      if (dry_run) {
        cat("[SKIP] ", from, " (no change)\n", sep = "")
      }
      return(invisible(NULL))
    }
    
    if (use_git && nzchar(Sys.which("git"))) {
      cmd <- sprintf('git mv "%s" "%s"', from, to)
      if (dry_run) {
        cat("[DRY] ", cmd, "\n", sep = "")
      } else {
        status <- system(cmd)
        if (status != 0) stop("git mv failed: ", cmd)
      }
    } else {
      if (dry_run) {
        cat("[DRY] file.rename(", from, " -> ", to, ")\n", sep = "")
      } else {
        ok <- file.rename(from, to)
        if (!ok) stop("file.rename failed: ", from, " -> ", to)
      }
    }
  }

  plan <- data.frame(
    old = file.path("tests/testthat", old_files),
    new = file.path("tests/testthat", unname(MAP)),
    stringsAsFactors = FALSE
  )

  cat("\n--- Planned moves (old -> new) ---\n")
  print(plan, row.names = FALSE)

  cat("\n--- Executing moves ---\n")
  for (i in seq_len(nrow(plan))) {
    from <- file.path(root, plan$old[i])
    to   <- file.path(root, plan$new[i])
    if (file.exists(from)) move_one(from, to)
  }

  invisible(plan)
}

# Default behavior: print plan only
if (sys.nframe() == 0L) {
  reorg_testthat(dry_run = TRUE, use_git = TRUE)
}
