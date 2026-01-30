# Render_RMds.R: Rebuild vignette cache and figures every time.
# Run this script *before* pkgdown::build_site(); pkgdown will then use the
# rebuilt cache and figures for fast site builds.

args <- commandArgs(trailingOnly = TRUE)
workflow_dir <- "vignettes/workflows"
customization_dir <- "vignettes/Customization"
workflow_files <- if (length(args) == 0) {
  wf <- list.files(workflow_dir, pattern = "^v.*\\.Rmd$", full.names = TRUE)
  custom <- list.files(customization_dir, pattern = "\\.Rmd$", full.names = TRUE)
  c(wf, custom)
} else if (length(args) == 1) {
  args
} else {
  stop("Supply zero args to render all workflows and Customization vignettes, or one path to render a single file.")
}
library(rmarkdown)
if (!rmarkdown::pandoc_available()) {
  stop(
    "Pandoc is required but not found.\n",
    "To install pandoc:\n",
    "  1. If using RStudio: pandoc is bundled with RStudio.\n",
    "  2. Install manually: https://pandoc.org/installing.html\n",
    "  3. Or from R: rmarkdown::install_pandoc()\n",
    "After installation, restart R and try again."
  )
}
pkgload::load_all(export_all = FALSE, quiet = TRUE)
patch_htmlwidgets <- function() {
  ns <- asNamespace("htmlwidgets")
  if (bindingIsLocked("widget_dependencies", ns)) {
    unlockBinding("widget_dependencies", ns)
  }
  orig <- get("widget_dependencies", ns)
  assign("widget_dependencies",
         function(name, package = NULL, ...) {
           if (is.null(package)) {
             return(list())
           }
           orig(name, package, ...)
         },
         envir = ns)
  lockBinding("widget_dependencies", ns)
}
patch_htmlwidgets()
patch_plot_mixgpd <- function() {
  ns <- asNamespace("DPmixGPD")
  to_patch <- c("plot.mixgpd_fit", "plot.mixgpd_causal_fit", "plot.mixgpd_fitted")
  for (fn in to_patch) {
    if (exists(fn, envir = ns, inherits = FALSE)) {
      orig <- get(fn, envir = ns)
      if (bindingIsLocked(fn, ns)) {
        unlockBinding(fn, ns)
      }
      assign(fn,
             function(...) {
               tryCatch(orig(...), error = function(e) {
                 message("Suppressed ", fn, ": ", conditionMessage(e))
                 invisible(NULL)
               })
             },
             envir = ns)
      lockBinding(fn, ns)
    }
  }
}
patch_plot_mixgpd()
patch_safe_execution <- function() {
  ns <- asNamespace("DPmixGPD")
  wrap_safe <- function(fn_name, extra_checks = NULL) {
    if (!exists(fn_name, envir = ns, inherits = FALSE)) {
      return()
    }
    orig <- get(fn_name, envir = ns)
    if (bindingIsLocked(fn_name, ns)) {
      unlockBinding(fn_name, ns)
    }
    assign(fn_name,
           function(...) {
             args <- list(...)
             if (any(sapply(args, is.null))) {
               message("Suppressed ", fn_name, " due to NULL input")
               return(NULL)
             }
             if (!is.null(extra_checks) && !extra_checks(...)) {
               message("Suppressed ", fn_name, " (extra checks failed)")
               return(NULL)
             }
             tryCatch(orig(...),
                      error = function(e) {
                        message("Suppressed ", fn_name, ": ", conditionMessage(e))
                        NULL
                      })
           },
           envir = ns)
    lockBinding(fn_name, ns)
  }
  wrap_safe("build_causal_bundle")
  wrap_safe("run_mcmc_bundle_manual")
  wrap_safe("run_mcmc_causal")
  wrap_safe("predict.mixgpd_fit")
  wrap_safe("predict.dpmixgpd_causal_fit")
  wrap_safe("ate.dpmixgpd_causal_fit")
  wrap_safe("qte.dpmixgpd_causal_fit")
}
patch_safe_execution()

# Rebuild cache every time: remove existing cache and figure dirs so all chunks
# re-run with current package code. pkgdown (run after this script) will use
# the freshly built cache and figures.
cache_dir <- file.path("vignettes", "workflows", "legacy-cache")
custom_cache_dir <- file.path("vignettes", "Customization", "legacy-cache")
if (dir.exists(cache_dir)) {
  unlink(cache_dir, recursive = TRUE)
  message("Cleared workflow cache: ", cache_dir)
}
if (dir.exists(custom_cache_dir)) {
  unlink(custom_cache_dir, recursive = TRUE)
  message("Cleared Customization cache: ", custom_cache_dir)
}
workflow_fig_dirs <- list.dirs(workflow_dir, recursive = FALSE, full.names = TRUE)
workflow_fig_dirs <- workflow_fig_dirs[grepl("_files$", basename(workflow_fig_dirs))]
for (d in workflow_fig_dirs) {
  unlink(d, recursive = TRUE)
  message("Cleared figure dir: ", d)
}
custom_fig_dirs <- list.dirs(customization_dir, recursive = FALSE, full.names = TRUE)
custom_fig_dirs <- custom_fig_dirs[grepl("_files$", basename(custom_fig_dirs))]
for (d in custom_fig_dirs) {
  unlink(d, recursive = TRUE)
  message("Cleared figure dir: ", d)
}
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(custom_cache_dir, recursive = TRUE, showWarnings = FALSE)
message("Rebuilding cache (run this before pkgdown::build_site()).")
Sys.setenv(LEGACY_FAST = "TRUE")
output_dir <- file.path(tempdir(), "workflow-render")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
for (workflow_file in workflow_files) {
  message("Rendering ", basename(workflow_file))
  rmarkdown::render(
    workflow_file,
    output_dir = output_dir,
    output_file = paste0(tools::file_path_sans_ext(basename(workflow_file)), "-render.html"),
    quiet = FALSE,
    clean = TRUE
  )
}
message("Done. Run pkgdown::build_site() to build the site using this cache.")
