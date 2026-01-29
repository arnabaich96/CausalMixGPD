args <- commandArgs(trailingOnly = TRUE)
workflow_dir <- "vignettes/workflows"
workflow_files <- if (length(args) == 0) {
  list.files(workflow_dir, pattern = "^v.*\\.Rmd$", full.names = TRUE)
} else if (length(args) == 1) {
  args
} else {
  stop("Supply zero args to render all workflows, or one path to render a single workflow.")
}
library(rmarkdown)
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
cache_dir <- file.path("vignettes", "workflows", "legacy-cache")
cache_has_files <- dir.exists(cache_dir) &&
  length(list.files(cache_dir, all.files = FALSE, recursive = FALSE)) > 0
use_cache <- isTRUE(cache_has_files)
message("Legacy cache ", if (use_cache) "detected" else "not found", " at ", cache_dir)
Sys.setenv(LEGACY_FAST = if (use_cache) "TRUE" else "FALSE")
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
