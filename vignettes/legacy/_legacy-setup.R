# Legacy vignette shared setup for precomputed results
# This file is sourced by all legacy vignettes to provide consistent
# precomputed file handling and MCMC output suppression.

# Disable knitr caching in non-interactive renders to avoid serializing
# NIMBLE objects with external pointers (which can break pkgdown builds).
if (!interactive() && requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_chunk$set(cache = FALSE)
}

# Ensure ggplot2 functions are available in legacy vignettes without
# requiring each file to attach the package explicitly.
if (requireNamespace("ggplot2", quietly = TRUE)) {
  suppressPackageStartupMessages(library(ggplot2))
}

# kableExtra helpers are used in several legacy vignettes.
if (requireNamespace("kableExtra", quietly = TRUE)) {
  suppressPackageStartupMessages(library(kableExtra))
}

# tibble is used directly in a few legacy vignettes.
if (requireNamespace("tibble", quietly = TRUE)) {
  suppressPackageStartupMessages(library(tibble))
}

# gridExtra provides grid.arrange used in some legacy plots.
if (requireNamespace("gridExtra", quietly = TRUE)) {
  suppressPackageStartupMessages(library(gridExtra))
}

# dplyr verbs are used in a few legacy tables.
if (requireNamespace("dplyr", quietly = TRUE)) {
  suppressPackageStartupMessages(library(dplyr))
}

# Try multiple possible locations for precomputed files
.find_pkg_root <- function(start_path = getwd(), max_up = 6) {
  path <- normalizePath(start_path, winslash = "/", mustWork = FALSE)
  for (i in 0:max_up) {
    desc <- file.path(path, "DESCRIPTION")
    if (file.exists(desc)) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) break
    path <- parent
  }
  NULL
}

.find_precomp_dir <- function() {
  # Hardcode the known location of precomputed files
  # They are in vignettes/legacy/articles/legacy-precomputed relative to package root
  known_path <- file.path("vignettes", "legacy", "articles", "legacy-precomputed")
  
  # Try relative to vignette location first (for knitr context)
  vignette_dir <- tryCatch({
    input_file <- knitr::current_input(dir = TRUE)
    if (!is.null(input_file) && nzchar(input_file)) {
      dirname(input_file)
    } else {
      NULL
    }
  }, error = function(e) NULL)
  
  # Build list of candidate paths to check (in order of preference)
  candidates <- list()
  
  # 1. Relative to vignette directory (most reliable in knitr context)
  if (!is.null(vignette_dir)) {
    candidates[[length(candidates) + 1]] <- file.path(vignette_dir, "articles", "legacy-precomputed")
  }
  
  # 2. Known standard location (most common)
  candidates[[length(candidates) + 1]] <- known_path
  
  # 3. Absolute from current working directory
  cwd <- getwd()
  candidates[[length(candidates) + 1]] <- file.path(cwd, known_path)
  candidates[[length(candidates) + 1]] <- file.path(cwd, "articles", "legacy-precomputed")

  # 4. From detected package root (handles pkgdown/builds in subdirs)
  root_from_cwd <- .find_pkg_root(cwd)
  if (!is.null(root_from_cwd)) {
    candidates[[length(candidates) + 1]] <- file.path(root_from_cwd, known_path)
  }
  if (!is.null(vignette_dir)) {
    root_from_vignette <- .find_pkg_root(vignette_dir)
    if (!is.null(root_from_vignette)) {
      candidates[[length(candidates) + 1]] <- file.path(root_from_vignette, known_path)
    }
  }
  
  # 5. Try relative paths
  candidates[[length(candidates) + 1]] <- file.path("articles", "legacy-precomputed")
  
  # Check each candidate
  for (cand in candidates) {
    if (!is.null(cand) && dir.exists(cand)) {
      return(cand)
    }
  }
  
  # Default fallback - will be created if needed
  return(known_path)
}

# Initialize precomputed directory
PRECOMP_DIR <- .find_precomp_dir()
if (!dir.exists(PRECOMP_DIR)) dir.create(PRECOMP_DIR, recursive = TRUE, showWarnings = FALSE)

.resolve_precomp_path <- function(tag) {
  filename <- paste0(tag, ".rds")
  candidates <- list(file.path(PRECOMP_DIR, filename))

  # Try from current working directory
  candidates[[length(candidates) + 1]] <- file.path(getwd(), "vignettes", "legacy", "articles", "legacy-precomputed", filename)

  # Try from package root (if detectable)
  root_from_cwd <- .find_pkg_root(getwd())
  if (!is.null(root_from_cwd)) {
    candidates[[length(candidates) + 1]] <- file.path(root_from_cwd, "vignettes", "legacy", "articles", "legacy-precomputed", filename)
  }

  # Try from current vignette directory (if available)
  vignette_dir <- tryCatch(knitr::current_input(dir = TRUE), error = function(e) NULL)
  if (!is.null(vignette_dir) && nzchar(vignette_dir)) {
    candidates[[length(candidates) + 1]] <- file.path(vignette_dir, "articles", "legacy-precomputed", filename)
    root_from_vignette <- .find_pkg_root(vignette_dir)
    if (!is.null(root_from_vignette)) {
      candidates[[length(candidates) + 1]] <- file.path(root_from_vignette, "vignettes", "legacy", "articles", "legacy-precomputed", filename)
    }
  }

  for (cand in unique(candidates)) {
    if (!is.null(cand) && file.exists(cand)) {
      return(cand)
    }
  }
  # Fallback to default location
  file.path(PRECOMP_DIR, filename)
}

.precomp_path <- function(tag) .resolve_precomp_path(tag)

# Load from cache or fit and save
load_or_fit <- function(tag, expr) {
  path <- .precomp_path(tag)
  
  # Detect pkgdown build context (pkgdown runs in a subprocess via callr)
  # Check for callr parent process or pkgdown-specific environment
  is_pkgdown <- any(grepl("pkgdown|callr", Sys.getenv(), ignore.case = TRUE)) ||
                exists(".__PKGDOWN__", envir = .GlobalEnv, inherits = FALSE) ||
                any(grepl("pkgdown", commandArgs(), ignore.case = TRUE))
  
  # Try to load from cache first (only if in FAST mode)
  if (isTRUE(FAST) && file.exists(path)) {
    tryCatch({
      cached <- readRDS(path)
      return(cached)
    }, error = function(e) {
      # If reading fails, fall through to evaluate expression
      warning("Could not read precomputed file for tag '", tag, "' at: ", path, 
              " - will recompute. Error: ", conditionMessage(e))
    })
  }
  
  # In pkgdown builds with FAST mode, require precomputed files
  # to avoid evaluation issues in subprocess context
  if (isTRUE(FAST) && is_pkgdown && !file.exists(path)) {
    stop("Precomputed file not found for tag '", tag, "' in pkgdown build.\n",
         "  Expected path: ", path, "\n",
         "  PRECOMP_DIR: ", PRECOMP_DIR, "\n",
         "  PRECOMP_DIR exists: ", dir.exists(PRECOMP_DIR), "\n",
         "  Current working directory: ", getwd(), "\n",
         "  Please generate precomputed files by running:\n",
         "    devtools::build_vignettes()\n",
         "  or ensure precomputed files exist in: ", PRECOMP_DIR)
  }
  
  # Cache file doesn't exist or couldn't be read - evaluate expression
  # This provides graceful degradation: if cache is missing, run with current MCMC settings
  
  # In FAST mode when cache is missing, the expression may already be evaluated
  # (e.g., run_mcmc_bundle_manual(bundle) was called before being passed as argument)
  # or it may be a promise. We use force() to handle both cases.
  # Note: In pkgdown subprocess, some expressions may fail to evaluate due to
  # environment issues, so we provide helpful error messages.
  if (isTRUE(FAST)) {
    # In FAST mode, use expr directly to avoid match.call()/substitute() issues
    # The expression is evaluated when passed as argument, so expr is already the result
    # or it's a promise that force() will evaluate
    out <- tryCatch({
      # force() will evaluate if it's a promise, or return if already evaluated
      force(expr)
    }, error = function(e) {
      err_msg <- conditionMessage(e)
      # Provide helpful error message for pkgdown builds
      stop("Precomputed file not found for tag '", tag, "' and expression evaluation failed.\n",
           "  This often happens in pkgdown builds when precomputed files are missing.\n",
           "  Expected path: ", path, "\n",
           "  PRECOMP_DIR: ", PRECOMP_DIR, "\n",
           "  PRECOMP_DIR exists: ", dir.exists(PRECOMP_DIR), "\n",
           "  Current working directory: ", getwd(), "\n",
           "  Error: ", err_msg, "\n",
           "  To fix: Generate precomputed files by running vignettes outside pkgdown,\n",
           "  or ensure precomputed files exist in: ", PRECOMP_DIR)
    })
  } else {
    # In non-FAST mode, try to capture unevaluated expression for proper re-evaluation
    expr_sub <- NULL
    
    # Try match.call() first (more reliable in some contexts)
    mc <- tryCatch(match.call(), error = function(e) NULL)
    if (!is.null(mc)) {
      expr_sub <- mc$expr
    }
    
    # If match.call() didn't work, try substitute()
    if (is.null(expr_sub) || !is.language(expr_sub)) {
      expr_sub <- tryCatch(substitute(expr), error = function(e) NULL)
    }
    
    # Evaluate the expression
    if (!is.null(expr_sub) && is.language(expr_sub) && 
        (is.call(expr_sub) || is.expression(expr_sub) || is.name(expr_sub))) {
      # Successfully captured unevaluated expression - evaluate it
      out <- tryCatch({
        eval(expr_sub, envir = parent.frame())
      }, error = function(e) {
        # If evaluation fails, fall back to using expr directly
        force(expr)
      })
    } else {
      # Could not capture unevaluated expression - use expr directly
      out <- tryCatch({
        force(expr)
      }, error = function(e) {
        stop("Could not evaluate expression for tag '", tag, "'. ",
             "Error: ", conditionMessage(e))
      })
    }
  }
  
  # Save the result for future use (if path is valid)
  if (dir.exists(dirname(path))) {
    try(saveRDS(out, path), silent = TRUE)
  }
  out
}

# Suppress MCMC output during evaluation
quiet_mcmc <- function(expr) {
  nullfile <- if (.Platform$OS.type == 'windows') 'NUL' else '/dev/null'
  # Capture unevaluated expression to suppress output during evaluation
  # Use match.call() first (more reliable), then substitute() as fallback
  # Note: This function is typically only used in non-FAST mode (not in pkgdown builds)
  expr_sub <- NULL
  
  # Try match.call() first
  mc <- tryCatch(match.call(), error = function(e) NULL)
  if (!is.null(mc)) {
    expr_sub <- mc$expr
  }
  
  # If match.call() didn't work, try substitute()
  if (is.null(expr_sub) || !is.language(expr_sub)) {
    expr_sub <- tryCatch(substitute(expr), error = function(e) NULL)
  }
  
  # Evaluate the expression with output suppression
  # Be defensive - if expression capture fails or eval fails, fall back gracefully
  if (!is.null(expr_sub) && is.language(expr_sub) && 
      (is.call(expr_sub) || is.expression(expr_sub) || is.name(expr_sub))) {
    # Successfully captured expression - try to evaluate it with output suppressed
    out <- tryCatch({
      utils::capture.output(
        result <- eval(expr_sub, envir = parent.frame()),
        file = nullfile
      )
      result
    }, error = function(e) {
      # If eval fails (e.g., in pkgdown subprocess), fall back to using expr directly
      # This handles cases where match.call() returns invalid objects
      err_msg <- conditionMessage(e)
      if (grepl("first argument must be a string", err_msg, ignore.case = TRUE) ||
          grepl("native symbol reference", err_msg, ignore.case = TRUE)) {
        # This is the pkgdown subprocess eval() error - use expr directly
        utils::capture.output(
          out <- force(expr),
          file = nullfile
        )
        out
      } else {
        # For other errors, still try to suppress output on expr
        utils::capture.output(
          out <- force(expr),
          file = nullfile
        )
        out
      }
    })
  } else {
    # Fallback: expr may already be evaluated, but still try to suppress output
    utils::capture.output(
      out <- force(expr),
      file = nullfile
    )
  }
  out
}
