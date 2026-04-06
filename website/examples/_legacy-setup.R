# Legacy vignette shared setup for precomputed results
# This file is sourced by all legacy vignettes to provide consistent
# precomputed file handling and MCMC output suppression.

if (!requireNamespace("kableExtra", quietly = TRUE)) {
  stop("Please install 'kableExtra' using install.packages('kableExtra') so the legacy workflows can render.")
}
library(kableExtra)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Please install 'ggplot2' using install.packages('ggplot2') so the legacy workflows can render.")
}
library(ggplot2)

if (!requireNamespace("tibble", quietly = TRUE)) {
  stop("Please install 'tibble' using install.packages('tibble') so the legacy workflows can render.")
}
library(tibble)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Please install 'dplyr' using install.packages('dplyr') so the legacy workflows can render.")
}
library(dplyr)

if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Prefer interactive Plotly output in website HTML when available and renderable.
# Some environments can have plotly/htmlwidgets installed but broken for knitting.
.cmgpd_can_use_plotly <- function() {
  if (!requireNamespace("plotly", quietly = TRUE)) return(FALSE)
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) return(FALSE)
  ok <- tryCatch({
    w <- plotly::plot_ly(x = 1, y = 1)
    htmlwidgets::toHTML(w, standalone = FALSE)
    TRUE
  }, error = function(e) FALSE)
  isTRUE(ok)
}
options(CausalMixGPD.plotly = isTRUE(.cmgpd_can_use_plotly()))

# Disable knitr chunk caching for legacy website examples.
# NIMBLE fit objects may contain external pointers that cannot be serialized
# by knitr's cache backend on some systems.
if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_hooks$set(cache = function(options) {
    options$cache <- FALSE
    options
  })
  knitr::opts_chunk$set(cache = FALSE, autodep = FALSE)
}

# Use a per-vignette figure directory so pkgdown can find images (must be under vignettes/).
# Shared legacy-cache/figure-html is gitignored, so pkgdown on CI would miss images.
CACHE_DIR <- file.path("vignettes", "Examples", "legacy-cache")
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
CACHE_DIR_ABS <- normalizePath(CACHE_DIR, winslash = "/", mustWork = FALSE)
input_name <- tryCatch(knitr::current_input(), error = function(e) NULL)
input_file <- tryCatch(knitr::current_input(dir = TRUE), error = function(e) NULL)
if (!is.null(input_file) && nzchar(input_file)) {
  knitr::opts_knit$set(root.dir = dirname(input_file))
}
if (!is.null(input_name) && nzchar(input_name)) {
  base_name <- tools::file_path_sans_ext(basename(input_name))
  fig_path <- paste0(base_name, "_files/figure-html/")
  knitr::opts_chunk$set(fig.path = fig_path)
} else {
  FIG_DIR <- file.path(CACHE_DIR_ABS, "figure-html/")
  if (!dir.exists(FIG_DIR)) dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
  knitr::opts_chunk$set(fig.path = FIG_DIR)
}

# Try multiple possible locations for precomputed files
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
  # Legacy workflow cache location
  candidates[[length(candidates) + 1]] <- file.path("vignettes", "Examples", "legacy-cache")
  candidates[[length(candidates) + 1]] <- file.path(cwd, "vignettes", "Examples", "legacy-cache")
  
  # 4. Try relative paths
  candidates[[length(candidates) + 1]] <- file.path("articles", "legacy-precomputed")
  candidates[[length(candidates) + 1]] <- file.path("vignettes", "cookbook", "articles", "cookbook-precomputed")
  candidates[[length(candidates) + 1]] <- file.path(cwd, "vignettes", "cookbook", "articles", "cookbook-precomputed")
  
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

.precomp_path <- function(tag) file.path(PRECOMP_DIR, paste0(tag, ".rds"))

# Load from cache or fit and save
load_or_fit <- function(tag, expr) {
  path <- .precomp_path(tag)
  
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
  
  # Cache file doesn't exist or couldn't be read - evaluate expression
  # This provides graceful degradation: if cache is missing, run with current MCMC settings
  
  # In FAST mode when cache is missing, skip expression capture to avoid pkgdown subprocess issues
  # Just work with the expression/value as passed (it may already be evaluated or be a promise)
  if (isTRUE(FAST)) {
    # In FAST mode, evaluate expr directly (via force()) instead of reconstructing calls
    # with match.call()/substitute(), which can cause issues in pkgdown subprocesses.
    # The expression may already be evaluated (e.g., run_mcmc_bundle_manual(bundle) was called)
    # or it may be a promise that needs evaluation here.
    out <- tryCatch({
      # Use force() so that promises are evaluated here; if expr is already a value,
      # force() is a no-op and simply returns it.
      result <- force(expr)
      result
    }, error = function(e) {
      err_msg <- conditionMessage(e)
      # Check if this is the specific eval() error that happens in pkgdown
      if (grepl("first argument must be a string", err_msg, ignore.case = TRUE) ||
          grepl("native symbol reference", err_msg, ignore.case = TRUE)) {
        # This suggests the expression itself has an eval() issue
        # In pkgdown context, we should just require precomputed files
        stop("Precomputed file not found for tag '", tag, "'.\n",
             "  Expected path: ", path, "\n",
             "  PRECOMP_DIR: ", PRECOMP_DIR, "\n",
             "  PRECOMP_DIR exists: ", dir.exists(PRECOMP_DIR), "\n",
             "  Current working directory: ", getwd(), "\n",
             "  Expression evaluation failed in pkgdown context.\n",
             "  Please ensure precomputed files exist in: ", PRECOMP_DIR, "\n",
             "  Original error: ", err_msg)
      } else {
        stop("Precomputed file not found for tag '", tag, "' and cannot evaluate expression.\n",
             "  Expected path: ", path, "\n",
             "  PRECOMP_DIR: ", PRECOMP_DIR, "\n",
             "  PRECOMP_DIR exists: ", dir.exists(PRECOMP_DIR), "\n",
             "  Current working directory: ", getwd(), "\n",
             "  Error: ", err_msg, "\n",
             "  Please ensure precomputed files exist in: ", PRECOMP_DIR)
      }
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
  if (!is.null(expr_sub) && is.language(expr_sub)) {
    # Successfully captured expression - evaluate it with output suppressed
    utils::capture.output(
      out <- eval(expr_sub, envir = parent.frame()),
      file = nullfile
    )
  } else {
    # Fallback: expr may already be evaluated, but still try to suppress output
    utils::capture.output(
      out <- expr,
      file = nullfile
    )
  }
  out
}

.resolve_effect_ids <- function(obj, n_id) {
  id <- obj$id %||% NULL
  if (is.null(id) && !is.null(obj$fit)) {
    rn <- rownames(as.matrix(obj$fit))
    if (!is.null(rn) && length(rn) == n_id && any(nzchar(rn))) id <- rn
  }
  if (is.null(id)) id <- seq_len(n_id)
  as.vector(id)
}

# Format CATE output as: id, index, estimate, lower, upper
format_cate_table <- function(obj) {
  fit <- obj$fit %||% numeric(0)
  fit <- as.numeric(fit)
  n_id <- length(fit)
  if (!n_id) return(data.frame(id = numeric(0), index = integer(0), estimate = numeric(0), lower = numeric(0), upper = numeric(0)))

  lower <- obj$lower %||% rep(NA_real_, n_id)
  upper <- obj$upper %||% rep(NA_real_, n_id)
  ids <- .resolve_effect_ids(obj, n_id = n_id)

  data.frame(
    id = ids,
    index = rep.int(1L, n_id),
    estimate = fit,
    lower = as.numeric(lower),
    upper = as.numeric(upper),
    row.names = NULL
  )
}

# Format CQTE output as: id, index, estimate, lower, upper
format_cqte_table <- function(obj) {
  fit <- obj$fit %||% matrix(numeric(0), nrow = 0L, ncol = 0L)
  fit <- as.matrix(fit)
  n_id <- nrow(fit)
  n_idx <- ncol(fit)
  if (!n_id || !n_idx) {
    return(data.frame(id = numeric(0), index = integer(0), estimate = numeric(0), lower = numeric(0), upper = numeric(0)))
  }

  lower <- as.matrix(obj$lower %||% matrix(NA_real_, nrow = n_id, ncol = n_idx))
  upper <- as.matrix(obj$upper %||% matrix(NA_real_, nrow = n_id, ncol = n_idx))
  ids <- .resolve_effect_ids(obj, n_id = n_id)

  data.frame(
    id = rep(ids, each = n_idx),
    index = rep(seq_len(n_idx), times = n_id),
    estimate = as.vector(t(fit)),
    lower = as.vector(t(lower)),
    upper = as.vector(t(upper)),
    row.names = NULL
  )
}

# Render plot outputs defensively in knitr:
# - avoid auto-printing htmlwidgets that fail in website render sessions
# - print ggplot outputs (single or list) explicitly
