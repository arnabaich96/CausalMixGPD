# tools/build_docs.R
# Build CausalMixGPD documentation into ./docs
#
# - Quarto: render PROJECT -> <drive>/_quarto_stage/dp_<timestamp> (outside repo, same drive)
#           then sync to docs/ (excluding docs/pkgdown)
# - Pkgdown: build -> _build/pkgdown_stage_<timestamp> via override(destination=...)
#            then sync to docs/pkgdown
# - Uses rprojroot to locate project root robustly

build_docs <- function(
    render_legacy_vignettes = TRUE,
    build_pkgdown = TRUE,
    pkgdown_lazy = TRUE,
    quarto_targets = NULL,
    quarto_incremental = TRUE,
    docs_dir = "docs",
    pkgdown_dir = "pkgdown",
    quarto_dir = "quarto",
    keep_stage = FALSE,
    verbose = TRUE
) {
  msg <- function(...) if (isTRUE(verbose)) message(...)

  # ---------------------------------------------------------------------------
  # 1) Locate project root via rprojroot
  # ---------------------------------------------------------------------------
  if (!requireNamespace("rprojroot", quietly = TRUE)) {
    stop("Package 'rprojroot' is required but not installed.")
  }

  root <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  msg("Project root: ", root)

  quarto_project <- file.path(root, "website")
  if (!file.exists(file.path(quarto_project, "_quarto.yml"))) {
    stop("Quarto project not found at ", quarto_project)
  }


  # ---------------------------------------------------------------------------
  # 2) Locate Quarto CLI early
  # ---------------------------------------------------------------------------
  quarto_exe <- Sys.which("quarto")
  if (identical(quarto_exe, "")) stop("Quarto CLI not found on PATH.")

  # ---------------------------------------------------------------------------
  # 3) Helpers
  # ---------------------------------------------------------------------------
  ensure_clean_dir <- function(path) {
    if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  clean_docs_root <- function(path, preserve_top = character()) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      return(invisible(TRUE))
    }

    preserve_top <- unique(c(preserve_top, ".git", ".gitignore"))
    entries <- list.files(path, all.files = TRUE, full.names = TRUE, no.. = TRUE)
    if (!length(entries)) return(invisible(TRUE))

    for (entry in entries) {
      if (basename(entry) %in% preserve_top) next
      unlink(entry, recursive = TRUE, force = TRUE)
    }
    invisible(TRUE)
  }

  clean_forbidden_website_outputs <- function(project_dir) {
    forbidden <- c("docs", "_site", "quarto")
    for (name in forbidden) {
      candidate <- file.path(project_dir, name)
      if (dir.exists(candidate)) {
        msg("Removing forbidden in-source output: ", candidate)
        unlink(candidate, recursive = TRUE, force = TRUE)
      }
      if (dir.exists(candidate)) {
        stop("Forbidden in-source output still exists: ", candidate)
      }
    }
    invisible(TRUE)
  }

  sync_tree <- function(src, dst, exclude_prefix = character()) {
    src <- normalizePath(src, winslash = "/", mustWork = TRUE)
    if (!dir.exists(dst)) dir.create(dst, recursive = TRUE, showWarnings = FALSE)

    entries <- list.files(src, all.files = TRUE, full.names = TRUE,
                          recursive = TRUE, no.. = TRUE)
    if (!length(entries)) return(invisible(TRUE))

    rel <- substring(entries, nchar(src) + 2L)
    rel_norm <- gsub("\\\\", "/", rel)

    # Exclude cache folders and RDB/RDX files
    cache_patterns <- c(".quarto", "project-cache", "xref", "__pycache__")
    file_patterns <- c("\\.rdb$", "\\.Rdb$", "\\.RDB$", "\\.rdx$", "\\.Rdx$", "\\.RDX$")

    keep <- rep(TRUE, length(rel_norm))

    # Exclude cache folders
    for (pfx in cache_patterns) {
      keep <- keep & !(rel_norm == pfx | startsWith(rel_norm, paste0(pfx, "/")))
    }

    # Exclude cache prefixes passed as arguments
    if (length(exclude_prefix)) {
      for (pfx in exclude_prefix) {
        pfx <- sub("/+$", "", gsub("\\\\", "/", pfx))
        keep <- keep & !(rel_norm == pfx | startsWith(rel_norm, paste0(pfx, "/")))
      }
    }

    # Exclude RDB/RDX files
    for (pat in file_patterns) {
      keep <- keep & !grepl(pat, rel_norm)
    }

    entries <- entries[keep]
    rel <- rel[keep]

    is_dir <- vapply(entries, dir.exists, logical(1))
    if (any(is_dir)) {
      for (d in file.path(dst, rel[is_dir])) {
        dir.create(d, recursive = TRUE, showWarnings = FALSE)
      }
    }

    files <- entries[!is_dir]
    files_rel <- rel[!is_dir]
    for (i in seq_along(files)) {
      from <- files[i]
      to <- file.path(dst, files_rel[i])
      dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
      ok <- file.copy(from, to, overwrite = TRUE,
                      copy.mode = TRUE, copy.date = TRUE)
      if (!isTRUE(ok)) stop("Failed to copy: ", from)
    }

    invisible(TRUE)
  }

  run_cmd <- function(bin, args) {
    if (isTRUE(verbose)) {
      status <- system2(bin, args)
      if (!identical(as.integer(status), 0L)) {
        stop("Command failed: ", bin, " ", paste(args, collapse = " "))
      }
    } else {
      out <- system2(bin, args, stdout = TRUE, stderr = TRUE)
      status <- attr(out, "status")
      if (is.null(status)) status <- 0L
      if (!identical(as.integer(status), 0L)) {
        cat(paste(out, collapse = "\n"), "\n")
        stop("Command failed.")
      }
    }
    invisible(TRUE)
  }

  normalize_target <- function(x) {
    x <- gsub("\\\\", "/", x)
    x <- sub("^\\./", "", x)
    x <- sub("^website/", "", x)
    x
  }

  discover_quarto_targets <- function(root) {
    out <- tryCatch(
      system2("git", c("-C", root, "status", "--porcelain", "--", "website"), stdout = TRUE, stderr = TRUE),
      error = function(e) character(0)
    )
    if (!length(out)) return(character(0))

    changed <- trimws(sub("^..\\s+", "", out))
    changed <- changed[nzchar(changed)]
    changed <- gsub("\\\\", "/", changed)

    qmd <- changed[grepl("^website/.+\\.qmd$", changed)]
    normalize_target(qmd)
  }

  # ---------------------------------------------------------------------------
  # 4) Paths
  # ---------------------------------------------------------------------------
  docs_abs <- file.path(root, docs_dir)
  pkgdown_abs <- file.path(root, pkgdown_dir)
  pkgdown_docs_abs <- file.path(root, docs_dir, "pkgdown")
  quarto_abs <- file.path(root, quarto_dir)

  dir.create(docs_abs, recursive = TRUE, showWarnings = FALSE)
  dir.create(pkgdown_abs, recursive = TRUE, showWarnings = FALSE)
  dir.create(quarto_abs, recursive = TRUE, showWarnings = FALSE)
  clean_forbidden_website_outputs(quarto_project)

  # ---------------------------------------------------------------------------
  # 5) Quarto render (project render only; avoid per-file output-dir)
  # ---------------------------------------------------------------------------
  pkg_version <- read.dcf(file.path(root, "DESCRIPTION"), "Version")[1]
  build_date  <- format(Sys.Date(), "%Y-%m-%d")

  explicit_targets <- normalize_target(if (is.null(quarto_targets)) character(0) else quarto_targets)
  auto_targets <- if (isTRUE(quarto_incremental) && !length(explicit_targets)) {
    discover_quarto_targets(root)
  } else {
    character(0)
  }
  render_targets <- unique(c(explicit_targets, auto_targets))
  has_targets <- length(render_targets) > 0
  if (has_targets) {
    msg("[1/3] Quarto: targets detected (", length(render_targets), "), rendering full project")
  } else {
    msg("[1/3] Quarto: full project render -> quarto/ in root")
  }

  ensure_clean_dir(quarto_abs)
  run_cmd(quarto_exe, c(
    "render",
    quarto_project,
    "--cache-refresh",
    "--output-dir", quarto_abs,
    "--metadata", paste0("version:", pkg_version),
    "--metadata", paste0("updated:", build_date)
  ))

  msg("Quarto render complete.")

  # Copy CSS files manually to bypass permission issues
  website_dir <- file.path(root, "website")
  for (css_file in c("styles.css", "theme.css")) {
    src_file <- file.path(website_dir, css_file)
    dst_file <- file.path(quarto_abs, css_file)
    if (file.exists(src_file)) {
      file.copy(src_file, dst_file, overwrite = TRUE, copy.mode = FALSE)
      msg("Copied CSS file: ", css_file)
    }
  }

  # Sync Quarto -> docs (excluding pkgdown)
  msg("[2/3] Sync: Quarto output (quarto/) -> docs/ (excluding pkgdown/)")
  clean_docs_root(docs_abs, preserve_top = "pkgdown")
  sync_tree(quarto_abs, docs_abs, exclude_prefix = "pkgdown")

  # ---------------------------------------------------------------------------
  # 6) Optional legacy vignette hook
  # ---------------------------------------------------------------------------
  if (isTRUE(render_legacy_vignettes)) {
    msg("[3/3] legacy vignettes hook (docs/start/ ...)")
    # Hook your renderer here if needed.
  } else {
    msg("[3/3] Skipping legacy vignette hook.")
  }

  # Pkgdown: build to pkgdown/ in root, then copy to docs/pkgdown/
  # Lazy rendering requires the cache to persist in the output directory
  if (isTRUE(build_pkgdown)) {
    msg("[4/4] Pkgdown: building to pkgdown/ (lazy = ", pkgdown_lazy, ")")

    if (!requireNamespace("pkgdown", quietly = TRUE)) {
      stop("Package 'pkgdown' is required but not installed.")
    }

    # Initialize site if needed
    cwd <- getwd()
    on.exit(setwd(cwd), add = TRUE)
    setwd(root)

    msg("Initializing pkgdown site...")
    tryCatch({
      pkgdown::init_site(pkg = root)
    }, error = function(e) {
      msg("Note: init_site returned: ", conditionMessage(e))
    })

    # Build to pkgdown/ in root - this preserves cache for lazy builds
    pkgdown::build_site(
      pkg = root,
      override = list(destination = pkgdown_abs),
      lazy = isTRUE(pkgdown_lazy),
      devel = FALSE,
      preview = FALSE
    )

    # Copy pkgdown/ to docs/pkgdown/ (overwrite)
    msg("[4/4] Sync: pkgdown/ -> docs/pkgdown/")
    pkgdown_docs_abs <- file.path(root, docs_dir, "pkgdown")
    ensure_clean_dir(pkgdown_docs_abs)
    sync_tree(pkgdown_abs, pkgdown_docs_abs)
  } else {
    msg("Skipping pkgdown build.")
  }

  msg("Documentation build complete.")
  invisible(TRUE)
}

if (identical(environment(), globalenv())) {
  targets_env <- Sys.getenv("QUARTO_TARGETS", unset = "")
  targets <- if (nzchar(targets_env)) {
    trimws(strsplit(targets_env, ",", fixed = TRUE)[[1]])
  } else {
    NULL
  }
  build_docs(quarto_targets = targets)
}
