# tools/build_docs.R
# Build DPmixGPD documentation into ./docs
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
    docs_dir = "docs",
    pkgdown_subdir = file.path("docs", "pkgdown"),
    build_root = "_build",
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

  root <- rprojroot::find_root(rprojroot::has_file("_quarto.yml"))
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  msg("Project root: ", root)

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

  sync_tree <- function(src, dst, exclude_prefix = character()) {
    src <- normalizePath(src, winslash = "/", mustWork = TRUE)
    if (!dir.exists(dst)) dir.create(dst, recursive = TRUE, showWarnings = FALSE)

    entries <- list.files(src, all.files = TRUE, full.names = TRUE,
                          recursive = TRUE, no.. = TRUE)
    if (!length(entries)) return(invisible(TRUE))

    rel <- substring(entries, nchar(src) + 2L)
    rel_norm <- gsub("\\\\", "/", rel)

    if (length(exclude_prefix)) {
      keep <- rep(TRUE, length(rel_norm))
      for (pfx in exclude_prefix) {
        pfx <- sub("/+$", "", gsub("\\\\", "/", pfx))
        keep <- keep & !(rel_norm == pfx | startsWith(rel_norm, paste0(pfx, "/")))
      }
      entries <- entries[keep]
      rel <- rel[keep]
    }

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

  # ---------------------------------------------------------------------------
  # 4) Paths
  # ---------------------------------------------------------------------------
  docs_abs <- file.path(root, docs_dir)
  pkgdown_abs <- file.path(root, pkgdown_subdir)
  build_abs <- file.path(root, build_root)

  dir.create(docs_abs, recursive = TRUE, showWarnings = FALSE)
  dir.create(build_abs, recursive = TRUE, showWarnings = FALSE)

  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # Quarto staging OUTSIDE repo, SAME DRIVE
  drive_prefix <- substring(root, 1, 3) # "D:/"
  quarto_stage_abs <- file.path(drive_prefix, "_quarto_stage", paste0("dp_", stamp))
  quarto_stage_abs <- normalizePath(quarto_stage_abs, winslash = "/", mustWork = FALSE)

  # Pkgdown staging inside repo
  pkgdown_stage_abs <- file.path(build_abs, paste0("pkgdown_stage_", stamp))

  ensure_clean_dir(quarto_stage_abs)
  ensure_clean_dir(pkgdown_stage_abs)

  on.exit({
    if (!isTRUE(keep_stage)) {
      suppressWarnings(unlink(quarto_stage_abs, recursive = TRUE, force = TRUE))
      suppressWarnings(unlink(pkgdown_stage_abs, recursive = TRUE, force = TRUE))
    } else if (isTRUE(verbose)) {
      message("Keeping stage dirs:\n  ", quarto_stage_abs, "\n  ", pkgdown_stage_abs)
    }
  }, add = TRUE)

  # ---------------------------------------------------------------------------
  # 5) Quarto: PROJECT render -> stage
  # ---------------------------------------------------------------------------
  msg("[1/3] Quarto: rendering project -> stage (outside repo, same drive)")
  run_cmd(quarto_exe, c(
    "render",
    root,
    "--output-dir", quarto_stage_abs
  ))

  # Sync Quarto -> docs (exclude pkgdown)
  msg("[2/3] Sync: Quarto output -> docs/ (excluding pkgdown/)")
  sync_tree(quarto_stage_abs, docs_abs, exclude_prefix = "pkgdown")

  # ---------------------------------------------------------------------------
  # 6) Optional legacy vignette hook
  # ---------------------------------------------------------------------------
  if (isTRUE(render_legacy_vignettes)) {
    msg("[3/3] legacy vignettes hook (docs/start/ ...)")
    # Hook your renderer here if needed.
  } else {
    msg("[3/3] Skipping legacy vignette hook.")
  }

  # ---------------------------------------------------------------------------
  # 7) Pkgdown: stage -> docs/pkgdown
  # ---------------------------------------------------------------------------
  if (isTRUE(build_pkgdown)) {
    msg("Pkgdown: building -> stage (lazy = ", pkgdown_lazy, ")")

    if (!requireNamespace("pkgdown", quietly = TRUE)) {
      stop("Package 'pkgdown' is required but not installed.")
    }

    # Older pkgdown versions don't support dest_dir=.
    # Use override(destination=...) which is widely supported.
    pkgdown::build_site(
      pkg = root,
      override = list(destination = pkgdown_stage_abs),
      lazy = isTRUE(pkgdown_lazy),
      preview = FALSE,
      quiet = !isTRUE(verbose)
    )

    dir.create(pkgdown_abs, recursive = TRUE, showWarnings = FALSE)
    msg("Sync: pkgdown output -> docs/pkgdown/")
    sync_tree(pkgdown_stage_abs, pkgdown_abs)
  } else {
    msg("Skipping pkgdown build.")
  }

  msg("Documentation build complete.")
  invisible(TRUE)
}

if (identical(environment(), globalenv())) {
  build_docs()
}
