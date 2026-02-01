#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# Ensure working directory is repo root (supports source() and Rscript)
get_script_path <- function() {
  # When sourced: sys.frame(1)$ofile
  if (!is.null(sys.frame(1)$ofile)) return(normalizePath(sys.frame(1)$ofile))

  # When run via Rscript --file=...
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(sub("^--file=", "", file_arg[1])))

  return(NULL)
}

script_path <- get_script_path()
if (!is.null(script_path)) {
  repo_root <- normalizePath(file.path(dirname(script_path), ".."))
  if (dir.exists(repo_root)) setwd(repo_root)
}

has_flag <- function(x) any(args == x)
flag_val <- function(prefix) {
  # Supports: --quarto=site/index.html
  hit <- grep(paste0("^", prefix, "="), args, value = TRUE)
  if (length(hit) == 0) return(NULL)
  sub(paste0("^", prefix, "="), "", hit[1])
}

do_pkgdown <- !has_flag("--no-pkgdown")  # Enabled by default; use --no-pkgdown to disable
do_quarto  <- FALSE  # Disabled; Quarto pages are static in site/ folder

# pkgdown modes:
# default: fast (home + reference only)
# --full: full pkgdown site (articles too)
pkgdown_full <- has_flag("--full")

# pkgdown cleaning:
# --clean-pkgdown: run pkgdown::clean_site(force=TRUE) before build
clean_pkgdown <- has_flag("--clean-pkgdown")

# docs cleaning:
# --clean-docs: delete docs/ entirely before any build (use rarely)
clean_docs <- has_flag("--clean-docs")

# Quarto target:
# --quarto=PATH renders only that file (fast)
# e.g., --quarto=site/index.html or --quarto=site/roadmap/index.html
quarto_target <- flag_val("--quarto")

# If pkgdown output is missing, force a pkgdown build even without --build-pkgdown
if (!do_pkgdown && do_quarto && !dir.exists("docs/reference")) {
  cat("No existing pkgdown output found; enabling pkgdown build.\n")
  do_pkgdown <- TRUE
}

cache_existing_pkgdown <- function() {
  if (dir.exists("docs/reference")) {
    cat("Caching existing pkgdown reference/ ...\n")
    if (!dir.exists("_build_cache_reference")) dir.create("_build_cache_reference", recursive = TRUE)
    file.copy(list.files("docs/reference", full.names = TRUE), "_build_cache_reference", recursive = TRUE, overwrite = TRUE)
  }
  if (dir.exists("docs/articles")) {
    cat("Caching existing pkgdown articles/ ...\n")
    if (!dir.exists("_build_cache_articles")) dir.create("_build_cache_articles", recursive = TRUE)
    file.copy(list.files("docs/articles", full.names = TRUE), "_build_cache_articles", recursive = TRUE, overwrite = TRUE)
  }
  if (dir.exists("docs/deps")) {
    cat("Caching existing pkgdown deps/ ...\n")
    if (!dir.exists("_build_cache_deps")) dir.create("_build_cache_deps", recursive = TRUE)
    file.copy(list.files("docs/deps", full.names = TRUE), "_build_cache_deps", recursive = TRUE, overwrite = TRUE)
  }
}

copy_tree <- function(src_dir, dest_dir, skip_prefixes = character(0)) {
  if (!dir.exists(src_dir)) return(invisible(FALSE))
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  src_root <- normalizePath(src_dir)
  src_root_norm <- gsub("\\\\", "/", src_root)
  files <- list.files(src_dir, recursive = TRUE, full.names = TRUE)

  for (f in files) {
    f_norm <- gsub("\\\\", "/", normalizePath(f))
    rel_path <- sub(paste0("^", src_root_norm, "/"), "", f_norm)
    if (length(skip_prefixes) > 0 && any(startsWith(rel_path, skip_prefixes))) next
    target <- file.path(dest_dir, rel_path)
    target_dir <- dirname(target)
    if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(f, target, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  }

  invisible(TRUE)
}

restore_cached_pkgdown <- function() {
  if (dir.exists("_build_cache_reference")) {
    cat("Restoring pkgdown reference/ ...\n")
    if (!dir.exists("docs/reference")) dir.create("docs/reference", recursive = TRUE)
    file.copy(list.files("_build_cache_reference", full.names = TRUE), "docs/reference", recursive = TRUE, overwrite = TRUE)
    unlink("_build_cache_reference", recursive = TRUE, force = TRUE)
  }
  if (dir.exists("_build_cache_articles")) {
    cat("Restoring pkgdown articles/ ...\n")
    if (!dir.exists("docs/articles")) dir.create("docs/articles", recursive = TRUE)
    file.copy(list.files("_build_cache_articles", full.names = TRUE), "docs/articles", recursive = TRUE, overwrite = TRUE)
    unlink("_build_cache_articles", recursive = TRUE, force = TRUE)
  }
  if (dir.exists("_build_cache_deps")) {
    cat("Restoring pkgdown deps/ ...\n")
    if (!dir.exists("docs/deps")) dir.create("docs/deps", recursive = TRUE)
    file.copy(list.files("_build_cache_deps", full.names = TRUE), "docs/deps", recursive = TRUE, overwrite = TRUE)
    unlink("_build_cache_deps", recursive = TRUE, force = TRUE)
  }
}

generate_article_index <- function() {
  if (!dir.exists("docs/articles")) return(invisible(FALSE))
  
  cat("\n--- Generating article index ---\n")
  
  articles <- list.files("docs/articles", pattern = "\\.html$", full.names = FALSE)
  articles <- articles[articles != "index.html"]  # Exclude pkgdown's own index
  
  if (length(articles) == 0) {
    cat("No articles found to index.\n")
    return(invisible(FALSE))
  }
  
  # Extract titles from HTML files
  article_info <- lapply(articles, function(art) {
    html_path <- file.path("docs/articles", art)
    lines <- readLines(html_path, warn = FALSE)
    
    # Try to extract title from <title> tag or <h1>
    title_line <- grep("<title>", lines, value = TRUE, ignore.case = TRUE)
    if (length(title_line) > 0) {
      title <- sub(".*<title>\\s*([^<]+)\\s*</title>.*", "\\1", title_line[1], ignore.case = TRUE)
      title <- sub("\\s*•.*$", "", title)  # Remove package name suffix
    } else {
      h1_line <- grep("<h1[^>]*>", lines, value = TRUE, ignore.case = TRUE)
      if (length(h1_line) > 0) {
        title <- sub(".*<h1[^>]*>\\s*([^<]+)\\s*</h1>.*", "\\1", h1_line[1], ignore.case = TRUE)
      } else {
        title <- sub("\\.html$", "", art)
      }
    }
    
    list(file = art, title = title)
  })
  
  # Generate styled HTML page
  html_content <- c(
    '<!DOCTYPE html>',
    '<html lang="en">',
    '<head>',
    '    <meta charset="UTF-8">',
    '    <meta name="viewport" content="width=device-width, initial-scale=1.0">',
    '    <title>Articles - DPmixGPD</title>',
    '    <link rel="stylesheet" href="styles.css">',
    '    <style>',
    '        body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; line-height: 1.6; color: #333; max-width: 900px; margin: 0 auto; padding: 2rem; background: #f5f5f5; }',
    '        h1 { color: #667eea; margin-bottom: 2rem; }',
    '        .article-list { background: white; border-radius: 8px; padding: 2rem; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }',
    '        .article-item { padding: 1rem 0; border-bottom: 1px solid #eee; }',
    '        .article-item:last-child { border-bottom: none; }',
    '        .article-link { color: #667eea; text-decoration: none; font-size: 1.1rem; font-weight: 500; }',
    '        .article-link:hover { text-decoration: underline; }',
    '        .back-link { display: inline-block; margin-bottom: 2rem; color: #667eea; text-decoration: none; }',
    '        .back-link:hover { text-decoration: underline; }',
    '    </style>',
    '</head>',
    '<body>',
    '    <a href="index.html" class="back-link">← Back to Home</a>',
    '    <h1>Package Articles</h1>',
    '    <div class="article-list">'
  )
  
  for (info in article_info) {
    html_content <- c(
      html_content,
      '        <div class="article-item">',
      sprintf('            <a href="articles/%s" class="article-link">%s</a>', info$file, info$title),
      '        </div>'
    )
  }
  
  html_content <- c(
    html_content,
    '    </div>',
    '</body>',
    '</html>'
  )
  
  # Write to docs/articles.html (direct HTML, not qmd)
  writeLines(html_content, "docs/articles.html")
  cat("Article index generated at docs/articles.html\n")
  
  invisible(TRUE)
}

cat("\n=== DPmixGPD website build ===\n")
cat("pkgdown:", if (do_pkgdown) (if (pkgdown_full) "FULL" else "FAST (home+reference)") else "SKIP (use --build-pkgdown to enable)", "\n")
cat("quarto :", if (do_quarto) (if (!is.null(quarto_target)) paste0("ONE PAGE (", quarto_target, ")") else "PROJECT") else "SKIP", "\n")
cat("clean  :", paste(
  c(if (clean_docs) "docs" else NULL,
    if (clean_pkgdown) "pkgdown" else NULL),
  collapse = ", "
), if (!clean_docs && !clean_pkgdown) "none", "\n\n")

# Safety checks
if (do_pkgdown && !file.exists("_pkgdown.yml")) stop("Missing _pkgdown.yml at repo root.")
if (do_quarto  && !file.exists("_quarto.yml"))  stop("Missing _quarto.yml at repo root.")

# Rare: wipe docs/ entirely (use only if structure changed badly)
if (clean_docs && dir.exists("docs")) {
  cat("Cleaning docs/ ...\n")
  unlink("docs", recursive = TRUE, force = TRUE)
}

# 1) Build pkgdown first (outputs to pkgdown/)
# NOTE: Disabled by default. Use --build-pkgdown to regenerate pkgdown output.
#       Pkgdown outputs to pkgdown/, then pkgdown output merges into docs/.
if (do_pkgdown) {
  if (!requireNamespace("pkgdown", quietly = TRUE)) {
    stop("pkgdown is not installed. Install it: install.packages('pkgdown')")
  }

  if (clean_pkgdown) {
    cat("\n--- Cleaning pkgdown output (force=TRUE) ---\n")
    pkgdown::clean_site(force = TRUE)
  }

  cat("\n--- Building pkgdown ---\n")
  pkgdown_output <- "pkgdown"
  if (dir.exists(pkgdown_output)) unlink(pkgdown_output, recursive = TRUE, force = TRUE)
  if (pkgdown_full) {
    # Slow: rebuild reference + articles
    tryCatch(
      pkgdown::build_site(pkg = ".", override = list(destination = pkgdown_output)),
      error = function(e) {
        cat("pkgdown build failed (continuing): ", conditionMessage(e), "\n")
      }
    )
  } else {
    # Fast default: no article rendering
    tryCatch(pkgdown::build_home(pkg = ".", override = list(destination = pkgdown_output)), error = function(e) {
      cat("pkgdown build_home failed (continuing): ", conditionMessage(e), "\n")
    })
    tryCatch(pkgdown::build_reference(pkg = ".", override = list(destination = pkgdown_output)), error = function(e) {
      cat("pkgdown build_reference failed (continuing): ", conditionMessage(e), "\n")
    })
  }
  
  # Merge pkgdown output into docs/ (without clearing existing files)
  if (dir.exists(pkgdown_output)) {
    cat("Merging pkgdown output into docs/ ...\n")
    copy_tree(pkgdown_output, "docs")
  }

  # Preserve pkgdown index alongside Quarto index
  if (file.exists("docs/index.html")) {
    file.copy("docs/index.html", "docs/pkgdown.html", overwrite = TRUE)
  }
  
  # Generate article index after pkgdown merge
  generate_article_index()
}

# 2) Build Quarto into quarto/, then merge into docs/
if (do_quarto) {
  cat("\n--- Building Quarto ---\n")

  # If we're not rebuilding pkgdown, preserve existing pkgdown output
  if (!do_pkgdown) {
    cache_existing_pkgdown()
  }

  # Clean output dir before rendering
  if (dir.exists("quarto")) {
    unlink("quarto", recursive = TRUE, force = TRUE)
  }

  cmd <- if (!is.null(quarto_target)) {
    paste("quarto render", shQuote(quarto_target))
  } else {
    "quarto render"
  }

  status <- system(cmd, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE)

  if (!identical(status, 0L) || !dir.exists("quarto")) {
    cat("Quarto render failed or produced no output; falling back to copying site/ HTML.\n")
    if (!dir.exists("site")) stop("Missing site/ folder with HTML pages.")
    copy_tree("site", "docs")
  } else {
    if (!dir.exists("docs")) dir.create("docs", showWarnings = FALSE)

    # Copy Quarto output into docs/ (overwrite)
    copy_tree("quarto", "docs")
  }
}

# 3) Restore pkgdown output after site sync
restore_cached_pkgdown()

# 4) Overlay Quarto homepage and assets (so Quarto is the main entry point)
if (dir.exists("site")) {
  cat("\n--- Overlaying Quarto homepage and assets ---\n")
  
  # Copy Quarto homepage
  if (file.exists("site/index.html")) {
    file.copy("site/index.html", "docs/index.html", overwrite = TRUE)
  }
  
  # Copy Quarto assets (cookbook, kernels, roadmap, styles.css)
  quarto_assets <- c("cookbook", "kernels", "roadmap")
  for (asset in quarto_assets) {
    src <- file.path("site", asset)
    dst <- file.path("docs", asset)
    if (dir.exists(src)) {
      if (dir.exists(dst)) unlink(dst, recursive = TRUE, force = TRUE)
      dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
      file.copy(src, dirname(dst), recursive = TRUE, copy.mode = TRUE, copy.date = TRUE)
    }
  }
  
  # Copy Quarto styles
  if (file.exists("site/styles.css")) {
    file.copy("site/styles.css", "docs/styles.css", overwrite = TRUE)
  }
  
  cat("Quarto homepage and assets overlayed.\n")
}

cat("\n✅ Done.\n")
