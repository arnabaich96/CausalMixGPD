#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

has_flag <- function(x) any(args == x)
flag_val <- function(prefix) {
  # Supports: --quarto=site/index.html
  hit <- grep(paste0("^", prefix, "="), args, value = TRUE)
  if (length(hit) == 0) return(NULL)
  sub(paste0("^", prefix, "="), "", hit[1])
}

do_pkgdown <- !has_flag("--no-pkgdown")
do_quarto  <- !has_flag("--no-quarto")

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

cat("\n=== DPmixGPD website build ===\n")
cat("pkgdown:", if (do_pkgdown) (if (pkgdown_full) "FULL" else "FAST (home+reference)") else "SKIP", "\n")
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

# 1) Build pkgdown first (so Quarto wrapper doesn't overwrite it)
if (do_pkgdown) {
  if (!requireNamespace("pkgdown", quietly = TRUE)) {
    stop("pkgdown is not installed. Install it: install.packages('pkgdown')")
  }

  if (clean_pkgdown) {
    cat("\n--- Cleaning pkgdown output (force=TRUE) ---\n")
    pkgdown::clean_site(force = TRUE)
  }

  cat("\n--- Building pkgdown ---\n")
  if (pkgdown_full) {
    # Slow: rebuild reference + articles
    pkgdown::build_site()
  } else {
    # Fast default: no article rendering
    pkgdown::build_home()
    pkgdown::build_reference()
  }
}

# 2) Build Quarto wrapper
if (do_quarto) {
  cat("\n--- Building Quarto ---\n")
  cmd <- if (!is.null(quarto_target)) {
    paste("quarto render", shQuote(quarto_target))
  } else {
    "quarto render"
  }

  status <- system(cmd, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE)
  if (!identical(status, 0L)) stop("Quarto render failed.")
}

cat("\n✅ Done.\n")
