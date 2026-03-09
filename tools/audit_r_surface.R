args <- commandArgs(trailingOnly = TRUE)
out_path <- if (length(args)) args[[1L]] else file.path("tools", "audit", "r_surface_audit.csv")

`%||%` <- function(a, b) if (!is.null(a)) a else b

escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

namespace_lines <- readLines("NAMESPACE", warn = FALSE)
exported <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", namespace_lines, value = TRUE))
s3_lines <- grep("^S3method\\(", namespace_lines, value = TRUE)
s3_symbols <- sub("^S3method\\(([^,]+),([^\\)]+)\\)$", "\\1.\\2", s3_lines)

r_files <- sort(list.files("R", pattern = "\\.[Rr]$", full.names = TRUE))
ref_dirs <- c("R", "tests", "vignettes", "man")
ref_files <- unlist(lapply(ref_dirs, function(path) {
  if (!dir.exists(path)) return(character(0))
  list.files(path, recursive = TRUE, full.names = TRUE)
}), use.names = FALSE)
ref_files <- ref_files[file.exists(ref_files)]
text_ext <- "\\.(R|r|Rmd|rmd|Rnw|rnw|Rd|rd|md|MD|qmd|Qmd)$"
ref_files <- ref_files[grepl(text_ext, ref_files)]
exclude_pat <- "(^|[\\\\/])(cache|figure|artifacts|assets|docs|quarto|website|temp)([\\\\/]|$)"
ref_files <- ref_files[!grepl(exclude_pat, ref_files)]

read_group_lines <- function(pattern) {
  group_files <- ref_files[grepl(pattern, ref_files)]
  if (!length(group_files)) return(character(0))
  unlist(lapply(group_files, function(path) readLines(path, warn = FALSE)), use.names = FALSE)
}

tokenize_lines <- function(lines) {
  if (!length(lines)) return(character(0))
  txt <- paste(lines, collapse = "\n")
  txt <- gsub("`?%\\|\\|%`?", " percent_or_null_token ", txt, perl = TRUE)
  txt <- gsub("[^A-Za-z0-9._]+", " ", txt, perl = TRUE)
  tokens <- strsplit(txt, "\\s+", perl = TRUE)[[1L]]
  tokens <- tokens[nzchar(tokens)]
  tokens[tokens == "percent_or_null_token"] <- "%||%"
  tokens
}

group_lines <- list(
  R = read_group_lines("^R[\\\\/]"),
  tests = read_group_lines("^tests[\\\\/]"),
  vignettes = read_group_lines("^vignettes[\\\\/]"),
  man = read_group_lines("^man[\\\\/]")
)

group_token_tables <- lapply(group_lines, function(lines) table(tokenize_lines(lines)))

extract_symbols <- function(path) {
  lines <- readLines(path, warn = FALSE)
  hits <- regexec("^\\s*([`A-Za-z0-9_.%|]+)\\s*<-\\s*(function\\(|nimble::nimbleFunction\\()", lines, perl = TRUE)
  vals <- regmatches(lines, hits)
  keep <- lengths(vals) > 0L
  if (!any(keep)) return(NULL)
  data.frame(
    symbol = gsub("^`|`$", "", vapply(vals[keep], function(x) x[[2L]], character(1))),
    file = path,
    stringsAsFactors = FALSE
  )
}

defs <- do.call(rbind, Filter(Negate(is.null), lapply(r_files, extract_symbols)))
if (is.null(defs) || !nrow(defs)) stop("No R symbols found.")

count_refs <- function(symbol, token_table) {
  if (symbol %in% names(token_table)) as.integer(token_table[[symbol]]) else 0L
}

duplicate_canonical <- c(
  ".validate_fit" = "R/internal.R",
  ".get_dispatch_scalar" = "R/internal.R",
  ".get_dispatch" = "R/internal.R",
  ".detect_first_present" = "R/internal.R",
  ".wrap_scalar_first_arg" = "R/internal.R",
  ".wrap_scalar_p" = "R/internal.R",
  ".wrap_scalar_r" = "R/internal.R",
  ".extract_z_matrix" = "R/internal.R",
  ".compute_psm" = "R/internal.R",
  ".dahl_representative" = "R/internal.R",
  ".compute_cluster_probs" = "R/internal.R"
)

replacement_map <- c(
  ".validate_fit" = "canonical .validate_fit in R/internal.R",
  ".get_dispatch_scalar" = "canonical .get_dispatch_scalar in R/internal.R",
  ".get_dispatch" = "canonical .get_dispatch in R/internal.R",
  ".detect_first_present" = "canonical .detect_first_present in R/internal.R",
  ".wrap_scalar_first_arg" = "canonical .wrap_scalar_first_arg in R/internal.R",
  ".wrap_scalar_p" = "canonical .wrap_scalar_p in R/internal.R",
  ".wrap_scalar_r" = "canonical .wrap_scalar_r in R/internal.R",
  ".extract_z_matrix" = "canonical .extract_z_matrix in R/internal.R",
  ".compute_psm" = "canonical .compute_psm in R/internal.R",
  ".dahl_representative" = "canonical .dahl_representative in R/internal.R",
  ".compute_cluster_probs" = "canonical .compute_cluster_probs in R/internal.R"
)

classify_symbol <- function(symbol, file, exported_flag, s3_flag, counts) {
  canonical <- if (symbol %in% names(duplicate_canonical)) duplicate_canonical[[symbol]] else NA_character_
  if (!is.na(canonical) && normalizePath(file, winslash = "/", mustWork = FALSE) !=
      normalizePath(canonical, winslash = "/", mustWork = FALSE)) {
    return("merge-duplicate")
  }
  if (exported_flag || s3_flag) {
    if (counts[["refs_R"]] == 0L && (counts[["refs_tests"]] + counts[["refs_vignettes"]] + counts[["refs_man"]]) > 0L) {
      return("keep-public")
    }
    return("keep-core")
  }
  if ((counts[["refs_R"]] + counts[["refs_tests"]] + counts[["refs_vignettes"]] + counts[["refs_man"]]) <= 1L) {
    return("remove-stale")
  }
  "keep-core"
}

audit_rows <- lapply(seq_len(nrow(defs)), function(i) {
  symbol <- defs$symbol[[i]]
  file <- defs$file[[i]]
  refs_r <- count_refs(symbol, group_token_tables$R)
  refs_tests <- count_refs(symbol, group_token_tables$tests)
  refs_vignettes <- count_refs(symbol, group_token_tables$vignettes)
  refs_man <- count_refs(symbol, group_token_tables$man)
  exported_flag <- symbol %in% exported
  s3_flag <- symbol %in% s3_symbols
  counts <- c(refs_R = refs_r, refs_tests = refs_tests, refs_vignettes = refs_vignettes, refs_man = refs_man)

  data.frame(
    symbol = symbol,
    status = if (exported_flag) "exported" else if (s3_flag) "s3-method" else "internal",
    file = file,
    refs_R = refs_r,
    refs_tests = refs_tests,
    refs_vignettes = refs_vignettes,
    refs_examples_docs = refs_man,
    classification = classify_symbol(symbol, file, exported_flag, s3_flag, counts),
    replacement_candidate = if (symbol %in% names(replacement_map)) unname(replacement_map[[symbol]]) else "",
    stringsAsFactors = FALSE
  )
})

audit <- do.call(rbind, audit_rows)
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write.csv(audit[order(audit$classification, audit$status, audit$symbol), ], out_path, row.names = FALSE)
cat(sprintf("Wrote %d audit rows to %s\n", nrow(audit), out_path))
