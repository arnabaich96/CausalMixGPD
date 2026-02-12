get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- cmd_args[grepl("^--file=", cmd_args)]
  if (length(file_arg)) {
    script_path <- sub("^--file=", "", file_arg[[1]])
    return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

get_arg_value <- function(args, prefix, default) {
  hits <- args[startsWith(args, prefix)]
  if (!length(hits)) {
    return(default)
  }
  sub(prefix, "", hits[[length(hits)]], fixed = TRUE)
}

has_flag <- function(args, flag) {
  any(args == flag)
}

script_dir <- get_script_dir()
repo_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(script_dir, "extract_site_map.R"), local = FALSE)
source(file.path(script_dir, "analyze_site_map.R"), local = FALSE)

args <- commandArgs(trailingOnly = TRUE)
extract_only <- has_flag(args, "--extract-only")
report_only <- has_flag(args, "--report-only")

if (extract_only && report_only) {
  stop("Use only one mode flag: --extract-only OR --report-only.")
}

site_root_arg <- get_arg_value(args, "--site-root=", "docs")
output_dir_arg <- get_arg_value(args, "--output-dir=", file.path("tools", "site-map", "outputs"))

site_root <- if (grepl("^[A-Za-z]:|^/", site_root_arg)) {
  site_root_arg
} else {
  file.path(repo_root, site_root_arg)
}
output_dir <- if (grepl("^[A-Za-z]:|^/", output_dir_arg)) {
  output_dir_arg
} else {
  file.path(repo_root, output_dir_arg)
}

cat("Site root:", normalizePath(site_root, winslash = "/", mustWork = TRUE), "\n")
cat("Output dir:", normalizePath(output_dir, winslash = "/", mustWork = FALSE), "\n")

if (!report_only) {
  extract_site_map(
    site_root = site_root,
    output_dir = output_dir,
    verbose = TRUE
  )
}

if (!extract_only) {
  result <- analyze_site_map(
    site_root = site_root,
    output_dir = output_dir,
    verbose = TRUE
  )

  summary <- result$summary
  cat("\nSite-map summary\n")
  cat("  Nodes:", summary$node_count, "\n")
  cat("  Edges:", summary$edge_count, "\n")
  cat("  Broken links:", summary$broken_link_count, "\n")
  cat("  Unreachable pages:", summary$unreachable_count, "\n")
  cat("  Orphan pages:", summary$orphan_count, "\n")
  cat("  Example chain breaks:", summary$workflow_chain_break_count, "\n")
  cat("  Missing required pages:", summary$missing_required_page_count, "\n")
  cat("  Forbidden legacy pages present:", summary$legacy_page_present_count, "\n")
  cat("  Gate pass:", ifelse(isTRUE(summary$gate_pass), "YES", "NO"), "\n")

  if (summary$broken_link_count > 0) {
    cat("\nTop broken links (first 10)\n")
    to_show <- head(result$broken_links, 10)
    for (i in seq_len(nrow(to_show))) {
      cat(
        "  -", to_show$source[[i]],
        "=>", to_show$href_raw[[i]],
        "(resolved:", to_show$resolved_target[[i]], ")\n"
      )
    }
  }

  chain_breaks <- result$workflow_chain[result$workflow_chain$status != "ok", , drop = FALSE]
  if (nrow(chain_breaks) > 0) {
    cat("\nExample chain issues\n")
    for (i in seq_len(nrow(chain_breaks))) {
      cat(
        "  -", chain_breaks$from[[i]], "->", chain_breaks$to[[i]],
        ":", chain_breaks$status[[i]], "\n"
      )
    }
  }

  if (!isTRUE(summary$gate_pass)) {
    quit(status = 1, save = "no")
  }
}

quit(status = 0, save = "no")
