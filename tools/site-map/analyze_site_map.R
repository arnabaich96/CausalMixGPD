safe_read_csv <- function(path, strings_as_factors = FALSE) {
  read.csv(path, stringsAsFactors = strings_as_factors, check.names = FALSE)
}

write_json_with_fallback <- function(x, path) {
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
    return(invisible(path))
  }

  encode_scalar <- function(value) {
    if (is.null(value)) {
      return("null")
    }
    if (is.logical(value)) {
      return(ifelse(isTRUE(value), "true", "false"))
    }
    if (is.numeric(value)) {
      return(format(value, scientific = FALSE, trim = TRUE))
    }
    if (is.character(value)) {
      escaped <- gsub("\\\\", "\\\\\\\\", value)
      escaped <- gsub("\"", "\\\\\"", escaped)
      return(paste0("\"", escaped, "\""))
    }
    stop("Unsupported scalar type for fallback JSON writer.")
  }

  encode_value <- function(value) {
    if (length(value) == 1L || is.null(value)) {
      return(encode_scalar(value))
    }
    encoded <- vapply(value, encode_scalar, character(1))
    paste0("[", paste(encoded, collapse = ", "), "]")
  }

  fields <- names(x)
  body <- character(length(fields))
  for (i in seq_along(fields)) {
    name <- fields[[i]]
    body[[i]] <- paste0("  \"", name, "\": ", encode_value(x[[name]]))
  }

  json_lines <- c("{", paste(body, collapse = ",\n"), "}")
  writeLines(json_lines, con = path, useBytes = TRUE)
  invisible(path)
}

bfs_reachable <- function(start_node, adjacency, node_set) {
  if (!(start_node %in% node_set)) {
    return(character())
  }

  seen <- start_node
  queue <- start_node
  head_idx <- 1L

  while (head_idx <= length(queue)) {
    node <- queue[[head_idx]]
    head_idx <- head_idx + 1L

    neighbors <- adjacency[[node]]
    if (is.null(neighbors) || !length(neighbors)) {
      next
    }

    new_neighbors <- setdiff(neighbors, seen)
    if (length(new_neighbors)) {
      seen <- c(seen, new_neighbors)
      queue <- c(queue, new_neighbors)
    }
  }

  seen
}

expected_example_chain <- function() {
  c(
    "start/start-here.html",
    "start/basic-model-compile-run.html",
    "start/backends-and-workflow.html",
    "examples/ex01-unconditional-dpm-crp.html",
    "examples/ex02-unconditional-dpm-sb.html",
    "examples/ex03-unconditional-dpmgpd-crp.html",
    "examples/ex04-unconditional-dpmgpd-sb.html",
    "examples/ex05-conditional-dpm-crp.html",
    "examples/ex06-conditional-dpm-sb.html",
    "examples/ex07-conditional-dpmgpd-crp.html",
    "examples/ex08-conditional-dpmgpd-sb.html",
    "examples/ex09-causal-no-x-crp.html",
    "examples/ex10-causal-x-no-ps-sb.html",
    "examples/ex11-causal-same-backend-crp.html",
    "examples/ex12-causal-same-backend-sb.html",
    "examples/ex13-causal-different-backends-crp.html",
    "examples/ex14-causal-different-backends-sb.html",
    "start/troubleshooting.html"
  )
}

required_contract_pages <- function() {
  c(
    "index.html",
    "reference.html",
    "kernels.html",
    "news.html",
    "cite.html",
    "coverage.html",
    "start/index.html",
    "start/roadmap.html",
    "start/start-here.html",
    "start/basic-model-compile-run.html",
    "start/backends-and-workflow.html",
    "start/troubleshooting.html",
    "examples/index.html",
    "examples/ex01-unconditional-dpm-crp.html",
    "examples/ex02-unconditional-dpm-sb.html",
    "examples/ex03-unconditional-dpmgpd-crp.html",
    "examples/ex04-unconditional-dpmgpd-sb.html",
    "examples/ex05-conditional-dpm-crp.html",
    "examples/ex06-conditional-dpm-sb.html",
    "examples/ex07-conditional-dpmgpd-crp.html",
    "examples/ex08-conditional-dpmgpd-sb.html",
    "examples/ex09-causal-no-x-crp.html",
    "examples/ex10-causal-x-no-ps-sb.html",
    "examples/ex11-causal-same-backend-crp.html",
    "examples/ex12-causal-same-backend-sb.html",
    "examples/ex13-causal-different-backends-crp.html",
    "examples/ex14-causal-different-backends-sb.html",
    "advanced/index.html",
    "advanced/model-umbrella.html",
    "advanced/customization-and-tuning.html",
    "advanced/gpd-in-dpm-architecture.html",
    "kernels/kernels-index.html",
    "kernels/mathematical-definitions-and-conventions.html",
    "kernels/summary-distribution-backend-reference.html",
    "kernels/introduction-with-gpd-kernel.html",
    "kernels/kernel-normal.html",
    "kernels/kernel-laplace.html",
    "kernels/kernel-cauchy.html",
    "kernels/kernel-lognormal.html",
    "kernels/kernel-gamma.html",
    "kernels/kernel-amoroso.html",
    "kernels/kernel-inverse-gaussian-base.html",
    "kernels/kernel-inverse-gaussian-mixture.html",
    "developers/index.html",
    "developers/architecture.html",
    "developers/tools.html",
    "developers/testing.html",
    "developers/site-build.html",
    "developers/conventions.html",
    "developers/spec-and-contracts.html",
    "developers/registry.html",
    "developers/add-kernel.html",
    "developers/add-tail-option.html",
    "developers/causal-internals.html",
    "developers/releasing.html",
    "pkgdown/reference/index.html",
    "pkgdown/articles/index.html"
  )
}

forbidden_legacy_pages <- function() {
  c(
    "decision-guide.html",
    "faq.html",
    "roadmap.html",
    "developers.html",
    "start/introduction.html"
  )
}

write_summary_markdown <- function(
    summary,
    broken_links,
    workflow_chain,
    missing_required_pages,
    legacy_pages_present,
    path
) {
  lines <- c(
    "# Site Map Summary",
    "",
    paste0("- Timestamp (UTC): `", summary$timestamp_utc, "`"),
    paste0("- Site root: `", summary$site_root, "`"),
    paste0("- Output dir: `", summary$output_dir, "`"),
    paste0("- Node count: ", summary$node_count),
    paste0("- Edge count: ", summary$edge_count),
    paste0("- Internal page edges: ", summary$internal_page_edge_count),
    paste0("- Broken internal links: ", summary$broken_link_count),
    paste0("- Reachable from `index.html`: ", summary$reachable_from_index_count),
    paste0("- Unreachable pages: ", summary$unreachable_count),
    paste0("- Orphan pages: ", summary$orphan_count),
    paste0("- Example chain breaks: ", summary$workflow_chain_break_count),
    paste0("- Missing required pages: ", summary$missing_required_page_count),
    paste0("- Forbidden legacy pages present: ", summary$legacy_page_present_count),
    paste0("- Gate pass: ", ifelse(isTRUE(summary$gate_pass), "YES", "NO")),
    ""
  )

  lines <- c(
    lines,
    "## Gate Conditions",
    "",
    "- Fail if broken internal links > 0",
    "- Fail if any required contract page is missing",
    "- Fail if any forbidden legacy page is present",
    "- Fail if any example chain edge is missing (start/start-here -> ... -> examples/ex14 -> start/troubleshooting)",
    ""
  )

  if (length(missing_required_pages) > 0) {
    lines <- c(lines, "## Missing Required Pages", "")
    for (page in missing_required_pages) {
      lines <- c(lines, paste0("- `", page, "`"))
    }
    lines <- c(lines, "")
  }

  if (length(legacy_pages_present) > 0) {
    lines <- c(lines, "## Forbidden Legacy Pages Present", "")
    for (page in legacy_pages_present) {
      lines <- c(lines, paste0("- `", page, "`"))
    }
    lines <- c(lines, "")
  }

  if (nrow(broken_links) > 0) {
    lines <- c(lines, "## Broken Internal Links (First 20)", "")
    lines <- c(lines, "| Source | href_raw | Resolved target | Reason |")
    lines <- c(lines, "|---|---|---|---|")
    to_show <- head(broken_links, 20)
    for (i in seq_len(nrow(to_show))) {
      lines <- c(
        lines,
        paste0(
          "| `", to_show$source[[i]], "`",
          " | `", to_show$href_raw[[i]], "`",
          " | `", to_show$resolved_target[[i]], "`",
          " | ", to_show$reason[[i]], " |"
        )
      )
    }
    lines <- c(lines, "")
  }

  chain_breaks <- workflow_chain[workflow_chain$status != "ok", , drop = FALSE]
  if (nrow(chain_breaks) > 0) {
    lines <- c(lines, "## Example Chain Breaks", "")
    for (i in seq_len(nrow(chain_breaks))) {
      lines <- c(
        lines,
        paste0(
          "- `", chain_breaks$from[[i]], "` -> `", chain_breaks$to[[i]],
          "`: ", chain_breaks$status[[i]]
        )
      )
    }
    lines <- c(lines, "")
  }

  writeLines(lines, con = path, useBytes = TRUE)
}

analyze_site_map <- function(site_root = "docs", output_dir = file.path("tools", "site-map", "outputs"), verbose = TRUE) {
  msg <- function(...) {
    if (isTRUE(verbose)) {
      message(...)
    }
  }

  site_root <- normalizePath(site_root, winslash = "/", mustWork = TRUE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

  node_file <- file.path(output_dir, "website_link_nodes.csv")
  edge_file <- file.path(output_dir, "website_link_edges.csv")
  if (!file.exists(node_file)) {
    stop("Missing nodes file: ", node_file)
  }
  if (!file.exists(edge_file)) {
    stop("Missing edges file: ", edge_file)
  }

  nodes <- safe_read_csv(node_file, strings_as_factors = FALSE)
  edges <- safe_read_csv(edge_file, strings_as_factors = FALSE)

  if (!all(c("path", "section", "workflow_segment") %in% names(nodes))) {
    stop("Nodes file missing required columns.")
  }
  if (!all(c("source", "href_raw", "target", "target_exists", "link_type") %in% names(edges))) {
    stop("Edges file missing required columns.")
  }

  if (!is.logical(edges$target_exists)) {
    edges$target_exists <- as.logical(edges$target_exists)
    edges$target_exists[is.na(edges$target_exists)] <- FALSE
  }

  node_set <- nodes$path
  valid_edges <- edges[
    edges$link_type == "internal_page" &
      edges$target_exists &
      edges$source %in% node_set &
      edges$target %in% node_set,
    ,
    drop = FALSE
  ]

  if (nrow(valid_edges) > 0) {
    valid_key <- paste(valid_edges$source, valid_edges$target, sep = "\t")
    valid_edges <- valid_edges[!duplicated(valid_key), , drop = FALSE]
  }

  adjacency <- split(valid_edges$target, valid_edges$source)
  adjacency <- lapply(adjacency, unique)
  reachable <- bfs_reachable("index.html", adjacency, node_set)

  indegree_table <- table(valid_edges$target)
  indegree <- as.integer(indegree_table[nodes$path])
  indegree[is.na(indegree)] <- 0L

  reachability_report <- data.frame(
    path = nodes$path,
    section = nodes$section,
    reachable_from_index = nodes$path %in% reachable,
    indegree = indegree,
    is_orphan = indegree == 0L,
    stringsAsFactors = FALSE
  )
  reachability_report <- reachability_report[order(reachability_report$path), , drop = FALSE]
  rownames(reachability_report) <- NULL

  broken_links <- edges[
    edges$link_type == "internal_page" &
      !edges$target_exists,
    ,
    drop = FALSE
  ]
  if (nrow(broken_links) > 0) {
    broken_links <- data.frame(
      source = broken_links$source,
      href_raw = broken_links$href_raw,
      resolved_target = broken_links$target,
      reason = ifelse(nzchar(broken_links$target), "target_not_found", "empty_resolved_target"),
      stringsAsFactors = FALSE
    )
    broken_links <- broken_links[order(broken_links$source, broken_links$href_raw), , drop = FALSE]
    rownames(broken_links) <- NULL
  } else {
    broken_links <- data.frame(
      source = character(),
      href_raw = character(),
      resolved_target = character(),
      reason = character(),
      stringsAsFactors = FALSE
    )
  }

  expected_chain <- expected_example_chain()
  chain_rows <- vector("list", length = length(expected_chain) - 1L)
  for (i in seq_len(length(expected_chain) - 1L)) {
    expected_from <- expected_chain[[i]]
    expected_to <- expected_chain[[i + 1L]]
    from_node <- if (expected_from %in% node_set) expected_from else NA_character_
    to_node <- if (expected_to %in% node_set) expected_to else NA_character_

    status <- "ok"
    edge_present <- FALSE
    if (is.na(from_node)) {
      status <- "missing_from_node"
    } else if (is.na(to_node)) {
      status <- "missing_to_node"
    } else {
      edge_present <- any(valid_edges$source == from_node & valid_edges$target == to_node)
      status <- ifelse(edge_present, "ok", "missing_edge")
    }

    chain_rows[[i]] <- data.frame(
      from = ifelse(is.na(from_node), expected_from, from_node),
      to = ifelse(is.na(to_node), expected_to, to_node),
      edge_present = isTRUE(edge_present),
      status = status,
      stringsAsFactors = FALSE
    )
  }
  workflow_chain <- do.call(rbind, chain_rows)

  required_pages <- required_contract_pages()
  missing_required_pages <- setdiff(required_pages, node_set)

  legacy_pages <- forbidden_legacy_pages()
  legacy_pages_present <- intersect(legacy_pages, node_set)

  workflow_breaks <- workflow_chain[workflow_chain$status != "ok", , drop = FALSE]
  gate_pass <- (
    nrow(broken_links) == 0L &&
      nrow(workflow_breaks) == 0L &&
      length(missing_required_pages) == 0L &&
      length(legacy_pages_present) == 0L
  )
  fail_reasons <- character()
  if (nrow(broken_links) > 0L) {
    fail_reasons <- c(fail_reasons, paste0("broken_links=", nrow(broken_links)))
  }
  if (nrow(workflow_breaks) > 0L) {
    fail_reasons <- c(fail_reasons, paste0("workflow_chain_breaks=", nrow(workflow_breaks)))
  }
  if (length(missing_required_pages) > 0L) {
    fail_reasons <- c(fail_reasons, paste0("missing_required_pages=", length(missing_required_pages)))
  }
  if (length(legacy_pages_present) > 0L) {
    fail_reasons <- c(fail_reasons, paste0("legacy_pages_present=", length(legacy_pages_present)))
  }
  if (!length(fail_reasons)) {
    fail_reasons <- "none"
  }

  summary <- list(
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    site_root = site_root,
    output_dir = output_dir,
    node_count = nrow(nodes),
    edge_count = nrow(edges),
    internal_page_edge_count = nrow(edges[edges$link_type == "internal_page", , drop = FALSE]),
    broken_link_count = nrow(broken_links),
    reachable_from_index_count = sum(reachability_report$reachable_from_index),
    unreachable_count = sum(!reachability_report$reachable_from_index),
    orphan_count = sum(reachability_report$is_orphan),
    workflow_chain_break_count = nrow(workflow_breaks),
    missing_required_page_count = length(missing_required_pages),
    legacy_page_present_count = length(legacy_pages_present),
    gate_pass = gate_pass,
    gate_fail_reasons = fail_reasons
  )

  broken_file <- file.path(output_dir, "broken_links.csv")
  reachability_file <- file.path(output_dir, "reachability_report.csv")
  workflow_file <- file.path(output_dir, "workflow_chain_report.csv")
  summary_json_file <- file.path(output_dir, "site_map_summary.json")
  summary_md_file <- file.path(output_dir, "site_map_summary.md")

  write.csv(broken_links, broken_file, row.names = FALSE, na = "")
  write.csv(reachability_report, reachability_file, row.names = FALSE, na = "")
  write.csv(workflow_chain, workflow_file, row.names = FALSE, na = "")
  write_json_with_fallback(summary, summary_json_file)
  write_summary_markdown(
    summary = summary,
    broken_links = broken_links,
    workflow_chain = workflow_chain,
    missing_required_pages = missing_required_pages,
    legacy_pages_present = legacy_pages_present,
    path = summary_md_file
  )

  msg("Wrote broken links: ", broken_file)
  msg("Wrote reachability report: ", reachability_file)
  msg("Wrote workflow chain report: ", workflow_file)
  msg("Wrote summary JSON: ", summary_json_file)
  msg("Wrote summary markdown: ", summary_md_file)

  invisible(list(
    summary = summary,
    broken_links = broken_links,
    reachability_report = reachability_report,
    workflow_chain = workflow_chain,
    missing_required_pages = missing_required_pages,
    legacy_pages_present = legacy_pages_present,
    broken_file = broken_file,
    reachability_file = reachability_file,
    workflow_file = workflow_file,
    summary_json_file = summary_json_file,
    summary_md_file = summary_md_file
  ))
}
