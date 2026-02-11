normalize_slashes <- function(x) {
  gsub("\\\\", "/", x)
}

normalize_relative_path <- function(path) {
  path <- normalize_slashes(path)
  tokens <- strsplit(path, "/", fixed = TRUE)[[1]]
  stack <- character()

  for (token in tokens) {
    if (!nzchar(token) || identical(token, ".")) {
      next
    }
    if (identical(token, "..")) {
      if (length(stack) > 0) {
        stack <- stack[-length(stack)]
      }
      next
    }
    stack <- c(stack, token)
  }

  if (!length(stack)) {
    return("")
  }
  paste(stack, collapse = "/")
}

infer_section <- function(path) {
  if (!grepl("/", path, fixed = TRUE)) {
    return("root")
  }

  first_segment <- strsplit(path, "/", fixed = TRUE)[[1]][1]
  known_sections <- c("Examples", "QuickStart", "Advanced", "kernels", "vignettes", "pkgdown")
  if (first_segment %in% known_sections) {
    return(first_segment)
  }
  first_segment
}

infer_workflow_segment <- function(path) {
  if (grepl("^QuickStart/", path)) {
    return("quickstart")
  }

  if (!grepl("^Examples/ex\\d{2}-", path)) {
    return(NA_character_)
  }

  idx <- suppressWarnings(as.integer(sub("^Examples/ex(\\d{2})-.*$", "\\1", path)))
  if (is.na(idx)) {
    return(NA_character_)
  }

  if (idx == 20) {
    return("troubleshooting")
  }
  if (idx >= 5 && idx <= 9) {
    return("unconditional")
  }
  if (idx >= 10 && idx <= 13) {
    return("conditional")
  }
  if (idx >= 14 && idx <= 19) {
    return("causal")
  }

  NA_character_
}

extract_anchor_hrefs <- function(html_text) {
  tag_pattern <- "(?is)<a\\b[^>]*\\bhref\\s*=\\s*(\"[^\"]*\"|'[^']*'|[^'\"\\s>]+)[^>]*>"
  tag_matches <- regmatches(html_text, gregexpr(tag_pattern, html_text, perl = TRUE))[[1]]
  if (!length(tag_matches)) {
    return(character())
  }

  hrefs <- character(length(tag_matches))
  href_pattern <- "(?is)\\bhref\\s*=\\s*(\"([^\"]*)\"|'([^']*)'|([^'\"\\s>]+))"

  for (i in seq_along(tag_matches)) {
    parts <- regmatches(tag_matches[[i]], regexec(href_pattern, tag_matches[[i]], perl = TRUE))[[1]]
    if (length(parts) < 5) {
      hrefs[[i]] <- ""
      next
    }
    captures <- parts[3:5]
    non_empty <- captures[nzchar(captures)]
    hrefs[[i]] <- if (length(non_empty)) non_empty[[1]] else ""
  }

  hrefs[nzchar(hrefs)]
}

resolve_internal_href <- function(source, href_raw, node_set, site_root) {
  href <- trimws(href_raw)
  if (!nzchar(href)) {
    return(NULL)
  }

  if (startsWith(href, "#")) {
    return(NULL)
  }

  if (startsWith(href, "//")) {
    return(NULL)
  }

  if (grepl("^[a-zA-Z][a-zA-Z0-9+.-]*:", href)) {
    return(NULL)
  }

  cleaned <- sub("[?#].*$", "", href)
  if (!nzchar(cleaned)) {
    return(NULL)
  }

  is_dir_reference <- grepl("/$", cleaned)
  is_absolute <- startsWith(cleaned, "/")
  source_dir <- dirname(source)
  if (identical(source_dir, ".")) {
    source_dir <- ""
  }

  rel <- if (is_absolute) {
    sub("^/+", "", cleaned)
  } else if (nzchar(source_dir)) {
    paste(source_dir, cleaned, sep = "/")
  } else {
    cleaned
  }

  rel <- normalize_relative_path(rel)
  if (!nzchar(rel)) {
    rel <- "index.html"
  }

  ext <- tolower(tools::file_ext(rel))
  page_like <- is_dir_reference || ext %in% c("", "html", "htm")

  if (page_like) {
    candidates <- character()
    if (is_dir_reference) {
      candidates <- c(candidates, if (nzchar(rel)) paste0(rel, "/index.html") else "index.html")
    }
    candidates <- c(candidates, rel)
    if (ext == "") {
      candidates <- c(candidates, paste0(rel, ".html"), paste0(rel, "/index.html"))
    }
    if (ext == "htm") {
      candidates <- c(candidates, sub("\\.htm$", ".html", rel))
    }

    candidates <- unique(vapply(candidates, normalize_relative_path, character(1)))
    candidates <- candidates[nzchar(candidates)]
    if (!length(candidates)) {
      candidates <- "index.html"
    }

    chosen <- candidates[[1]]
    target_exists <- FALSE
    for (candidate in candidates) {
      if (candidate %in% node_set) {
        chosen <- candidate
        target_exists <- TRUE
        break
      }
    }

    return(list(
      target = chosen,
      target_exists = target_exists,
      link_type = "internal_page"
    ))
  }

  asset_exists <- file.exists(file.path(site_root, rel))
  list(
    target = rel,
    target_exists = asset_exists,
    link_type = "internal_asset"
  )
}

extract_site_map <- function(site_root = "docs", output_dir = file.path("tools", "site-map", "outputs"), verbose = TRUE) {
  msg <- function(...) {
    if (isTRUE(verbose)) {
      message(...)
    }
  }

  site_root <- normalizePath(site_root, winslash = "/", mustWork = TRUE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  html_files <- list.files(
    path = site_root,
    pattern = "\\.html?$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (!length(html_files)) {
    stop("No HTML files found under: ", site_root)
  }

  html_files <- normalize_slashes(normalizePath(html_files, winslash = "/", mustWork = TRUE))
  rel_paths <- sub(paste0("^", site_root, "/?"), "", html_files)
  rel_paths <- vapply(rel_paths, normalize_relative_path, character(1))
  rel_paths <- sort(unique(rel_paths[nzchar(rel_paths)]))

  nodes <- data.frame(
    path = rel_paths,
    section = vapply(rel_paths, infer_section, character(1)),
    workflow_segment = vapply(rel_paths, infer_workflow_segment, character(1)),
    stringsAsFactors = FALSE
  )
  nodes$workflow_segment[!nzchar(nodes$workflow_segment)] <- NA_character_

  node_set <- nodes$path
  edge_rows <- vector("list", length = 0L)

  msg("Extracting links from ", length(node_set), " HTML pages...")

  for (source in node_set) {
    source_file <- file.path(site_root, source)
    html_text <- paste(readLines(source_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    hrefs <- unique(extract_anchor_hrefs(html_text))
    if (!length(hrefs)) {
      next
    }

    for (href_raw in hrefs) {
      resolved <- resolve_internal_href(
        source = source,
        href_raw = href_raw,
        node_set = node_set,
        site_root = site_root
      )
      if (is.null(resolved)) {
        next
      }

      edge_rows[[length(edge_rows) + 1L]] <- data.frame(
        source = source,
        href_raw = href_raw,
        target = resolved$target,
        target_exists = isTRUE(resolved$target_exists),
        link_type = resolved$link_type,
        stringsAsFactors = FALSE
      )
    }
  }

  edges <- if (length(edge_rows)) {
    do.call(rbind, edge_rows)
  } else {
    data.frame(
      source = character(),
      href_raw = character(),
      target = character(),
      target_exists = logical(),
      link_type = character(),
      stringsAsFactors = FALSE
    )
  }

  if (nrow(edges) > 0) {
    dedupe_key <- paste(edges$source, edges$href_raw, edges$target, edges$link_type, sep = "\t")
    edges <- edges[!duplicated(dedupe_key), , drop = FALSE]
    edges <- edges[order(edges$source, edges$target, edges$href_raw), , drop = FALSE]
    rownames(edges) <- NULL
  }

  node_file <- file.path(output_dir, "website_link_nodes.csv")
  edge_file <- file.path(output_dir, "website_link_edges.csv")
  write.csv(nodes, node_file, row.names = FALSE, na = "")
  write.csv(edges, edge_file, row.names = FALSE, na = "")

  msg("Wrote nodes: ", node_file)
  msg("Wrote edges: ", edge_file)

  invisible(list(
    nodes = nodes,
    edges = edges,
    node_file = node_file,
    edge_file = edge_file,
    site_root = site_root,
    output_dir = output_dir
  ))
}
