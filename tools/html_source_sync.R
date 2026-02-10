#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
target <- if (length(args) >= 1) args[[1]] else "docs/index.html"
target <- normalizePath(target, winslash = "/", mustWork = FALSE)

if (!file.exists(target)) {
  stop("File not found: ", target, call. = FALSE)
}

lines <- readLines(target, warn = FALSE, encoding = "UTF-8")

annotate_line <- function(x, i, src_file) {
  if (!grepl("<[A-Za-z]", x, perl = TRUE)) return(x)
  if (grepl("^\\s*</", x, perl = TRUE)) return(x)
  if (grepl("^\\s*<!", x, perl = TRUE)) return(x)
  if (grepl("data-src-line\\s*=|data-src-file\\s*=", x, perl = TRUE)) return(x)

  sub(
    "(<[A-Za-z][A-Za-z0-9:-]*)\\b",
    sprintf("\\1 data-src-line=\\\"%d\\\" data-src-file=\\\"%s\\\"", i, src_file),
    x,
    perl = TRUE
  )
}

for (i in seq_along(lines)) {
  lines[[i]] <- annotate_line(lines[[i]], i, target)
}

inject_script <- c(
  "<script id=\"html-source-sync\">",
  "(function(){",
  "  if (window.__htmlSourceSyncLoaded) return;",
  "  window.__htmlSourceSyncLoaded = true;",
  "  document.addEventListener('click', function(e){",
  "    if (!e.altKey) return;",
  "    var el = e.target && e.target.closest('[data-src-line][data-src-file]');",
  "    if (!el) return;",
  "    e.preventDefault();",
  "    e.stopPropagation();",
  "    var line = el.getAttribute('data-src-line');",
  "    var file = el.getAttribute('data-src-file');",
  "    if (!line || !file) return;",
  "    var prevOutline = el.style.outline;",
  "    el.style.outline = '2px solid #d35400';",
  "    setTimeout(function(){ el.style.outline = prevOutline; }, 1200);",
  "    var uri = 'vscode://file/' + encodeURI(file) + ':' + encodeURIComponent(line);",
  "    window.location.href = uri;",
  "  }, true);",
  "})();",
  "</script>"
)

if (!any(grepl("id=\"html-source-sync\"", lines, fixed = TRUE))) {
  body_close <- grep("</body>", lines, ignore.case = TRUE)
  if (length(body_close) > 0) {
    idx <- body_close[[length(body_close)]]
    lines <- append(lines, inject_script, after = idx - 1L)
  } else {
    lines <- c(lines, inject_script)
  }
}

writeLines(lines, target, useBytes = TRUE)
cat("HTML source sync enabled for:", target, "\n")
cat("Use Alt+Click in browser preview to jump to source in VS Code.\n")
