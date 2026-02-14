# AGENTS.md

Repository guidance for coding agents working in `DPmixGPD`.

## Project context

- This is an R package repo (renv-managed).
- Primary sources live in `R/`, documentation in `man/` and
  `vignettes/`, site config in `_pkgdown.yml`.

## Workflow expectations

- Prefer `rg` for search and `rg --files` for file discovery.
- Preserve existing style and structure; keep edits minimal and focused.
- Update roxygen docs when modifying exported functions, then run
  [`roxygen2::roxygenise()`](https://roxygen2.r-lib.org/reference/roxygenize.html)
  (or note that it is required).
- If you change the API or documentation, also update any related
  vignette or README content.

## Testing

- Default local checks: `R CMD check` or
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
  (note if not run).
- For quick validation, prefer targeted tests under `tests/` when
  available.

## Docs & site

- If you touch `_pkgdown.yml` or vignettes, consider running
  [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
  (note if not run).
- When adding new vignettes, ensure titles/identifiers are unique to
  avoid duplicate identifier warnings.
- Main vignettes (`*.Rmd` plus `manuscript/`) should stay directly under
  `vignettes/`; every other website-specific content lives under
  `vignettes/website/` (e.g., `workflows`, `cookbook`, `kernels`, etc.).

## Output expectations

- Keep responses concise.
- Reference file paths using inline code formatting.

## Local policy (from maintainer)

- No local plotting/error-handling wrappers in website or vignette
  sources (e.g., `safe_plot` in `website/*.qmd` or `vignettes/*`).
- Use package APIs directly (e.g., plot(…)). If rendering surfaces an
  error, investigate and fix the underlying issue in package/docs rather
  than adding wrapper helpers outside the package.
