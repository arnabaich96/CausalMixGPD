# DPmixGPD Documentation QA Checklist

## Purpose
This document serves as a reusable checklist to ensure the pkgdown site maintains professional quality and consistency across all releases.

## Quick Reference Checklist

### ✅ Links & Navigation
- [ ] All article links in `docs/articles/index.html` are functional
- [ ] Navbar buttons (Home, Reference, Articles, Coverage) navigate correctly
- [ ] GitHub link opens external site properly
- [ ] Kernel reference links in article content point to correct pages
- [ ] Cross-article references (e.g., "see unconditional models section") are valid

### ✅ Titles & Readability
- [ ] All YAML titles in vignettes are descriptive (not showing as "v06-...")
- [ ] Titles are consistent in numbering: v01–v15 in order
- [ ] Article index groups follow learning flow: Getting Started → Unconditional → Conditional → Causal → Kernel Reference
- [ ] No missing or placeholder titles

### ✅ Code & Output
- [ ] Kernel reference vignettes have `eval = FALSE` (intentional—they are checklists)
- [ ] Main tutorial vignettes (v01–v15) have `eval = TRUE` or specific code blocks set as intended
- [ ] R code blocks that execute show expected output or plots
- [ ] No error messages or warnings visible unless intentional/explained

### ✅ Content Consistency
- [ ] GPD naming is consistent (e.g., "GPD" or "generalized Pareto distribution" as chosen)
- [ ] Backend names consistent: "CRP" (Chinese Restaurant Process) and "SB" (Stick-Breaking)
- [ ] Kernel names match vignette titles and reference docs
- [ ] No hardcoded references to old vignette names

### ✅ Visibility & Polish
- [ ] No "TODO", "FIXME", or placeholder text visible on public pages
- [ ] All generated files are present: `docs/articles/*.html`, `docs/reference/*.html`
- [ ] No broken image links (search for `![` in source or missing `/img/` paths)
- [ ] No orphaned markdown files in docs folder

### ✅ Build & Deploy
- [ ] `pkgdown::build_site()` completes without critical errors
- [ ] Latest commit includes built docs if applicable
- [ ] `_pkgdown.yml` articles section lists all vignettes in reading order
- [ ] `DESCRIPTION` version matches what appears in navbar

---

## Step-by-Step QA Process

### Run Before Each Release

#### 1. Rebuild the Site
```r
devtools::load_all(quiet = TRUE)
pkgdown::build_site(preview = FALSE)
```

#### 2. Spot-Check Key Pages
- Open `docs/index.html` → click through navbar
- Open `docs/articles/index.html` → verify all links work
- Open 3–5 random article links → check title, code outputs

#### 3. Search for Common Issues
```r
# In vignettes/*.Rmd files, check for:
# - eval = FALSE (should only be in kernel references and intro)
# - TODO, FIXME, XXX (none should be visible)
# - Broken references or images
```

#### 4. Validate Build Output
```bash
# Check docs folder for critical files
ls -la docs/articles/ | grep "\.html$"
ls -la docs/reference/ | grep "\.html$"
```

#### 5. Final Visual Check
- Open `docs/index.html` in browser
- Test navbar navigation
- Click through articles index
- Spot-check reference page

---

## Issue Triage

### Issue: Vignette Fails to Render
- Check for syntax errors in YAML header
- Ensure all required packages are installed
- If code is slow: add `eval = FALSE` to setup or specific blocks
- Do NOT delete code—wrap or comment instead

### Issue: Broken Link in Article
- Check relative paths (use `../` not `/`)
- Verify target file exists in `docs/`
- Update `_pkgdown.yml` if article was renamed

### Issue: Title Shows as "v06-..." Instead of Full Name
- Update YAML `title:` field in source `.Rmd`
- Rebuild with `pkgdown::build_site()`

### Issue: Missing Code Output
- Set `eval = TRUE` in code block header
- Ensure code doesn't require interactive input
- Check for missing data files or dependencies

---

## Notes
- This checklist should be run **before each release** to ensure quality
- Keep this file for easy reference
- Update this checklist if new issues are discovered
