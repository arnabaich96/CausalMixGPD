# Contributing to DPmixGPD

Thanks for your interest in contributing! This project is an experimental R package for Dirichlet process mixture models with optional GPD tails. We welcome issues, discussions, and pull requests.

## Getting Started

- Prerequisites:
  - R >= 4.5.0 (see DESCRIPTION)
  - On Windows: Rtools; on macOS: Xcode Command Line Tools
  - Suggested: `renv` for dependency management, `devtools`, `roxygen2`, `testthat`
- Setup:
  1. Fork and clone the repository.
  2. Open the RStudio project (`DPmixGPD.Rproj`) or the folder in VS Code.
  3. Restore deps (if using renv):
     ```r
     install.packages("renv"); renv::restore()
     ```
  4. Load the package for development:
     ```r
     install.packages(c("devtools","roxygen2","testthat"))
     devtools::load_all(quiet = TRUE)
     ```

## Branching & Pull Requests

- Create a feature branch from `master` (e.g., `feature/your-change`).
- Keep PRs focused and small.
- Include tests for new behavior and update documentation.
- Before opening a PR:
  ```r
  devtools::document()      # update Rd from roxygen2
  devtools::test()          # run testthat tests
  devtools::check()         # run R CMD check
  ```
- Open a PR against this repo and describe the motivation, approach, and any trade-offs.

## Code Style & Docs

- Follow a tidyverse-style R formatting (use `styler` if desired).
- Use roxygen2 for function documentation; examples should be runnable or guarded.
- Prefer explicit, vectorized code over side effects; avoid premature optimization.
- Keep changes minimal and focused; avoid unrelated refactors.

### Optional pre-commit formatting

This repo includes a pre-commit hook that runs `styler` on staged `.R` and `.Rmd` files.
To enable it locally:

```bash
git config core.hooksPath .githooks
```

To skip formatting for a commit, set `DPMIXGPD_SKIP_STYLER=1` in your environment.

## Testing

- Use `testthat` (edition 3). Place tests under `tests/testthat/`.
- Aim for deterministic tests; mark long-running or stochastic tests with `skip_on_cran()` if applicable.

## Reporting Issues

- Search existing issues first.
- Include reproducible examples (minimal data + code), session info, and expected vs actual behavior.
- File issues here: https://github.com/arnabaich96/DPmixGPD_Package/issues

## Code of Conduct

By participating, you agree to abide by our [Code of Conduct](CODE_OF_CONDUCT.md). Please report unacceptable behavior to the Maintainer email in DESCRIPTION.

## License

Contributions are licensed under the project’s license (GPL-3). By submitting a PR, you agree your contributions will be licensed as GPL-3.
