# Contributing to CausalMixGPD

Thanks for your interest in contributing! This project is an experimental R package for Dirichlet process mixture models with optional GPD tails. We welcome issues, discussions, and pull requests.

## Getting Started

- Prerequisites:
  - R >= 4.5.0 (see DESCRIPTION)
  - On Windows: Rtools; on macOS: Xcode Command Line Tools
  - Suggested: `renv` for dependency management, `devtools`, `roxygen2`, `testthat`
- Setup:
  1. Fork and clone the repository.
  2. Open the RStudio project (`CausalMixGPD.Rproj`) or the folder in VS Code.
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

### Optional local formatting

If you want automatic formatting before a commit, run `styler` manually on the
files you changed. This repository does not install or rely on Git hooks for
formatting.

## Testing

CausalMixGPD uses a **tiered testing system** to balance thoroughness with speed. For comprehensive documentation, see [tests/testthat/README.md](../tests/testthat/README.md).

### Quick Start

```r
# Run fast tests (default "cran" level)
devtools::test()

# Run integration tests including MCMC
Sys.setenv(DPMIXGPD_TEST_LEVEL = "ci")
devtools::test()

# Run exhaustive tests (all kernel/backend combinations)
Sys.setenv(DPMIXGPD_TEST_LEVEL = "full")
devtools::test()
```

### Test Tiers

| Level  | What Runs                     | When to Use             |
|--------|-------------------------------|-------------------------|
| `cran` | Fast unit tests only          | Development, quick CI   |
| `ci`   | + MCMC integration tests      | PR validation           |
| `full` | + Exhaustive kernel combos    | Pre-release             |

### Writing Tests

- Place tests in `tests/testthat/test-*.R`
- Use `skip_if_not_test_level("ci")` for MCMC-dependent tests
- Use `skip_if_not_full()` for exhaustive tests
- Use `mcmc_fast()` helper for minimal MCMC settings
- Set random seeds for reproducibility

### Coverage

```r
source("tools/.Rscripts/coverage.R")

# Generate local coverage report
coverage_report()

# Tests only (fastest)
coverage_report(sources = "tests")

# All sources (tests + examples + vignettes)
coverage_report(sources = "all")

# Upload to Codecov (requires CODECOV_TOKEN)
coverage_upload()

# Generate locally and upload in one step
coverage_push()
```

Coverage reports are generated canonically in `covr/assets/` and mirrored to
`docs/coverage/` for the published site. The interactive `report.html` also
includes a generated `lib/` dependency directory in both retained locations.

Coverage generation remains local. Running `coverage_report()` or
`coverage_push()` now also writes `covr/assets/cobertura.xml`, which is the
committed report consumed by the GitHub Actions Codecov upload workflow on each
push. The workflow does not generate coverage remotely; it uploads the checked-in
local Cobertura XML when present and exits successfully if no valid report is
available.

## Reporting Issues

- Search existing issues first.
- Include reproducible examples (minimal data + code), session info, and expected vs actual behavior.
- File issues here: https://github.com/arnabaich96/CausalMixGPD_Package/issues

## Code of Conduct

By participating, you agree to abide by our [Code of Conduct](CODE_OF_CONDUCT.md). Please report unacceptable behavior to the Maintainer email in DESCRIPTION.

## License

Contributions are licensed under the project’s license (GPL-3). By submitting a PR, you agree your contributions will be licensed as GPL-3.
