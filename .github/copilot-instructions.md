# Copilot Instructions for DPmixGPD

## Overview
This repository implements the DPmixGPD package, which utilizes a three-phase model-building paradigm for statistical modeling. The architecture is designed for clarity and control, focusing on the following phases:

1. **Specification**: Validate inputs without generating NIMBLE code.
2. **Bundle**: Generate NIMBLE code and compile the sampler.
3. **MCMC**: Execute posterior sampling.

## Key Components
- **Core Workflows**: The main workflows are defined in `tests/testthat/test-smoke-core-workflows.R`, which covers essential user interactions and regression tests.
- **Vignettes**: Documentation and examples are provided in the `vignettes/` directory, with a recommended reading order outlined in `Misc/README.md`.

## Developer Workflows
### Building the Site
To build the documentation site, use:
```r
devtools::load_all(quiet = TRUE)
pkgdown::build_site(preview = FALSE)
```

### Testing
Run the regression tests with:
```r
testthat::test_file("tests/testthat/test-smoke-core-workflows.R")
```

### Pre-Release Checklist
Before releasing, follow this workflow:
1. **Check**: Review `DOCS_QA.md` for a quick reference checklist.
2. **Build**: Execute the build command as shown above.
3. **Test**: Run the tests as described.
4. **Verify**: Spot-check the built site in a browser.
5. **Deploy**: Commit and push the changes to the `docs/` directory.

## Project-Specific Conventions
- **Model Specification**: Use `compile_model_spec()` for input validation and model specification.
- **NIMBLE Bundling**: Use `build_nimble_bundle()` to generate and compile the model.
- **MCMC Execution**: Use `run_mcmc_bundle_manual()` for posterior sampling.

## Integration Points
The package integrates with external dependencies such as NIMBLE for model compilation and sampling. Ensure that these dependencies are correctly installed and configured in your environment.

## Communication Patterns
Components communicate through well-defined function calls, with data flowing from specifications to bundles and then to MCMC execution. Ensure that inputs and outputs are correctly matched across these phases.

## Conclusion
This document serves as a guide for AI coding agents to navigate the DPmixGPD codebase effectively. For further details, refer to the specific vignettes and the README files in the `Misc/` directory.