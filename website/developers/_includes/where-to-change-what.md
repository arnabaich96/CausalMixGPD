## Where to change what (quick routing)

| You want to... | Start here | Then check |
|---|---|---|
| Add a new kernel (bulk distribution) | `R/00-kernel-registry.R` | `R/*-mixgpd.R`, tests in `tests/testthat/`, kernel docs in `website/kernels/` |
| Add or modify a tail option | `R/0-base-kernels.R` | contracts in `R/00-global-contracts.R`, docs in `website/advanced/` and `website/start/` |
| Change SB or CRP code generation | `R/03-build-and-run.R` | backend comparison tests and example pages |
| Change causal pipeline | `R/05-causal.R` | `qte()`, `ate()`, and S3 methods in `R/04-S3-Methods.R` |
| Change print, summary, or plot methods | `R/04-S3-Methods.R` | helpers in `R/06-visualization-helpers.R` |
| Update website navigation | `website/_quarto.yml` | hubs like `website/developers/index.qmd`, `website/start/index.qmd`, and `website/examples/index.qmd` |
