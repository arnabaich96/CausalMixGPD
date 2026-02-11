## Where to change what (quick routing)

| You want toâ€¦ | Start here | Then check |
|---|---|---|
| Add a new kernel (bulk distribution) | `R/00-kernel-registry.R` | `R/*-mixgpd.R`, tests under `tests/testthat/`, kernel docs under `website/kernels/` |
| Add/modify a tail option (GPD behavior) | `R/0-base-kernels.R` | contracts in `R/00-global-contracts.R`, vignette pages |
| Change SB/CRP code generation | `R/03-build-and-run.R` | tests comparing backends |
| Change causal pipeline | `R/05-causal.R` | `qte()`, `ate()`, S3 methods in `R/04-S3-Methods.R` |
| Change printing/summary/plot | `R/04-S3-Methods.R` | plotting helpers `R/06-visualization-helpers.R` |
| Update website navigation | `website/_quarto.yml` | landing hubs like `website/developers.qmd`, `website/Examples/index.qmd` |

