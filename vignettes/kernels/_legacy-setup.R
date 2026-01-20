# Kernel vignette shared setup.
# Sourced by all kernel vignettes for consistent settings.

# Disable knitr caching in non-interactive renders to avoid serializing
# NIMBLE objects with external pointers (which can break pkgdown builds).
if (!interactive() && requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_chunk$set(cache = FALSE, fig.align = "center")
} else if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_chunk$set(fig.align = "center")
}

# Load ggplot2 for plotting.
if (requireNamespace("ggplot2", quietly = TRUE)) {
  suppressPackageStartupMessages(library(ggplot2))
}

# Load kableExtra for table formatting.
if (requireNamespace("kableExtra", quietly = TRUE)) {
  suppressPackageStartupMessages(library(kableExtra))
}

# Load tibble for data manipulation.
if (requireNamespace("tibble", quietly = TRUE)) {
  suppressPackageStartupMessages(library(tibble))
}

# Load patchwork for combining plots.
if (requireNamespace("patchwork", quietly = TRUE)) {
  suppressPackageStartupMessages(library(patchwork))
}

# Load dplyr for data manipulation.
if (requireNamespace("dplyr", quietly = TRUE)) {
  suppressPackageStartupMessages(library(dplyr))
}
