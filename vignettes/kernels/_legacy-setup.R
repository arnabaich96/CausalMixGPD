# Kernel vignette shared setup
# This file is sourced by all kernel vignettes to provide consistent settings.

# Disable knitr caching in non-interactive renders to avoid serializing
# NIMBLE objects with external pointers (which can break pkgdown builds).
if (!interactive() && requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_chunk$set(cache = FALSE)
}

# Ensure ggplot2 functions are available in kernel vignettes without
# requiring each file to attach the package explicitly.
if (requireNamespace("ggplot2", quietly = TRUE)) {
  suppressPackageStartupMessages(library(ggplot2))
}

# kableExtra helpers are used in several vignettes.
if (requireNamespace("kableExtra", quietly = TRUE)) {
  suppressPackageStartupMessages(library(kableExtra))
}

# tibble is used directly in a few vignettes.
if (requireNamespace("tibble", quietly = TRUE)) {
  suppressPackageStartupMessages(library(tibble))
}

# gridExtra provides grid.arrange used in some plots.
if (requireNamespace("gridExtra", quietly = TRUE)) {
  suppressPackageStartupMessages(library(gridExtra))
}

# dplyr verbs are used in a few tables.
if (requireNamespace("dplyr", quietly = TRUE)) {
  suppressPackageStartupMessages(library(dplyr))
}
