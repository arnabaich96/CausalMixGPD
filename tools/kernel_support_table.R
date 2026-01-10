devtools::load_all(".")
tab <- kernel_support_table()
cat("# Kernel support matrix\n\n")
cat(knitr::kable(tab, format = "markdown"), sep = "\n")
