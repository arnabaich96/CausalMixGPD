## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
if (requireNamespace('devtools', quietly = TRUE)) devtools::load_all(quiet = TRUE) else library(DPmixGPD)
library(nimble)

# Helper functions
resolve_data_file <- function(fname) {
  .pkg_root <- knitr::opts_knit$get("root.dir")
  if (is.null(.pkg_root) || is.na(.pkg_root)) {
    .pkg_root <- normalizePath(file.path(dirname(knitr::current_input()), ".."), winslash = "/", mustWork = FALSE)
  }
  candidates <- unique(c(
    system.file("data", fname, package = "DPmixGPD"),
    file.path(.pkg_root, "data", fname)
  ))
  existing <- candidates[file.exists(candidates) & nzchar(candidates)]
  if (length(existing) > 0) return(existing[[1]])
  NULL
}

print_dataset_header <- function(data, name = deparse(substitute(data))) {
  cat(sprintf("Dataset: %s (n=%d", name, length(data$y)))
  if (!is.null(data$X)) cat(sprintf(", p=%d", ncol(data$X)))
  cat(")\n")
  invisible(NULL)
}

default_mcmc <- function() {
  list(niter = 1000, nburnin = 500, nchains = 2, thin = 1)
}


## ----data, eval=FALSE---------------------------------------------------------
# .kg <- knitr::knit_global()
# if (!exists("causal_pos500_p3_k2", envir = .kg, inherits = FALSE)) {
#   utils::data(list = "causal_pos500_p3_k2", package = "DPmixGPD", envir = .kg)
# }
# if (!exists("causal_pos500_p3_k2", envir = .kg, inherits = FALSE)) {
#   path <- resolve_data_file("causal_pos500_p3_k2.rda")
#   if (!is.null(path)) load(path, envir = .kg)
# }
# stopifnot(exists("causal_pos500_p3_k2", envir = .kg, inherits = FALSE))
# causal_pos500_p3_k2 <- get("causal_pos500_p3_k2", envir = .kg, inherits = FALSE)
# print_dataset_header(causal_pos500_p3_k2, "causal_pos500_p3_k2")

## ----qte, eval=FALSE----------------------------------------------------------
# mcmc <- default_mcmc()
# cb <- build_causal_bundle(
#   y = causal_pos500_p3_k2$y,
#   X = causal_pos500_p3_k2$X,
#   T = causal_pos500_p3_k2$T,
#   backend = c("sb", "sb"),
#   kernel = c("gamma", "gamma"),
#   GPD = c(FALSE, FALSE),
#   J = c(causal_pos500_p3_k2$meta$K0, causal_pos500_p3_k2$meta$K1),
#   mcmc_outcome = mcmc,
#   mcmc_ps = mcmc
# )
# cf <- run_mcmc_causal(cb, show_progress = FALSE)
# 
# qte(cf, probs = c(0.1, 0.25, 0.5, 0.9))
# 
# # Average treatment effect (posterior mean)
# ate(cf)

