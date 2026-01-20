## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.align = "center"
)
library(ggplot2)
library(kableExtra)
set.seed(1)

## -----------------------------------------------------------------------------
library(DPmixGPD)

y <- abs(rnorm(40)) + 0.2
X <- data.frame(x = rnorm(40))

bundle <- build_nimble_bundle(
  y = y,
  X = X,
  backend = "sb",
  kernel  = "normal",
  GPD     = TRUE,
  components = 6,
  mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 1, seed = 1)
)

bundle

## ----echo=FALSE---------------------------------------------------------------
args_df <- data.frame(
  Argument = c("backend", "backend", "kernel", "GPD", "GPD"),
  Value = c("sb", "crp", "(various)", "TRUE", "FALSE"),
  Description = c(
    "Stick-breaking mixture (finite truncation)",
    "Chinese Restaurant Process mixture",
    "Bulk component family (normal, gamma, lognormal, ...)",
    "Splice GPD tail beyond threshold",
    "Bulk-only model"
  )
)

kable(args_df, align = "c", caption = "Model Specification Arguments") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE, position = "center")

## ----eval=FALSE---------------------------------------------------------------
# # Rename reserved keywords
# names(X)[names(X) == "if"] <- "x_if"

