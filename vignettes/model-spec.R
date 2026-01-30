## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  fig.align = "center"
)
library(ggplot2)
library(kableExtra)
set.seed(1)

## -----------------------------------------------------------------------------
library(DPmixGPD)

data("faithful", package = "datasets")
y <- faithful$eruptions

bundle_uncond <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel  = "normal",
  GPD     = TRUE,
  components = 6,
  mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 1, seed = 1)
)

## -----------------------------------------------------------------------------
data("mtcars", package = "datasets")
df <- mtcars
X <- df[, c("wt", "hp")]
X <- as.data.frame(X)
y <- df$mpg

bundle_cond <- build_nimble_bundle(
  y = y,
  X = X,
  backend = "sb",
  kernel  = "normal",
  GPD     = TRUE,
  components = 6,
  mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 1, seed = 1)
)

## ----echo=FALSE---------------------------------------------------------------
args_df <- data.frame(
  Argument = c("backend", "backend", "kernel", "GPD", "GPD", "components", "alpha_random"),
  Value = c("sb", "crp", "(various)", "TRUE", "FALSE", "K", "TRUE/FALSE"),
  Description = c(
    "Stick-breaking mixture (finite truncation)",
    "Chinese Restaurant Process mixture",
    "Bulk component family (normal, gamma, lognormal, ...)",
    "Splice GPD tail beyond threshold",
    "Bulk-only model",
    "Truncation level for SB (maximum components)",
    "Learn concentration parameter (DP strength)"
  )
)

kable(args_df, align = "c", caption = "Model Specification Arguments") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE, position = "center")

## ----eval=FALSE---------------------------------------------------------------
# # Rename reserved keywords
# names(X)[names(X) == "if"] <- "x_if"

