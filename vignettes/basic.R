## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  echo = FALSE
)

## ----echo=FALSE---------------------------------------------------------------
library(DPmixGPD)
data("nc_pos_tail200_k4")
str(nc_pos_tail200_k4$meta)
str(nc_pos_tail200_k4$truth)

