
library(testthat)
library(DPmixGPD)
library(nimble)
library(svglite)
library(vdiffr)
test_check("DPmixGPD")
testthat::set_max_fails(Inf)
testthat::set_reporter("summary")
