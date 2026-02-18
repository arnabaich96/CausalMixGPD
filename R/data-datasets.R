#' nc_real200_k2 dataset
#'
#' Real-line, bulk-only mixture dataset with K=2 components and no covariates.
#' Intended for non-causal bulk-only vignettes (normal/laplace/cauchy, GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{X}{NULL.}
#'   \item{meta}{List with n, support, p, K_true, tail, exceed_frac, seed.}
#'   \item{truth}{List with kernel, weights, params, threshold, tail_params.}
#' }
#' @examples
#' data("nc_real200_k2")
#' head(nc_real200_k2$y)
#' @keywords datasets
"nc_real200_k2"

#' nc_pos200_k3 dataset
#'
#' Positive-support, bulk-only mixture dataset with K=3 components and no covariates.
#' Intended for non-causal bulk-only positive-kernel vignettes (gamma/lognormal/invgauss/amoroso).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{X}{NULL.}
#'   \item{meta}{List with n, support, p, K_true, tail, exceed_frac, seed.}
#'   \item{truth}{List with kernel, weights, params, threshold, tail_params.}
#' }
#' @examples
#' data("nc_pos200_k3")
#' head(nc_pos200_k3$y)
#' @keywords datasets
"nc_pos200_k3"

#' nc_pos_tail200_k4 dataset
#'
#' Positive-support, tail-designed mixture dataset with K=4 components and no covariates.
#' Intended for GPD vignettes (gamma/lognormal/invgauss/amoroso with GPD=TRUE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{X}{NULL.}
#'   \item{meta}{List with n, support, p, K_true, tail, exceed_frac, seed.}
#'   \item{truth}{List with kernel, weights, params, threshold, tail_params.}
#' }
#' @examples
#' data("nc_pos_tail200_k4")
#' head(nc_pos_tail200_k4$y)
#' @keywords datasets
"nc_pos_tail200_k4"

#' nc_posX100_p3_k2 dataset
#'
#' Positive-support dataset with covariates (p=3) and K=2 mixture components.
#' Intended for covariate and prediction vignettes (GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{X}{data.frame with x1-x3.}
#'   \item{meta}{List with n, support, p, K_true, tail, exceed_frac, seed.}
#'   \item{truth}{List with kernel, weights, params, threshold, tail_params.}
#' }
#' @examples
#' data("nc_posX100_p3_k2")
#' head(nc_posX100_p3_k2$X)
#' @keywords datasets
"nc_posX100_p3_k2"

#' nc_posX100_p4_k3 dataset
#'
#' Positive-support dataset with covariates (p=4) and K=3 mixture components.
#' Intended for covariate and prediction vignettes (GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{X}{data.frame with x1-x4.}
#'   \item{meta}{List with n, support, p, K_true, tail, exceed_frac, seed.}
#'   \item{truth}{List with kernel, weights, params, threshold, tail_params.}
#' }
#' @examples
#' data("nc_posX100_p4_k3")
#' head(nc_posX100_p4_k3$X)
#' @keywords datasets
"nc_posX100_p4_k3"

#' nc_posX100_p5_k4 dataset
#'
#' Positive-support dataset with covariates (p=5) and K=4 mixture components.
#' Intended for covariate and prediction vignettes (GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{X}{data.frame with x1-x5.}
#'   \item{meta}{List with n, support, p, K_true, tail, exceed_frac, seed.}
#'   \item{truth}{List with kernel, weights, params, threshold, tail_params.}
#' }
#' @examples
#' data("nc_posX100_p5_k4")
#' head(nc_posX100_p5_k4$X)
#' @keywords datasets
"nc_posX100_p5_k4"

#' nc_realX100_p3_k2 dataset
#'
#' Real-line dataset with covariates (p=3) and K=2 mixture components.
#' Intended for covariate and prediction vignettes (GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{X}{data.frame with x1-x3.}
#'   \item{meta}{List with n, support, p, K_true, tail, exceed_frac, seed.}
#'   \item{truth}{List with kernel, weights, params, threshold, tail_params.}
#' }
#' @examples
#' data("nc_realX100_p3_k2")
#' head(nc_realX100_p3_k2$X)
#' @keywords datasets
"nc_realX100_p3_k2"

#' nc_realX100_p5_k3 dataset
#'
#' Real-line dataset with covariates (p=5) and K=3 mixture components.
#' Intended for covariate and prediction vignettes (GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{X}{data.frame with x1-x5.}
#'   \item{meta}{List with n, support, p, K_true, tail, exceed_frac, seed.}
#'   \item{truth}{List with kernel, weights, params, threshold, tail_params.}
#' }
#' @examples
#' data("nc_realX100_p5_k3")
#' head(nc_realX100_p5_k3$X)
#' @keywords datasets
"nc_realX100_p5_k3"

#' causal_pos500_p3_k2 dataset
#'
#' Causal dataset (N=500, p=3) with the same positive-support kernel for both arms.
#' Intended for same-kernel causal baselines (GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{A}{Binary treatment indicator (0/1).}
#'   \item{X}{data.frame with x1-x3.}
#'   \item{meta}{List with N, support, p, K0, K1, tail, exceed_frac.}
#'   \item{truth}{List with kernel0, kernel1, params0, params1, tail_params.}
#' }
#' @examples
#' data("causal_pos500_p3_k2")
#' head(causal_pos500_p3_k2$X)
#' @keywords datasets
"causal_pos500_p3_k2"

#' causal_alt_pos500_p3_k3 dataset
#'
#' Causal dataset (N=500, p=3) with different positive-support kernels by arm.
#' Intended for alternating-kernel causal vignettes (GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{A}{Binary treatment indicator (0/1).}
#'   \item{X}{data.frame with x1-x3.}
#'   \item{meta}{List with N, support, p, K0, K1, tail, exceed_frac.}
#'   \item{truth}{List with kernel0, kernel1, params0, params1, tail_params.}
#' }
#' @examples
#' data("causal_alt_pos500_p3_k3")
#' head(causal_alt_pos500_p3_k3$X)
#' @keywords datasets
"causal_alt_pos500_p3_k3"

#' causal_alt_real500_p4_k2 dataset
#'
#' Causal dataset (N=500, p=4) with different real-line kernels by arm.
#' Intended for alternating-kernel causal vignettes (GPD=FALSE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{A}{Binary treatment indicator (0/1).}
#'   \item{X}{data.frame with x1-x4.}
#'   \item{meta}{List with N, support, p, K0, K1, tail, exceed_frac.}
#'   \item{truth}{List with kernel0, kernel1, params0, params1, tail_params.}
#' }
#' @examples
#' data("causal_alt_real500_p4_k2")
#' head(causal_alt_real500_p4_k2$X)
#' @keywords datasets
"causal_alt_real500_p4_k2"

#' causal_alt_pos500_p5_k4_tail dataset
#'
#' Causal dataset (N=500, p=5) with different positive-support kernels by arm
#' and tail-designed exceedances (GPD=TRUE).
#'
#' @format A list with:
#' \describe{
#'   \item{y}{Numeric outcome vector.}
#'   \item{A}{Binary treatment indicator (0/1).}
#'   \item{X}{data.frame with x1-x5.}
#'   \item{meta}{List with N, support, p, K0, K1, tail, exceed_frac.}
#'   \item{truth}{List with kernel0, kernel1, params0, params1, tail_params.}
#' }
#' @examples
#' data("causal_alt_pos500_p5_k4_tail")
#' head(causal_alt_pos500_p5_k4_tail$X)
#' @keywords datasets
"causal_alt_pos500_p5_k4_tail"
