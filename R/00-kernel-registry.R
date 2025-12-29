#' Initialize kernel registries
#'
#' Creates/refreshes the internal kernel and tail registries in the package namespace.
#' This is called on package load via \code{.onLoad()}.
#'
#' @return Invisibly returns \code{TRUE}.
#' @keywords internal
#' @noRd
#' @export
init_kernel_registry <- function() {
  ns <- asNamespace("DPmixGPD")

  kernel_registry <- list(
    normal = list(
      key = "normal",
      bulk_params = c("mean", "sd"),
      bulk_support = c(mean = "real", sd = "positive_sd"),
      allow_gpd = TRUE,
      # SB always uses MIX densities
      sb  = list(d = "dNormMix",      d_gpd = "dNormMixGpd"),
      # CRP always uses non-mix densities
      crp = list(d_base = "dnorm",    d_gpd = "dNormGpd")
    ),

    gamma = list(
      key = "gamma",
      bulk_params = c("shape", "scale"),
      bulk_support = c(shape = "positive_shape", scale = "positive_scale"),
      allow_gpd = TRUE,
      sb  = list(d = "dGammaMix",     d_gpd = "dGammaMixGpd"),
      crp = list(d_base = "dgamma",   d_gpd = "dGammaGpd")
    ),

    lognormal = list(
      key = "lognormal",
      bulk_params = c("meanlog", "sdlog"),
      bulk_support = c(meanlog = "real", sdlog = "positive_sd"),
      allow_gpd = TRUE,
      sb  = list(d = "dLognormalMix", d_gpd = "dLognormalMixGpd"),
      crp = list(d_base = "dlnorm",   d_gpd = "dLognormalGpd")
    ),

    laplace = list(
      key = "laplace",
      bulk_params = c("location", "scale"),
      bulk_support = c(location = "real", scale = "positive_scale"),
      allow_gpd = TRUE,
      sb  = list(d = "dLaplaceMix",   d_gpd = "dLaplaceMixGpd"),
      crp = list(d_base = "ddexp",    d_gpd = "dLaplaceGpd")
    ),

    invgauss = list(
      key = "invgauss",
      bulk_params = c("mean", "shape"),
      bulk_support = c(mean = "positive_mean", shape = "positive_shape"),
      allow_gpd = TRUE,
      sb  = list(d = "dInvGaussMix",  d_gpd = "dInvGaussMixGpd"),
      crp = list(d_base = "dInvGauss", d_gpd = "dInvGaussGpd")
    ),

    amoroso = list(
      key = "amoroso",
      bulk_params = c("loc", "scale", "shape1", "shape2"),
      # scale is REAL in your implementation (can be negative)
      bulk_support = c(loc = "real", scale = "real",
                       shape1 = "positive_shape", shape2 = "positive_shape"),
      allow_gpd = TRUE,
      sb  = list(d = "dAmorosoMix",   d_gpd = "dAmorosoMixGpd"),
      crp = list(d_base = "dAmoroso", d_gpd = "dAmorosoGpd")
    ),

    cauchy = list(
      key = "cauchy",
      bulk_params = c("location", "scale"),
      bulk_support = c(location = "real", scale = "positive_scale"),
      allow_gpd = FALSE,
      # SB uses MIX density even when no GPD is allowed
      sb  = list(d = "dCauchyMix",    d_gpd = NA_character_),
      crp = list(d_base = "dcauchy",  d_gpd = NA_character_)
    )
  )

  tail_registry <- list(
    params = c("threshold", "tail_scale", "tail_shape"),
    support = c(threshold = "real", tail_scale = "positive_scale", tail_shape = "real"),
    indexed_by_cluster_in_crp = FALSE
  )

  assign("kernel_registry", kernel_registry, envir = ns)
  assign("tail_registry", tail_registry, envir = ns)

  invisible(TRUE)
}


#' Get kernel registry
#'
#' Returns the internal kernel registry from the package namespace.
#'
#' @return A named list.
#' @keywords internal
#' @noRd
#' @export
get_kernel_registry <- function() {
  get("kernel_registry", envir = asNamespace("DPmixGPD"))
}

#' Get tail registry
#'
#' Returns the internal tail registry from the package namespace.
#'
#' @return A list.
#' @keywords internal
#' @noRd
#' @export
get_tail_registry <- function() {
  get("tail_registry", envir = asNamespace("DPmixGPD"))
}
