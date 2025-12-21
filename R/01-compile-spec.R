#' Compile a DPmixGPD model specification (internal)
#'
#' Creates an explicit, validated "spec" object describing nodes, dimensions,
#' priors, links (if any), monitor names, and init shapes. This function does
#' not generate nimbleCode or run MCMC.
#'
#' @param y Numeric outcome vector of length N.
#' @param backend Either \code{"sb"} or \code{"crp"}.
#' @param kernel Kernel key in the internal registry.
#' @param GPD Logical; whether to use GPD augmentation.
#' @param X Optional design matrix (N x p). Not used unless regression \code{type="link"} is requested.
#' @param components Alias for \code{J} when backend="sb".
#' @param J For SB backend only, number of mixture components (>= 2).
#' @param Kmax For CRP backend only, truncation level for cluster parameters. Default is N.
#' @param param_specs Optional list with entries \code{bulk} and \code{tail}.
#' @param mcmc Optional MCMC settings (accepted for internal API compatibility; not used here).
#' @param alpha_random Logical; whether concentration \code{alpha} is stochastic.
#'
#' @return A \code{dpmixgpd_spec} list.
#' @keywords internal
#' @noRd
compile_model_spec <- function(y,
                               X = NULL,
                               backend = c("sb","crp"),
                               kernel,
                               GPD = FALSE,
                               Kmax = 10L,
                               mcmc = list(),
                               # optional inputs used by build_nimble_bundle()
                               param_specs = NULL,
                               alpha_random = TRUE,
                               priors = list()) {

  backend <- match.arg(backend)

  if (is.null(y) || length(y) < 2) .stopf("`y` must be a numeric vector of length >= 2.")
  y <- as.numeric(y)
  N <- length(y)

  has_X <- !is.null(X)
  if (has_X) {
    X <- as.matrix(X)
    if (nrow(X) != N) .stopf("`X` must have nrow(X) == length(y). Got %d vs %d.", nrow(X), N)
    p <- ncol(X)
    if (p < 1) .stopf("`X` must have at least one column.")
  } else {
    p <- 0L
  }

  Kmax <- as.integer(Kmax)
  if (Kmax < 2L) .stopf("`Kmax` must be >= 2.")

  kreg <- get_kernel_registry()
  if (!kernel %in% names(kreg)) .stopf("Unknown kernel='%s'.", kernel)
  kdef <- kreg[[kernel]]

  # sanity checks for dispatch entries
  if (isTRUE(GPD) && is.null(kdef$crp$d_gpd) && backend == "crp") {
    .stopf("Kernel '%s' does not define a CRP+GPD density dispatcher.", kernel)
  }
  if (isFALSE(GPD) && is.null(kdef$crp$d_base) && backend == "crp") {
    .stopf("Kernel '%s' does not define a CRP bulk density dispatcher.", kernel)
  }
  if (isTRUE(GPD) && is.null(kdef$sb$d_gpd) && backend == "sb") {
    .stopf("Kernel '%s' does not define an SB+GPD density dispatcher.", kernel)
  }
  if (isFALSE(GPD) && is.null(kdef$sb$d) && backend == "sb") {
    .stopf("Kernel '%s' does not define an SB bulk density dispatcher.", kernel)
  }

  # ---- dispatch: what distribution call to use for y[i] ~ d...( ... ) ----
  dispatch <- list(
    backend = backend,
    kernel  = kernel,
    GPD     = isTRUE(GPD),
    bulk_params = kdef$bulk_params %||% character(0),
    density = switch(
      backend,
      crp = if (isTRUE(GPD)) kdef$crp$d_gpd else kdef$crp$d_base,
      sb  = if (isTRUE(GPD)) kdef$sb$d_gpd  else kdef$sb$d
    )
  )

  # parameters that appear in y[i] ~ d...( ... )
  # NOTE: `z[i]` is NEVER a parameter of the distribution; it is the mixture index.
  dispatch$params <- c(dispatch$bulk_params, if (isTRUE(GPD)) c("threshold","tail_scale","tail_shape") else character(0))

  spec <- list(
    meta = list(
      backend = backend,
      kernel  = kernel,
      GPD     = isTRUE(GPD),
      has_X   = has_X,
      N       = N,
      p       = as.integer(p),
      Kmax    = Kmax
    ),
    data = list(y = y, X = X),
    kernel = kdef,
    dispatch = dispatch,
    priors = list(
      alpha_shape = 1,
      alpha_rate  = 1,
      # generic weakly-informative defaults used by builders
      loc_sd      = 10,
      scale_rate  = 1,
      shape_rate  = 1,
      positive_meanlog = 0,
      positive_sdlog   = 1,
      beta_sd     = 10
    ),
    mcmc = mcmc
  )

  spec
}

