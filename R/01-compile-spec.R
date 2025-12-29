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
#' @param X Optional design matrix (N x P). Not used unless regression \code{type="link"} is requested.
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
compile_model_spec <- function(
    y,
    X = NULL,
    backend = c("sb", "crp"),
    kernel,
    GPD = FALSE,
    Kmax = NULL,
    J = NULL,
    components = NULL,
    mcmc = list(),
    param_specs = NULL,
    alpha_random = TRUE,
    priors = list()
) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  .stopf <- function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)

  backend <- match.arg(backend)

  # -----------------------------
  # Inputs
  # -----------------------------
  if (is.null(y) || length(y) < 2) .stopf("`y` must be a numeric vector of length >= 2.")
  y <- as.numeric(y)
  N <- length(y)

  has_X <- !is.null(X)
  if (has_X) {
    X <- as.matrix(X)
    if (nrow(X) != N) .stopf("`X` must have nrow(X) == length(y). Got %d vs %d.", nrow(X), N)
    P <- as.integer(ncol(X))
    if (P < 1L) .stopf("`X` must have at least one column.")
  } else {
    P <- 0L
  }

  # -----------------------------
  # Registry + kernel definition
  # -----------------------------
  kreg <- get_kernel_registry()
  if (!kernel %in% names(kreg)) .stopf("Unknown kernel='%s'.", kernel)
  kdef <- kreg[[kernel]]

  # -----------------------------
  # Backend sizes
  # -----------------------------
  if (identical(backend, "crp")) {
    if (is.null(Kmax)) Kmax <- N
    Kmax <- as.integer(Kmax)
    if (Kmax < 2L) .stopf("`Kmax` must be >= 2.")
    J_use <- NULL
  } else {
    # backend == "sb"
    if (!is.null(components) && is.null(J)) J <- components
    if (is.null(J)) J <- min(max(10L, 2L), N)
    J_use <- as.integer(J)
    if (J_use < 2L) .stopf("`J` must be >= 2 for backend='sb'.")
    # Keep Kmax defined for compatibility, even if unused by SB
    if (is.null(Kmax)) Kmax <- N
    Kmax <- as.integer(Kmax)
  }

  # -----------------------------
  # Priors (defaults + override)
  # -----------------------------
  pri_default <- list(
    # DP concentration:
    alpha_shape = 1,
    alpha_rate  = 1,

    # generic bulk priors:
    normal_mean = 0,
    normal_sd   = 10,
    gamma_shape = 2,
    gamma_rate  = 1,

    # threshold prior (scalar)
    threshold_meanlog = 0,
    threshold_sdlog   = 1,

    # threshold regression (if X)
    beta_threshold_mean = 0,
    beta_threshold_sd   = 10,

    # tail priors
    tail_scale_sdlog = 1,
    tail_shape_sd    = 1,

    # optional nested bulk priors (CRP builder supports this pattern)
    bulk = list()
  )
  pri <- modifyList(pri_default, priors)

  # -----------------------------
  # Bulk/tail node plan (lightweight)
  # -----------------------------
  bulk_params <- kdef$bulk_params %||% character(0)

  bulk_support <- kdef$bulk_support %||% list()
  bulk_plan <- list()
  for (pn in bulk_params) {
    sup <- bulk_support[[pn]] %||% "real"
    fam0 <- if (grepl("^positive", sup)) "gamma" else "normal"
    bulk_plan[[pn]] <- list(type = "stochastic", support = sup, prior_family = fam0)
  }

  # Apply optional param_specs overrides
  if (!is.null(param_specs) && is.list(param_specs) && !is.null(param_specs$bulk)) {
    for (nm in intersect(names(param_specs$bulk), names(bulk_plan))) {
      bulk_plan[[nm]] <- modifyList(bulk_plan[[nm]], param_specs$bulk[[nm]])
    }
  }

  tail_plan <- NULL
  if (isTRUE(GPD)) {
    tail_plan <- list(
      threshold  = list(type = if (has_X) "link" else "stochastic", support = "real",
                        prior_family = if (has_X) "normal" else "lognormal"),
      tail_scale = list(type = "stochastic", support = "positive_scale", prior_family = "lognormal"),
      tail_shape = list(type = "stochastic", support = "real", prior_family = "normal")
    )
    if (!is.null(param_specs) && is.list(param_specs) && !is.null(param_specs$tail)) {
      for (nm in intersect(names(param_specs$tail), names(tail_plan))) {
        tail_plan[[nm]] <- modifyList(tail_plan[[nm]], param_specs$tail[[nm]])
      }
    }
  }

  node_plan <- list(
    dp = list(nodes = list(
      stochastic    = if (identical(backend, "sb")) c("alpha", "v") else c("alpha", "z"),
      deterministic = if (identical(backend, "sb")) c("weights") else character(0)
    )),
    bulk = bulk_plan,
    tail = tail_plan
  )

  # -----------------------------
  # Likelihood dispatch (YOUR RULE)
  #   CRP: always non-mix (base or *Gpd)
  #   SB : always mix (mix or mix*gpd)
  # -----------------------------
  if (identical(backend, "sb")) {
    dens_name <- if (isTRUE(GPD)) kdef$sb$d_gpd else kdef$sb$d
  } else {
    dens_name <- if (isTRUE(GPD)) kdef$crp$d_gpd else kdef$crp$d_base
  }

  dispatch <- list(
    backend = backend,
    kernel  = kernel,
    GPD     = isTRUE(GPD),
    density = dens_name
  )

  # -----------------------------
  # Assemble spec
  # -----------------------------
  spec <- list(
    N = as.integer(N),
    P = as.integer(P),
    p = as.integer(P),          # legacy alias
    Kmax = as.integer(Kmax),
    meta = list(
      backend = backend,
      kernel  = kernel,
      GPD     = isTRUE(GPD),
      has_X   = has_X,
      N       = as.integer(N),
      P       = as.integer(P),
      p       = as.integer(P),  # legacy alias
      Kmax    = as.integer(Kmax),
      J       = if (identical(backend, "sb")) as.integer(J_use) else NULL,
      alpha_random = isTRUE(alpha_random)
    ),
    data = list(y = y, X = X),
    kernel = kdef,
    dispatch = dispatch,
    priors = pri,
    node_plan = node_plan,
    mcmc = mcmc
  )

  class(spec) <- "dpmixgpd_spec"
  spec
}


