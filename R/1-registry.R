#+ filtered ###################################################################
#' Global contracts for CausalMixGPD
#'
#' These lists and helpers capture the modeling rules used throughout the
#' package: backends, kernels, GPD usage, and mixture sizes.
#' They are intentionally loaded before the rest of the core code so downstream
#' builders can reuse the same constants.
#'
#' Backend summary:
#' - "sb": Stick-breaking (truncated Dirichlet process) with mixture evaluation
#' - "crp": Chinese Restaurant Process with component-indexed parameters
#' - "spliced": CRP variant enforcing component-level GPD parameterization with
#'              flexible modes (fixed/dist/link) for threshold, tail_scale, tail_shape.
#'              Uses CRP signatures but allows link mode for tail params.
#'
#' @keywords internal
#' @noRd
allowed_backends <- c("crp", "sb", "spliced")
allowed_kernels <- c("gamma", "lognormal", "invgauss", "normal", "laplace", "cauchy", "amoroso")
positive_support_kernels <- c("gamma", "lognormal", "invgauss", "amoroso")
real_support_kernels <- c("normal", "laplace", "cauchy")

is_allowed_kernel <- function(x) {
  as.character(x) %in% allowed_kernels
}

check_gpd_contract <- function(GPD, kernel) {
  if (isTRUE(GPD) && identical(as.character(kernel), "cauchy")) {
    stop("Cauchy kernels are never paired with GPD tails.", call. = FALSE)
  }
  invisible(NULL)
}




#' Initialize kernel registries
#'
#' Creates/refreshes registries used by the model specification compiler and
#' code generators. Each kernel entry stores bulk parameters, supports, default
#' regression/link behavior, and distribution signatures for SB/CRP backends.
#'
#' @details
#' The kernel registry is the package-level contract that keeps model building,
#' prediction, and documentation aligned. Each entry records the natural bulk
#' parameters for one kernel, the support constraints they must satisfy, the
#' default covariate-link strategy, and the backend-specific distribution names
#' used when generating NIMBLE code.
#'
#' The companion tail registry records the generalized Pareto tail parameters
#' \eqn{u} (threshold), \eqn{\sigma_u} (tail scale), and \eqn{\xi_u} (tail
#' shape) together with their support and allowed modeling modes. Calling
#' `init_kernel_registry()` makes those contracts available in the package
#' namespace so later builders can validate requests without duplicating lookup
#' logic.
#'
#' @return Invisibly returns TRUE.
#' @examples
#' init_kernel_registry()
#' reg <- get_kernel_registry()
#' names(reg)
#' tail <- get_tail_registry()
#' tail$params
#' @export
init_kernel_registry <- function() {
  ns <- getNamespace("CausalMixGPD")

  has_ns <- exists("kernel_registry", envir = ns, inherits = FALSE) &&
    exists("tail_registry", envir = ns, inherits = FALSE)

  if (has_ns) {
    return(invisible(TRUE))
  }

  kernel_registry <- list(

    normal = list(
      key = "normal",
      bulk_params = c("mean", "sd"),
      bulk_support = c(mean = "real", sd = "positive_sd"),
      param_types = c(mean = "location", sd = "sd"),
      allow_gpd = TRUE,
      defaults_X = list(mean = list(mode = "link", link = "identity"),
                        sd   = list(mode = "dist")),
      sb = list(
        d = "dNormMix",
        d_gpd = "dNormMixGpd",
        args = c("w", "mean", "sd"),
        mean = "meanNormMix",
        mean_trunc = "meanNormMixTrunc",
        args_gpd = c("w", "mean", "sd", "threshold", "tail_scale", "tail_shape")
      ),
      crp = list(
        d_base = "dnorm",
        mean_base = "meanNormMix",
        mean_trunc_base = "meanNormMixTrunc",
        d_gpd = "dNormGpd",
        args_gpd = c("mean", "sd", "threshold", "tail_scale", "tail_shape")
      )
    ),

    lognormal = list(
      key = "lognormal",
      bulk_params = c("meanlog", "sdlog"),
      bulk_support = c(meanlog = "real", sdlog = "positive_sd"),
      param_types = c(meanlog = "location", sdlog = "sd"),
      allow_gpd = TRUE,
      defaults_X = list(meanlog = list(mode = "link", link = "identity"),
                        sdlog   = list(mode = "dist")),
      sb = list(
        d = "dLognormalMix",
        d_gpd = "dLognormalMixGpd",
        args = c("w", "meanlog", "sdlog"),
        mean = "meanLognormalMix",
        mean_trunc = "meanLognormalMixTrunc",
        args_gpd = c("w", "meanlog", "sdlog", "threshold", "tail_scale", "tail_shape")
      ),
      crp = list(
        d_base = "dlnorm",
        mean_base = "meanLognormalMix",
        mean_trunc_base = "meanLognormalMixTrunc",
        d_gpd = "dLognormalGpd",
        args_gpd = c("meanlog", "sdlog", "threshold", "tail_scale", "tail_shape")
      )
    ),

    invgauss = list(
      key = "invgauss",
      bulk_params = c("mean", "shape"),
      bulk_support = c(mean = "positive_location", shape = "positive_shape"),
      param_types = c(mean = "location", shape = "shape"),
      allow_gpd = TRUE,
      defaults_X = list(mean  = list(mode = "link", link = "exp"),
                        shape = list(mode = "dist")),
      sb = list(
        d = "dInvGaussMix",
        d_gpd = "dInvGaussMixGpd",
        args = c("w", "mean", "shape"),
        mean = "meanInvGaussMix",
        mean_trunc = "meanInvGaussMixTrunc",
        args_gpd = c("w", "mean", "shape", "threshold", "tail_scale", "tail_shape")
      ),
      crp = list(
        d_base = "dInvGauss",
        mean_base = "meanInvGaussMix",
        mean_trunc_base = "meanInvGaussMixTrunc",
        d_gpd = "dInvGaussGpd",
        args_gpd = c("mean", "shape", "threshold", "tail_scale", "tail_shape")
      )
    ),

    gamma = list(
      key = "gamma",
      bulk_params = c("shape", "scale"),
      bulk_support = c(shape = "positive_shape", scale = "positive_scale"),
      param_types = c(shape = "shape", scale = "scale"),
      allow_gpd = TRUE,
      defaults_X = list(shape = list(mode = "dist"),
                        scale = list(mode = "link", link = "exp")),
      sb = list(
        d = "dGammaMix",
        d_gpd = "dGammaMixGpd",
        args = c("w", "shape", "scale"),
        mean = "meanGammaMix",
        mean_trunc = "meanGammaMixTrunc",
        args_gpd = c("w", "shape", "scale", "threshold", "tail_scale", "tail_shape")
      ),
      crp = list(
        d_base = "dgamma",
        mean_base = "meanGammaMix",
        mean_trunc_base = "meanGammaMixTrunc",
        d_gpd = "dGammaGpd",
        args_gpd = c("shape", "scale", "threshold", "tail_scale", "tail_shape")
      )
    ),

    laplace = list(
      key = "laplace",
      bulk_params = c("location", "scale"),
      bulk_support = c(location = "real", scale = "positive_scale"),
      param_types = c(location = "location", scale = "scale"),
      allow_gpd = TRUE,
      defaults_X = list(location = list(mode = "link", link = "identity"),
                        scale    = list(mode = "dist")),
      sb = list(
        d = "dLaplaceMix",
        d_gpd = "dLaplaceMixGpd",
        args = c("w", "location", "scale"),
        mean = "meanLaplaceMix",
        mean_trunc = "meanLaplaceMixTrunc",
        args_gpd = c("w", "location", "scale", "threshold", "tail_scale", "tail_shape")
      ),
      crp = list(
        d_base = "ddexp",
        mean_base = "meanLaplaceMix",
        mean_trunc_base = "meanLaplaceMixTrunc",
        d_gpd = "dLaplaceGpd",
        args_gpd = c("location", "scale", "threshold", "tail_scale", "tail_shape")
      )
    ),

    amoroso = list(
      key = "amoroso",
      bulk_params = c("loc", "scale", "shape1", "shape2"),
      bulk_support = c(loc = "real", scale = "positive_scale",
                       shape1 = "positive_shape", shape2 = "positive_shape"),
      param_types = c(loc = "location", scale = "scale",
                      shape1 = "shape", shape2 = "shape"),
      allow_gpd = TRUE,
      defaults_X = list(
        loc    = list(mode = "link", link = "identity"),
        scale  = list(mode = "link", link = "exp"),
        shape1 = list(mode = "fixed", value = 1),
        shape2 = list(mode = "dist")
      ),
      sb = list(
        d = "dAmorosoMix",
        d_gpd = "dAmorosoMixGpd",
        args = c("w", "loc", "scale", "shape1", "shape2"),
        mean = "meanAmorosoMix",
        mean_trunc = "meanAmorosoMixTrunc",
        args_gpd = c("w", "loc", "scale", "shape1", "shape2",
                     "threshold", "tail_scale", "tail_shape")
      ),
      crp = list(
        d_base = "dAmoroso",
        mean_base = "meanAmorosoMix",
        mean_trunc_base = "meanAmorosoMixTrunc",
        d_gpd = "dAmorosoGpd",
        args_gpd = c("loc", "scale", "shape1", "shape2",
                     "threshold", "tail_scale", "tail_shape")
      )
    ),

    cauchy = list(
      key = "cauchy",
      bulk_params = c("location", "scale"),
      bulk_support = c(location = "real", scale = "positive_scale"),
      param_types = c(location = "location", scale = "scale"),
      allow_gpd = FALSE,
      defaults_X = list(location = list(mode = "link", link = "identity"),
                        scale    = list(mode = "dist")),
      sb = list(
        d = "dCauchyMix",
        d_gpd = NA_character_,
        args = c("w", "location", "scale"),
        mean = "meanCauchyMix",
        args_gpd = NA_character_
      ),
      crp = list(
        d_base = "dCauchy",
        mean_base = "meanCauchyMix",
        d_gpd = NA_character_,
        args_gpd = NA_character_
      )
    )
  )

  # ---- add signatures derived from sb/crp blocks ----
  for (k in names(kernel_registry)) {
    ki <- kernel_registry[[k]]

    sigs <- list()

    # SB signatures
    if (!is.null(ki$sb)) {
      sb_bulk_ok <- !is.null(ki$sb$d) && !is.null(ki$sb$args)
      sb_gpd_ok  <- !is.null(ki$sb$d_gpd) && !is.null(ki$sb$args_gpd) &&
        !isTRUE(is.na(ki$sb$d_gpd)) && !isTRUE(is.na(ki$sb$args_gpd))

      sigs$sb <- list(
        bulk = if (sb_bulk_ok) list(dist_name = ki$sb$d, args = ki$sb$args) else NULL,
        gpd  = if (sb_gpd_ok)  list(dist_name = ki$sb$d_gpd, args = ki$sb$args_gpd) else NULL
      )
    }

    # CRP signatures
    if (!is.null(ki$crp)) {
      crp_bulk_ok <- !is.null(ki$crp$d_base) && !isTRUE(is.na(ki$crp$d_base))
      crp_gpd_ok  <- !is.null(ki$crp$d_gpd) && !is.null(ki$crp$args_gpd) &&
        !isTRUE(is.na(ki$crp$d_gpd)) && !isTRUE(is.na(ki$crp$args_gpd))

      sigs$crp <- list(
        bulk = if (crp_bulk_ok) list(dist_name = ki$crp$d_base, args = ki$bulk_params) else NULL,
        gpd  = if (crp_gpd_ok)  list(dist_name = ki$crp$d_gpd, args = ki$crp$args_gpd) else NULL
      )
    }

    # Spliced signatures (identical to CRP, since spliced is a CRP variant)
    if (!is.null(ki$crp)) {
      sigs$spliced <- sigs$crp
    }

    kernel_registry[[k]]$signatures <- sigs
  }

  # Tail registry: GPD parameter metadata
  # indexed_by_cluster_in_crp=FALSE allows link mode for tail params in CRP/spliced
  # backends without NIMBLE sampler conflicts (tail params handled separately from
  # cluster-indexed bulk params that cause deterministic node issues with dCRP).
  tail_registry <- list(
    params = c("threshold", "tail_scale", "tail_shape"),
    support = c(threshold = "real", tail_scale = "positive_scale", tail_shape = "real"),
    indexed_by_cluster_in_crp = FALSE
  )

  ns_assign_ok <- TRUE
  tryCatch({
    assign("kernel_registry", kernel_registry, envir = ns)
    assign("tail_registry", tail_registry, envir = ns)
  }, error = function(e) {
    ns_assign_ok <<- FALSE
  })

  if (!ns_assign_ok ||
      !exists("kernel_registry", envir = ns, inherits = FALSE) ||
      !exists("tail_registry", envir = ns, inherits = FALSE)) {
    stop("Failed to initialize kernel/tail registry in package namespace.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Get kernel registry
#'
#' @details
#' This accessor returns the registry created by [init_kernel_registry()]. The
#' returned object is a named list keyed by kernel name. Each kernel definition
#' describes which bulk parameters are present, how those parameters may depend
#' on covariates, whether a GPD tail is allowed, and which density or mean
#' functions should be dispatched for the supported backends.
#'
#' Downstream builders use this registry as the package-level reference for
#' kernel-specific implementation metadata. Reading it is appropriate when you
#' need to inspect what the package believes a kernel can do before constructing
#' or debugging a model specification.
#'
#' @return A list of kernel metadata.
#' @examples
#' init_kernel_registry()
#' reg <- get_kernel_registry()
#' reg$normal$bulk_params
#' @export
get_kernel_registry <- function() {
  ns <- getNamespace("CausalMixGPD")
  if (exists("kernel_registry", envir = ns, inherits = FALSE)) {
    return(get("kernel_registry", envir = ns, inherits = FALSE))
  }
  init_kernel_registry()
  if (exists("kernel_registry", envir = ns, inherits = FALSE)) {
    return(get("kernel_registry", envir = ns, inherits = FALSE))
  }
  stop("kernel_registry is not initialized.", call. = FALSE)
}

#' Get tail registry
#'
#' @details
#' The tail registry records the generalized Pareto splice used by bulk-tail
#' models. It stores the tail parameter names
#' `threshold`, `tail_scale`, and `tail_shape`, together with the support each
#' parameter must satisfy and the modeling modes the builders may assign to
#' them.
#'
#' In mathematical terms, for a threshold \eqn{u} the upper tail is represented
#' with a generalized Pareto law for excesses above \eqn{u}. Accessing this
#' registry is useful when inspecting how the package encodes those tail
#' parameters before model compilation.
#'
#' @return A list of tail metadata.
#' @examples
#' init_kernel_registry()
#' tail <- get_tail_registry()
#' tail$params
#' @export
get_tail_registry <- function() {
  ns <- getNamespace("CausalMixGPD")
  if (exists("tail_registry", envir = ns, inherits = FALSE)) {
    return(get("tail_registry", envir = ns, inherits = FALSE))
  }
  init_kernel_registry()
  if (exists("tail_registry", envir = ns, inherits = FALSE)) {
    return(get("tail_registry", envir = ns, inherits = FALSE))
  }
  stop("tail_registry is not initialized.", call. = FALSE)
}



NULL

#' Kernel support matrix
#'
#' Returns a data frame summarizing each kernel's supported features.
#'
#' @details
#' The returned table is a compact view of the registry contracts. Each row
#' corresponds to one kernel, while the logical columns indicate whether that
#' kernel can be paired with a GPD tail, whether covariate-linked parameter
#' models are defined, and whether the stick-breaking (`sb`) and
#' Chinese-restaurant (`crp`) backends are implemented.
#'
#' This helper is intended for inspection and reporting rather than model
#' fitting. It is a quick way to verify that a requested combination of kernel,
#' tail, and backend is supported before calling higher-level workflow
#' constructors.
#'
#' @param round Logical; `TRUE` to replace logical values with symbols.
#' @return data.frame with columns `kernel`, `gpd`, `covariates`, `sb`, `crp`.
#' @export
kernel_support_table <- function(round = TRUE) {
  registry <- get_kernel_registry()
  rows <- lapply(registry, function(def) {
    has_gpd <- isTRUE(def$allow_gpd) && (
      (!is.null(def$sb) && !is.null(def$sb$d_gpd) && !isTRUE(is.na(def$sb$d_gpd))) ||
        (!is.null(def$crp) && !is.null(def$crp$d_gpd) && !isTRUE(is.na(def$crp$d_gpd)))
    )
    has_cov <- length(def$defaults_X) > 0 &&
      any(vapply(def$defaults_X, function(x) identical(x$mode, "link"), logical(1)))
    has_sb <- !is.null(def$sb) && !is.null(def$sb$d)
    has_crp <- !is.null(def$crp) && !is.null(def$crp$d_base)

    data.frame(
      kernel = def$key,
      gpd = has_gpd,
      covariates = has_cov,
      sb = has_sb,
      crp = has_crp,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  if (round) {
    fmt <- function(ok) vapply(ok, function(sf) if (isTRUE(sf)) "\u2714" else "\u274C", character(1))
    out$gpd <- fmt(out$gpd)
    out$covariates <- fmt(out$covariates)
    out$sb <- fmt(out$sb)
    out$crp <- fmt(out$crp)
  }
  out
}
