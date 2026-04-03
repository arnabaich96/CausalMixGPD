#' Compile model specification (pre-NIMBLE)
#'
#' Build a fully normalized model specification for later bundle construction.
#' This function is structural: it decides node types, dimensions, defaults, and
#' how parameters are represented (fixed / dist / link) but does not run NIMBLE.
#'
#' The model size is controlled by a single user-facing parameter:
#' \code{components}. For the stick-breaking (SB) backend it is the truncation
#' level of the finite mixture. For the CRP backend it is the maximum number of
#' represented clusters in the finite NIMBLE model.
#'
#' @param y Numeric outcome vector.
#' @param X Optional design matrix (N x P). Can be matrix or data.frame.
#' @param backend Dirichlet process representation:
#'   \itemize{
#'     \item \code{"sb"}: stick-breaking truncation
#'     \item \code{"crp"}: Chinese Restaurant Process
#'   }
#' @param kernel Kernel name; must exist in \code{get_kernel_registry()}.
#' @param GPD Logical; include GPD tail if TRUE.
#' @param components Integer >= 2; single truncation parameter used for both backends.
#' @param param_specs Optional list to override defaults. Expected structure:
#' \preformatted{
#' list(
#'   bulk = list(
#'     <param> = list(mode="fixed"/"dist"/"link", ...),
#'     ...
#'   ),
#'   gpd = list(
#'     threshold = list(...),
#'     tail_scale = list(...),
#'     tail_shape = list(...),
#'     ...
#'   ),
#'   concentration = list( ... ) # optional alpha override
#'   ps = list(
#'     prior = list(dist = "normal", args = list(mean = 0, sd = 2))
#'   )
#' )
#' }
#' @param alpha_random Logical; if TRUE, the DP concentration parameter (\eqn{\kappa}) is stochastic with default Gamma(1,1) prior.
#' @param ps Optional numeric vector of propensity scores (length N). When provided, the
#'   compiled spec will include \code{beta_ps_<param>} coefficients for link-mode parameters.
#' @param ... Unused; accepted for forward compatibility.
#'
#' @return A named list \code{spec} containing \code{meta}, \code{kernel_info},
#' \code{signatures}, and a canonical \code{plan}.
#' @keywords internal
#' @noRd
compile_model_spec <- function(
    y,
    X = NULL,
    backend = c("sb", "crp", "spliced"),
    kernel,
    GPD = FALSE,
    components,
    param_specs = NULL,
    alpha_random = TRUE,
    ps = NULL,
    ...
) {

  backend <- match.arg(backend, choices = allowed_backends)

  y <- as.numeric(y)
  if (!length(y)) stop("y must be a non-empty numeric vector.", call. = FALSE)
  N <- length(y)

  has_X <- !is.null(X)
  if (has_X) {
    if (!is.matrix(X)) X <- as.matrix(X)
    if (nrow(X) != N) stop("X must have the same number of rows as length(y).", call. = FALSE)
    P <- ncol(X)
    if (P < 1) stop("X must have at least one column.", call. = FALSE)
    if (!is.null(colnames(X))) {
      .validate_nimble_reserved_names(colnames(X), "X column names")
    }
  } else {
    P <- 0L
  }

  # ps is optional; validate if provided
  if (!is.null(ps)) {
    ps <- as.numeric(ps)
    if (length(ps) != N) stop("ps must have the same length as y.", call. = FALSE)
  }

  if (missing(components) || is.null(components)) {
    stop("components is required (single truncation parameter for both backends).", call. = FALSE)
  }
  components <- as.integer(components)
  if (!is.finite(components) || components < 2L) {
    stop("components must be an integer >= 2.", call. = FALSE)
  }

  # Kernel registry lookup + validation
  krn <- get_kernel_registry()
  kernel <- match.arg(kernel, choices = names(krn))
  kinfo <- krn[[kernel]]

  if (!is_allowed_kernel(kernel)) stop(sprintf("Kernel '%s' is not supported.", kernel), call. = FALSE)
  if (is.null(kinfo$bulk_params) || !length(kinfo$bulk_params)) {
    stop("Kernel registry entry is missing bulk_params.", call. = FALSE)
  }
  if (is.null(kinfo$param_types) || !length(kinfo$param_types)) {
    stop("Kernel registry entry is missing param_types.", call. = FALSE)
  }

  # signatures are required for likelihood emission
  check_gpd_contract(GPD, kernel)
  if (is.null(kinfo$signatures) || is.null(kinfo$signatures[[backend]])) {
    stop("Kernel registry entry is missing signatures for this backend.", call. = FALSE)
  }
  if (GPD) {
    if (is.null(kinfo$signatures[[backend]]$gpd)) {
      stop("Requested GPD=TRUE but kernel registry has no GPD signature for this backend.", call. = FALSE)
    }
  } else {
    if (is.null(kinfo$signatures[[backend]]$bulk)) {
      stop("Requested GPD=FALSE but kernel registry has no bulk signature for this backend.", call. = FALSE)
    }
  }

  # ---- helpers for default priors by parameter type ----
  default_prior_by_type <- function(type, support = NULL) {
    type <- as.character(type)
    support <- as.character(support %||% "")
    if (support %in% c("positive_location", "positive_scale", "positive_shape", "positive_sd")) {
      return(list(dist = "gamma", args = list(shape = 2, rate = 1)))
    }
    if (type %in% c("location")) {
      list(dist = "normal", args = list(mean = 0, sd = 5))
    } else if (type %in% c("scale")) {
      list(dist = "gamma", args = list(shape = 2, rate = 1))
    } else if (type %in% c("shape", "sd")) {
      list(dist = "invgamma", args = list(shape = 2, scale = 1))
    } else {
      # fallback: weak normal
      list(dist = "normal", args = list(mean = 0, sd = 5))
    }
  }

  default_beta_prior <- function(kind = c("bulk", "threshold", "tail_scale", "tail_shape")) {
    kind <- match.arg(kind)
    if (kind == "threshold") {
      list(dist = "normal", args = list(mean = 0, sd = 0.2))
    } else if (kind == "tail_scale") {
      list(dist = "normal", args = list(mean = 0, sd = 0.5))
    } else if (kind == "tail_shape") {
      list(dist = "normal", args = list(mean = 0, sd = 0.3))
    } else {
      list(dist = "normal", args = list(mean = 0, sd = 2))
    }
  }

  validate_link_spec <- function(param_name,
                                 link,
                                 link_power = NULL,
                                 support = NULL,
                                 beta_prior = NULL,
                                 link_dist = NULL) {
    link <- as.character(link %||% "identity")
    support <- as.character(support %||% "")

    if (!link %in% c("identity", "exp", "log", "softplus", "power")) {
      stop(sprintf("Unsupported link '%s' for '%s'.", link, param_name), call. = FALSE)
    }

    bp <- beta_prior %||% list(dist = "normal", args = list(mean = 0, sd = 1))
    bp_dist <- as.character(bp$dist %||% "normal")
    if (!identical(bp_dist, "normal")) {
      stop(
        sprintf(
          "link-mode coefficient prior for '%s' must use dist = 'normal'; got '%s'.",
          param_name,
          bp_dist
        ),
        call. = FALSE
      )
    }

    bp_mean <- bp$args$mean %||% 0
    bp_sd <- bp$args$sd %||% NA_real_
    if (!is.numeric(bp_mean) || length(bp_mean) != 1L || !is.finite(bp_mean)) {
      stop(sprintf("link-mode coefficient prior mean for '%s' must be a finite scalar.", param_name), call. = FALSE)
    }
    if (!is.numeric(bp_sd) || length(bp_sd) != 1L || !is.finite(bp_sd) || bp_sd <= 0) {
      stop(sprintf("link-mode coefficient prior sd for '%s' must be a positive finite scalar.", param_name), call. = FALSE)
    }

    if (identical(link, "log")) {
      stop(
        sprintf(
          "link '%s' is not supported for '%s' with unrestricted normal coefficient priors; use identity, exp, softplus, or a supported power link instead.",
          link,
          param_name
        ),
        call. = FALSE
      )
    }

    if (identical(link, "power")) {
      pw <- suppressWarnings(as.numeric(link_power))
      if (length(pw) != 1L || !is.finite(pw)) {
        stop(sprintf("power link for '%s' requires a finite numeric link_power.", param_name), call. = FALSE)
      }
      if (abs(pw - round(pw)) > sqrt(.Machine$double.eps) || pw <= 0) {
        stop(
          sprintf(
            "power link for '%s' requires a positive integer link_power when coefficient priors are normal.",
            param_name
          ),
          call. = FALSE
        )
      }
      if (support %in% c("positive_location", "positive_scale", "positive_shape", "positive_sd") &&
          as.integer(round(pw)) %% 2L != 0L) {
        stop(
          sprintf(
            "positive-support parameter '%s' requires an even integer power link, or use exp/softplus.",
            param_name
          ),
          call. = FALSE
        )
      }
    }

    if (support %in% c("positive_location", "positive_scale", "positive_shape", "positive_sd") &&
        !link %in% c("exp", "softplus", "power")) {
      stop(
        sprintf(
          "link '%s' is not appropriate for positive-support parameter '%s'; use exp, softplus, or an even-integer power link.",
          link,
          param_name
        ),
        call. = FALSE
      )
    }

    if (!is.null(link_dist)) {
      ld_dist <- as.character(link_dist$dist %||% "")
      if (!nzchar(ld_dist) || !identical(ld_dist, "lognormal")) {
        stop(sprintf("Only lognormal link_dist is supported for '%s'.", param_name), call. = FALSE)
      }
    }

    invisible(TRUE)
  }

  # ---- normalize/merge user param specs ----
  param_specs <- param_specs %||% list()
  user_bulk <- param_specs$bulk %||% list()
  user_gpd  <- param_specs$gpd  %||% list()
  user_conc <- param_specs$concentration %||% list()
  user_ps   <- param_specs$ps %||% list()

  # ---- concentration (alpha) plan ----
  conc_plan <- NULL
  if (length(user_conc)) {
    # user override must supply mode
    mode <- user_conc$mode %||% NA_character_
    if (!is.character(mode) || length(mode) != 1L) stop("concentration$mode must be a single character.", call. = FALSE)
    if (!mode %in% c("fixed", "dist")) stop("concentration$mode must be 'fixed' or 'dist'.", call. = FALSE)

    if (mode == "fixed") {
      if (is.null(user_conc$value) || !is.numeric(user_conc$value) || length(user_conc$value) != 1L) {
        stop("concentration fixed mode requires concentration$value (single numeric).", call. = FALSE)
      }
      conc_plan <- list(mode = "fixed", value = as.numeric(user_conc$value))
    } else {
      # dist
      dist <- user_conc$dist %||% "gamma"
      args <- user_conc$args %||% list(shape = 1, rate = 1)
      conc_plan <- list(mode = "dist", dist = dist, args = args)
    }
  } else {
    # default behavior
    if (isTRUE(alpha_random)) {
      conc_plan <- list(mode = "dist", dist = "gamma", args = list(shape = 1, rate = 1))
    } else {
      # fixed alpha default (can be overridden later by user_conc)
      conc_plan <- list(mode = "fixed", value = 1)
    }
  }

  # ---- bulk parameter plans ----
  bulk_params <- kinfo$bulk_params
  fixed_defaults <- kinfo$fixed_defaults %||% list()
  defaults_X <- kinfo$defaults_X %||% list()

  bulk_plan <- vector("list", length(bulk_params))
  names(bulk_plan) <- bulk_params

  for (nm in bulk_params) {
    # fixed-by-default (e.g. amoroso shape1=1)
    if (!is.null(fixed_defaults[[nm]])) {
      bulk_plan[[nm]] <- list(mode = "fixed", value = fixed_defaults[[nm]])
      next
    }

    # user override?
    u <- user_bulk[[nm]]
    if (!is.null(u)) {
      mode <- u$mode %||% NA_character_
      if (!is.character(mode) || length(mode) != 1L) stop(sprintf("bulk[%s] mode must be a single character.", nm), call. = FALSE)
      if (!mode %in% c("fixed", "dist", "link")) stop(sprintf("bulk[%s] mode must be fixed/dist/link.", nm), call. = FALSE)

      if (mode == "fixed") {
        if (is.null(u$value)) stop(sprintf("bulk[%s] fixed mode requires value.", nm), call. = FALSE)
        bulk_plan[[nm]] <- list(mode = "fixed", value = u$value)
      } else if (mode == "dist") {
        dist <- u$dist %||% default_prior_by_type(kinfo$param_types[[nm]], kinfo$bulk_support[[nm]])$dist
        args <- u$args %||% default_prior_by_type(kinfo$param_types[[nm]], kinfo$bulk_support[[nm]])$args
        bulk_plan[[nm]] <- list(mode = "dist", dist = dist, args = args)
      } else {
        # link
        if (!has_X) stop(sprintf("bulk[%s] requested link mode but X is NULL.", nm), call. = FALSE)
        link <- u$link %||% "identity"
        link_power <- u$link_power %||% NULL
        beta_prior <- u$beta_prior %||% default_beta_prior("bulk")
        validate_link_spec(
          param_name = nm,
          link = link,
          link_power = link_power,
          support = kinfo$bulk_support[[nm]],
          beta_prior = beta_prior
        )
        bulk_plan[[nm]] <- list(mode = "link", link = link, link_power = link_power, beta_prior = beta_prior)
      }
      next
    }

    # defaults
    if (has_X) {
      # if defaults_X provides link behavior for this param, use it; otherwise fall back to dist
      dx <- defaults_X[[nm]]
      if (!is.null(dx) && is.list(dx) && identical(dx$mode, "link")) {
        validate_link_spec(
          param_name = nm,
          link = dx$link %||% "identity",
          link_power = dx$link_power %||% NULL,
          support = kinfo$bulk_support[[nm]],
          beta_prior = dx$beta_prior %||% default_beta_prior("bulk")
        )
        bulk_plan[[nm]] <- list(
          mode = "link",
          link = dx$link %||% "identity",
          link_power = dx$link_power %||% NULL,
          beta_prior = dx$beta_prior %||% default_beta_prior("bulk")
        )
      } else {
        pr <- default_prior_by_type(kinfo$param_types[[nm]], kinfo$bulk_support[[nm]])
        bulk_plan[[nm]] <- list(mode = "dist", dist = pr$dist, args = pr$args)
      }
    } else {
      pr <- default_prior_by_type(kinfo$param_types[[nm]], kinfo$bulk_support[[nm]])
      bulk_plan[[nm]] <- list(mode = "dist", dist = pr$dist, args = pr$args)
    }
  }

  # ---- GPD plan ----
  gpd_plan <- list()
  if (isTRUE(GPD)) {
    # threshold
    thr_u <- user_gpd$threshold
    if (!is.null(thr_u)) {
      # allow fixed/dist/link; link may optionally include link_dist
      mode <- thr_u$mode %||% NA_character_
      if (!mode %in% c("fixed", "dist", "link")) stop("gpd$threshold mode must be fixed/dist/link.", call. = FALSE)

      if (mode == "fixed") {
        gpd_plan$threshold <- list(mode = "fixed", value = thr_u$value)
      } else if (mode == "dist") {
        gpd_plan$threshold <- list(mode = "dist", dist = thr_u$dist, args = thr_u$args)
      } else {
        if (!has_X) stop("gpd$threshold link mode requires X.", call. = FALSE)
        if (identical(backend, "spliced") && !is.null(thr_u$link_dist)) {
          stop("gpd$threshold link_dist is not supported for backend = 'spliced'.", call. = FALSE)
        }
        validate_link_spec(
          param_name = "threshold",
          link = thr_u$link %||% "identity",
          link_power = thr_u$link_power %||% NULL,
          support = "real",
          beta_prior = thr_u$beta_prior %||% default_beta_prior("threshold"),
          link_dist = thr_u$link_dist %||% NULL
        )
        # if user supplies link_dist, keep it; otherwise allow default below
        gpd_plan$threshold <- list(
          mode = "link",
          link = thr_u$link %||% "identity",
          link_power = thr_u$link_power %||% NULL,
          beta_prior = thr_u$beta_prior %||% default_beta_prior("threshold"),
          link_dist = thr_u$link_dist %||% NULL
        )
      }
    } else {
      # default: threshold[i] ~ Lognormal(meanlog=Xβ, sdlog_u) when X present; else dist
      if (has_X) {
        gpd_plan$threshold <- list(
          mode = "link",
          link = "identity",
          beta_prior = default_beta_prior("threshold")
        )
        if (!identical(backend, "spliced")) {
          gpd_plan$threshold$link_dist <- list(
            dist = "lognormal",
            mean_arg = "meanlog",
            sd_name = "sdlog_u"
          )
          gpd_plan$sdlog_u <- list(mode = "dist", dist = "invgamma", args = list(shape = 2, scale = 1))
        }
      } else {
        gpd_plan$threshold <- list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1))
      }
    }

    # tail_scale
    ts_u <- user_gpd$tail_scale
    if (!is.null(ts_u)) {
      mode <- ts_u$mode %||% NA_character_
      if (!mode %in% c("fixed", "dist", "link")) stop("gpd$tail_scale mode must be fixed/dist/link.", call. = FALSE)
      if (mode == "link" && !has_X) stop("gpd$tail_scale link mode requires X.", call. = FALSE)
      if (mode == "link") {
        validate_link_spec(
          param_name = "tail_scale",
          link = ts_u$link %||% "exp",
          link_power = ts_u$link_power %||% NULL,
          support = "positive_scale",
          beta_prior = ts_u$beta_prior %||% default_beta_prior("tail_scale")
        )
        gpd_plan$tail_scale <- list(
          mode = "link",
          link = ts_u$link %||% "exp",
          link_power = ts_u$link_power %||% NULL,
          beta_prior = ts_u$beta_prior %||% default_beta_prior("tail_scale")
        )
      } else if (mode == "dist") {
        gpd_plan$tail_scale <- list(mode = "dist", dist = ts_u$dist, args = ts_u$args)
      } else {
        gpd_plan$tail_scale <- list(mode = "fixed", value = ts_u$value)
      }
    } else {
      if (has_X) {
        gpd_plan$tail_scale <- list(mode = "link", link = "exp", beta_prior = default_beta_prior("tail_scale"))
      } else {
        gpd_plan$tail_scale <- list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1))
      }
    }

    # tail_shape
    tsh_u <- user_gpd$tail_shape
    if (!is.null(tsh_u)) {
      mode <- tsh_u$mode %||% NA_character_
      if (!mode %in% c("fixed", "dist", "link")) stop("gpd$tail_shape mode must be fixed/dist/link.", call. = FALSE)
      if (mode == "link" && !has_X) stop("gpd$tail_shape link mode requires X.", call. = FALSE)
      if (mode == "link") {
        validate_link_spec(
          param_name = "tail_shape",
          link = tsh_u$link %||% "identity",
          link_power = tsh_u$link_power %||% NULL,
          support = "real",
          beta_prior = tsh_u$beta_prior %||% default_beta_prior("tail_shape")
        )
        gpd_plan$tail_shape <- list(
          mode = "link",
          link = tsh_u$link %||% "identity",
          link_power = tsh_u$link_power %||% NULL,
          beta_prior = tsh_u$beta_prior %||% default_beta_prior("tail_shape")
        )
      } else if (mode == "fixed") {
        gpd_plan$tail_shape <- list(mode = "fixed", value = tsh_u$value)
      } else {
        gpd_plan$tail_shape <- list(mode = "dist", dist = tsh_u$dist, args = tsh_u$args)
      }
    } else {
      gpd_plan$tail_shape <- list(mode = "dist", dist = "normal", args = list(mean = 0, sd = 0.2))
    }

    # ---- Spliced backend validation ----
    # Spliced backend enforces component-level GPD parameterization with flexible modes
    if (backend == "spliced") {
      # Ensure all GPD params support component-level specification (automatically satisfied by structure)
      # Link mode requires X - already validated above for each parameter
      # Store level metadata for consistency
      for (param_name in c("threshold", "tail_scale", "tail_shape")) {
        if (!is.null(gpd_plan[[param_name]])) {
          gpd_plan[[param_name]]$level <- "component"
        }
      }
    }
  }

  # ---- finalize plan ----
  ps_plan <- NULL
  if (!is.null(ps)) {
    ps_plan <- list(prior = user_ps$prior %||% list(dist = "normal", args = list(mean = 0, sd = 2)))
  }

  plan <- list(
    backend = backend,
    kernel = kernel,
    GPD = isTRUE(GPD),
    has_X = has_X,
    N = N,
    P = as.integer(P),
    components = components,
    concentration = conc_plan,
    bulk = bulk_plan,
    gpd = gpd_plan,
    ps = ps_plan
  )

  spec <- list(
    meta = list(
      backend = backend,
      kernel = kernel,
      GPD = isTRUE(GPD),
      has_X = has_X,
      has_ps = !is.null(ps),
      N = N,
      P = as.integer(P),
      components = components,
      custom_build = !is.null(param_specs) && length(param_specs) > 0
    ),
    kernel_info = kinfo,
    signatures = kinfo$signatures[[backend]],
    plan = plan
  )

  class(spec) <- c("causalmixgpd_spec", "list")
  spec
}
