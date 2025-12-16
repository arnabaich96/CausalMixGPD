# Build model specification ----------------------------------------------------

# Helper: validate transformation specification

# Returns a named list, one entry per parameter
#' Validate trans
#'
#' Validate trans.
#'
#' @param kinfo kinfo.
#' @param trans trans.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".validate_trans", "DPmixGPD")
#' f
#'
#' @keywords internal
.validate_trans <- function(kinfo, trans) {

  k_trans <- kinfo$default_trans  # list from registry

  if (length(trans) > 0L) {
    for (nm in names(trans)) {
      if (!(nm %in% kinfo$params)) {
        stop("Unknown parameter name in `trans`: ", nm,
             ". Must be one of: ", paste(kinfo$params, collapse = ", "))
      }

      val <- trans[[nm]]

      if (is.null(val)) {
        # Explicitly: no covariate link for this parameter
        k_trans[[nm]] <- NULL

      } else if (is.character(val)) {

        if (length(val) != 1L) {
          stop("Each transformation label must be a single character value.")
        }
        if (!val %in% .valid_trans_labels) {
          stop(
            "Invalid transformation label '", val, "'. ",
            "Allowed labels: ",
            paste(.valid_trans_labels, collapse = ", ")
          )
        }
        k_trans[[nm]] <- val

      } else if (is.function(val)) {

        # User supplied a custom function f(xb)
        k_trans[[nm]] <- val

      } else {
        stop(
          "Each transformation must be either NULL (no covariate link), ",
          "a single character label (",
          paste(.valid_trans_labels, collapse = ", "),
          "), or a function."
        )
      }
    }
  }

  k_trans
}
#' Build model spec xy
#'
#' Build model spec xy.
#'
#' @param Y Y.
#' @param X X.
#' @param mode mode.
#' @param kernel kernel.
#' @param dp_rep dp_rep.
#' @param dp_ctrl dp_ctrl.
#' @param priors priors.
#' @param trans trans.
#' @param tail tail.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("build_model_spec_xy", "DPmixGPD")
#' f
#'
#' @keywords internal
build_model_spec_xy <- function(
  Y,
  X        = NULL,
  mode     = c("response_only", "regression"),
  kernel   = c("gamma", "lognormal", "normal", "laplace", "inverse_gaussian", "amoroso", "pareto"),
  dp_rep   = c("stick_breaking", "crp"),
  dp_ctrl  = list(),
  priors   = list(),
  trans    = list(),
  tail     = c("none", "gpd")
) {
  mode   <- match.arg(mode)
  kernel <- match.arg(kernel)
  dp_rep <- match.arg(dp_rep)
  tail   <- match.arg(tail)

  Y <- as.numeric(Y)
  if (any(!is.finite(Y))) {
    stop("Non-finite values in Y are not allowed.")
  }

  N <- length(Y)
  if (N < 2L) stop("Need at least two observations in Y.")

  if (!is.null(X)) {
    if (mode != "regression") {
      stop("Internal error: X not NULL but mode != 'regression'.")
    }
    if (is.data.frame(X)) X <- as.matrix(X)
    if (!is.matrix(X)) stop("X must be a matrix or data.frame or NULL.")
    if (nrow(X) != N) stop("nrow(X) must equal length(Y).")
    p <- ncol(X)
  } else {
    p <- 0L
  }

  # DP control defaults
  if (dp_rep == "stick_breaking") {
    if (is.null(dp_ctrl$K)) dp_ctrl$K <- 10L
    if (dp_ctrl$K < 1L) stop("dp_ctrl$K must be >= 1 for stick-breaking.")
  } else {
    if (!is.null(dp_ctrl$K_max) && dp_ctrl$K_max < 1L) {
      stop("dp_ctrl$K_max must be >= 1 when provided for CRP.")
    }
  }
  if (is.null(dp_ctrl$alpha_prior)) {
    dp_ctrl$alpha_prior <- list(shape = 1, rate = 1)
  }

  kinfo   <- get_kernel(kernel)
  k_trans <- .validate_trans(kinfo, trans)

  spec <- list(
    Y      = Y,
    X      = X,
    N      = N,
    p      = p,
    mode   = mode,
    kernel = kernel,
    kernel_info = kinfo,
    dp_rep  = dp_rep,
    dp_ctrl = dp_ctrl,
    priors  = priors,
    trans   = k_trans,  # list per parameter
    tail    = tail
  )
  class(spec) <- "mixgpd_spec"
  spec
}

# Internal: construct a model specification list used by the engines
#' Build model spec
#'
#' Build model spec.
#'
#' @param Y Y.
#' @param X X.
#' @param kernel kernel.
#' @param tail tail.
#' @param dp_rep dp_rep.
#' @param priors priors.
#' @param dp_ctrl dp_ctrl.
#' @param trans trans.
#' @param alpha alpha.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("build_model_spec", "DPmixGPD")
#' f
#'
#' @keywords internal
build_model_spec <- function(Y,
                             X,
                             kernel,
                             tail,
                             dp_rep,
                             priors  = list(),
                             dp_ctrl = list(),
                             trans   = list(),
                             alpha   = 0.05) {

  ## ---- 1) Basic dimensions ----
  N <- length(Y)

  ## ---- 2) Mode: response-only vs regression ----
  mode <- if (is.null(X) || (is.matrix(X) && ncol(X) == 0L)) "response_only" else "regression"

  ## ---- 3) Ensure dp_ctrl exists ----
  if (is.null(dp_ctrl)) dp_ctrl <- list()

  # Stick-breaking uses a fixed truncation K; CRP does not.
  if (identical(dp_rep, "stick_breaking")) {
    if (is.null(dp_ctrl$K)) dp_ctrl$K <- 5L  # default truncation
  } else {
    # For CRP, ignore any user-provided K to avoid confusion.
    dp_ctrl$K <- NULL
  }

  ## ---- 4) Keep raw X, then apply transforms ----
  X_raw <- X

  tr_out <- .transform_resolve(
    X      = X_raw,
    trans  = trans,
    caller = "build_model_spec"
  )

  X_tr <- tr_out$X

  ## ---- 5) Return structured spec ----
  list(
    Y          = Y,
    X          = X_tr,       # transformed X used by engines
    X_raw      = X_raw,      # original X for predict/diagnostics
    N          = N,
    mode       = mode,
    kernel     = kernel,
    tail       = tail,
    dp_rep     = dp_rep,
    priors     = priors,
    dp_ctrl    = dp_ctrl,
    trans      = trans,
    trans_meta = tr_out$meta,
    alpha      = alpha
  )
}
# ============================================================================
# Canonical quantile solver for Gamma DP mixtures (UNIROOT backbone)
# Everything else should call this.
# ============================================================================
#' Normalize weights
#'
#' Normalize weights.
#'
#' @param w w.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".normalize_weights", "DPmixGPD")
#' f
#'
#' @keywords internal
.normalize_weights <- function(w) {
  w[!is.finite(w)] <- NA_real_
  w[w < 0] <- NA_real_
  s <- sum(w, na.rm = TRUE)
  if (!is.finite(s) || s <= 0) stop("Invalid weights in a draw.", call. = FALSE)
  w / s
}
#' Mix cdf gamma
#'
#' Mix cdf gamma.
#'
#' @param x x.
#' @param w w.
#' @param shape shape.
#' @param scale scale.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".mix_cdf_gamma", "DPmixGPD")
#' f
#'
#' @keywords internal
.mix_cdf_gamma <- function(x, w, shape, scale) {
  # x scalar
  sum(w * stats::pgamma(x, shape = shape, scale = scale))
}
#' Bracket root
#'
#' Bracket root.
#'
#' @param fun fun.
#' @param target target.
#' @param lower lower.
#' @param upper upper.
#' @param expand expand.
#' @param max_expand max_expand.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".bracket_root", "DPmixGPD")
#' f
#'
#' @keywords internal
.bracket_root <- function(fun, target = 0, lower = 0, upper = 1,
                          expand = 2, max_expand = 60) {
  # want fun(lower) < target and fun(upper) > target
  fL <- fun(lower) - target
  fU <- fun(upper) - target

  if (is.nan(fL) || is.nan(fU)) stop("NaN when bracketing root.", call. = FALSE)

  # If already bracketed, done
  if (is.finite(fL) && is.finite(fU) && fL <= 0 && fU >= 0) {
    return(c(lower, upper))
  }

  # If lower is too high (rare for CDF), try shrinking lower
  # (for our CDF use-case, lower=0 should already be fine)
  if (is.finite(fL) && fL > 0) {
    lower_try <- lower
    for (k in seq_len(max_expand)) {
      lower_try <- lower_try / expand
      fL <- fun(lower_try) - target
      if (is.finite(fL) && fL <= 0) {
        lower <- lower_try
        break
      }
    }
  }

  # Expand upper until it crosses target
  upper_try <- upper
  for (k in seq_len(max_expand)) {
    fU <- fun(upper_try) - target
    if (is.finite(fU) && fU >= 0) {
      return(c(lower, upper_try))
    }
    upper_try <- upper_try * expand
  }

  stop("Failed to bracket quantile (upper bound too small even after expansion).",
       call. = FALSE)
}
#' Q gamma dp one draw
#'
#' Q gamma dp one draw.
#'
#' @param p p.
#' @param w w.
#' @param shape shape.
#' @param scale scale.
#' @param lower lower.
#' @param upper0 upper0.
#' @param expand expand.
#' @param max_expand max_expand.
#' @param tol tol.
#' @param maxiter maxiter.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".q_gamma_dp_one_draw", "DPmixGPD")
#' f
#'
#' @keywords internal
.q_gamma_dp_one_draw <- function(p, w, shape, scale,
                                 lower = 0,
                                 upper0 = 1,
                                 expand = 2,
                                 max_expand = 60,
                                 tol = .Machine$double.eps^0.5,
                                 maxiter = 1000) {

  if (!is.finite(p) || p <= 0 || p >= 1) {
    stop("'p' must be strictly between 0 and 1 for uniroot.", call. = FALSE)
  }

  w <- .normalize_weights(w)

  # Root function: CDF(x) - p = 0
  f <- function(x) .mix_cdf_gamma(x, w, shape, scale) - p

  # bracket then uniroot
  br <- .bracket_root(fun = function(x) .mix_cdf_gamma(x, w, shape, scale),
                      target = p,
                      lower = lower,
                      upper = upper0,
                      expand = expand,
                      max_expand = max_expand)

  stats::uniroot(f, interval = br, tol = tol, maxiter = maxiter)$root
}
#' Q gamma dp over draws
#'
#' Q gamma dp over draws.
#'
#' @param p_vec p_vec.
#' @param W W.
#' @param Shape Shape.
#' @param Scale Scale.
#' @param lower lower.
#' @param upper0 upper0.
#' @param expand expand.
#' @param max_expand max_expand.
#' @param tol tol.
#' @param maxiter maxiter.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".q_gamma_dp_over_draws", "DPmixGPD")
#' f
#'
#' @keywords internal
.q_gamma_dp_over_draws <- function(p_vec, W, Shape, Scale,
                                   lower = 0,
                                   upper0 = 1,
                                   expand = 2,
                                   max_expand = 60,
                                   tol = .Machine$double.eps^0.5,
                                   maxiter = 1000) {
  p_vec <- as.numeric(p_vec)
  M <- nrow(W)
  Q <- matrix(NA_real_, nrow = M, ncol = length(p_vec))
  colnames(Q) <- paste0("q", p_vec)

  for (m in seq_len(M)) {
    w_m  <- as.numeric(W[m, ])
    sh_m <- as.numeric(Shape[m, ])
    sc_m <- as.numeric(Scale[m, ])

    for (k in seq_along(p_vec)) {
      Q[m, k] <- .q_gamma_dp_one_draw(
        p = p_vec[k], w = w_m, shape = sh_m, scale = sc_m,
        lower = lower, upper0 = upper0, expand = expand, max_expand = max_expand,
        tol = tol, maxiter = maxiter
      )
    }
  }
  Q
}


#' Qmix gamma dp uncond.
#'
#' @param tau tau.
#' @param w w.
#' @param shape shape.
#' @param scale scale.
#' @param lower lower.
#' @param upper upper.
#' @param expand expand.
#' @param max_expand max_expand.
#' @param tol tol.
#' @param maxiter maxiter.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace(".qmix_gamma_dp_uncond", "DPmixGPD")
#' f
#'
#' @keywords internal
.qmix_gamma_dp_uncond <- function(tau, w, shape, scale,
                                  lower = 0, upper = NULL,
                                  expand = 2, max_expand = 60,
                                  tol = 1e-10, maxiter = 1000) {
  # Defensive
  if (!is.finite(tau) || tau < 0 || tau > 1) stop("'tau' must be in [0,1].", call. = FALSE)
  if (any(!is.finite(w)) || any(w < 0)) stop("Bad weights.", call. = FALSE)
  s <- sum(w)
  if (!is.finite(s) || s <= 0) stop("Sum of weights must be positive.", call. = FALSE)
  w <- w / s

  if (tau == 0) return(0)
  if (tau == 1) {
    # crude but safe: keep expanding until cdf ~ 1
    tau <- 1 - 1e-12
  }

  Fmix <- function(x) sum(w * stats::pgamma(x, shape = shape, scale = scale))

  # choose upper if not given: use a big multiple of mean as a starting guess
  if (is.null(upper)) {
    mu_mix <- sum(w * shape * scale)
    upper <- max(1, 10 * mu_mix)
  }

  fL <- Fmix(lower) - tau
  fU <- Fmix(upper) - tau

  n_expand <- 0L
  while (fU < 0 && n_expand < max_expand) {
    upper <- upper * expand
    fU <- Fmix(upper) - tau
    n_expand <- n_expand + 1L
  }

  if (fU < 0) {
    stop("Failed to bracket quantile (upper bound too small even after expansion).",
         call. = FALSE)
  }

  stats::uniroot(function(x) Fmix(x) - tau,
                 lower = lower, upper = upper,
                 tol = tol, maxiter = maxiter)$root
}