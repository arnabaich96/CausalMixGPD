# Build model specification ----------------------------------------------------

# Helper: validate transformation specification
.validate_transform_label <- function(x) {
  if (is.null(x)) return(NULL)
  ok <- c("identity", "exp", "log", "inv", "sqrt", "sq")
  if (!x %in% ok) {
    stop("Unknown transform label: '", x,
         "'. Allowed: ", paste(ok, collapse = ", "), call. = FALSE)
  }
  x
}

# Returns a named list, one entry per parameter
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
  mode <- if (is.null(X)) "response_only" else "regression"

  ## ---- 3) Ensure dp_ctrl and K exist ----
  if (is.null(dp_ctrl)) dp_ctrl <- list()
  if (is.null(dp_ctrl$K)) {
    dp_ctrl$K <- 5L  # default truncation
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

