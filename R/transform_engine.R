# ------------------------------------------------------------------
# Transform engine: resolve user transforms + generate nimble code
# ------------------------------------------------------------------

# Internal: validate and normalize column names for transforms
# scale_spec can be:
#   - TRUE  -> all columns of X
#   - FALSE / NULL / character(0) -> no columns
#   - character vector of names -> must be subset of colnames(X)
#   - numeric indices -> converted to names
#' @keywords internal
.transform_valid_names <- function(scale_spec, X, caller = ".transform_resolve") {

  # No covariates at all
  if (is.null(X)) {
    return(character(0L))
  }

  cn <- colnames(X)
  if (is.null(cn)) {
    cn <- paste0("x", seq_len(ncol(X)))
    colnames(X) <- cn
  }

  # 1) Logical TRUE/FALSE
  if (is.logical(scale_spec) && length(scale_spec) == 1L) {
    if (isTRUE(scale_spec)) {
      # all columns
      return(cn)
    } else {
      # FALSE
      return(character(0L))
    }
  }

  # 2) NULL or empty
  if (is.null(scale_spec) || length(scale_spec) == 0L) {
    return(character(0L))
  }

  # 3) Numeric indices
  if (is.numeric(scale_spec)) {
    idx <- as.integer(scale_spec)
    if (any(is.na(idx)) || any(idx < 1L) || any(idx > ncol(X))) {
      stop(
        sprintf(
          "%s: numeric indices in 'scale' must be between 1 and %d.",
          caller, ncol(X)
        ),
        call. = FALSE
      )
    }
    return(cn[idx])
  }

  # 4) Character names
  if (is.character(scale_spec)) {
    bad <- setdiff(scale_spec, cn)
    if (length(bad) > 0L) {
      stop(
        sprintf(
          "%s: unknown variable(s) in 'scale': %s. Available: %s",
          caller,
          paste(bad, collapse = ", "),
          paste(cn, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    return(scale_spec)
  }

  # 5) Anything else is not supported
  stop(
    sprintf(
      "%s: 'scale' must be TRUE/FALSE, NULL, character names, or numeric indices.",
      caller
    ),
    call. = FALSE
  )
}


#' Resolve transform for a given parameter
#'
#' @param trans List supplied by user to fit.dpm()/fit.mixgpd(), e.g.
#'   list(scale = "exp", shape = "identity").
#' @param param_name Character, name of parameter ("scale", "shape",
#'   "threshold", "xi", "sigma", etc.).
#' @param default Character, default transform if none is provided.
#' @param must_be_positive Logical, whether this parameter is required
#'   to be positive for the chosen kernel/tail.
#'
#' @return A single canonical transform name.
.transform_resolve <- function(trans,
                               param_name,
                               default = "identity",
                               must_be_positive = FALSE) {
  valid <- .transform_valid_names()

  # 1) Pull user choice, if any
  if (is.null(trans) || is.null(trans[[param_name]]) ||
      length(trans[[param_name]]) == 0L) {
    t_raw <- default
  } else {
    t_raw <- trans[[param_name]]
  }

  if (!is.character(t_raw) || length(t_raw) != 1L) {
    stop(sprintf(
      "Transform for '%s' must be a single character string.",
      param_name
    ), call. = FALSE)
  }

  # 2) Canonicalize
  t_raw <- tolower(trimws(t_raw))
  # treat "none" as identity
  if (t_raw == "none") {
    t_raw <- "identity"
  }

  if (!t_raw %in% valid) {
    stop(sprintf(
      "Invalid transform '%s' for parameter '%s'. Allowed: %s",
      t_raw, param_name, paste(valid, collapse = ", ")
    ), call. = FALSE)
  }

  # 3) Optional safety warning for positivity
  if (must_be_positive && identical(t_raw, "identity")) {
    warning(sprintf(
      paste0(
        "Transform for '%s' is 'identity' but the parameter is required ",
        "to be positive. This may cause numerical or sampling issues."
      ),
      param_name
    ), call. = FALSE)
  }

  t_raw
}

#' Generate nimble code implementing a transform
#'
#' @param lhs Character, left-hand side (e.g. 'sigma[i]', 'shape[i,j]').
#' @param lp  Character, linear predictor (e.g. 'lp_sigma[i]').
#' @param transform Character, one of .transform_valid_names().
#'
#' @return A character string containing an R/NIMBLE expression,
#'   e.g. 'sigma[i] <- exp(lp_sigma[i])'.
.transform_generate_code <- function(lhs, lp, transform) {
  transform <- tolower(trimws(transform))

  # "none" is treated as identity for safety
  if (identical(transform, "none")) {
    transform <- "identity"
  }

  expr <- switch(
    transform,
    "identity" = sprintf("%s <- %s", lhs, lp),

    "exp"      = sprintf("%s <- exp(%s)", lhs, lp),

    "log"      = sprintf("%s <- log(%s)", lhs, lp),

    "sq"       = sprintf("%s <- (%s)^2", lhs, lp),

    "sqrt"     = sprintf("%s <- sqrt(%s)", lhs, lp),

    stop(sprintf(
      "Unknown transform '%s' in .transform_generate_code().",
      transform
    ), call. = FALSE)
  )

  expr
}
## Resolve and apply simple transformations to X
##
## Right now we support:
##   trans = NULL                -> no change
##   trans = list(scale = TRUE)  -> center & scale all columns of X
##   trans = list(scale = c("x1","x2")) -> center & scale only those columns
##
.transform_resolve <- function(X, trans, caller = "fit.dpm") {
  # no covariates or no transforms requested
  if (is.null(X) || is.null(trans) || length(trans) == 0L) {
    return(list(X = X,
                meta = list(
                  scale_cols  = integer(0),
                  scale_means = numeric(0),
                  scale_sds   = numeric(0)
                )))
  }

  if (!is.list(trans)) {
    stop(sprintf("%s: 'trans' must be a list or NULL.", caller), call. = FALSE)
  }

  ## ----- currently we only implement 'scale' -----
  scale_spec <- trans$scale

  if (is.null(scale_spec)) {
    # nothing to do
    return(list(X = X,
                meta = list(
                  scale_cols  = integer(0),
                  scale_means = numeric(0),
                  scale_sds   = numeric(0)
                )))
  }

  # decide which columns to scale
  if (isTRUE(scale_spec)) {
    cols <- seq_len(ncol(X))
  } else if (is.character(scale_spec)) {
    .transform_valid_names(scale_spec, X, caller = caller)
    cols <- match(scale_spec, colnames(X))
  } else if (is.numeric(scale_spec)) {
    cols <- as.integer(scale_spec)
    if (any(cols < 1L | cols > ncol(X))) {
      stop(sprintf("%s: 'trans$scale' column indices out of range.", caller),
           call. = FALSE)
    }
  } else {
    stop(sprintf("%s: 'trans$scale' must be TRUE, character, or numeric.",
                 caller),
         call. = FALSE)
  }

  cols <- unique(cols)
  cols <- cols[!is.na(cols)]

  if (length(cols) == 0L) {
    return(list(X = X,
                meta = list(
                  scale_cols  = integer(0),
                  scale_means = numeric(0),
                  scale_sds   = numeric(0)
                )))
  }

  means <- numeric(ncol(X))
  sds   <- numeric(ncol(X))

  for (j in cols) {
    xj <- X[, j]
    if (!is.numeric(xj)) {
      stop(sprintf(
        "%s: cannot scale non-numeric column '%s'.",
        caller, colnames(X)[j]
      ), call. = FALSE)
    }
    m  <- mean(xj, na.rm = TRUE)
    sdj <- stats::sd(xj, na.rm = TRUE)

    # guard against zero variance
    if (is.na(sdj) || sdj == 0) {
      sdj <- 1
    }

    X[, j] <- (xj - m) / sdj
    means[j] <- m
    sds[j]   <- sdj
  }

  meta <- list(
    scale_cols  = cols,
    scale_means = means[cols],
    scale_sds   = sds[cols]
  )

  list(X = X, meta = meta)
}
# Internal: ensure X is a proper matrix, optionally add intercept
#' @keywords internal
.prepare_design_matrix <- function(X, intercept = TRUE) {
  # No covariates case
  if (is.null(X)) return(NULL)

  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)

  # Give generic names if missing
  if (is.null(colnames(X))) {
    colnames(X) <- paste0("x", seq_len(p))
  }

  # If user does NOT want an intercept, just return matrix as-is
  if (!intercept) {
    return(X)
  }

  ## Intercept = TRUE:

  # 1) If it already looks like it has an intercept, keep it
  if (p >= 1L) {
    # named "(Intercept)" in first column
    if (!is.null(colnames(X)) && identical(colnames(X)[1L], "(Intercept)")) {
      return(X)
    }
    # or numerically all ones in first column
    if (all(abs(X[, 1L] - 1) < .Machine$double.eps^0.5)) {
      return(X)
    }
  }

  # 2) Otherwise, prepend an intercept column of ones
  intercept_col <- rep(1, n)
  X_new <- cbind("(Intercept)" = intercept_col, X)

  X_new
}
