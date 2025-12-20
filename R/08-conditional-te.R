#' Conditional quantile treatment effects
#'
#' Compute conditional quantile treatment effects (CQTE) at covariate values.
#'
#' For a fitted treatment-effect object \code{fit} and covariates \code{x},
#' \deqn{\mathrm{CQTE}(\tau \mid x) = Q_1(\tau \mid x) - Q_0(\tau \mid x).}
#'
#' This function is intended for regression fits (e.g., \code{y ~ x}).
#' Unconditional effects for response-only fits (\code{y ~ 0}) remain available
#' via \code{\link{qte}} and \code{\link{ate}}.
#'
#' @param object A \code{"mixgpd_te_fit"} object returned by \code{\link{fit.TE}}.
#' @param x A \code{data.frame} (or object coercible to a data.frame) containing
#'   covariates at which to evaluate the conditional effects.
#' @param tau Numeric vector of quantile levels in \eqn{(0,1)}.
#' @param level Credible interval level in \eqn{(0,1)}. Default is 0.95.
#' @param renormalize_weights Logical; if \code{TRUE}, renormalize mixture weights per draw.
#' @param ... Unused; included for S3 consistency.
#'
#' @return An object of class \code{"mixgpd_cqte"}.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(y = rgamma(200, 2, 1), A = rbinom(200, 1, 0.5), x1 = rnorm(200), x2 = rnorm(200))
#' fit <- fit.TE(y ~ x1 + x2, data = dat, A = "A", kernel = "gamma", tail = FALSE,
#'               dp_rep = "stick_breaking", dp_ctrl = list(K = 5),
#'               mcmc = list(n_iter = 500, burn_in = 250, chains = 1))
#' out <- cqte(fit, x = dat[1:3, c("x1","x2")], tau = c(0.25, 0.5, 0.9))
#' summary(out)
#' plot(out, which_x = 1)
#' }
#'
#' @export
cqte <- function(object, x, tau, level = 0.95, renormalize_weights = TRUE, ...) {
  UseMethod("cqte")
}

#' @export
cqte.mixgpd_te_fit <- function(object, x, tau, level = 0.95, renormalize_weights = TRUE, ...) {
  stopifnot(inherits(object, "mixgpd_te_fit"))

  tau <- as.numeric(tau)
  if (!length(tau) || any(!is.finite(tau)) || any(tau <= 0) || any(tau >= 1)) {
    stop("'tau' must be a numeric vector in (0,1).", call. = FALSE)
  }
  level <- as.numeric(level)
  if (!is.finite(level) || level <= 0 || level >= 1) stop("'level' must be in (0,1).", call. = FALSE)

  x <- as.data.frame(x)

  q1 <- .cqte_arm_quantiles(object$fit_trt, x = x, tau = tau, renormalize_weights = renormalize_weights)
  q0 <- .cqte_arm_quantiles(object$fit_con, x = x, tau = tau, renormalize_weights = renormalize_weights)

  # align draw counts if needed
  M <- min(dim(q1)[1L], dim(q0)[1L])
  if (dim(q1)[1L] != M) q1 <- q1[seq_len(M), , , drop = FALSE]
  if (dim(q0)[1L] != M) q0 <- q0[seq_len(M), , , drop = FALSE]

  draws <- q1 - q0

  structure(
    list(
      tau   = tau,
      x     = x,
      draws = draws,
      level = level,
      call  = match.call()
    ),
    class = "mixgpd_cqte"
  )
}

#' Conditional average treatment effects
#'
#' Compute conditional average treatment effects (CATE) at covariate values.
#'
#' For covariates \code{x}, \deqn{\mathrm{CATE}(x) = \mathbb{E}[Y(1)\mid x] - \mathbb{E}[Y(0)\mid x].}
#'
#' When the tail model is a GPD, the mean exists only for \eqn{\xi < 1}. Draws with
#' \eqn{\xi \ge 1} are dropped with a warning; if all draws are invalid, \code{NA}
#' is returned with a warning.
#'
#' @param object A \code{"mixgpd_te_fit"} object returned by \code{\link{fit.TE}}.
#' @param x A \code{data.frame} (or object coercible to a data.frame) containing
#'   covariates at which to evaluate the conditional effects.
#' @param level Credible interval level in \eqn{(0,1)}. Default is 0.95.
#' @param renormalize_weights Logical; if \code{TRUE}, renormalize mixture weights per draw.
#' @param ... Unused; included for S3 consistency.
#'
#' @return An object of class \code{"mixgpd_cate"}.
#'
#' @export
cate <- function(object, x, level = 0.95, renormalize_weights = TRUE, ...) {
  UseMethod("cate")
}

#' @export
cate.mixgpd_te_fit <- function(object, x, level = 0.95, renormalize_weights = TRUE, ...) {
  stopifnot(inherits(object, "mixgpd_te_fit"))

  level <- as.numeric(level)
  if (!is.finite(level) || level <= 0 || level >= 1) stop("'level' must be in (0,1).", call. = FALSE)

  x <- as.data.frame(x)

  m1 <- .cate_arm_means(object$fit_trt, x = x, renormalize_weights = renormalize_weights)
  m0 <- .cate_arm_means(object$fit_con, x = x, renormalize_weights = renormalize_weights)

  # align draw counts
  M <- min(nrow(m1), nrow(m0))
  if (nrow(m1) != M) m1 <- m1[seq_len(M), , drop = FALSE]
  if (nrow(m0) != M) m0 <- m0[seq_len(M), , drop = FALSE]

  draws <- m1 - m0

  structure(
    list(
      x     = x,
      draws = draws,
      level = level,
      call  = match.call()
    ),
    class = "mixgpd_cate"
  )
}

# ---- internal: build model matrix for a fit, aligned to the fitted design matrix
#' Internal: build model matrix for newdata without requiring response
#'
#' This helper is used by conditional TE utilities (cqte/cate). It ensures
#' that prediction design matrices do not require the response variable to be
#' present in `newdata` by removing the response from the stored terms object.
#'
#' @param fit_arm A fitted mixgpd_fit object for one arm.
#' @param newdata A data.frame of covariates for prediction.
#'
#' @return A numeric model matrix.
#'
#' @keywords internal
.build_X_newdata <- function(fit_arm, newdata) {
  if (is.null(fit_arm$formula)) stop("fit_arm$formula is missing.", call. = FALSE)
  if (is.null(newdata)) stop("newdata must be provided.", call. = FALSE)

  tt <- stats::terms(fit_arm$formula)
  tt <- stats::delete.response(tt)
  mm <- stats::model.matrix(tt, data = newdata)

  # Align columns to the design matrix used at fit time.
  # This avoids accidental re-introduction of an intercept column when the
  # model was fit with intercept = FALSE.
  if (!is.null(fit_arm$spec) && !is.null(fit_arm$spec$X_raw)) {
    want <- colnames(fit_arm$spec$X_raw)
    have <- colnames(mm)
    missing <- setdiff(want, have)
    if (length(missing) > 0L) {
      stop(
        "newdata is missing required covariate columns: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    mm <- mm[, want, drop = FALSE]
  }

  mm
}

# ---- internal: extract SB gamma regression parameters from draws ----
# NOTE: implemented in 03-engine-sb-gamma.R (single source of truth).



# ---- internal: compute gamma mixture quantiles for one arm at x
# returns array [M] x [n] x [T]
#' @keywords internal
.cqte_arm_quantiles <- function(fit_arm, x, tau, renormalize_weights = TRUE) {
  stopifnot(inherits(fit_arm, "mixgpd_fit"))
  if (!identical(fit_arm$spec$kernel %||% NA_character_, "gamma")) {
    stop("cqte currently supports kernel='gamma' only.", call. = FALSE)
  }
  if (!identical(fit_arm$spec$dp_rep %||% NA_character_, "stick_breaking")) {
    stop("cqte currently supports dp_rep='stick_breaking' only.", call. = FALSE)
  }
  if (!identical(fit_arm$spec$mode %||% NA_character_, "regression")) {
    stop("cqte is intended for regression fits (y ~ x). Use qte()/ate() for response-only fits.", call. = FALSE)
  }

  Xnew <- .build_X_newdata(fit_arm, x)
  K <- as.integer(fit_arm$spec$dp_ctrl$K %||% NA_integer_)
  p <- ncol(Xnew)
  if (!is.finite(K) || K < 1L) stop("fit_arm$spec$dp_ctrl$K must be a positive integer for stick-breaking.", call. = FALSE)

  pars <- .extract_sb_gamma_reg_params(fit_arm$mcmc_draws, K = K, p = p, renormalize_weights = renormalize_weights)

  M <- pars$M
  n <- nrow(Xnew)
  T <- length(tau)

  out <- array(NA_real_, dim = c(M, n, T))

  for (i in seq_len(n)) {
    xvec <- as.numeric(Xnew[i, , drop = TRUE])
    # component scales per draw: exp(beta %*% x)
    # Beta: [M,K,p]
    eta <- apply(pars$Beta, c(1,2), function(b) sum(b * xvec))
    Scale <- exp(eta)

    for (m in seq_len(M)) {
      w_m <- pars$W[m, ]
      sh_m <- pars$Shape[m, ]
      sc_m <- Scale[m, ]

      # choose a pragmatic upper bound seed
      # use high quantile of max component
      sc_max <- max(sc_m, na.rm = TRUE)
      sh_max <- max(sh_m, na.rm = TRUE)
      upper0 <- stats::qgamma(max(tau), shape = sh_max, scale = sc_max)
      upper0 <- if (is.finite(upper0) && upper0 > 0) upper0 else sc_max * sh_max * 10
      for (t in seq_along(tau)) {
        tau_t <- tau[t]
        cdf_fun <- function(q) .gamma_mix_cdf_1(q, w = w_m, shape = sh_m, scale = sc_m)
        out[m, i, t] <- .invert_cdf_uniroot(cdf_fun, tau = tau_t, lower = 0, upper = upper0)
      }
    }
  }

  out
}

# ---- internal: compute conditional means for one arm at x (matrix [M] x [n])
#' @keywords internal
.cate_arm_means <- function(fit_arm, x, renormalize_weights = TRUE) {
  stopifnot(inherits(fit_arm, "mixgpd_fit"))
  if (!identical(fit_arm$spec$kernel %||% NA_character_, "gamma")) {
    stop("cate currently supports kernel='gamma' only.", call. = FALSE)
  }
  if (!identical(fit_arm$spec$dp_rep %||% NA_character_, "stick_breaking")) {
    stop("cate currently supports dp_rep='stick_breaking' only.", call. = FALSE)
  }

  Xnew <- .build_X_newdata(fit_arm, x)
  K <- as.integer(fit_arm$spec$dp_ctrl$K %||% NA_integer_)
  if (!is.finite(K) || K < 1L) stop("fit_arm$spec$dp_ctrl$K must be a positive integer for stick-breaking.", call. = FALSE)

  # response-only: mean does not depend on x; replicate to n rows
  if (identical(fit_arm$spec$mode %||% NA_character_, "response_only")) {
    p <- .extract_gamma_dp_params(fit_arm$mcmc_draws, K = K, renormalize_weights = renormalize_weights)
    Ey <- vapply(seq_len(p$M), function(m) sum(p$W[m, ] * p$Shape[m, ] * p$Scale[m, ]), numeric(1))
    return(matrix(Ey, nrow = length(Ey), ncol = nrow(Xnew)))
  }

  # regression: scale depends on x via beta_scale
  pars <- .extract_sb_gamma_reg_params(fit_arm$mcmc_draws, K = K, p = ncol(Xnew), renormalize_weights = renormalize_weights)

  M <- pars$M
  n <- nrow(Xnew)
  out <- matrix(NA_real_, nrow = M, ncol = n)

  for (i in seq_len(n)) {
    xvec <- as.numeric(Xnew[i, , drop = TRUE])
    eta <- apply(pars$Beta, c(1,2), function(b) sum(b * xvec))
    Scale <- exp(eta) # [M,K]
    out[, i] <- rowSums(pars$W * pars$Shape * Scale)
  }

  out
}

# ---- summaries and plots -----------------------------------------------------

#' @export
summary.mixgpd_cqte <- function(object, probs = NULL, ...) {
  stopifnot(inherits(object, "mixgpd_cqte"))
  if (is.null(probs)) {
    lvl <- object$level %||% 0.95
    a <- (1 - lvl) / 2
    probs <- c(a, 0.5, 1 - a)
  }
  probs <- as.numeric(probs)
  if (any(!is.finite(probs)) || any(probs < 0) || any(probs > 1)) stop("'probs' must be in [0,1].", call. = FALSE)

  dr <- object$draws
  M <- dim(dr)[1L]; n <- dim(dr)[2L]; T <- dim(dr)[3L]

  # produce long data.frame: row id, tau, mean, q_lo, q_med, q_hi
  res <- vector("list", n * T)
  k <- 0L
  for (i in seq_len(n)) {
    for (t in seq_len(T)) {
      k <- k + 1L
      v <- dr[, i, t]
      qs <- stats::quantile(v, probs = probs, na.rm = TRUE, names = FALSE)
      res[[k]] <- data.frame(
        row = i,
        tau = object$tau[t],
        mean = mean(v, na.rm = TRUE),
        q1 = qs[1L],
        q2 = qs[min(2L, length(qs))],
        q3 = qs[length(qs)]
      )
    }
  }
  out <- do.call(rbind, res)
  class(out) <- c("summary.mixgpd_cqte", class(out))
  attr(out, "probs") <- probs
  out
}

#' @export
print.mixgpd_cqte <- function(x, ...) {
  stopifnot(inherits(x, "mixgpd_cqte"))
  dr <- x$draws
  cat("<mixgpd_cqte>\n")
  cat("  draws:", dim(dr)[1L], "\n")
  cat("  n(x): ", dim(dr)[2L], "\n")
  cat("  tau:  ", paste0(format(x$tau), collapse = ", "), "\n")
  invisible(x)
}

#' @export
plot.mixgpd_cqte <- function(x, which_x = 1L, probs = NULL, ...) {
  stopifnot(inherits(x, "mixgpd_cqte"))
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
  }
  s <- summary.mixgpd_cqte(x, probs = probs)
  which_x <- as.integer(which_x)
  ss <- s[s$row == which_x, , drop = FALSE]
  ggplot2::ggplot(ss, ggplot2::aes(x = tau, y = mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q1, ymax = q3), alpha = 0.2) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "tau", y = "CQTE", title = paste0("CQTE at row ", which_x))
}

#' @export
summary.mixgpd_cate <- function(object, probs = NULL, ...) {
  stopifnot(inherits(object, "mixgpd_cate"))
  if (is.null(probs)) {
    lvl <- object$level %||% 0.95
    a <- (1 - lvl) / 2
    probs <- c(a, 0.5, 1 - a)
  }
  probs <- as.numeric(probs)
  if (any(!is.finite(probs)) || any(probs < 0) || any(probs > 1)) stop("'probs' must be in [0,1].", call. = FALSE)

  dr <- object$draws  # [M,n]
  M <- nrow(dr); n <- ncol(dr)

  res <- vector("list", n)
  for (i in seq_len(n)) {
    v <- dr[, i]
    qs <- stats::quantile(v, probs = probs, na.rm = TRUE, names = FALSE)
    res[[i]] <- data.frame(
      row = i,
      mean = mean(v, na.rm = TRUE),
      q1 = qs[1L],
      q2 = qs[min(2L, length(qs))],
      q3 = qs[length(qs)]
    )
  }
  out <- do.call(rbind, res)
  class(out) <- c("summary.mixgpd_cate", class(out))
  attr(out, "probs") <- probs
  out
}

#' @export
print.mixgpd_cate <- function(x, ...) {
  stopifnot(inherits(x, "mixgpd_cate"))
  dr <- x$draws
  cat("<mixgpd_cate>\n")
  cat("  draws:", nrow(dr), "\n")
  cat("  n(x): ", ncol(dr), "\n")
  invisible(x)
}

#' @export
plot.mixgpd_cate <- function(x, probs = NULL, ...) {
  stopifnot(inherits(x, "mixgpd_cate"))
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
  }
  s <- summary.mixgpd_cate(x, probs = probs)
  ggplot2::ggplot(s, ggplot2::aes(x = row, y = mean)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = q1, ymax = q3), width = 0.2) +
    ggplot2::labs(x = "row", y = "CATE", title = "CATE by covariate row")
}
