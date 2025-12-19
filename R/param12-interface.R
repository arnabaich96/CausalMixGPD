#' Canonical Param1/Param2 interface and draw extraction
#'
#' The model engine stores all two-parameter kernel parameters using the
#' canonical names \code{Param1} and \code{Param2}. When covariate regression is
#' used, regression coefficients are stored using \code{beta_Param1} and
#' \code{beta_Param2}. Kernel-specific parameter names are applied only for
#' presentation in \code{summary()} outputs.
#'
#' @name param12_interface
#' @keywords internal
NULL

#' Check whether a kernel is eligible for the Param1/Param2 interface
#'
#' Amoroso is excluded because it is a four-parameter kernel in this package.
#'
#' @param kernel Character scalar. Kernel name.
#' @return Logical scalar.
#' @keywords internal
.kernel_is_param12 <- function(kernel) {
  !identical(kernel, "amoroso")
}

#' Kernel-specific label map for Param1/Param2
#'
#' @param kernel Character scalar. Kernel name.
#' @return A named list with entries \code{Param1}, \code{Param2},
#'   \code{beta_Param1}, and \code{beta_Param2}.
#' @keywords internal
.kernel_param12_label_map <- function(kernel) {
  if (!.kernel_is_param12(kernel)) {
    stop("Kernel 'amoroso' is excluded from the Param1/Param2 interface.",
         call. = FALSE)
  }

  switch(
    kernel,
    gamma = list(
      Param1 = "shape", Param2 = "mean",
      beta_Param1 = "beta_shape", beta_Param2 = "beta_mean"
    ),
    normal = list(
      Param1 = "mu", Param2 = "sd",
      beta_Param1 = "beta_mu", beta_Param2 = "beta_sd"
    ),
    lognormal = list(
      Param1 = "meanlog", Param2 = "sdlog",
      beta_Param1 = "beta_meanlog", beta_Param2 = "beta_sdlog"
    ),
    laplace = list(
      Param1 = "location", Param2 = "scale",
      beta_Param1 = "beta_location", beta_Param2 = "beta_scale"
    ),
    inverse_gaussian = list(
      Param1 = "mu", Param2 = "lambda",
      beta_Param1 = "beta_mu", beta_Param2 = "beta_lambda"
    ),
    cauchy = list(
      Param1 = "location", Param2 = "scale",
      beta_Param1 = "beta_location", beta_Param2 = "beta_scale"
    ),
    pareto = list(
      Param1 = "shape", Param2 = "scale",
      beta_Param1 = "beta_shape", beta_Param2 = "beta_scale"
    ),
    stop("Unknown kernel '", kernel, "' for label mapping.", call. = FALSE)
  )
}

#' Relabel canonical parameter names for presentation
#'
#' @param x A named vector or an object with dimnames (for example, a matrix)
#'   whose dimnames include canonical parameter names.
#' @param kernel Kernel name.
#' @param regression Logical. If \code{TRUE}, also relabel
#'   \code{beta_Param1}/\code{beta_Param2} where present.
#' @return \code{x} with relabeled names where applicable.
#' @keywords internal
.relabel_param12 <- function(x, kernel, regression = FALSE) {
  m <- .kernel_param12_label_map(kernel)
  ren <- c(Param1 = m$Param1, Param2 = m$Param2)
  if (isTRUE(regression)) {
    ren <- c(ren, beta_Param1 = m$beta_Param1, beta_Param2 = m$beta_Param2)
  }

  if (!is.null(names(x))) {
    nn <- names(x)
    hit <- intersect(nn, names(ren))
    if (length(hit)) nn[match(hit, nn)] <- unname(ren[hit])
    names(x) <- nn
    return(x)
  }

  dn <- dimnames(x)
  if (!is.null(dn)) {
    for (k in seq_along(dn)) {
      if (is.null(dn[[k]])) next
      hit <- intersect(dn[[k]], names(ren))
      if (length(hit)) dn[[k]][match(hit, dn[[k]])] <- unname(ren[hit])
    }
    dimnames(x) <- dn
  }
  x
}

#' Extract canonical draws from an MCMC sample matrix
#'
#' The engine monitors parameters using conventional indexed names (for example,
#' \code{Param1[1]} or \code{beta_Param1[2,3]}). This extractor parses column
#' names and returns a structured draw list.
#'
#' @param mcmc_draws A numeric matrix with one row per saved draw and named columns.
#' @param spec A specification list containing at least \code{kernel},
#'   \code{backend} ("sb" or "crp"), \code{K}, and \code{p}.
#' @return A list of canonical draw arrays.
#' @keywords internal
.extract_draws_param12 <- function(mcmc_draws, spec) {
  if (!is.matrix(mcmc_draws) || is.null(colnames(mcmc_draws))) {
    stop("`mcmc_draws` must be a numeric matrix with column names.", call. = FALSE)
  }
  K <- as.integer(spec$K %||% 0L)
  p <- as.integer(spec$p %||% 0L)

  pick_mat1d <- function(prefix, K) {
    cols <- grep(paste0("^", prefix, "\\["), colnames(mcmc_draws), value = TRUE)
    if (!length(cols)) return(NULL)
    idx <- as.integer(sub(paste0("^", prefix, "\\[(\\d+)\\]$"), "\\1", cols))
    out <- matrix(NA_real_, nrow(mcmc_draws), K)
    colnames(out) <- paste0(prefix, "[", seq_len(K), "]")
    for (j in seq_along(cols)) out[, idx[j]] <- mcmc_draws[, cols[j]]
    out
  }

  pick_arr2d <- function(prefix, K, p) {
    cols <- grep(paste0("^", prefix, "\\["), colnames(mcmc_draws), value = TRUE)
    if (!length(cols)) return(NULL)
    m <- matrix(NA_real_, nrow(mcmc_draws), K * p)
    dim(m) <- c(nrow(mcmc_draws), K, p)
    dimnames(m) <- list(NULL, paste0(prefix, "[", seq_len(K), ",]"), paste0("V", seq_len(p)))
    for (nm in cols) {
      ij <- sub(paste0("^", prefix, "\\[(\\d+),(\\d+)\\]$"), "\\1,\\2", nm)
      sp <- strsplit(ij, ",", fixed = TRUE)[[1]]
      j <- as.integer(sp[1]); k <- as.integer(sp[2])
      m[, j, k] <- mcmc_draws[, nm]
    }
    m
  }

  draws <- list(
    Param1 = pick_mat1d("Param1", K),
    Param2 = pick_mat1d("Param2", K),
    beta_Param1 = if (p > 0L) pick_arr2d("beta_Param1", K, p) else NULL,
    beta_Param2 = if (p > 0L) pick_arr2d("beta_Param2", K, p) else NULL,
    w = if (identical(spec$backend, "sb")) pick_mat1d("w", K) else NULL,
    u = if ("u" %in% colnames(mcmc_draws)) mcmc_draws[, "u"] else NULL,
    sigma = if ("sigma" %in% colnames(mcmc_draws)) mcmc_draws[, "sigma"] else NULL,
    xi = if ("xi" %in% colnames(mcmc_draws)) mcmc_draws[, "xi"] else NULL
  )

  draws
}
