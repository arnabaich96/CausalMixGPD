#' Plot method for mixgpd_fit objects
#'
#' Currently supports predictive density plots for response-only Gamma DPM fits.
#'
#' @param x A `mixgpd_fit` object.
#' @param type Plot type. Currently supports `"density"`.
#' @param grid_n Number of grid points for density evaluation.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#' @export
plot.mixgpd_fit <- function(x, type = c("density"), grid_n = 200, ...) {
  type <- match.arg(type)

  if (!inherits(x, "mixgpd_fit")) stop("plot.mixgpd_fit: 'x' must be a mixgpd_fit.", call. = FALSE)

  # Only implemented for unconditional Gamma mixture at the moment
  draws <- .as_mcmc_matrix(x)
  .require_uncond_gamma(draws, where = "plot(type = 'density')")

  y <- x$spec$Y %||% x$Y
  if (is.null(y)) {
    stop("Cannot build plotting grid: outcome vector is missing.", call. = FALSE)
  }
  y <- as.numeric(y)
  y <- y[is.finite(y)]
  if (!length(y)) stop("Cannot build plotting grid: outcome vector is missing or invalid.", call. = FALSE)

  xmin <- max(0, min(y))
  xmax <- max(y)
  if (!is.finite(xmin) || !is.finite(xmax) || xmax <= xmin) {
    stop("Cannot build plotting grid: object$x_range is missing or invalid.", call. = FALSE)
  }

  grid_n <- as.integer(grid_n)
  if (!is.finite(grid_n) || grid_n < 25) grid_n <- 200L
  xgrid <- seq(from = xmin, to = xmax, length.out = grid_n)

  p <- .extract_gamma_dp_params(draws, renormalize_weights = TRUE)
  M <- p$M
  if (M <= 0L) stop("No posterior draws available for plotting.", call. = FALSE)

  # compute posterior mean predictive density on grid
  dens <- numeric(length(xgrid))
  for (m in seq_len(M)) {
    w <- p$w[m, ]
    shape <- p$shape[m, ]
    scale <- p$scale[m, ]
    # mixture density at grid
    dens <- dens + vapply(xgrid, function(xx) .gamma_mix_pdf_1(xx, w, shape, scale), numeric(1))
  }
  dens <- dens / M

  df <- data.frame(x = xgrid, density = dens)

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = density)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "y", y = "Posterior mean predictive density")
}
