# ============================================================
# 05-visualization-helpers.R
# Helper functions for type-specific prediction visualizations
# ============================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Plot quantile predictions with CI
#' @keywords internal
#' @noRd
.plot_quantile_pred <- function(pred, ...) {
  fit_df <- pred$fit

  if (!is.data.frame(fit_df)) {
    stop("Quantile prediction must return a data frame in $fit.", call. = FALSE)
  }

  plot_data <- fit_df
  has_id <- "id" %in% names(plot_data)

  if (has_id) {
    n_id <- length(unique(plot_data$id))
    pal <- .plot_palette(max(2L, n_id))
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = factor(index), y = estimate)) +
      ggplot2::geom_point(ggplot2::aes(color = factor(id)), size = 3) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper,
                                          group = id, color = factor(id)),
                             width = 0.2, linewidth = 1) +
      ggplot2::scale_color_manual(values = pal) +
      .plot_theme() +
      ggplot2::labs(
        title = "Quantile Predictions with Pointwise Credible Intervals",
        x = "Quantile Index",
        y = "Estimate"
      )
  } else {
    pal <- .plot_palette(2L)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = factor(index), y = estimate)) +
      ggplot2::geom_point(color = pal[1], size = 3) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                             width = 0.2, linewidth = 1, color = pal[1]) +
      .plot_theme() +
      ggplot2::labs(
        title = "Quantile Predictions with Pointwise Credible Intervals",
        x = "Quantile Index",
        y = "Estimate"
      )
  }

  .wrap_plotly(p)
}

#' Plot sample predictions: histogram with density overlay
#' @keywords internal
#' @noRd
.plot_sample_pred <- function(pred, ...) {
  samples <- pred$fit

  if (!is.numeric(samples)) {
    stop("Sample prediction must return a numeric vector in $fit.", call. = FALSE)
  }

  plot_data <- data.frame(value = samples)

  pal <- .plot_palette(8L)
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                           bins = 30, alpha = 0.7, fill = pal[5], color = pal[7]) +
    ggplot2::geom_density(color = pal[2], linewidth = 1) +
    .plot_theme() +
    ggplot2::labs(
      title = "Posterior Predictive Samples",
      x = "Value",
      y = "Density"
    )

  .wrap_plotly(p)
}

#' Plot mean predictions: histogram with mean line
#' @keywords internal
#' @noRd
.plot_mean_pred <- function(pred, ...) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  fit_df <- pred$fit

  # Extract estimate, lower, upper from data frame
  if (is.data.frame(fit_df)) {
    mean_val <- mean(fit_df$estimate, na.rm = TRUE)
    lower_val <- if ("lower" %in% names(fit_df)) mean(fit_df$lower, na.rm = TRUE) else NULL
    upper_val <- if ("upper" %in% names(fit_df)) mean(fit_df$upper, na.rm = TRUE) else NULL
  } else {
    # Fallback for old format
    mean_val <- mean(fit_df, na.rm = TRUE)
    lower_val <- NULL
    upper_val <- NULL
  }

  # Use posterior samples for histogram
  if (!is.null(pred$draws) && is.numeric(pred$draws) && length(pred$draws) > 1) {
    samples <- as.numeric(pred$draws)
    plot_data <- data.frame(value = samples)

    pal <- .plot_palette(8L)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                             bins = 30, alpha = 0.7, fill = pal[5], color = pal[7]) +
      ggplot2::geom_density(color = pal[1], linewidth = 1) +
      ggplot2::geom_vline(xintercept = mean_val, color = pal[2],
                         linewidth = 1.2, linetype = "solid") +
      {if (!is.null(lower_val) && !is.null(upper_val) && !is.na(lower_val) && !is.na(upper_val)) {
        list(
          ggplot2::geom_vline(xintercept = lower_val, color = pal[6],
                             linewidth = 0.8, linetype = "dashed"),
          ggplot2::geom_vline(xintercept = upper_val, color = pal[6],
                             linewidth = 0.8, linetype = "dashed")
        )
      }} +
      .plot_theme() +
      ggplot2::labs(
        title = "Posterior Predictive Mean Distribution",
        subtitle = paste0("Mean = ", round(mean_val, 3)),
        x = "Value",
        y = "Density"
      )
  } else {
    # If no samples available, show vertical lines only
    pal <- .plot_palette(8L)
    p <- ggplot2::ggplot(data.frame(x = mean_val), ggplot2::aes(x = x)) +
      ggplot2::geom_vline(xintercept = mean_val, color = pal[2],
                         linewidth = 2, linetype = "solid") +
      {if (!is.null(lower_val) && !is.null(upper_val) && !is.na(lower_val) && !is.na(upper_val)) {
        list(
          ggplot2::geom_vline(xintercept = lower_val, color = pal[6],
                             linewidth = 1, linetype = "dashed"),
          ggplot2::geom_vline(xintercept = upper_val, color = pal[6],
                             linewidth = 1, linetype = "dashed")
        )
      }} +
      .plot_theme() +
      ggplot2::labs(
        title = "Posterior Mean Estimate",
        subtitle = paste0("Mean = ", round(mean_val, 3)),
        x = "Value",
        y = ""
      ) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }

  .wrap_plotly(p)
}

#' Plot fit predictions (per-observation dots with CI)
#' @keywords internal
#' @noRd
.plot_fit_pred <- function(pred, ...) {
  fit_df <- pred$fit
  if (!is.data.frame(fit_df) || !all(c("id", "estimate") %in% names(fit_df))) {
    stop("Fit prediction must return a data.frame with columns 'id' and 'estimate'.", call. = FALSE)
  }

  plot_data <- fit_df
  pal <- .plot_palette(max(2L, length(unique(plot_data$id))))
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = factor(id), y = estimate)) +
    ggplot2::geom_point(ggplot2::aes(color = factor(id)), size = 3) +
    {if ("lower" %in% names(plot_data) && "upper" %in% names(plot_data)) {
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper, group = id, color = factor(id)),
                             width = 0.2, linewidth = 1)
    }} +
    ggplot2::scale_color_manual(values = pal) +
    .plot_theme() +
    ggplot2::labs(
      title = "Per-observation Fit Estimates with Pointwise Credible Intervals",
      x = "Observation (id)",
      y = "Estimate"
    )

  .wrap_plotly(p)
}

#' Plot density predictions
#' @keywords internal
#' @noRd
.plot_density_pred <- function(pred, ...) {
  fit_val <- pred$fit

  # Handle both data frame and vector cases
  if (is.data.frame(fit_val)) {
    plot_data <- fit_val
    x_col <- if ("y" %in% names(plot_data)) "y" else if ("grid" %in% names(plot_data)) "grid" else names(plot_data)[1]
    y_col <- if ("density" %in% names(plot_data)) "density" else names(plot_data)[2]
    has_id <- "id" %in% names(plot_data)
  } else {
    y_grid <- pred$grid %||% seq_along(fit_val)
    plot_data <- data.frame(
      y = y_grid,
      density = as.numeric(fit_val)
    )
    x_col <- "y"
    y_col <- "density"
    has_id <- FALSE
  }

  if (has_id) {
    n_id <- length(unique(plot_data$id))
    pal <- .plot_palette(max(2L, n_id))
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])) +
      ggplot2::geom_line(ggplot2::aes(group = id, color = factor(id)), linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(color = factor(id)), size = 1.5) +
      ggplot2::scale_color_manual(values = pal) +
      .plot_theme() +
      ggplot2::labs(
        title = "Posterior Predictive Density",
        x = "Value",
        y = "Density"
      )
  } else {
    pal <- .plot_palette(2L)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])) +
      ggplot2::geom_line(color = pal[1], linewidth = 1) +
      ggplot2::geom_point(color = pal[1], size = 2) +
      .plot_theme() +
      ggplot2::labs(
        title = "Posterior Predictive Density",
        x = "Value",
        y = "Density"
      )
  }

  .wrap_plotly(p)
}

#' Plot survival function predictions
#' @keywords internal
#' @noRd
.plot_survival_pred <- function(pred, ...) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  fit_val <- pred$fit

  # Handle data frame format from prediction
  if (is.data.frame(fit_val)) {
    plot_data <- fit_val
    if (!("y" %in% names(plot_data))) names(plot_data)[1] <- "y"
    if (!("survival" %in% names(plot_data))) names(plot_data)[2] <- "survival"
    has_id <- "id" %in% names(plot_data)
  } else {
    # Handle vector format (old style)
    y_vals <- pred$grid %||% seq_along(fit_val)
    plot_data <- data.frame(
      y = y_vals,
      survival = as.numeric(fit_val)
    )
    has_id <- FALSE
  }

  # Sort by y values for proper survival curve
  plot_data <- plot_data[order(plot_data$y), ]

  if (has_id) {
    n_id <- length(unique(plot_data$id))
    pal <- .plot_palette(max(2L, n_id))
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = y, y = survival)) +
      ggplot2::geom_step(ggplot2::aes(group = id, color = factor(id)), direction = "hv",
                         linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(color = factor(id)), size = 1.5) +
      ggplot2::scale_color_manual(values = pal) +
      .plot_theme() +
      ggplot2::labs(
        title = "Posterior Predictive Survival Function",
        x = "Value",
        y = "Survival Probability"
      ) +
      ggplot2::ylim(0, 1)
  } else {
    pal <- .plot_palette(2L)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = y, y = survival)) +
      ggplot2::geom_step(direction = "hv", color = pal[1], linewidth = 1) +
      ggplot2::geom_point(color = pal[1], size = 2) +
      .plot_theme() +
      ggplot2::labs(
        title = "Posterior Predictive Survival Function",
        x = "Value",
        y = "Survival Probability"
      ) +
      ggplot2::ylim(0, 1)
  }

  .wrap_plotly(p)
}

#' Plot mean/median location predictions
#' @keywords internal
#' @noRd
.plot_location_pred <- function(pred, ...) {
  fit_df <- pred$fit

  if (!is.data.frame(fit_df)) {
    stop("Location prediction must return a data frame in $fit.", call. = FALSE)
  }

  has_id <- "id" %in% names(fit_df)
  if (!has_id) {
    plot_data <- data.frame(
      type = factor(c("mean", "median"), levels = c("mean", "median")),
      estimate = c(fit_df$mean[1], fit_df$median[1]),
      lower = c(fit_df$mean_lower[1], fit_df$median_lower[1]),
      upper = c(fit_df$mean_upper[1], fit_df$median_upper[1])
    )

    pal <- .plot_palette(2L)
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = type, y = estimate, color = type)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                             width = 0.2, linewidth = 1) +
      ggplot2::scale_color_manual(values = pal) +
      .plot_theme() +
      ggplot2::labs(
        title = "Location Predictions",
        x = "",
        y = "Estimate"
      )
    return(.wrap_plotly(p))
  }

  plot_data <- rbind(
    data.frame(
      id = fit_df$id,
      type = "mean",
      estimate = fit_df$mean,
      lower = fit_df$mean_lower,
      upper = fit_df$mean_upper
    ),
    data.frame(
      id = fit_df$id,
      type = "median",
      estimate = fit_df$median,
      lower = fit_df$median_lower,
      upper = fit_df$median_upper
    )
  )

  pal <- .plot_palette(2L)
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = id, y = estimate, color = type)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                           width = 0.2, linewidth = 0.8) +
    ggplot2::scale_color_manual(values = pal) +
    .plot_theme() +
      ggplot2::labs(
        title = "Location Predictions",
        x = "Observation",
        y = "Estimate"
      )
  .wrap_plotly(p)
}
