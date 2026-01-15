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
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = factor(index), y = estimate)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Quantile Predictions with Credible Intervals",
      x = "Quantile Index",
      y = "Estimate"
    )
  
  print(p)
  invisible(p)
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
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), 
                           bins = 30, alpha = 0.6, fill = "blue") +
    ggplot2::geom_density(color = "red", linewidth = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Posterior Predictive Samples",
      x = "Value",
      y = "Density"
    )
  
  print(p)
  invisible(p)
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
    samples <- pred$draws
    plot_data <- data.frame(value = samples)
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), 
                             bins = 30, alpha = 0.6, fill = "lightblue") +
      ggplot2::geom_density(color = "blue", linewidth = 1) +
      ggplot2::geom_vline(xintercept = mean_val, color = "red", 
                         linewidth = 1.2, linetype = "solid") +
      {if (!is.null(lower_val) && !is.null(upper_val) && !is.na(lower_val) && !is.na(upper_val)) {
        list(
          ggplot2::geom_vline(xintercept = lower_val, color = "orange", 
                             linewidth = 0.8, linetype = "dashed"),
          ggplot2::geom_vline(xintercept = upper_val, color = "orange", 
                             linewidth = 0.8, linetype = "dashed")
        )
      }} +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Posterior Predictive Mean Distribution",
        subtitle = paste0("Mean = ", round(mean_val, 3)),
        x = "Value",
        y = "Density"
      )
  } else {
    # If no samples available, show vertical lines only
    p <- ggplot2::ggplot(data.frame(x = mean_val), ggplot2::aes(x = x)) +
      ggplot2::geom_vline(xintercept = mean_val, color = "red", 
                         linewidth = 2, linetype = "solid") +
      {if (!is.null(lower_val) && !is.null(upper_val) && !is.na(lower_val) && !is.na(upper_val)) {
        list(
          ggplot2::geom_vline(xintercept = lower_val, color = "orange", 
                             linewidth = 1, linetype = "dashed"),
          ggplot2::geom_vline(xintercept = upper_val, color = "orange", 
                             linewidth = 1, linetype = "dashed")
        )
      }} +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Posterior Mean Estimate",
        subtitle = paste0("Mean = ", round(mean_val, 3)),
        x = "Value",
        y = ""
      ) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }
  
  print(p)
  invisible(p)
}

#' Plot density predictions
#' @keywords internal
#' @noRd
.plot_density_pred <- function(pred, ...) {
  fit_val <- pred$fit
  
  # Handle both data frame and vector cases
  if (is.data.frame(fit_val)) {
    # fit is a data frame with grid and density columns
    plot_data <- fit_val
    if (!("grid" %in% names(plot_data))) {
      names(plot_data)[1] <- "grid"
    }
    if (!("density" %in% names(plot_data))) {
      names(plot_data)[2] <- "density"
    }
  } else {
    # fit is a vector of density values
    y_grid <- pred$grid %||% seq_along(fit_val)
    plot_data <- data.frame(
      grid = y_grid,
      density = as.numeric(fit_val)
    )
  }
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = grid, y = density)) +
    ggplot2::geom_line(color = "blue", linewidth = 1) +
    ggplot2::geom_point(color = "blue", size = 2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Posterior Predictive Density",
      x = "Value",
      y = "Density"
    )
  
  print(p)
  invisible(p)
}

#' Plot survival function predictions
#' @keywords internal
#' @noRd
.plot_survival_pred <- function(pred, ...) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  fit_val <- pred$fit
  
  # Handle data frame format from prediction
  if (is.data.frame(fit_val)) {
    # Extract y and survival columns
    if ("y" %in% names(fit_val) && "survival" %in% names(fit_val)) {
      plot_data <- fit_val[, c("y", "survival")]
    } else {
      # Fallback: assume first two columns are y and survival
      plot_data <- data.frame(
        y = fit_val[[1]],
        survival = fit_val[[2]]
      )
      names(plot_data) <- c("y", "survival")
    }
  } else {
    # Handle vector format (old style)
    y_vals <- pred$grid %||% seq_along(fit_val)
    plot_data <- data.frame(
      y = y_vals,
      survival = as.numeric(fit_val)
    )
  }
  
  # Sort by y values for proper survival curve
  plot_data <- plot_data[order(plot_data$y), ]
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = y, y = survival)) +
    ggplot2::geom_step(direction = "hv", color = "blue", linewidth = 1) +
    ggplot2::geom_point(color = "blue", size = 2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Posterior Predictive Survival Function",
      x = "Value",
      y = "Survival Probability"
    ) +
    ggplot2::ylim(0, 1)
  
  print(p)
  invisible(p)
}
