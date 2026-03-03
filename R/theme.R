#' Plot styling and theme helpers (internal)
#'
#' Internal helper functions for consistent plot styling across the package.
#' These functions provide default color palettes, themes, and plotly conversion
#' utilities.
#'
#' @name viz-theme
#' @keywords internal
#' @noRd
NULL

#' Get default color palette
#'
#' Returns a consistent color palette for plotting. Uses colorblind-friendly
#' Wong palette as base.
#'
#' @param n Integer, number of colors needed. If NULL or greater than 8,
#'   repeats the base palette.
#'
#' @return Character vector of hex colors.
#' @keywords internal
#' @noRd
.plot_palette <- function(n = 8L) {
  base <- c(
    "#0072B2", # blue
    "#D55E00", # vermillion
    "#009E73", # green
    "#CC79A7", # purple
    "#56B4E9", # sky blue
    "#E69F00", # orange
    "#000000", # black
    "#999999"  # gray
  )
  n <- as.integer(n %||% length(base))
  if (n <= length(base)) return(base[seq_len(n)])
  rep_len(base, n)
}

#' Get default ggplot2 theme
#'
#' Returns a minimal theme with package-specific styling.
#'
#' @return A ggplot2 theme object.
#' @keywords internal
#' @noRd
.plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 20),
      axis.title = ggplot2::element_text(size = 17),
      axis.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 15),
      legend.text = ggplot2::element_text(size = 13),
      strip.text = ggplot2::element_text(size = 15),
      legend.position = "top"
    )
}

#' Strip fill scales from ggplot object
#'
#' Removes fill aesthetics scales from a ggplot object. Used to work around
#' plotly conversion issues with certain fill scales.
#'
#' @param p A ggplot object.
#' @return The modified ggplot object.
#' @keywords internal
#' @noRd
.strip_fill_scales <- function(p) {
  if (!inherits(p, "ggplot")) return(p)
  if (is.null(p$scales) || !length(p$scales$scales)) return(p)
  keep <- vapply(p$scales$scales, function(s) {
    !("fill" %in% (s$aesthetics %||% character()))
  }, logical(1))
  p$scales$scales <- p$scales$scales[keep]
  p
}

#' Safely convert ggplot to plotly
#'
#' Attempts to convert a ggplot to plotly, with fallback to stripping
#' fill scales if the initial conversion fails.
#'
#' @param p A ggplot object.
#' @return A plotly object or the original ggplot if conversion fails.
#' @keywords internal
#' @noRd
.safe_ggplotly <- function(p) {
  tryCatch(
    plotly::ggplotly(p),
    error = function(e) {
      p2 <- .strip_fill_scales(p)
      tryCatch(plotly::ggplotly(p2), error = function(e2) p2)
    }
  )
}

#' Wrap plots in plotly if requested
#'
#' Conditionally converts ggplot objects to plotly based on user option.
#' Respects `options(CausalMixGPD.plotly = TRUE)` setting. Handles both
#' single ggplot objects and lists of plots.
#'
#' @param p A ggplot object or list of ggplot objects.
#' @return Either plotly object(s) or the original plot(s).
#' @keywords internal
#' @noRd
.wrap_plotly <- function(p) {
  # Default to static output; opt in via options(CausalMixGPD.plotly = TRUE).
  if (isTRUE(getOption("CausalMixGPD.plotly", FALSE)) &&
      requireNamespace("plotly", quietly = TRUE)) {
    if (is.list(p) && !inherits(p, "ggplot")) {
      # List of plots - wrap each, preserve class
      result <- lapply(p, function(plt) {
        if (inherits(plt, "ggplot")) .safe_ggplotly(plt) else plt
      })
      # Preserve original class attributes
      class(result) <- class(p)
      result
    } else if (inherits(p, "ggplot")) {
      # Single ggplot - wrap it
      .safe_ggplotly(p)
    } else {
      # Not a ggplot, return as-is
      p
    }
  } else {
    # plotly not available - return original
    p
  }
}
