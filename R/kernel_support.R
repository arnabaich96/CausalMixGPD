NULL

#' Kernel support matrix
#
#' Returns a data frame summarizing each kernel's supported features.
#'
#' @param round Logical; `TRUE` to replace logical values with symbols.
#' @return data.frame with columns `kernel`, `gpd`, `covariates`, `sb`, `crp`.
#' @export
kernel_support_table <- function(round = TRUE) {
  registry <- get_kernel_registry()
  rows <- lapply(registry, function(def) {
    has_gpd <- isTRUE(def$allow_gpd) && (
      (!is.null(def$sb) && !is.null(def$sb$d_gpd) && !isTRUE(is.na(def$sb$d_gpd))) ||
        (!is.null(def$crp) && !is.null(def$crp$d_gpd) && !isTRUE(is.na(def$crp$d_gpd)))
    )
    has_cov <- length(def$defaults_X) > 0 &&
      any(vapply(def$defaults_X, function(x) identical(x$mode, "link"), logical(1)))
    has_sb <- !is.null(def$sb) && !is.null(def$sb$d)
    has_crp <- !is.null(def$crp) && !is.null(def$crp$d_base)

    data.frame(
      kernel = def$key,
      gpd = has_gpd,
      covariates = has_cov,
      sb = has_sb,
      crp = has_crp,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  if (round) {
    fmt <- function(ok) vapply(ok, function(sf) if (isTRUE(sf)) "\u2714" else "\u274C", character(1))
    out$gpd <- fmt(out$gpd)
    out$covariates <- fmt(out$covariates)
    out$sb <- fmt(out$sb)
    out$crp <- fmt(out$crp)
  }
  out
}
