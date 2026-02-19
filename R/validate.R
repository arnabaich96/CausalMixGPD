#' Input validation and invariant enforcement (internal)
#'
#' Internal helper functions for validating inputs and enforcing package
#' invariants. These functions check for correct object types, reserved
#' keywords, and structural requirements.
#'
#' @name validation
#' @keywords internal
#' @noRd
NULL

#' Validate NIMBLE reserved keywords
#'
#' Checks that user-provided names (e.g., column names, variable names) do not
#' conflict with NIMBLE's reserved keywords. This prevents compilation failures
#' when building NIMBLE models.
#'
#' @param names Character vector of names to validate.
#' @param context Human-readable context for error messages (e.g., "Column names").
#' @return Invisibly TRUE if valid; otherwise stops with an error.
#' @keywords internal
#' @noRd
.validate_nimble_reserved_names <- function(names, context = "names") {
  if (is.null(names) || !length(names)) return(invisible(TRUE))
  names <- as.character(names)
  names <- names[!is.na(names) & nzchar(names)]
  if (!length(names)) return(invisible(TRUE))

  reserved <- c(
    "if", "else", "for", "while", "repeat", "break", "next", "in",
    "function", "return",
    "true", "false", "null", "na", "nan", "inf",
    "na_integer_", "na_real_", "na_character_", "na_complex_",
    "t", "f"
  )

  bad <- unique(names[tolower(names) %in% reserved])
  if (length(bad)) {
    stop(sprintf(
      "%s include reserved NIMBLE keywords: %s. Rename columns (e.g., if -> x_if).",
      context,
      paste(bad, collapse = ", ")
    ), call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate fitted object structure
#'
#' Ensures an object has the required structure of a fitted model object,
#' including proper class inheritance and presence of MCMC samples.
#'
#' @param object A fitted object to validate.
#' @return Invisibly TRUE if valid; otherwise stops with an error.
#' @keywords internal
#' @noRd
.validate_fit <- function(object) {
  if (!inherits(object, "mixgpd_fit")) {
    stop("Object must inherit from class 'mixgpd_fit'.", call. = FALSE)
  }
  smp <- object$mcmc$samples %||% object$samples
  if (is.null(smp)) {
    stop("No samples found in object$mcmc$samples (or object$samples).", call. = FALSE)
  }
  invisible(TRUE)
}
