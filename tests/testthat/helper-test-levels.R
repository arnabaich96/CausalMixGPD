# helper-test-levels.R
# Tiered testing infrastructure for CausalMixGPD
#
# Test Tiers:
#   - "cran": Fast unit/contract tests only (~seconds to minutes). Default for R CMD check.
#   - "ci":   Integration tests (Tier A + B). For CI pipelines.
#   - "full": Exhaustive combo grids (Tier A + B + C). Nightly/local only.
#
# Environment variable: DPMIXGPD_TEST_LEVEL (default: "cran")
#
# Usage in test files:
#   skip_if_not_test_level("ci")   # Skip unless level is "ci" or "full"
#   skip_if_not_test_level("full") # Skip unless level is "full"

#' Get current test level
#' @return Character: "cran", "ci", or "full"
#' @keywords internal
get_test_level <- function() {

  level <- tolower(Sys.getenv("DPMIXGPD_TEST_LEVEL", "cran"))
  if (!level %in% c("cran", "ci", "full")) {
    warning("Unknown DPMIXGPD_TEST_LEVEL '", level, "'; defaulting to 'cran'")
    level <- "cran"
  }
  level
}

#' Check if current test level meets minimum requirement
#' @param min_level Minimum required level: "cran", "ci", or "full"
#' @return Logical
#' @keywords internal
test_level_at_least <- function(min_level) {
  levels <- c("cran" = 1L, "ci" = 2L, "full" = 3L)
  current <- get_test_level()
  current_rank <- levels[[current]]
  min_rank <- levels[[min_level]]
  if (is.na(min_rank)) stop("Unknown test level: ", min_level)
  current_rank >= min_rank
}

#' Skip test if current level is below minimum
#' @param min_level Minimum required level: "ci" or "full"
#' @keywords internal
skip_if_not_test_level <- function(min_level) {
  if (!test_level_at_least(min_level)) {
    testthat::skip(paste0(
      "Test level '", min_level, "' required (current: '", get_test_level(), "'). ",
      "Set DPMIXGPD_TEST_LEVEL='", min_level, "' to run."
    ))
  }
}

#' Skip slow/integration tests (convenience wrapper)
#' @keywords internal
skip_if_cran <- function() {
  skip_if_not_test_level("ci")
}

#' Skip exhaustive tests (convenience wrapper)
#' @keywords internal
skip_if_not_full <- function() {
  skip_if_not_test_level("full")
}

#' Standard minimal MCMC settings for fast tests
#' @keywords internal
mcmc_fast <- function(seed = 1L) {
  list(niter = 20L, nburnin = 5L, thin = 1L, nchains = 1L, seed = seed)
}

#' Standard MCMC settings for integration tests (slightly longer)
#' @keywords internal
mcmc_integration <- function(seed = 1L) {
  list(niter = 50L, nburnin = 15L, thin = 1L, nchains = 1L, seed = seed)
}

#' Standard MCMC settings for exhaustive tests
#' @keywords internal
mcmc_full <- function(seed = 1L) {
  list(niter = 200L, nburnin = 50L, thin = 2L, nchains = 1L, seed = seed)
}

#' Representative kernel/backend/GPD combos for integration tests
#' Covers the main branches without exhaustive enumeration
#' @keywords internal
representative_combos <- function() {
  list(
    # Normal: most common, test both backends
    list(kernel = "normal", backend = "sb",  GPD = FALSE, label = "normal_sb_bulk"),
    list(kernel = "normal", backend = "crp", GPD = TRUE,  label = "normal_crp_gpd"),
    # Gamma: positive support, different code path

    list(kernel = "gamma",  backend = "sb",  GPD = TRUE,  label = "gamma_sb_gpd"),
    # Lognormal: another positive kernel
    list(kernel = "lognormal", backend = "crp", GPD = FALSE, label = "lognormal_crp_bulk")
  )
}

#' Representative causal combos for integration tests
#' @keywords internal
representative_causal_combos <- function() {
  list(
    # Standard observational with PS
    list(
      kernel = c("normal", "normal"),
      backend = c("sb", "sb"),
      GPD = c(FALSE, FALSE),
      PS = "logit",
      label = "normal_sb_logit"
    ),
    # RCT without PS
    list(
      kernel = c("gamma", "gamma"),
      backend = c("crp", "crp"),
      GPD = c(TRUE, TRUE),
      PS = FALSE,
      label = "gamma_crp_gpd_rct"
    ),
    # Mixed backends
    list(
      kernel = c("lognormal", "normal"),
      backend = c("sb", "crp"),
      GPD = c(FALSE, FALSE),
      PS = "probit",
      label = "mixed_probit"
    )
  )
}

# Message at load time if verbose
if (Sys.getenv("DPMIXGPD_TEST_VERBOSE", "0") %in% c("1", "true", "TRUE")) {
  message("[CausalMixGPD tests] Test level: ", get_test_level())
}
