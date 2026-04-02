# Historical top-level entrypoint retained for compatibility.
#
# The current test implementation lives in the integration fragment.

source(
  testthat::test_path("fragments", "integration", "coverage_heavy.R"),
  local = TRUE,
  encoding = "UTF-8"
)
