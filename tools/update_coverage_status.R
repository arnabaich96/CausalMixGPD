devtools::load_all(".")
coverage_status(
  type = "tests",
  path = ".",
  data_file = file.path("inst", "extdata", "coverage_status.json"),
  quiet = TRUE
)
