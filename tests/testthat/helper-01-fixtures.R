.fixture_path <- function(name) {
  file.path("tests", "testthat", "fixtures", name)
}

.load_fixture <- function(name) {
  path <- .fixture_path(name)
  if (!file.exists(path)) {
    skip(sprintf("Fixture not found: %s", path))
  }
  readRDS(path)
}
