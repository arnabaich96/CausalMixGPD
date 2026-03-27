setwd(here::here())
library(covr)
library(jsonlite)

Sys.setenv(
  COVERAGE = "1",
  DPMIXGPD_TEST_LEVEL = "full",
  DPMIXGPD_CI_COVERAGE_ONLY = "1"
)

options(covr.install = c("--no-build-vignettes", "--no-manual"))

cov <- covr::package_coverage(
  type = "tests",
  quiet = FALSE,
  pre_clean = TRUE
)

if (!dir.exists("covr/assets")) dir.create("covr/assets", recursive = TRUE)
if (!dir.exists("docs/coverage")) dir.create("docs/coverage", recursive = TRUE)

pct <- covr::percent_coverage(cov)
line_cov <- covr::tally_coverage(cov, by = "line")
file_stats <- aggregate(cbind(lines = 1, covered = value > 0) ~ filename, data = line_cov, FUN = sum)
file_stats$percent <- round(100 * file_stats$covered / file_stats$lines, 1)
file_stats <- file_stats[order(-file_stats$percent, file_stats$filename), ]

status <- list(
  percent = round(pct, 2),
  timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  pipeline = "tests/full-alt-package-coverage",
  total_lines = sum(file_stats$lines),
  covered_lines = sum(file_stats$covered),
  files = nrow(file_stats),
  file_coverage = lapply(seq_len(nrow(file_stats)), function(i) {
    list(
      file = file_stats$filename[i],
      lines = file_stats$lines[i],
      covered = file_stats$covered[i],
      percent = file_stats$percent[i]
    )
  })
)

jsonlite::write_json(status, "covr/assets/coverage_status.json", auto_unbox = TRUE, pretty = TRUE)
file.copy("covr/assets/coverage_status.json", "docs/coverage/coverage_status.json", overwrite = TRUE)
cat("OVERALL:", pct, "\n")
cat("WROTE: covr/assets/coverage_status.json\n")
