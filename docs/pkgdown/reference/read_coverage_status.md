# Read stored coverage status

Read stored coverage status

## Usage

``` r
read_coverage_status(
  file = system.file("extdata/coverage_status.json", package = "DPmixGPD")
)
```

## Arguments

- file:

  Path to a JSON status file produced by
  `coverage_status(..., data_file = ...)`. Defaults to
  `inst/extdata/coverage_status.json`.

## Value

Named list as saved by
[`coverage_status()`](https://arnabaich96.github.io/DPmixGPD/pkgdown/reference/coverage_status.md).
