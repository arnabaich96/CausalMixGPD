# Test coverage status helper

\`coverage_status()\` runs \`covr::package_coverage()\` against the
package tests and returns a small summary that can be stored for docs or
dashboards. Use \`read_coverage_status()\` to read previously saved
results.

## Usage

``` r
coverage_status(
  type = c("tests", "all"),
  path = ".",
  data_file = NULL,
  quiet = TRUE
)
```

## Arguments

- type:

  Coverage type, passed to \`covr::package_coverage()\`. Currently
  supports \`tests\` (the default) and \`all\`.

- path:

  Path to the package root. Defaults to the current working directory.

- data_file:

  Optional file path to write the status JSON (useful for documentation
  or pkgdown badges). When \`NULL\` no file is written.

- quiet:

  Passed to \`covr::package_coverage()\` to control console output.

## Value

A named list with \`percent\`, \`type\`, \`timestamp\`, \`statements\`,
and \`files\`. Invisibly returns the same list when writing to disk.
