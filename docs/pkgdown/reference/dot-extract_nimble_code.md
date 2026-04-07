# Extract nimbleCode from bundle code

Extract nimbleCode from bundle code

## Usage

``` r
.extract_nimble_code(code)
```

## Details

Bundle objects may store generated NIMBLE code either directly as a
`nimbleCode` object or inside a small wrapper list used for package
storage. This helper normalizes those storage conventions so downstream
code can work with the underlying code object without repeatedly
checking both cases.
