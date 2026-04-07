# Detect the first present argument name in dots.

Detect the first present argument name in dots.

## Usage

``` r
.detect_first_present(dots, candidates = c("q", "x"))
```

## Details

Some wrapped scalar functions accept either `q` or `x` as their first
numeric argument depending on the original API. This helper inspects
`...` and returns the first candidate name that is actually present so
wrapper code can map user input onto the target function signature.
