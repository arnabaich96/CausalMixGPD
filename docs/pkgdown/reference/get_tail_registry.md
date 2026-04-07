# Get tail registry

Get tail registry

## Usage

``` r
get_tail_registry()
```

## Value

A list of tail metadata.

## Details

The tail registry is the authoritative description of the generalized
Pareto splice used by bulk-tail models. It records the tail parameter
names `threshold`, `tail_scale`, and `tail_shape`, together with the
support each parameter must satisfy and the modeling modes the builders
may assign to them.

In mathematical terms, for a threshold \\u\\ the upper tail is
represented with a generalized Pareto law for excesses above \\u\\.
Accessing this registry is useful when inspecting how the package
encodes those tail parameters before model compilation.

## Examples

``` r
init_kernel_registry()
tail <- get_tail_registry()
tail$params
#> [1] "threshold"  "tail_scale" "tail_shape"
```
