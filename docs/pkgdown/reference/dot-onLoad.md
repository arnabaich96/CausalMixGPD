# Package hooks

Internal package initialization.

## Usage

``` r
.onLoad(libname, pkgname)
```

## Details

The load hook initializes package-wide defaults that should exist as
soon as the namespace is attached. In particular, it ensures that the
kernel and tail registries are ready for later model-building code and
sets the package plotly option if the user has not already chosen one.
