# Print a propensity score bundle

Print a propensity score bundle

## Usage

``` r
# S3 method for class 'causalmixgpd_ps_bundle'
print(x, code = FALSE, max_code_lines = 200L, ...)
```

## Arguments

- x:

  A `"causalmixgpd_ps_bundle"` object.

- code:

  Logical; if TRUE, print generated NIMBLE code for the PS model.

- max_code_lines:

  Integer; maximum number of code lines to print when `code=TRUE`.

- ...:

  Unused.

## Value

The input object (invisibly).

## Details

A PS bundle is the pre-sampling representation of the
treatment-assignment model \\e(x) = \Pr(A = 1 \mid X = x)\\. Depending
on the stored model type, the latent linear predictor is later mapped to
a probability through a logit link, a probit link, or a naive Bayes
factorization.

The printed output is limited to the structural PS choices because
posterior draws do not exist yet. Use this method as a quick check that
the requested treatment model was encoded correctly before fitting the
full causal bundle.
