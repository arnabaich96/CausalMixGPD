### Name: cauchy_mix_lowercase
### Title: Lowercase vectorized Cauchy mixture distribution functions
### Aliases: cauchy_mix_lowercase dcauchymix pcauchymix qcauchymix
###   rcauchymix

### ** Examples

w <- c(0.6, 0.3, 0.1)
loc <- c(-1, 0, 1)
scl <- c(1, 1.2, 2)

dcauchymix(c(-2, 0, 2), w = w, location = loc, scale = scl)
rcauchymix(5, w = w, location = loc, scale = scl)




