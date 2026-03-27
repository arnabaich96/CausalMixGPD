### Name: base_lowercase
### Title: Lowercase vectorized distribution functions (base kernels)
### Aliases: base_lowercase dgpd pgpd qgpd rgpd dinvgauss pinvgauss
###   qinvgauss rinvgauss damoroso pamoroso qamoroso ramoroso dcauchy_vec
###   pcauchy_vec qcauchy_vec rcauchy_vec

### ** Examples

# GPD
dgpd(c(1.5, 2.0, 2.5), threshold = 1, scale = 0.8, shape = 0.2)
pgpd(c(1.5, 2.0), threshold = 1, scale = 0.8, shape = 0.2)
qgpd(c(0.5, 0.9), threshold = 1, scale = 0.8, shape = 0.2)
rgpd(5, threshold = 1, scale = 0.8, shape = 0.2)

# Inverse Gaussian
dinvgauss(c(1, 2, 3), mean = 2, shape = 5)
rinvgauss(5, mean = 2, shape = 5)

# Amoroso
damoroso(c(1, 2), loc = 0, scale = 1.5, shape1 = 2, shape2 = 1.2)
ramoroso(5, loc = 0, scale = 1.5, shape1 = 2, shape2 = 1.2)

# Cauchy
dcauchy_vec(c(-1, 0, 1), location = 0, scale = 1)
rcauchy_vec(5, location = 0, scale = 1)




