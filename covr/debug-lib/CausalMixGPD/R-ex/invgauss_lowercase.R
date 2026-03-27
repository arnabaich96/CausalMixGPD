### Name: invgauss_lowercase
### Title: Lowercase vectorized inverse Gaussian distribution functions
### Aliases: invgauss_lowercase dinvgaussmix pinvgaussmix qinvgaussmix
###   rinvgaussmix dinvgaussmixgpd pinvgaussmixgpd qinvgaussmixgpd
###   rinvgaussmixgpd dinvgaussgpd pinvgaussgpd qinvgaussgpd rinvgaussgpd

### ** Examples

w <- c(0.6, 0.3, 0.1)
mu <- c(1, 1.5, 2)
lam <- c(2, 3, 4)

# Inverse Gaussian mixture
dinvgaussmix(c(1, 2, 3), w = w, mean = mu, shape = lam)
rinvgaussmix(5, w = w, mean = mu, shape = lam)




