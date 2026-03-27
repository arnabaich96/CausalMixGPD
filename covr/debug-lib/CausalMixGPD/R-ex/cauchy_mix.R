### Name: cauchy_mix
### Title: Cauchy mixture distribution
### Aliases: cauchy_mix dCauchyMix pCauchyMix rCauchyMix qCauchyMix

### ** Examples

w <- c(0.50, 0.30, 0.20)
location <- c(-2, 0, 3)
scale <- c(1.0, 0.7, 1.5)

dCauchyMix(0.5, w = w, location = location, scale = scale, log = FALSE)
pCauchyMix(0.5, w = w, location = location, scale = scale,
           lower.tail = TRUE, log.p = FALSE)
qCauchyMix(0.50, w = w, location = location, scale = scale)
qCauchyMix(0.95, w = w, location = location, scale = scale)
replicate(10, rCauchyMix(1, w = w, location = location, scale = scale))




