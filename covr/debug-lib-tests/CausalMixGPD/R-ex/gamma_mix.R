### Name: gamma_mix
### Title: Gamma mixture distribution
### Aliases: gamma_mix dGammaMix pGammaMix rGammaMix qGammaMix

### ** Examples

w <- c(0.55, 0.30, 0.15)
scale <- c(1.0, 2.5, 5.0)
shape <- c(2, 4, 6)

dGammaMix(2.0, w = w, scale = scale, shape = shape, log = 0)
pGammaMix(2.0, w = w, scale = scale, shape = shape, lower.tail = 1, log.p = 0)
qGammaMix(0.50, w = w, scale = scale, shape = shape)
qGammaMix(0.95, w = w, scale = scale, shape = shape)
replicate(10, rGammaMix(1, w = w, scale = scale, shape = shape))



