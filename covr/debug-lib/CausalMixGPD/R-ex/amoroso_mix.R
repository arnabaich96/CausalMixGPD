### Name: amoroso_mix
### Title: Amoroso mixture distribution
### Aliases: amoroso_mix dAmorosoMix pAmorosoMix rAmorosoMix qAmorosoMix

### ** Examples

w <- c(0.60, 0.25, 0.15)
loc <- c(0, 1, 2)
scale <- c(1.0, 1.2, 1.6)
shape1 <- c(2, 4, 6)
shape2 <- c(1.0, 1.2, 1.5)

dAmorosoMix(2.0, w, loc, scale, shape1, shape2, log = 0)
pAmorosoMix(2.0, w, loc, scale, shape1, shape2, lower.tail = 1, log.p = 0)
qAmorosoMix(0.50, w, loc, scale, shape1, shape2)
qAmorosoMix(0.95, w, loc, scale, shape1, shape2)
replicate(10, rAmorosoMix(1, w, loc, scale, shape1, shape2))



