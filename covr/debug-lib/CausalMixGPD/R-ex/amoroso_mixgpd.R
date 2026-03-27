### Name: amoroso_mixgpd
### Title: Amoroso mixture with a GPD tail
### Aliases: amoroso_mixgpd dAmorosoMixGpd pAmorosoMixGpd rAmorosoMixGpd
###   qAmorosoMixGpd

### ** Examples

w <- c(0.60, 0.25, 0.15)
loc <- c(0, 1, 2)
scale <- c(1.0, 1.2, 1.6)
shape1 <- c(2, 4, 6)
shape2 <- c(1.0, 1.2, 1.5)
threshold <- 3
tail_scale <- 1.0
tail_shape <- 0.2

dAmorosoMixGpd(4.0, w, loc, scale, shape1, shape2,
              threshold, tail_scale, tail_shape, log = 0)
pAmorosoMixGpd(4.0, w, loc, scale, shape1, shape2,
              threshold, tail_scale, tail_shape, lower.tail = 1, log.p = 0)
qAmorosoMixGpd(0.50, w, loc, scale, shape1, shape2,
              threshold, tail_scale, tail_shape)
qAmorosoMixGpd(0.95, w, loc, scale, shape1, shape2,
              threshold, tail_scale, tail_shape)
replicate(10, rAmorosoMixGpd(1, w, loc, scale, shape1, shape2,
                            threshold, tail_scale, tail_shape))



