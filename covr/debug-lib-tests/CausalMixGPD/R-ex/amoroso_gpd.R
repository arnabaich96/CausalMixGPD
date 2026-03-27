### Name: amoroso_gpd
### Title: Amoroso with a GPD tail
### Aliases: amoroso_gpd dAmorosoGpd pAmorosoGpd rAmorosoGpd qAmorosoGpd

### ** Examples

loc <- 0
scale <- 1.5
shape1 <- 2
shape2 <- 1.2
threshold <- 3
tail_scale <- 1.0
tail_shape <- 0.2

dAmorosoGpd(4.0, loc, scale, shape1, shape2,
           threshold, tail_scale, tail_shape, log = 0)
pAmorosoGpd(4.0, loc, scale, shape1, shape2,
           threshold, tail_scale, tail_shape, lower.tail = 1, log.p = 0)
qAmorosoGpd(0.50, loc, scale, shape1, shape2,
           threshold, tail_scale, tail_shape)
qAmorosoGpd(0.95, loc, scale, shape1, shape2,
           threshold, tail_scale, tail_shape)
replicate(10, rAmorosoGpd(1, loc, scale, shape1, shape2,
                         threshold, tail_scale, tail_shape))



