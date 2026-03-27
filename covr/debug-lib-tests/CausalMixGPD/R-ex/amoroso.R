### Name: amoroso
### Title: Amoroso distribution
### Aliases: amoroso dAmoroso pAmoroso rAmoroso qAmoroso

### ** Examples

loc <- 0
scale <- 1.5
shape1 <- 2
shape2 <- 1.2

dAmoroso(1.0, loc, scale, shape1, shape2, log = 0)
pAmoroso(1.0, loc, scale, shape1, shape2, lower.tail = 1, log.p = 0)
qAmoroso(0.50, loc, scale, shape1, shape2)
qAmoroso(0.95, loc, scale, shape1, shape2)
replicate(10, rAmoroso(1, loc, scale, shape1, shape2))



