### Name: gpd
### Title: Generalized Pareto distribution
### Aliases: gpd dGpd pGpd rGpd qGpd

### ** Examples

threshold <- 1
tail_scale <- 0.8
tail_shape <- 0.2

dGpd(1.5, threshold, tail_scale, tail_shape, log = 0)
pGpd(1.5, threshold, tail_scale, tail_shape, lower.tail = 1, log.p = 0)
qGpd(0.50, threshold, tail_scale, tail_shape)
qGpd(0.95, threshold, tail_scale, tail_shape)
replicate(10, rGpd(1, threshold, tail_scale, tail_shape))



