### Name: laplace_gpd
### Title: Laplace with a GPD tail
### Aliases: laplace_gpd dLaplaceGpd pLaplaceGpd rLaplaceGpd qLaplaceGpd

### ** Examples

location <- 0.5
scale <- 1.0
threshold <- 1
tail_scale <- 1.0
tail_shape <- 0.2

dLaplaceGpd(2.0, location, scale, threshold, tail_scale, tail_shape, log = FALSE)
pLaplaceGpd(2.0, location, scale, threshold, tail_scale, tail_shape,
           lower.tail = TRUE, log.p = FALSE)
qLaplaceGpd(0.50, location, scale, threshold, tail_scale, tail_shape)
qLaplaceGpd(0.95, location, scale, threshold, tail_scale, tail_shape)
replicate(10, rLaplaceGpd(1, location, scale, threshold, tail_scale, tail_shape))



