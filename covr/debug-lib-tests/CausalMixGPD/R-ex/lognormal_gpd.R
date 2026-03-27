### Name: lognormal_gpd
### Title: Lognormal with a GPD tail
### Aliases: lognormal_gpd dLognormalGpd pLognormalGpd rLognormalGpd
###   qLognormalGpd

### ** Examples

meanlog <- 0.4
sdlog <- 0.35
threshold <- 3
tail_scale <- 0.9
tail_shape <- 0.2

dLognormalGpd(4.0, meanlog, sdlog, threshold, tail_scale, tail_shape, log = FALSE)
pLognormalGpd(4.0, meanlog, sdlog, threshold, tail_scale, tail_shape,
             lower.tail = TRUE, log.p = FALSE)
qLognormalGpd(0.50, meanlog, sdlog, threshold, tail_scale, tail_shape)
qLognormalGpd(0.95, meanlog, sdlog, threshold, tail_scale, tail_shape)
replicate(10, rLognormalGpd(1, meanlog, sdlog, threshold, tail_scale, tail_shape))



