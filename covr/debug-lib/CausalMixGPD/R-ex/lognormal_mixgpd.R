### Name: lognormal_mixgpd
### Title: Lognormal mixture with a GPD tail
### Aliases: lognormal_mixgpd dLognormalMixGpd pLognormalMixGpd
###   rLognormalMixGpd qLognormalMixGpd

### ** Examples

w <- c(0.60, 0.25, 0.15)
meanlog <- c(-0.2, 0.6, 1.2)
sdlog <- c(0.4, 0.3, 0.5)
threshold <- 3
tail_scale <- 0.9
tail_shape <- 0.2

dLognormalMixGpd(4.0, w = w, meanlog = meanlog, sdlog = sdlog,
                threshold = threshold, tail_scale = tail_scale,
                tail_shape = tail_shape, log = FALSE)
pLognormalMixGpd(4.0, w = w, meanlog = meanlog, sdlog = sdlog,
                threshold = threshold, tail_scale = tail_scale,
                tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE)
qLognormalMixGpd(0.50, w = w, meanlog = meanlog, sdlog = sdlog,
                threshold = threshold, tail_scale = tail_scale,
                tail_shape = tail_shape)
qLognormalMixGpd(0.95, w = w, meanlog = meanlog, sdlog = sdlog,
                threshold = threshold, tail_scale = tail_scale,
                tail_shape = tail_shape)
replicate(10, rLognormalMixGpd(1, w = w, meanlog = meanlog, sdlog = sdlog,
                              threshold = threshold,
                              tail_scale = tail_scale,
                              tail_shape = tail_shape))



