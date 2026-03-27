### Name: laplace_MixGpd
### Title: Laplace mixture with a GPD tail
### Aliases: laplace_MixGpd dLaplaceMixGpd pLaplaceMixGpd rLaplaceMixGpd
###   qLaplaceMixGpd

### ** Examples

w <- c(0.50, 0.30, 0.20)
location <- c(-1, 0.5, 2.0)
scale <- c(1.0, 0.7, 1.4)
threshold <- 1
tail_scale <- 1.0
tail_shape <- 0.2

dLaplaceMixGpd(2.0, w = w, location = location, scale = scale,
              threshold = threshold, tail_scale = tail_scale,
              tail_shape = tail_shape, log = FALSE)
pLaplaceMixGpd(2.0, w = w, location = location, scale = scale,
              threshold = threshold, tail_scale = tail_scale,
              tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE)
qLaplaceMixGpd(0.50, w = w, location = location, scale = scale,
              threshold = threshold, tail_scale = tail_scale,
              tail_shape = tail_shape)
qLaplaceMixGpd(0.95, w = w, location = location, scale = scale,
              threshold = threshold, tail_scale = tail_scale,
              tail_shape = tail_shape)
replicate(10, rLaplaceMixGpd(1, w = w, location = location, scale = scale,
                            threshold = threshold,
                            tail_scale = tail_scale,
                            tail_shape = tail_shape))



