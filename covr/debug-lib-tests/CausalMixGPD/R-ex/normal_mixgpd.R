### Name: normal_mixgpd
### Title: Normal mixture with a GPD tail
### Aliases: normal_mixgpd dNormMixGpd pNormMixGpd rNormMixGpd qNormMixGpd

### ** Examples

w <- c(0.60, 0.25, 0.15)
mean <- c(-1, 0.5, 2.0)
sd <- c(1.0, 0.7, 1.3)
threshold <- 2
tail_scale <- 1.0
tail_shape <- 0.2

dNormMixGpd(3.0, w, mean, sd, threshold, tail_scale, tail_shape, log = FALSE)
pNormMixGpd(3.0, w, mean, sd, threshold, tail_scale, tail_shape,
           lower.tail = TRUE, log.p = FALSE)
qNormMixGpd(0.50, w, mean, sd, threshold, tail_scale, tail_shape)
qNormMixGpd(0.95, w, mean, sd, threshold, tail_scale, tail_shape)
replicate(10, rNormMixGpd(1, w, mean, sd, threshold, tail_scale, tail_shape))



