### Name: InvGauss_mixgpd
### Title: Inverse Gaussian mixture with a GPD tail
### Aliases: InvGauss_mixgpd dInvGaussMixGpd pInvGaussMixGpd
###   rInvGaussMixGpd qInvGaussMixGpd

### ** Examples

w <- c(0.55, 0.30, 0.15)
mean <- c(1.0, 2.5, 5.0)
shape <- c(2, 4, 8)
threshold <- 3
tail_scale <- 0.9
tail_shape <- 0.2

dInvGaussMixGpd(4.0, w = w, mean = mean, shape = shape,
               threshold = threshold, tail_scale = tail_scale,
               tail_shape = tail_shape, log = 0)
pInvGaussMixGpd(4.0, w = w, mean = mean, shape = shape,
               threshold = threshold, tail_scale = tail_scale,
               tail_shape = tail_shape, lower.tail = 1, log.p = 0)
qInvGaussMixGpd(0.50, w = w, mean = mean, shape = shape,
               threshold = threshold, tail_scale = tail_scale,
               tail_shape = tail_shape)
qInvGaussMixGpd(0.95, w = w, mean = mean, shape = shape,
               threshold = threshold, tail_scale = tail_scale,
               tail_shape = tail_shape)
replicate(10, rInvGaussMixGpd(1, w = w, mean = mean, shape = shape,
                             threshold = threshold,
                             tail_scale = tail_scale,
                             tail_shape = tail_shape))



