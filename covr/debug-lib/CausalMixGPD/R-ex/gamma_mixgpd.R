### Name: gamma_mixgpd
### Title: Gamma mixture with a GPD tail
### Aliases: gamma_mixgpd dGammaMixGpd pGammaMixGpd rGammaMixGpd
###   qGammaMixGpd

### ** Examples

w <- c(0.55, 0.30, 0.15)
scale <- c(1.0, 2.5, 5.0)
shape <- c(2, 4, 6)
threshold <- 3
tail_scale <- 0.9
tail_shape <- 0.2

dGammaMixGpd(4.0, w = w, scale = scale, shape = shape,
            threshold = threshold, tail_scale = tail_scale,
            tail_shape = tail_shape, log = 0)
pGammaMixGpd(4.0, w = w, scale = scale, shape = shape,
            threshold = threshold, tail_scale = tail_scale,
            tail_shape = tail_shape, lower.tail = 1, log.p = 0)
qGammaMixGpd(0.50, w = w, scale = scale, shape = shape,
            threshold = threshold, tail_scale = tail_scale,
            tail_shape = tail_shape)
qGammaMixGpd(0.95, w = w, scale = scale, shape = shape,
            threshold = threshold, tail_scale = tail_scale,
            tail_shape = tail_shape)
replicate(10, rGammaMixGpd(1, w = w, scale = scale, shape = shape,
                          threshold = threshold,
                          tail_scale = tail_scale,
                          tail_shape = tail_shape))



