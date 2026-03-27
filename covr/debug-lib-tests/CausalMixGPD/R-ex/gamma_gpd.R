### Name: gamma_gpd
### Title: Gamma with a GPD tail
### Aliases: gamma_gpd dGammaGpd pGammaGpd rGammaGpd qGammaGpd

### ** Examples

scale <- 2.5
shape <- 4
threshold <- 3
tail_scale <- 0.9
tail_shape <- 0.2

dGammaGpd(4.0, scale = scale, shape = shape,
         threshold = threshold, tail_scale = tail_scale,
         tail_shape = tail_shape, log = 0)
pGammaGpd(4.0, scale = scale, shape = shape,
         threshold = threshold, tail_scale = tail_scale,
         tail_shape = tail_shape, lower.tail = 1, log.p = 0)
qGammaGpd(0.50, scale = scale, shape = shape,
         threshold = threshold, tail_scale = tail_scale,
         tail_shape = tail_shape)
qGammaGpd(0.95, scale = scale, shape = shape,
         threshold = threshold, tail_scale = tail_scale,
         tail_shape = tail_shape)
replicate(10, rGammaGpd(1, scale = scale, shape = shape,
                       threshold = threshold,
                       tail_scale = tail_scale,
                       tail_shape = tail_shape))




