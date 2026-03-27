### Name: normal_gpd
### Title: Normal with a GPD tail
### Aliases: normal_gpd dNormGpd pNormGpd rNormGpd qNormGpd

### ** Examples

mean <- 0.5
sd <- 1.0
threshold <- 2
tail_scale <- 1.0
tail_shape <- 0.2

dNormGpd(3.0, mean, sd, threshold, tail_scale, tail_shape, log = FALSE)
pNormGpd(3.0, mean, sd, threshold, tail_scale, tail_shape,
        lower.tail = TRUE, log.p = FALSE)
qNormGpd(0.50, mean, sd, threshold, tail_scale, tail_shape)
qNormGpd(0.95, mean, sd, threshold, tail_scale, tail_shape)
replicate(10, rNormGpd(1, mean, sd, threshold, tail_scale, tail_shape))



