### Name: laplace_mix
### Title: Laplace (double exponential) mixture distribution
### Aliases: laplace_mix dLaplaceMix pLaplaceMix rLaplaceMix qLaplaceMix

### ** Examples

w <- c(0.50, 0.30, 0.20)
location <- c(-1, 0.5, 2.0)
scale <- c(1.0, 0.7, 1.4)

dLaplaceMix(0.8, w = w, location = location, scale = scale, log = FALSE)
pLaplaceMix(0.8, w = w, location = location, scale = scale,
           lower.tail = TRUE, log.p = FALSE)
qLaplaceMix(0.50, w = w, location = location, scale = scale)
qLaplaceMix(0.95, w = w, location = location, scale = scale)
replicate(10, rLaplaceMix(1, w = w, location = location, scale = scale))



