### Name: InvGauss_mix
### Title: Inverse Gaussian mixture distribution
### Aliases: InvGauss_mix dInvGaussMix pInvGaussMix rInvGaussMix
###   qInvGaussMix

### ** Examples

w <- c(0.55, 0.30, 0.15)
mean <- c(1.0, 2.5, 5.0)
shape <- c(2, 4, 8)

dInvGaussMix(2.0, w = w, mean = mean, shape = shape, log = 0)
pInvGaussMix(2.0, w = w, mean = mean, shape = shape,
            lower.tail = 1, log.p = 0)
qInvGaussMix(0.50, w = w, mean = mean, shape = shape)
qInvGaussMix(0.95, w = w, mean = mean, shape = shape)
replicate(10, rInvGaussMix(1, w = w, mean = mean, shape = shape))



