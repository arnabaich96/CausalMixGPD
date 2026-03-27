### Name: InvGauss
### Title: Inverse Gaussian (Wald) distribution
### Aliases: InvGauss dInvGauss pInvGauss rInvGauss qInvGauss

### ** Examples

mean <- 2
shape <- 5

dInvGauss(2.0, mean, shape, log = 0)
pInvGauss(2.0, mean, shape, lower.tail = 1, log.p = 0)
qInvGauss(0.50, mean, shape)
qInvGauss(0.95, mean, shape)
replicate(10, rInvGauss(1, mean, shape))




