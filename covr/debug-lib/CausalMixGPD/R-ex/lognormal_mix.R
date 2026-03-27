### Name: lognormal_mix
### Title: Lognormal mixture distribution
### Aliases: lognormal_mix dLognormalMix pLognormalMix rLognormalMix
###   qLognormalMix

### ** Examples

w <- c(0.60, 0.25, 0.15)
meanlog <- c(-0.2, 0.6, 1.2)
sdlog <- c(0.4, 0.3, 0.5)

dLognormalMix(2.0, w = w, meanlog = meanlog, sdlog = sdlog, log = FALSE)
pLognormalMix(2.0, w = w, meanlog = meanlog, sdlog = sdlog,
             lower.tail = TRUE, log.p = FALSE)
qLognormalMix(0.50, w = w, meanlog = meanlog, sdlog = sdlog)
qLognormalMix(0.95, w = w, meanlog = meanlog, sdlog = sdlog)
replicate(10, rLognormalMix(1, w = w, meanlog = meanlog, sdlog = sdlog))



