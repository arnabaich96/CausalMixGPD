### Name: normal_mix
### Title: Normal mixture distribution
### Aliases: normal_mix dNormMix pNormMix rNormMix qNormMix

### ** Examples

w <- c(0.60, 0.25, 0.15)
mean <- c(-1, 0.5, 2.0)
sd <- c(1.0, 0.7, 1.3)

dNormMix(0.5, w = w, mean = mean, sd = sd, log = FALSE)
pNormMix(0.5, w = w, mean = mean, sd = sd,
        lower.tail = TRUE, log.p = FALSE)
qNormMix(0.50, w = w, mean = mean, sd = sd)
qNormMix(0.95, w = w, mean = mean, sd = sd)
replicate(10, rNormMix(1, w = w, mean = mean, sd = sd))



