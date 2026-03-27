### Name: cauchy
### Title: Cauchy distribution
### Aliases: cauchy dCauchy pCauchy rCauchy qCauchy

### ** Examples

location <- 0
scale <- 1.5

dCauchy(0.5, location, scale, log = 0)
pCauchy(0.5, location, scale, lower.tail = 1, log.p = 0)
qCauchy(0.50, location, scale)
qCauchy(0.95, location, scale)
replicate(10, rCauchy(1, location, scale))




