### Name: build_causal_bundle
### Title: Build a causal bundle (design + two outcome arms)
### Aliases: build_causal_bundle

### ** Examples

## Not run: 
##D set.seed(1)
##D N <- 100
##D X <- cbind(x1 = rnorm(N), x2 = runif(N))
##D A <- rbinom(N, 1, plogis(0.3 + 0.5 * X[, 1]))
##D y <- rexp(N) + 0.1
##D 
##D cb <- build_causal_bundle(
##D   y = y,
##D   X = X,
##D   A = A,
##D   backend = "sb",
##D   kernel = "gamma",
##D   GPD = TRUE,
##D   components = 10,
##D   PS = "probit"
##D )
## End(Not run)



