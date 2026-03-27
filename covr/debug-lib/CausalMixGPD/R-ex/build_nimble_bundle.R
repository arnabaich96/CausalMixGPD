### Name: build_nimble_bundle
### Title: Build the explicit one-arm NIMBLE bundle
### Aliases: build_nimble_bundle

### ** Examples

## Not run: 
##D y <- abs(rnorm(60)) + 0.1
##D bundle <- build_nimble_bundle(
##D   y = y,
##D   backend = "sb",
##D   kernel = "normal",
##D   GPD = FALSE,
##D   components = 4,
##D   mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
##D )
##D bundle
## End(Not run)



