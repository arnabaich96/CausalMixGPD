### Name: laplace_lowercase
### Title: Lowercase vectorized Laplace distribution functions
### Aliases: laplace_lowercase dlaplacemix plaplacemix qlaplacemix
###   rlaplacemix dlaplacemixgpd plaplacemixgpd qlaplacemixgpd
###   rlaplacemixgpd dlaplacegpd plaplacegpd qlaplacegpd rlaplacegpd

### ** Examples

w <- c(0.6, 0.3, 0.1)
loc <- c(0, 1, -2)
scl <- c(1, 0.9, 1.1)

# Laplace mixture
dlaplacemix(c(-1, 0, 1), w = w, location = loc, scale = scl)
rlaplacemix(5, w = w, location = loc, scale = scl)




