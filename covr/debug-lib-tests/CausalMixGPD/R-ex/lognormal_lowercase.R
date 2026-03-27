### Name: lognormal_lowercase
### Title: Lowercase vectorized lognormal distribution functions
### Aliases: lognormal_lowercase dlognormalmix plognormalmix qlognormalmix
###   rlognormalmix dlognormalmixgpd plognormalmixgpd qlognormalmixgpd
###   rlognormalmixgpd dlognormalgpd plognormalgpd qlognormalgpd
###   rlognormalgpd

### ** Examples

w <- c(0.6, 0.3, 0.1)
ml <- c(0, 0.3, 0.6)
sl <- c(0.4, 0.5, 0.6)

# Lognormal mixture
dlognormalmix(c(1, 2, 3), w = w, meanlog = ml, sdlog = sl)
rlognormalmix(5, w = w, meanlog = ml, sdlog = sl)

# Lognormal mixture + GPD
dlognormalmixgpd(c(2, 3, 4), w = w, meanlog = ml, sdlog = sl,
                 threshold = 2.5, tail_scale = 0.5, tail_shape = 0.2)




