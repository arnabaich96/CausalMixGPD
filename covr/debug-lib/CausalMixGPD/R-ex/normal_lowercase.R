### Name: normal_lowercase
### Title: Lowercase vectorized normal distribution functions
### Aliases: normal_lowercase dnormmix pnormmix qnormmix rnormmix
###   dnormmixgpd pnormmixgpd qnormmixgpd rnormmixgpd dnormgpd pnormgpd
###   qnormgpd rnormgpd

### ** Examples

w <- c(0.6, 0.25, 0.15)
mu <- c(-1, 0.5, 2)
sig <- c(1, 0.7, 1.3)

# Normal mixture
dnormmix(c(0, 1, 2), w = w, mean = mu, sd = sig)
rnormmix(5, w = w, mean = mu, sd = sig)

# Normal mixture + GPD
dnormmixgpd(c(1, 2, 3), w = w, mean = mu, sd = sig,
            threshold = 2, tail_scale = 1, tail_shape = 0.2)

# Normal + GPD (single component)
dnormgpd(c(1, 2, 3), mean = 0.5, sd = 1, threshold = 2,
         tail_scale = 1, tail_shape = 0.2)




