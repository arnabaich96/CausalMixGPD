### Name: gamma_lowercase
### Title: Lowercase vectorized gamma distribution functions
### Aliases: gamma_lowercase dgammamix pgammamix qgammamix rgammamix
###   dgammamixgpd pgammamixgpd qgammamixgpd rgammamixgpd dgammagpd
###   pgammagpd qgammagpd rgammagpd

### ** Examples

w <- c(0.55, 0.3, 0.15)
shp <- c(2, 4, 6)
scl <- c(1, 2.5, 5)

# Gamma mixture
dgammamix(c(1, 2, 3), w = w, shape = shp, scale = scl)
rgammamix(5, w = w, shape = shp, scale = scl)

# Gamma mixture + GPD
dgammamixgpd(c(2, 3, 4), w = w, shape = shp, scale = scl,
             threshold = 3, tail_scale = 0.9, tail_shape = 0.2)

# Gamma + GPD (single component)
dgammagpd(c(2, 3, 4), shape = 4, scale = 2.5, threshold = 3,
          tail_scale = 0.9, tail_shape = 0.2)




