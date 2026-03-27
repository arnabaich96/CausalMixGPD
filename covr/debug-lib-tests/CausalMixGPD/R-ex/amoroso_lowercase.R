### Name: amoroso_lowercase
### Title: Lowercase vectorized Amoroso distribution functions
### Aliases: amoroso_lowercase damorosomix pamorosomix qamorosomix
###   ramorosomix damorosomixgpd pamorosomixgpd qamorosomixgpd
###   ramorosomixgpd damorosogpd pamorosogpd qamorosogpd ramorosogpd

### ** Examples

w <- c(0.6, 0.3, 0.1)
locs <- c(0.5, 0.5, 0.5)
scls <- c(1, 1.3, 1.6)
s1 <- c(2.5, 3, 4)
s2 <- c(1.2, 1.2, 1.2)

# Amoroso mixture
damorosomix(c(1, 2, 3), w = w, loc = locs, scale = scls, shape1 = s1, shape2 = s2)
ramorosomix(5, w = w, loc = locs, scale = scls, shape1 = s1, shape2 = s2)




