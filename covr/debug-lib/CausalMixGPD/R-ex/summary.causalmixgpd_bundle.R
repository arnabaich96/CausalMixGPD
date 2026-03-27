### Name: summary.causalmixgpd_bundle
### Title: Summarize a one-arm workflow bundle
### Aliases: summary.causalmixgpd_bundle

### ** Examples

## Not run: 
##D y <- abs(stats::rnorm(50)) + 0.1
##D bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
##D                              GPD = FALSE, components = 6)
##D summary(bundle)
## End(Not run)



