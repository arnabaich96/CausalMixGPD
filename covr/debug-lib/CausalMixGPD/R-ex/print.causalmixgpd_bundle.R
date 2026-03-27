### Name: print.causalmixgpd_bundle
### Title: Print a one-arm workflow bundle
### Aliases: print.causalmixgpd_bundle

### ** Examples

## Not run: 
##D y <- abs(stats::rnorm(50)) + 0.1
##D bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
##D                              GPD = FALSE, components = 6)
##D print(bundle)
##D print(bundle, code = TRUE, max_code_lines = 30)
## End(Not run)



