### Name: plot.causalmixgpd_ate
### Title: Plot ATE-style effect summaries
### Aliases: plot.causalmixgpd_ate

### ** Examples

## Not run: 
##D ate_result <- cate(fit, newdata = X_new, interval = "credible")
##D plot(ate_result)  # CATE default: effect plot
##D plot(ate_result, type = "effect")  # single ATE plot
##D plot(ate_result, type = "arms")    # single arms plot
## End(Not run)



