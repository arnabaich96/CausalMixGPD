### Name: plot.causalmixgpd_qte
### Title: Plot QTE-style effect summaries
### Aliases: plot.causalmixgpd_qte

### ** Examples

## Not run: 
##D qte_result <- cqte(fit, probs = c(0.1, 0.5, 0.9), newdata = X_new)
##D plot(qte_result)  # CQTE default: effect plot (faceted by id when needed)
##D plot(qte_result, type = "effect")  # single QTE plot
##D plot(qte_result, type = "arms")    # single arms plot
## End(Not run)



