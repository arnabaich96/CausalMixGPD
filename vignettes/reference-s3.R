## ----results='hide'-----------------------------------------------------------
causal_fit <- run_mcmc_causal(causal_bundle, show_progress = FALSE)

## -----------------------------------------------------------------------------
ate_result <- ate(causal_fit, interval = "hpd", nsim_mean = 50)
print(ate_result)

## -----------------------------------------------------------------------------
summary(ate_result)

## ----fig.width=7, fig.height=5------------------------------------------------
ate_plots <- plot(ate_result)
ate_plots$treatment_effect

## ----fig.width=7, fig.height=5------------------------------------------------
plot(ate_result, type = "effect")

## -----------------------------------------------------------------------------
qte_result <- qte(causal_fit, probs = c(0.1, 0.5, 0.9), interval = "hpd")
print(qte_result)

## -----------------------------------------------------------------------------
summary(qte_result)

## ----fig.width=7, fig.height=5------------------------------------------------
qte_plots <- plot(qte_result)
qte_plots$treatment_effect

## ----fig.width=7, fig.height=5------------------------------------------------
plot(qte_result, type = "effect")

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()

