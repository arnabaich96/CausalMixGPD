devtools::load_all()

set.seed(2)
n <- 35
y <- abs(stats::rnorm(n)) + 0.1
X <- data.frame(x1 = stats::rnorm(n), x2 = stats::runif(n))

fast <- TRUE
mcmc_args <- if (isTRUE(fast)) {
  list(niter = 80, nburnin = 20, thin = 1, nchains = 1, seed = 1)
} else {
  list(niter = 400, nburnin = 100, thin = 2, nchains = 2, seed = c(1, 2))
}

surv_combos <- list(
  list(label = "sb-normal-gpd", backend = "sb", kernel = "normal", GPD = TRUE, components = 6, X = NULL),
  list(label = "crp-gamma-noGPD", backend = "crp", kernel = "gamma", GPD = FALSE, components = 8, X = X)
)

for (cfg in surv_combos) {
  cat("\n--- running survival combo:", cfg$label, "---\n")
  bundle <- build_nimble_bundle(
    y = y,
    X = cfg$X,
    backend = cfg$backend,
    kernel = cfg$kernel,
    GPD = cfg$GPD,
    components = cfg$components,
    mcmc = mcmc_args
  )

  fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
  surv <- survival(fit, y = sort(y), interval = "credible")

  print(surv)
  summary(surv)

  tbl <- survival_table(surv)
  print(str(tbl))

  q <- survival_quantile(surv, p = c(0.25, 0.5, 0.75))
  print(q)

  risk <- survival_risk(surv, threshold = median(y))
  print(risk)

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    g <- plot(surv, ci = TRUE)
    print(g)
  }
}

cat("\n--- causal survival checks ---\n")
set.seed(3)
N <- 45
X_c <- cbind(x1 = stats::rnorm(N), x2 = stats::runif(N))
T <- stats::rbinom(N, 1, stats::plogis(0.2 + 0.5 * X_c[, 1]))
y_c <- stats::rexp(N) + 0.1

cb <- build_causal_bundle(
  y = y_c,
  X = X_c,
  T = T,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc_outcome = mcmc_args,
  mcmc_ps = mcmc_args
)

cf <- run_mcmc_causal(cb, show_progress = FALSE)
surv_causal <- survival(cf, y = sort(y_c), newdata = X_c, interval = "credible")

print(surv_causal)
summary(surv_causal, contrast = TRUE)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  plot(surv_causal, overlay = TRUE, ci = TRUE)
}
