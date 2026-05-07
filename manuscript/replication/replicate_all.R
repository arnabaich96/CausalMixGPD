# =============================================================================
# replicate_all.R
# Consolidated manuscript replication script for CausalMixGPD.
#
# This file combines the five manuscript replication helper scripts from
# build_helpers/ into one directly runnable script. The original helper scripts
# are retained unchanged.
#
# Usage:
#   Rscript manuscript/replication/replicate_all.R
#   Rscript manuscript/replication/replicate_all.R overview_onearm
#   Rscript manuscript/replication/replicate_all.R overview_onearm,data_analysis_causal
# =============================================================================

available_sections <- c(
  "overview_onearm",
  "overview_clustering",
  "overview_causal",
  "data_analysis_cluster",
  "data_analysis_causal"
)

args <- commandArgs(trailingOnly = TRUE)
sections_to_run <- if (length(args)) {
  unique(trimws(unlist(strsplit(paste(args, collapse = ","), ",", fixed = TRUE))))
} else {
  available_sections
}
sections_to_run <- sections_to_run[nzchar(sections_to_run)]

unknown_sections <- setdiff(sections_to_run, available_sections)
if (length(unknown_sections)) {
  stop(
    "Unknown replication section(s): ",
    paste(unknown_sections, collapse = ", "),
    "\nAvailable sections: ",
    paste(available_sections, collapse = ", "),
    call. = FALSE
  )
}

run_section <- function(name, fun) {
  message("\n", strrep("=", 78))
  message("Running replication section: ", name)
  message(strrep("=", 78))
  fun()
  invisible(TRUE)
}

run_overview_onearm <- function() {
  # Package overview - one-arm modeling API (dpmgpd, dpmix, predict).
  # Uses bundled synthetic data shipped with CausalMixGPD.
  library(CausalMixGPD)
  cmgpd_seed <- 2026

  # MCMC settings used for all fits below.
  mcmc_fixed <- list(
    niter   = 2000,
    nburnin = 500,
    thin    = 1,
    nchains = 1,
    seed    = cmgpd_seed
  )

  # Data
  # nc_posX100_p3_k2: n=100, p=3 covariates, K=2 true components,
  # outcomes on the positive real line.
  data("nc_posX100_p3_k2", package = "CausalMixGPD")
  dat <- data.frame(y = nc_posX100_p3_k2$y, nc_posX100_p3_k2$X)

  # dpmgpd(): DP mixture with spliced GPD tail.
  fit <- dpmgpd(
    formula = y ~ x1 + x2 + x3,
    data    = dat,
    backend = "crp",
    kernel  = "gamma",
    mcmc    = mcmc_fixed
  )

  # dpmix(): bulk-only DP mixture, no GPD tail splicing.
  fit <- dpmix(
    formula = y ~ x1 + x2 + x3,
    data    = dat,
    backend = "crp",
    kernel  = "gamma",
    mcmc    = mcmc_fixed
  )

  # Diagnostics
  print(summary(fit))
  print(params(fit))
  print(plot(fit, family = "traceplot", params = "alpha"))
  # print(plot(fit, family = "auto"))

  # In-sample posterior predictive summaries
  print(predict(fit, type = "density", interval = "credible", level = 0.95))
  print(predict(fit, type = "quantile", index = c(0.25, 0.5, 0.75)))
  print(predict(fit, type = "mean", interval = "hpd", level = 0.9))

  # Out-of-sample prediction
  # x_new: covariate grid at each predictor's quartiles (3 rows).
  # y_grid: response grid for evaluating P(Y > y | x).
  x_new <- as.data.frame(
    lapply(
      dat[, c("x1", "x2", "x3")],
      quantile,
      probs = c(0.25, 0.50, 0.75),
      na.rm = TRUE
    )
  )
  y_grid <- seq(0, 10, length.out = 200)

  print(predict(
    fit,
    newdata = x_new,
    y       = y_grid,
    type    = "survival",
    level   = 0.95
  ))
  print(predict(
    fit,
    newdata  = x_new,
    type     = "quantile",
    index    = c(0.5, 0.99),
    interval = "credible"
  ))
}

run_overview_clustering <- function() {
  # Package overview - clustering API (dpmix.cluster, predict).
  # Uses bundled synthetic data shipped with CausalMixGPD.
  library(CausalMixGPD)
  cmgpd_seed <- 2026

  # MCMC settings used for the clustering fit below.
  mcmc_fixed <- list(
    niter   = 2000,
    nburnin = 500,
    thin    = 1,
    nchains = 1,
    seed    = cmgpd_seed
  )

  # Clustering data
  # nc_realX100_p3_k2: n=100, p=3, K=2, real-valued outcomes.
  # Rows 1:90 are used for fitting; rows 91:100 are held out.
  data("nc_realX100_p3_k2", package = "CausalMixGPD")
  dat_cl <- data.frame(
    y = nc_realX100_p3_k2$y,
    nc_realX100_p3_k2$X
  )

  # dpmix.cluster(): DP mixture clustering with covariate-dependent weights.
  # type = "both" means covariates enter mixture weights and component parameters.
  fit_cluster <- dpmix.cluster(
    y ~ x1 + x2 + x3,
    data       = dat_cl[1:90, ],
    kernel     = "laplace",
    type       = "both",
    components = 10,
    mcmc       = mcmc_fixed
  )

  # Posterior similarity matrix: PSM[i,j] = P(z_i = z_j | data)
  print(predict(fit_cluster, type = "psm"))

  # Classify held-out observations into training clusters
  print(predict(
    fit_cluster,
    newdata = dat_cl[91:100, ],
    type    = "label"
  ))
}

run_overview_causal <- function() {
  # Package overview - causal inference API (dpmgpd.causal, qtt, predict).
  # Uses bundled synthetic causal dataset shipped with CausalMixGPD.
  library(CausalMixGPD)
  cmgpd_seed <- 2026

  # MCMC settings used for both the outcome models and the PS model.
  mcmc_fixed <- list(
    niter   = 2000,
    nburnin = 500,
    thin    = 1,
    nchains = 1,
    seed    = cmgpd_seed
  )

  print(data.frame(mcmc_fixed, row.names = NULL))

  # Data
  # causal_pos500_p3_k2: n=500, p=3 covariates, K=2 true components,
  # positive-support outcomes, binary treatment indicator A.
  data("causal_pos500_p3_k2", package = "CausalMixGPD")
  causal_dat <- causal_pos500_p3_k2
  causal_df <- data.frame(y = causal_dat$y, A = causal_dat$A, causal_dat$X)

  # dpmgpd.causal(): arm-specific DP mixture + GPD tail models.
  # formula / data / treat - response, covariates, treatment column
  # backend                - "crp", "sb", or "spliced"
  # kernel                 - bulk component family: "laplace", "gamma", etc.
  # PS                     - propensity-score model: "logit", "probit", FALSE
  # ps_scale               - PS augmentation scale ("logit" = log-odds)
  # ps_summary             - PS posterior summary: "mean" or "median"
  # mcmc_outcome           - MCMC settings for the two outcome-arm models
  # mcmc_ps                - MCMC settings for the PS model
  causal_fit <- dpmgpd.causal(
    formula      = y ~ x1 + x2 + x3,
    data         = causal_df,
    treat        = "A",
    backend      = "crp",
    kernel       = "laplace",
    components   = 5,
    PS           = "logit",
    ps_scale     = "logit",
    ps_summary   = "mean",
    mcmc_outcome = mcmc_fixed,
    mcmc_ps      = mcmc_fixed
  )

  # qtt(): Quantile Treatment effect on the Treated.
  # QTT(tau) = Q_1(tau | A=1) - Q_0(tau | A=1),
  # marginalised over the treated subpopulation covariate distribution.
  qtt_fit <- qtt(
    causal_fit,
    probs    = c(0.50, 0.90, 0.95),
    interval = "credible"
  )
  print(summary(qtt_fit))
  print(plot(qtt_fit, type = "effect"))

  # Out-of-sample conditional survival prediction.
  # causal_xgrid: all predictor-quartile combinations (3^3 = 27 rows).
  # predict() with type = "survival": P(Y(a) > y | X = x) for each row.
  # Align columns to training design matrix; optional id avoids length mismatch.
  causal_xgrid <- expand.grid(lapply(
    causal_df[c("x1", "x2", "x3")],
    quantile,
    probs = c(0.25, 0.50, 0.75),
    na.rm = TRUE
  ))
  causal_xgrid <- as.matrix(causal_xgrid)
  causal_xgrid <- causal_xgrid[, c("x1", "x2", "x3"), drop = FALSE]

  print(predict(
    causal_fit,
    newdata = causal_xgrid,
    type    = "survival",
    y       = rep(4, length.out = nrow(causal_xgrid))
  ))
}

run_data_analysis_cluster <- function() {
  # Data analysis I - clustering.
  # Data: Boston housing (MASS), outcome = medv, predictors = lstat, rm, nox.

  # MCMC settings - match the Rnw setup chunk exactly for reproducibility.
  # A single shared seed controls both the NIMBLE RNG and the train/test split.
  cmgpd_seed <- 2026
  mcmc_fixed <- list(
    niter   = 2000,
    nburnin = 500,
    thin    = 1,
    nchains = 1,
    seed    = cmgpd_seed
  )

  library(CausalMixGPD)
  library(MASS)
  library(ggplot2)
  library(patchwork)

  # Colour palette and ggplot2 theme - defined in the Rnw setup chunk.
  # Copied here so standalone figures match the manuscript.
  cmgpd_pal <- c(
    navy   = "#183A5A",
    teal   = "#2B6F77",
    copper = "#B16A3A",
    gold   = "#C59B4D",
    sage   = "#5F7F68",
    rose   = "#A64B5A",
    slate  = "#5C6773",
    mist   = "#D7E3ED"
  )

  cmgpd_theme <- function(base_size = 14) {
    ggplot2::theme_minimal(base_size = base_size, base_family = "serif") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold",
          colour = cmgpd_pal[["navy"]]
        ),
        plot.subtitle = ggplot2::element_text(colour = cmgpd_pal[["slate"]]),
        axis.title = ggplot2::element_text(colour = cmgpd_pal[["navy"]]),
        axis.text = ggplot2::element_text(colour = cmgpd_pal[["slate"]]),
        legend.title = ggplot2::element_text(
          face = "bold",
          colour = cmgpd_pal[["navy"]]
        ),
        legend.text = ggplot2::element_text(colour = cmgpd_pal[["slate"]]),
        legend.position    = "top",
        panel.grid.minor   = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank()
      )
  }

  # Load Boston, keep the four variables used in the model, and split 80/20.
  set.seed(cmgpd_seed)
  data("Boston", package = "MASS")

  dat <- Boston

  n <- nrow(dat)
  idx_train <- sample(seq_len(n), size = floor(0.80 * n), replace = FALSE)
  train_dat <- dat[idx_train, ]
  test_dat <- dat[-idx_train, ]

  # Boxplot and histogram of medv across all 506 tracts.
  app_cluster_plot_df <- data.frame(
    medv   = dat$medv,
    sample = "All tracts"
  )

  app_cluster_box_plot <- ggplot(
    app_cluster_plot_df,
    aes(x = sample, y = medv)
  ) +
    geom_boxplot(
      width          = 0.45,
      fill           = cmgpd_pal[["mist"]],
      colour         = cmgpd_pal[["navy"]],
      outlier.colour = cmgpd_pal[["rose"]],
      outlier.alpha  = 0.85
    ) +
    labs(title = "House prices: boxplot", x = NULL, y = "medv") +
    cmgpd_theme(base_size = 12) +
    theme(legend.position = "none")

  app_cluster_hist_plot <- ggplot(
    app_cluster_plot_df,
    aes(x = medv, y = after_stat(density))
  ) +
    geom_histogram(
      bins      = 30,
      fill      = cmgpd_pal[["mist"]],
      colour    = "white",
      linewidth = 0.25
    ) +
    labs(title = "House prices: histogram", x = "medv", y = "Density") +
    cmgpd_theme(base_size = 12)

  print(app_cluster_box_plot + app_cluster_hist_plot + plot_layout(ncol = 2))

  # DP mixture model with Normal bulk kernel; covariates enter mixture weights only.
  fit_clust <- dpmix.cluster(
    formula    = medv ~ .,
    data       = train_dat,
    kernel     = "normal",
    type       = "weights",
    components = 10,
    mcmc       = mcmc_fixed
  )

  # Posterior similarity matrix for the training sample.
  # PSM[i,j] = posterior probability that observations i and j share a cluster.
  z_train_psm <- predict(fit_clust, type = "psm")

  # Heatmap of the PSM with observations reordered to reveal block structure.
  print(plot(z_train_psm, type = "summary"))

  # Hard cluster labels via the medoid of the posterior partition distribution.
  z_train_lab <- predict(fit_clust, type = "label")

  # Training observations coloured by cluster label.
  print(plot(z_train_lab, type = "summary"))

  # Assign test observations to the nearest training cluster.
  z_test <- predict(
    fit_clust,
    newdata = test_dat,
    type    = "label"
  )

  # Per-cluster covariate profile table for the test observations.
  print(cluster_profiles(z_test))

  # Bar chart of test-observation counts per predicted cluster.
  print(plot(z_test, type = "sizes"))
}

run_data_analysis_causal <- function() {
  # Data analysis II - causal inference.
  # Data: Lalonde (1978) job-training experiment from the MatchIt package.

  # MCMC settings
  cmgpd_seed <- 2026
  mcmc_fixed <- list(
    niter   = 2000,
    nburnin = 500,
    thin    = 1,
    nchains = 1,
    seed    = cmgpd_seed
  )

  library(CausalMixGPD)
  library(ggplot2)
  library(patchwork)
  library(future)
  library(future.apply)

  cmgpd_pal <- c(
    navy   = "#183A5A",
    teal   = "#2B6F77",
    copper = "#B16A3A",
    gold   = "#C59B4D",
    sage   = "#5F7F68",
    rose   = "#A64B5A",
    slate  = "#5C6773",
    mist   = "#D7E3ED"
  )

  cmgpd_theme <- function(base_size = 14) {
    ggplot2::theme_minimal(base_size = base_size, base_family = "serif") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold",
          colour = cmgpd_pal[["navy"]]
        ),
        plot.subtitle = ggplot2::element_text(colour = cmgpd_pal[["slate"]]),
        axis.title = ggplot2::element_text(colour = cmgpd_pal[["navy"]]),
        axis.text = ggplot2::element_text(colour = cmgpd_pal[["slate"]]),
        legend.title = ggplot2::element_text(
          face = "bold",
          colour = cmgpd_pal[["navy"]]
        ),
        legend.text = ggplot2::element_text(colour = cmgpd_pal[["slate"]]),
        legend.position    = "top",
        panel.grid.minor   = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank()
      )
  }

  # Load Lalonde data; relevel factors; z-score continuous covariates.
  # Outcome: re78 shifted by +0.5 and divided by 1000.
  data("lalonde", package = "MatchIt")

  # Relevel categorical covariates - white / no / no are the reference levels.
  app_lalonde_covars <- within(lalonde, {
    race <- factor(race, levels = c("white", "black", "hispan"))
    married <- factor(married, levels = c(0, 1), labels = c("no", "yes"))
    nodegree <- factor(nodegree, levels = c(0, 1), labels = c("no", "yes"))
  })

  # Store centering and scaling constants for later rescaling of new profiles.
  app_lalonde_scale_vars <- c("age", "educ", "re74", "re75")
  app_lalonde_scale_center <- vapply(
    app_lalonde_scale_vars,
    function(v) mean(app_lalonde_covars[[v]], na.rm = TRUE),
    numeric(1)
  )
  app_lalonde_scale_scale <- vapply(
    app_lalonde_scale_vars,
    function(v) stats::sd(app_lalonde_covars[[v]], na.rm = TRUE),
    numeric(1)
  )

  # Append z-scored columns (suffix _z).
  for (v in app_lalonde_scale_vars) {
    app_lalonde_covars[[paste0(v, "_z")]] <-
      (app_lalonde_covars[[v]] - app_lalonde_scale_center[[v]]) /
      app_lalonde_scale_scale[[v]]
  }

  app_lalonde_x_formula <- ~ age_z + educ_z + race +
    married + nodegree + re74_z + re75_z

  app_lalonde_y <- (app_lalonde_covars$re78 + 0.5) / 1000
  app_lalonde_A <- as.integer(app_lalonde_covars$treat)
  app_lalonde_taus <- c(0.25, 0.50, 0.75, 0.90, 0.95)

  # Design matrix (intercept column removed - the model adds its own).
  app_lalonde_X <- stats::model.matrix(
    app_lalonde_x_formula,
    data = app_lalonde_covars
  )[, -1, drop = FALSE]

  # Arm-specific boxplot and overlaid histogram of 1978 earnings (re78).
  app_lalonde_plot_df <- data.frame(
    re78 = app_lalonde_covars$re78,
    treat_label = factor(
      app_lalonde_covars$treat,
      levels = c(0, 1),
      labels = c("Control", "Treated")
    )
  )

  app_lalonde_box_plot <- ggplot(
    app_lalonde_plot_df,
    aes(x = treat_label, y = re78, fill = treat_label)
  ) +
    geom_boxplot(
      width          = 0.55,
      colour         = cmgpd_pal[["navy"]],
      outlier.colour = cmgpd_pal[["rose"]],
      outlier.alpha  = 0.85
    ) +
    scale_fill_manual(values = c(cmgpd_pal[["mist"]], cmgpd_pal[["copper"]])) +
    labs(
      title = "Boxplot of 1978 earnings",
      x     = NULL,
      y     = "re78 (USD)",
      fill  = NULL
    ) +
    cmgpd_theme(base_size = 12) +
    theme(legend.position = "bottom")

  app_lalonde_hist_plot <- ggplot(
    app_lalonde_plot_df,
    aes(x = re78, y = after_stat(density), fill = treat_label, colour = treat_label)
  ) +
    geom_histogram(
      bins      = 35,
      position  = "identity",
      linewidth = 0.2,
      alpha     = 0.45
    ) +
    scale_colour_manual(values = c(cmgpd_pal[["teal"]], cmgpd_pal[["rose"]])) +
    scale_fill_manual(values = c(cmgpd_pal[["mist"]], cmgpd_pal[["copper"]])) +
    labs(
      title = "Histogram of 1978 earnings",
      x     = "re78 (USD)",
      y     = "Density"
    ) +
    cmgpd_theme(base_size = 12) +
    theme(legend.position = "none")

  print(
    app_lalonde_box_plot + app_lalonde_hist_plot +
      plot_layout(ncol = 2, guides = "collect") &
      theme(legend.position = "bottom")
  )

  # Fit arm-specific DP mixture + GPD tail models via dpmgpd.causal().
  # backend = "crp": Chinese Restaurant Process weight representation.
  # kernel  = "gamma": positive-support bulk component (required for re78 > 0).
  # PS logistic propensity-score model is estimated internally; its posterior
  # mean is appended as an extra covariate in each arm's outcome model.
  fit <- dpmgpd.causal(
    y          = app_lalonde_y,
    X          = app_lalonde_X,
    treat      = app_lalonde_A,
    backend    = "crp",
    kernel     = "gamma",
    components = 10,
    mcmc       = mcmc_fixed
  )

  # Wall-clock timing for each stage: ps / control arm / treated arm.
  print(fit)

  # Average Treatment Effect: ATE = E[Y(1)] - E[Y(0)], 95% HPD interval.
  ate_fit <- ate(fit, interval = "hpd", level = 0.95)
  print(summary(ate_fit))

  # Marginal Quantile Treatment Effects at tau = 0.25, 0.50, 0.75.
  # QTE(tau) = Q_1^m(tau) - Q_0^m(tau), marginalised over the covariate distribution.
  qte_fit <- qte(
    fit,
    probs    = c(0.25, 0.50, 0.75),
    interval = "credible"
  )

  # Left: QTE curve with pointwise 95% credible band.
  # Right: arm-specific marginal quantile curves Q_1(tau) and Q_0(tau).
  qte_effect_plot <- plot(qte_fit, type = "effect")
  qte_arms_plot <- plot(qte_fit, type = "arms")
  print(qte_effect_plot + qte_arms_plot + patchwork::plot_layout(ncol = 2))

  # Four synthetic participant profiles spanning quantiles of the continuous
  # covariates; used as new data for profile-specific causal contrasts.
  numeric_quantile <- function(x, prob = 0.50) {
    as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
  }

  profile_raw <- data.frame(
    profile = c("Profile 1", "Profile 2", "Profile 3", "Profile 4"),
    age = c(
      numeric_quantile(app_lalonde_covars$age, 0.25),
      numeric_quantile(app_lalonde_covars$age, 0.50),
      numeric_quantile(app_lalonde_covars$age, 0.75),
      numeric_quantile(app_lalonde_covars$age, 0.90)
    ),
    educ = c(
      numeric_quantile(app_lalonde_covars$educ, 0.25),
      numeric_quantile(app_lalonde_covars$educ, 0.50),
      numeric_quantile(app_lalonde_covars$educ, 0.75),
      numeric_quantile(app_lalonde_covars$educ, 0.90)
    ),
    race = factor(
      c("black", "hispan", "white", "black"),
      levels = levels(app_lalonde_covars$race)
    ),
    married = factor(
      c("no", "no", "yes", "yes"),
      levels = levels(app_lalonde_covars$married)
    ),
    nodegree = factor(
      c("yes", "no", "no", "yes"),
      levels = levels(app_lalonde_covars$nodegree)
    ),
    re74 = c(
      numeric_quantile(app_lalonde_covars$re74, 0.20),
      numeric_quantile(app_lalonde_covars$re74, 0.40),
      numeric_quantile(app_lalonde_covars$re74, 0.60),
      numeric_quantile(app_lalonde_covars$re74, 0.80)
    ),
    re75 = c(
      numeric_quantile(app_lalonde_covars$re75, 0.20),
      numeric_quantile(app_lalonde_covars$re75, 0.40),
      numeric_quantile(app_lalonde_covars$re75, 0.60),
      numeric_quantile(app_lalonde_covars$re75, 0.80)
    ),
    row.names   = c("p1", "p2", "p3", "p4"),
    check.names = FALSE
  )

  # Z-score continuous covariates using the training-data scaling constants.
  profile_model <- within(profile_raw, {
    age_z <- (age - app_lalonde_scale_center[["age"]]) /
      app_lalonde_scale_scale[["age"]]
    educ_z <- (educ - app_lalonde_scale_center[["educ"]]) /
      app_lalonde_scale_scale[["educ"]]
    re74_z <- (re74 - app_lalonde_scale_center[["re74"]]) /
      app_lalonde_scale_scale[["re74"]]
    re75_z <- (re75 - app_lalonde_scale_center[["re75"]]) /
      app_lalonde_scale_scale[["re75"]]
  })

  # Build design matrix; column order must match app_lalonde_X.
  xnew <- stats::model.matrix(app_lalonde_x_formula, data = profile_model)[, -1, drop = FALSE]
  xnew <- xnew[, colnames(app_lalonde_X), drop = FALSE]
  rownames(xnew) <- profile_raw$profile

  profile_display <- profile_raw[
    , c("profile", "age", "educ", "race", "married", "nodegree", "re74", "re75"),
    drop = FALSE
  ]
  rownames(profile_display) <- NULL

  # Print the profile table directly.
  profile_t <- as.data.frame(t(profile_display[, -which(names(profile_display) == "profile")]))
  colnames(profile_t) <- profile_display$profile
  profile_t[] <- lapply(profile_t, function(x) {
    num <- suppressWarnings(as.numeric(as.character(x)))
    ifelse(!is.na(num), formatC(round(num, 4), format = "f", digits = 4), as.character(x))
  })
  print(profile_t)

  # Conditional Average Treatment Effect at each of the four profiles.
  # CATE(x) = E[Y(1) | X=x] - E[Y(0) | X=x), 95% HPD interval.
  cate_fit <- cate(
    fit,
    newdata       = xnew,
    interval      = "hpd",
    show_progress = FALSE
  )
  print(summary(cate_fit))

  # Conditional Quantile Treatment Effects at five tau levels and four profiles.
  # CQTE(tau, x) = Q_1(tau | x) - Q_0(tau | x).
  cqte_fit <- cqte(
    fit,
    probs    = app_lalonde_taus,
    newdata  = xnew,
    interval = "credible"
  )
  print(summary(cqte_fit)$effect_table)

  # Profile-specific CATE point estimates with 95% HPD bars.
  cate_plot_obj <- plot(cate_fit) +
    ggplot2::labs(title = "CATE by profile", x = NULL) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(size = 10.5, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 9.5),
      axis.text    = ggplot2::element_text(size = 8.5),
      plot.margin  = ggplot2::margin(5.5, 6, 5.5, 5.5)
    )

  print(cate_plot_obj)
}

section_functions <- list(
  overview_onearm       = run_overview_onearm,
  overview_clustering  = run_overview_clustering,
  overview_causal      = run_overview_causal,
  data_analysis_cluster = run_data_analysis_cluster,
  data_analysis_causal  = run_data_analysis_causal
)

for (section in sections_to_run) {
  run_section(section, section_functions[[section]])
}

message("\nConsolidated replication script completed.")
