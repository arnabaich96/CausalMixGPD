# =============================================================================
# data_analysis_causal.R
# (Section: Data analysis II — causal inference)
#
# Data: Lalonde (1978) job-training experiment from the MatchIt package.
# =============================================================================

# MCMC settings
mcmc_fixed <- list(
  niter   = 2000,
  nburnin = 500,
  thin    = 1,
  nchains = 1,
  seed    = 2026
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
      plot.title    = ggplot2::element_text(face = "bold",  colour = cmgpd_pal[["navy"]]),
      plot.subtitle = ggplot2::element_text(colour = cmgpd_pal[["slate"]]),
      axis.title    = ggplot2::element_text(colour = cmgpd_pal[["navy"]]),
      axis.text     = ggplot2::element_text(colour = cmgpd_pal[["slate"]]),
      legend.title  = ggplot2::element_text(face = "bold", colour = cmgpd_pal[["navy"]]),
      legend.text   = ggplot2::element_text(colour = cmgpd_pal[["slate"]]),
      legend.position      = "top",
      panel.grid.minor     = ggplot2::element_blank(),
      panel.grid.major.x   = ggplot2::element_blank()
    )
}

# Load Lalonde data; relevel factors; z-score continuous covariates.
# Outcome: re78 shifted by +0.5 and divided by 1000 (strict positivity for Gamma kernel).
data("lalonde", package = "MatchIt")

# Relevel categorical covariates — white / no / no are the reference levels
app_lalonde_covars <- within(lalonde, {
  race     <- factor(race,     levels = c("white", "black", "hispan"))
  married  <- factor(married,  levels = c(0, 1), labels = c("no", "yes"))
  nodegree <- factor(nodegree, levels = c(0, 1), labels = c("no", "yes"))
})

# Store centering and scaling constants for later rescaling of new profiles
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

# Append z-scored columns (suffix _z)
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

# Design matrix (intercept column removed — the model adds its own)
app_lalonde_X <- stats::model.matrix(
  app_lalonde_x_formula,
  data = app_lalonde_covars
)[, -1, drop = FALSE]

# Arm-specific boxplot and overlaid histogram of 1978 earnings (re78).
app_lalonde_plot_df <- data.frame(
  re78        = app_lalonde_covars$re78,
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
  scale_fill_manual(values   = c(cmgpd_pal[["mist"]], cmgpd_pal[["copper"]])) +
  labs(
    title = "Histogram of 1978 earnings",
    x     = "re78 (USD)",
    y     = "Density"
  ) +
  cmgpd_theme(base_size = 12) +
  theme(legend.position = "none")

app_lalonde_box_plot + app_lalonde_hist_plot +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

# =============================================================================
# Fit arm-specific DP mixture + GPD tail models via dpmgpd.causal().
# backend = "crp": Chinese Restaurant Process weight representation.
# kernel  = "gamma": positive-support bulk component (required for re78 > 0).
# PS logistic propensity-score model is estimated internally; its posterior
# mean is appended as an extra covariate in each arm's outcome model.
# =============================================================================
fit <- dpmgpd.causal(
  y          = app_lalonde_y,
  X          = app_lalonde_X,
  treat      = app_lalonde_A,
  backend    = "crp",
  kernel     = "gamma",
  components = 10,
  mcmc       = mcmc_fixed
)

# =============================================================================
# Wall-clock timing for each stage: ps / control arm / treated arm.
# =============================================================================
data.frame(fit$timing)

# Average Treatment Effect: ATE = E[Y(1)] - E[Y(0)], 95% HPD interval.
ate_fit <- ate(fit, interval = "hpd", level = 0.95)
summary(ate_fit)

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
qte_arms_plot   <- plot(qte_fit, type = "arms")
qte_effect_plot + qte_arms_plot + patchwork::plot_layout(ncol = 2)

# Four synthetic participant profiles spanning quantiles of the continuous
# covariates; used as new data for profile-specific causal contrasts.
numeric_quantile <- function(x, prob = 0.50) {
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
}

profile_raw <- data.frame(
  profile  = c("Profile 1", "Profile 2", "Profile 3", "Profile 4"),
  age      = c(
    numeric_quantile(app_lalonde_covars$age, 0.25),
    numeric_quantile(app_lalonde_covars$age, 0.50),
    numeric_quantile(app_lalonde_covars$age, 0.75),
    numeric_quantile(app_lalonde_covars$age, 0.90)
  ),
  educ     = c(
    numeric_quantile(app_lalonde_covars$educ, 0.25),
    numeric_quantile(app_lalonde_covars$educ, 0.50),
    numeric_quantile(app_lalonde_covars$educ, 0.75),
    numeric_quantile(app_lalonde_covars$educ, 0.90)
  ),
  race     = factor(
    c("black", "hispan", "white", "black"),
    levels = levels(app_lalonde_covars$race)
  ),
  married  = factor(
    c("no", "no", "yes", "yes"),
    levels = levels(app_lalonde_covars$married)
  ),
  nodegree = factor(
    c("yes", "no", "no", "yes"),
    levels = levels(app_lalonde_covars$nodegree)
  ),
  re74     = c(
    numeric_quantile(app_lalonde_covars$re74, 0.20),
    numeric_quantile(app_lalonde_covars$re74, 0.40),
    numeric_quantile(app_lalonde_covars$re74, 0.60),
    numeric_quantile(app_lalonde_covars$re74, 0.80)
  ),
  re75     = c(
    numeric_quantile(app_lalonde_covars$re75, 0.20),
    numeric_quantile(app_lalonde_covars$re75, 0.40),
    numeric_quantile(app_lalonde_covars$re75, 0.60),
    numeric_quantile(app_lalonde_covars$re75, 0.80)
  ),
  row.names    = c("p1", "p2", "p3", "p4"),
  check.names  = FALSE
)

# Z-score continuous covariates using the training-data scaling constants
profile_model <- within(profile_raw, {
  age_z  <- (age  - app_lalonde_scale_center[["age"]])  / app_lalonde_scale_scale[["age"]]
  educ_z <- (educ - app_lalonde_scale_center[["educ"]]) / app_lalonde_scale_scale[["educ"]]
  re74_z <- (re74 - app_lalonde_scale_center[["re74"]]) / app_lalonde_scale_scale[["re74"]]
  re75_z <- (re75 - app_lalonde_scale_center[["re75"]]) / app_lalonde_scale_scale[["re75"]]
})

# Build design matrix; column order must match app_lalonde_X
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
# CATE(x) = E[Y(1) | X=x] - E[Y(0) | X=x], 95% HPD interval.
cate_fit <- cate(
  fit,
  newdata       = xnew,
  interval      = "hpd",
  show_progress = FALSE
)
summary(cate_fit)

# Conditional Quantile Treatment Effects at five tau levels and four profiles.
# CQTE(tau, x) = Q_1(tau | x) - Q_0(tau | x).
cqte_fit <- cqte(
  fit,
  probs    = c(0.25, 0.50, 0.75, 0.90, 0.95),
  newdata  = xnew,
  interval = "credible"
)
cqte_fit$fit_df

# Profile-specific CATE point estimates with 95% HPD bars.
cate_plot_obj <- plot(cate_fit) +
  ggplot2::labs(title = "CATE by profile", x = NULL) +
  ggplot2::theme(
    plot.title    = ggplot2::element_text(size = 10.5, face = "bold"),
    axis.title.y  = ggplot2::element_text(size = 9.5),
    axis.text     = ggplot2::element_text(size = 8.5),
    plot.margin   = ggplot2::margin(5.5, 6, 5.5, 5.5)
  )

cate_plot_obj
