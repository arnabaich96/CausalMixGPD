# =============================================================================
# data_analysis_causal.R
# Reproduces the causal inference data analysis section of
# CausalMixGPD_JSS_article.Rnw  (Section: Data analysis II — causal inference)
#
# This script is self-contained. Run it from any working directory.
# All eval=TRUE chunks from the Rnw are included in the order they appear.
# =============================================================================

# -----------------------------------------------------------------------------
# MCMC settings — kept identical to the Rnw setup chunk so results are
# reproducible.  seed controls the NIMBLE MCMC seed.
# -----------------------------------------------------------------------------
mcmc_fixed <- list(
  niter   = 2000,
  nburnin = 500,
  thin    = 1,
  nchains = 1,
  seed    = 2026
)

# -----------------------------------------------------------------------------
# Package loading
# CausalMixGPD — main modelling package (ate, qte, cate, cqte, dpmgpd.causal)
# ggplot2      — needed for the exploratory response-panel figure
# patchwork    — side-by-side plot composition
# kableExtra   — LaTeX table rendering (xnew-results chunk)
# -----------------------------------------------------------------------------
library(CausalMixGPD)
library(ggplot2)
library(patchwork)

# -----------------------------------------------------------------------------
# Colour palette and theme used throughout the Rnw manuscript figures.
# Defined here so standalone plots match the paper exactly.
# -----------------------------------------------------------------------------
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

# =============================================================================
# chunk: causal-data-setup (eval=TRUE)
# Load the Lalonde (1978) job-training dataset from MatchIt.
# Categorical predictors are relevelled for modelling; continuous predictors
# are z-scored so the MCMC sampler operates on comparable scales.
# The outcome app_lalonde_y is re78 (1978 earnings in $000s) shifted by 0.5
# to ensure strict positivity before the Gamma bulk kernel is applied.
# =============================================================================
data("lalonde", package = "MatchIt")

# Relevel categorical covariates — white/no/no are the reference categories
app_lalonde_covars <- within(lalonde, {
  race     <- factor(race,     levels = c("white", "black", "hispan"))
  married  <- factor(married,  levels = c(0, 1), labels = c("no", "yes"))
  nodegree <- factor(nodegree, levels = c(0, 1), labels = c("no", "yes"))
})

# Store centering and scaling constants for later use with new profiles
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

# Add z-scored versions as new columns (suffix _z)
for (v in app_lalonde_scale_vars) {
  app_lalonde_covars[[paste0(v, "_z")]] <-
    (app_lalonde_covars[[v]] - app_lalonde_scale_center[[v]]) /
    app_lalonde_scale_scale[[v]]
}

# Formula for the covariate design matrix (intercept excluded; handled inside bundle)
app_lalonde_x_formula <- ~ age_z + educ_z + race +
  married + nodegree + re74_z + re75_z

# Outcome: shift re78 by +0.5 and divide by 1000 to ensure strict positivity
app_lalonde_y <- (app_lalonde_covars$re78 + 0.5) / 1000

# Treatment indicator (integer 0/1)
app_lalonde_A <- as.integer(app_lalonde_covars$treat)

# Quantile levels used later for QTE and CQTE estimation
app_lalonde_taus <- c(0.25, 0.50, 0.75, 0.90, 0.95)

# Design matrix (intercept column removed — the model adds its own)
app_lalonde_X <- stats::model.matrix(
  app_lalonde_x_formula,
  data = app_lalonde_covars
)[, -1, drop = FALSE]

# =============================================================================
# chunk: app_lalonde_response_panels (eval=TRUE, echo=FALSE)
# Exploratory figure: distribution of 1978 earnings (re78) stratified by
# treatment arm.  Left panel — arm-specific boxplots; Right panel — overlaid
# density histograms.  Motivates full-distribution modelling rather than
# average-only summaries.
# Corresponds to Figure app_lalonde_response_panels-1 in the Rnw.
# =============================================================================
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

# Print the combined figure
app_lalonde_box_plot + app_lalonde_hist_plot +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

# =============================================================================
# chunk: fit-code (eval=TRUE)
# Fit arm-specific spliced bulk-Gamma / GPD tail DPM models via
# dpmgpd.causal().  The CRP backend runs a Chinese Restaurant Process sampler.
# A logistic propensity-score model (PS = default inside causal bundle) is
# estimated first; the PS posterior mean is then appended as an extra covariate
# in each arm's outcome model (doubly-robust augmentation).
# =============================================================================
fit <- dpmgpd.causal(
  y          = app_lalonde_y,
  X          = app_lalonde_X,
  treat      = app_lalonde_A,
  backend    = "crp",    # Chinese Restaurant Process DP representation
  kernel     = "gamma",  # Gamma bulk kernel (positive support)
  components = 10,       # truncation for mixture weights
  mcmc       = mcmc_fixed
)

# =============================================================================
# chunk: posterior-code (eval=TRUE, echo=FALSE)
# Print wall-clock timing for each stage of the causal MCMC run:
# ps (propensity score), con (control arm), trt (treated arm).
# =============================================================================
data.frame(fit$timing)

# =============================================================================
# chunk: ate-code (eval=TRUE)
# Estimate the Average Treatment Effect (ATE):
#   ATE = E[Y(1)] - E[Y(0)]
# with a 95% HPD interval.  summary() prints point estimate and interval.
# =============================================================================
ate_fit <- ate(fit, interval = "hpd", level = 0.95)
summary(ate_fit)

# =============================================================================
# chunk: qte-call (eval=TRUE, results='hide')
# Compute marginal Quantile Treatment Effects (QTE) at tau = 0.25, 0.50, 0.75:
#   QTE(tau) = Q_1^m(tau) - Q_0^m(tau)
# where Q_a^m are marginalised (over X) quantile curves under each arm.
# =============================================================================
qte_fit <- qte(
  fit,
  probs    = c(0.25, 0.50, 0.75),
  interval = "credible"
)

# =============================================================================
# chunk: qte_plot (eval=TRUE)
# Two-panel figure:
#   Left  — QTE curve vs tau with pointwise 95% credible band
#   Right — arm-specific quantile curves Q_1(tau) and Q_0(tau)
# Corresponds to Figure qte_plot-1 in the Rnw.
# =============================================================================
qte_effect_plot <- plot(qte_fit, type = "effect")
qte_arms_plot   <- plot(qte_fit, type = "arms")
qte_effect_plot + qte_arms_plot + patchwork::plot_layout(ncol = 2)

# =============================================================================
# chunk: xnew-code (eval=TRUE, echo=FALSE)
# Build four synthetic covariate profiles used as "new data" for
# profile-specific causal contrasts (CATE and CQTE).
# Profiles span age/education quantiles and fixed race/marital/degree values.
# profile_raw   — original scale values (for display in the table)
# profile_model — z-scored values fed to model.matrix
# xnew          — design matrix fed to cate() and cqte()
# =============================================================================
numeric_quantile <- function(x, prob = 0.50) {
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
}

# Four representative profiles at different quantiles of the continuous covariates
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

# Z-score the continuous covariates using the training-data scaling constants
profile_model <- within(profile_raw, {
  age_z  <- (age  - app_lalonde_scale_center[["age"]])  / app_lalonde_scale_scale[["age"]]
  educ_z <- (educ - app_lalonde_scale_center[["educ"]]) / app_lalonde_scale_scale[["educ"]]
  re74_z <- (re74 - app_lalonde_scale_center[["re74"]]) / app_lalonde_scale_scale[["re74"]]
  re75_z <- (re75 - app_lalonde_scale_center[["re75"]]) / app_lalonde_scale_scale[["re75"]]
})

# Build the design matrix; reorder columns to match app_lalonde_X exactly
xnew <- stats::model.matrix(app_lalonde_x_formula, data = profile_model)[, -1, drop = FALSE]
xnew <- xnew[, colnames(app_lalonde_X), drop = FALSE]
rownames(xnew) <- profile_raw$profile

# Display data frame (original scale, row-names stripped)
profile_display <- profile_raw[
  , c("profile", "age", "educ", "race", "married", "nodegree", "re74", "re75"),
  drop = FALSE
]
rownames(profile_display) <- NULL

# =============================================================================
# chunk: xnew-results (eval=TRUE, echo=FALSE, results='asis')
# Render the profile table in LaTeX format (matches Table xnew-results in Rnw).
# Outside of knitr, this prints the raw LaTeX code to the console.
# =============================================================================
profile_t <- as.data.frame(t(profile_display[, -which(names(profile_display) == "profile")]))
colnames(profile_t) <- profile_display$profile
profile_t[] <- lapply(profile_t, function(x) {
  num <- suppressWarnings(as.numeric(as.character(x)))
  ifelse(!is.na(num), formatC(round(num, 4), format = "f", digits = 4), as.character(x))
})
kableExtra::kbl(
  profile_t,
  format    = "latex",
  booktabs  = TRUE,
  caption   = paste0(
    "Representative Lalonde participant profiles used as new-data inputs for ",
    "the conditional causal contrasts. Columns (Profile 1--Profile 4) give the ",
    "covariate values on the original data scale."
  ),
  row.names = TRUE
) |>
  kableExtra::kable_styling(
    latex_options = c("scale_down"),
    font_size     = 8
  )

# =============================================================================
# chunk: cate-call (eval=TRUE, results='hide')
# Conditional Average Treatment Effect at each of the four profiles:
#   CATE(x) = E[Y(1) | X=x] - E[Y(0) | X=x]
# interval = "hpd" returns the shortest 95% credible interval.
# summary() prints a table of CATE estimates and intervals per profile.
# =============================================================================
cate_fit <- cate(
  fit,
  newdata       = xnew,
  interval      = "hpd",
  show_progress = FALSE
)
summary(cate_fit)

# =============================================================================
# chunk: cqte-call (eval=TRUE)
# Conditional Quantile Treatment Effects at five probability levels and four
# profiles:  CQTE(tau, x) = Q_1(tau | x) - Q_0(tau | x).
# cqte_fit$fit_df is a tidy data frame of all CQTE estimates.
# =============================================================================
cqte_fit <- cqte(
  fit,
  probs    = c(0.25, 0.50, 0.75, 0.90, 0.95),
  newdata  = xnew,
  interval = "credible"
)
cqte_fit$fit_df  # full CQTE summary table

# =============================================================================
# chunk: cate_plot (eval=TRUE, echo=FALSE)
# Profile-specific CATE plot: one point per profile with 95% HPD bars.
# Corresponds to Figure cate_plot-1 in the Rnw.
# =============================================================================
cate_plot_obj <- plot(cate_fit) +
  ggplot2::labs(title = "CATE by profile", x = NULL) +
  ggplot2::theme(
    plot.title    = ggplot2::element_text(size = 10.5, face = "bold"),
    axis.title.y  = ggplot2::element_text(size = 9.5),
    axis.text     = ggplot2::element_text(size = 8.5),
    plot.margin   = ggplot2::margin(5.5, 6, 5.5, 5.5)
  )

cate_plot_obj
