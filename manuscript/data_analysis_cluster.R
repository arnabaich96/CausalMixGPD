# =============================================================================
# data_analysis_cluster.R
# Reproduces the clustering data analysis section of CausalMixGPD_JSS_article.Rnw
# (Section: Data analysis I — clustering)
#
# This script is self-contained. Run it from any working directory.
# All eval=TRUE chunks from the Rnw are included in the order they appear.
# =============================================================================

# -----------------------------------------------------------------------------
# MCMC settings — kept identical to the Rnw setup chunk so results are
# reproducible.  seed controls the NIMBLE MCMC seed; set.seed(123) below
# governs the 80/20 train/test split of the Boston data.
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
# CausalMixGPD — main modelling package
# MASS         — provides the Boston housing dataset
# ggplot2      — needed for the exploratory response-panel figure
# patchwork    — side-by-side plot composition
# -----------------------------------------------------------------------------
library(CausalMixGPD)
library(MASS)
library(ggplot2)
library(patchwork)

# -----------------------------------------------------------------------------
# Colour palette and theme used throughout the Rnw manuscript figures.
# Defined here so the standalone plots match the paper exactly.
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
# chunk: app_cluster_data (eval=TRUE)
# Load the Boston housing dataset, restrict to the four variables of interest,
# and create an 80/20 stratified-random train/test split.
# set.seed(123) ensures the split is identical to the Rnw.
# =============================================================================
set.seed(123)
data("Boston", package = "MASS")

# Keep only the outcome (medv) and the three predictors used in the model
dat <- Boston
dat <- subset(dat, select = c(medv, lstat, rm, nox))

n         <- nrow(dat)                          # 506 census tracts
idx_train <- sample(seq_len(n), size = floor(0.80 * n), replace = FALSE)
train_dat <- dat[idx_train, ]   # 404 tracts for fitting
test_dat  <- dat[-idx_train, ]  # 102 tracts for held-out evaluation

# =============================================================================
# chunk: app_cluster_response_panels (eval=TRUE, echo=FALSE)
# Exploratory figure: full-sample distribution of medv across all 506 tracts.
# Left panel — boxplot highlighting upper outliers.
# Right panel — density-scaled histogram.
# Corresponds to Figure app_cluster_response_panels-1 in the Rnw.
# =============================================================================
app_cluster_plot_df <- data.frame(
  medv   = dat$medv,
  sample = "All tracts"
)

app_cluster_box_plot <- ggplot(
  app_cluster_plot_df,
  aes(x = sample, y = medv)
) +
  geom_boxplot(
    width           = 0.45,
    fill            = cmgpd_pal[["mist"]],
    colour          = cmgpd_pal[["navy"]],
    outlier.colour  = cmgpd_pal[["rose"]],
    outlier.alpha   = 0.85
  ) +
  labs(
    title = "House prices: boxplot",
    x     = NULL,
    y     = "medv"
  ) +
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
  labs(
    title = "House prices: histogram",
    x     = "medv",
    y     = "Density"
  ) +
  cmgpd_theme(base_size = 12)

# Print the side-by-side figure (matches manuscript figure layout)
app_cluster_box_plot + app_cluster_hist_plot + plot_layout(ncol = 2)

# =============================================================================
# chunk: app_cluster_fit-code (eval=TRUE)
# Fit a covariate-dependent Dirichlet process mixture with a Normal bulk kernel
# and weight-only covariate dependence (type = "weights").
# components = 10 sets the truncation level for the DP mixture.
# MCMC uses the settings in mcmc_fixed defined above.
# =============================================================================
fit_clust <- dpmix.cluster(
  formula    = medv ~ lstat + rm + nox,
  data       = train_dat,
  kernel     = "normal",
  type       = "weights",   # covariates enter only the mixture weights
  components = 10,
  mcmc       = mcmc_fixed
)

# =============================================================================
# chunk: app_cluster_psm-code (eval=TRUE)
# Compute the posterior similarity matrix (PSM) for the training sample.
# The PSM[i, j] = P(z_i = z_j | data) averages co-clustering over MCMC draws.
# =============================================================================
z_train_psm <- predict(fit_clust, type = "psm")

# =============================================================================
# chunk: app_cluster_psm_plot-code (eval=TRUE)
# Plot a heatmap summary of the PSM.  Rows/columns are reordered to expose
# block structure; darker cells indicate higher co-clustering probability.
# Corresponds to Figure app_cluster_psm_plot-code-1 in the Rnw.
# =============================================================================
plot(z_train_psm, type = "summary")

# =============================================================================
# chunk: app_cluster_label-code (eval=TRUE)
# Derive a representative hard partition (cluster labels) from the PSM via
# the medoid of the posterior distribution over partitions.
# =============================================================================
z_train_lab <- predict(fit_clust, type = "label")

# =============================================================================
# chunk: app_cluster_overlay (eval=TRUE)
# Plot training observations coloured by their representative cluster label.
# Corresponds to Figure app_cluster_overlay-1 in the Rnw.
# =============================================================================
plot(z_train_lab, type = "summary")

# =============================================================================
# chunk: app_cluster_predict_new-code (eval=TRUE)
# Assign held-out test observations to the nearest training cluster.
# predict(..., newdata = test_dat, type = "label") uses the training-partition
# medoid to classify each test observation.
# =============================================================================
z_test <- predict(
  fit_clust,
  newdata = test_dat,
  type    = "label"
)

# =============================================================================
# chunk: app_cluster_test_profiles (eval=TRUE)
# Print a per-cluster covariate profile table for the test observations.
# Shows the mean and SD of each variable within every predicted cluster.
# =============================================================================
summary(z_test)$cluster_profiles

# =============================================================================
# chunk: app_cluster_test_sizes (eval=TRUE)
# Bar plot of the number of test observations assigned to each training cluster.
# Corresponds to Figure app_cluster_test_sizes-1 in the Rnw.
# =============================================================================
plot(z_test, type = "sizes")
