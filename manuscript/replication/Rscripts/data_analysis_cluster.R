# =============================================================================
# data_analysis_cluster.R
# (Section: Data analysis I — clustering)
# Data: Boston housing (MASS), outcome = medv, predictors = lstat, rm, nox.
# =============================================================================

# MCMC settings — match the Rnw setup chunk exactly for reproducibility.
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

# Colour palette and ggplot2 theme — defined in the Rnw setup chunk.
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

# Load Boston, keep the four variables used in the model, and split 80/20.
set.seed(cmgpd_seed)
data("Boston", package = "MASS")

dat <- Boston

n         <- nrow(dat)
idx_train <- sample(seq_len(n), size = floor(0.80 * n), replace = FALSE)
train_dat <- dat[idx_train, ]
test_dat  <- dat[-idx_train, ]

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
    width           = 0.45,
    fill            = cmgpd_pal[["mist"]],
    colour          = cmgpd_pal[["navy"]],
    outlier.colour  = cmgpd_pal[["rose"]],
    outlier.alpha   = 0.85
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

app_cluster_box_plot + app_cluster_hist_plot + plot_layout(ncol = 2)

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
plot(z_train_psm, type = "summary")

# Hard cluster labels via the medoid of the posterior partition distribution.
z_train_lab <- predict(fit_clust, type = "label")

# Training observations coloured by cluster label.
plot(z_train_lab, type = "summary")

# Assign test observations to the nearest training cluster.
z_test <- predict(
  fit_clust,
  newdata = test_dat,
  type    = "label"
)

# Per-cluster covariate profile table for the test observations.
summary(z_test)$cluster_profiles

# Bar chart of test-observation counts per predicted cluster.
plot(z_test, type = "sizes")
