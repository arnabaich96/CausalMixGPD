data("lalonde", package = "MatchIt")
library(ggplot2)
library(CausalMixGPD)
mcmc_fixed <- list(
  niter = 1000,
  nburnin = 250,
  thin = 1,
  nchains = 1,
  seed = 2026,
  show_progress = TRUE,
  quiet = FALSE,
  waic = FALSE,
  timing = TRUE
)
app_lalonde_covars <- within(lalonde, {
  race <- factor(race, levels = c("white", "black", "hispan"))
  married <- factor(married, levels = c(0, 1), labels = c("no", "yes"))
  nodegree <- factor(nodegree, levels = c(0, 1), labels = c("no", "yes"))
})
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
for (v in app_lalonde_scale_vars) {
  app_lalonde_covars[[paste0(v, "_z")]] <-
    (app_lalonde_covars[[v]] - app_lalonde_scale_center[[v]]) /
    app_lalonde_scale_scale[[v]]
}
app_lalonde_x_formula <- ~ age_z + educ_z + race + married + nodegree + re74_z + re75_z
app_lalonde_y <- (app_lalonde_covars$re78 + 0.5) / 1000
app_lalonde_A <- as.integer(app_lalonde_covars$treat)
app_lalonde_taus <- c(0.25, 0.50, 0.75, 0.90, 0.95)
app_lalonde_X <- stats::model.matrix(
  app_lalonde_x_formula,
  data = app_lalonde_covars
)[, -1, drop = FALSE]

app_lalonde_fit <- dpmgpd.causal(
  y = app_lalonde_y,
  X = app_lalonde_X,
  treat = app_lalonde_A,
  backend = "crp",
  kernel = "gamma",
  components = 10,
  mcmc = mcmc_fixed,
  parallel_arms =TRUE
)

data.frame(app_lalonde_fit$timing, row.names = NULL)
summary(app_lalonde_fit)
params(app_lalonde_fit)


app_lalonde_ate_fit <- ate(app_lalonde_fit, interval = "hpd" ,
                           level = 0.95, show_progress = FALSE)

summary(app_lalonde_ate_fit)



app_lalonde_qte_fit <- qte(
  app_lalonde_fit,
  probs = c(0.25, 0.50, 0.75, 0.9),
  interval = "credible",
  show_progress = FALSE
)



app_lalonde_qte_effect_plot <- plot(app_lalonde_qte_fit, type = "effect")
app_lalonde_qte_arms_plot <- plot(app_lalonde_qte_fit, type = "arms")

grid::grid.newpage()
app_lalonde_qte_layout <- grid::grid.layout(nrow = 1, ncol = 2)
grid::pushViewport(grid::viewport(layout = app_lalonde_qte_layout))
print(
  app_lalonde_qte_effect_plot,
  vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1)
)
print(
  app_lalonde_qte_arms_plot,
  vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2)
)
grid::upViewport()






app_lalonde_numeric_quantile <- function(x, prob = 0.50) {
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
}

app_lalonde_profile_raw <- data.frame(
  profile = c("Profile 1", "Profile 2", "Profile 3", "Profile 4"),
  age = c(
    app_lalonde_numeric_quantile(app_lalonde_covars$age, 0.25),
    app_lalonde_numeric_quantile(app_lalonde_covars$age, 0.50),
    app_lalonde_numeric_quantile(app_lalonde_covars$age, 0.75),
    app_lalonde_numeric_quantile(app_lalonde_covars$age, 0.90)
  ),
  educ = c(
    app_lalonde_numeric_quantile(app_lalonde_covars$educ, 0.25),
    app_lalonde_numeric_quantile(app_lalonde_covars$educ, 0.50),
    app_lalonde_numeric_quantile(app_lalonde_covars$educ, 0.75),
    app_lalonde_numeric_quantile(app_lalonde_covars$educ, 0.90)
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
    app_lalonde_numeric_quantile(app_lalonde_covars$re74, 0.20),
    app_lalonde_numeric_quantile(app_lalonde_covars$re74, 0.40),
    app_lalonde_numeric_quantile(app_lalonde_covars$re74, 0.60),
    app_lalonde_numeric_quantile(app_lalonde_covars$re74, 0.80)
  ),
  re75 = c(
    app_lalonde_numeric_quantile(app_lalonde_covars$re75, 0.20),
    app_lalonde_numeric_quantile(app_lalonde_covars$re75, 0.40),
    app_lalonde_numeric_quantile(app_lalonde_covars$re75, 0.60),
    app_lalonde_numeric_quantile(app_lalonde_covars$re75, 0.80)
  ),
  row.names = c("p1", "p2", "p3", "p4"),
  check.names = FALSE
)

app_lalonde_profile_model <- within(app_lalonde_profile_raw, {
  age_z <- (age - app_lalonde_scale_center[["age"]]) /
    app_lalonde_scale_scale[["age"]]
  educ_z <- (educ - app_lalonde_scale_center[["educ"]]) /
    app_lalonde_scale_scale[["educ"]]
  re74_z <- (re74 - app_lalonde_scale_center[["re74"]]) /
    app_lalonde_scale_scale[["re74"]]
  re75_z <- (re75 - app_lalonde_scale_center[["re75"]]) /
    app_lalonde_scale_scale[["re75"]]
})

app_lalonde_xnew <- stats::model.matrix(
  app_lalonde_x_formula,
  data = app_lalonde_profile_model
)[, -1, drop = FALSE]
app_lalonde_xnew <- app_lalonde_xnew[, colnames(app_lalonde_X), drop = FALSE]
rownames(app_lalonde_xnew) <- app_lalonde_profile_raw$profile




app_lalonde_profile_display <- app_lalonde_profile_raw[
  ,
  c("profile", "age", "educ", "race", "married", "nodegree", "re74", "re75"),
  drop = FALSE
]
rownames(app_lalonde_profile_display) <- NULL


app_lalonde_cate_fit <- cate(
  app_lalonde_fit,
  newdata = app_lalonde_xnew,
  type = "mean",
  interval = "hpd",
  show_progress = FALSE
)
summary(app_lalonde_cate_fit)

app_lalonde_cqte_fit <- cqte(
  app_lalonde_fit,
  probs = c(0.25, 0.50, 0.75, 0.90, 0.95),
  newdata = app_lalonde_xnew,
  interval = "credible",
  show_progress = FALSE
)
app_lalonde_cqte_fit$fit_df
