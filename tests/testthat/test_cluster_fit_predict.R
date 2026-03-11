test_that("cluster wrappers fit and predict across type modes", {
  skip_if_not_test_level("ci")
  skip_if_not(exists("dpmix.cluster", mode = "function"))

  set.seed(123)
  dat <- data.frame(
    y = abs(stats::rnorm(24)) + 0.25,
    x1 = stats::rnorm(24),
    x2 = stats::runif(24)
  )

  for (tp in c("weights", "param", "both")) {
    fit <- dpmix.cluster(
      y ~ x1 + x2,
      data = dat,
      kernel = "normal",
      components = 4,
      type = tp,
      mcmc = mcmc_fast(seed = 10L)
    )
    expect_s3_class(fit, "dpmixgpd_cluster_fit")
    expect_true(is.list(fit$timing))
    expect_true(all(c("build", "compile", "mcmc", "cache_hit", "total") %in% names(fit$timing)))

    psm <- predict(fit, type = "psm")
    expect_s3_class(psm, "dpmixgpd_cluster_psm")
    expect_true(is.matrix(psm$psm))
    expect_equal(nrow(psm$psm), ncol(psm$psm))
    expect_equal(unname(diag(psm$psm)), rep(1, nrow(psm$psm)), tolerance = 1e-8)
    expect_true(all(psm$psm >= -1e-10 & psm$psm <= 1 + 1e-10))

    lbl_train <- predict(fit, type = "label")
    expect_s3_class(lbl_train, "dpmixgpd_cluster_labels")
    expect_equal(length(lbl_train$labels), nrow(dat))
    expect_false("scores" %in% names(lbl_train))

    lbl_train_scores <- predict(fit, type = "label", return_scores = TRUE)
    expect_true(is.matrix(lbl_train_scores$scores))
    expect_equal(rowSums(lbl_train_scores$scores), rep(1, nrow(dat)), tolerance = 1e-8)

    nd <- dat[1:6, c("y", "x1", "x2")]
    lbl_new <- predict(fit, newdata = nd, type = "label")
    expect_s3_class(lbl_new, "dpmixgpd_cluster_labels")
    expect_equal(length(lbl_new$labels), nrow(nd))
    expect_false("scores" %in% names(lbl_new))

    lbl_new_scores <- predict(fit, newdata = nd, type = "label", return_scores = TRUE)
    expect_true(is.matrix(lbl_new_scores$scores))
    expect_equal(rowSums(lbl_new_scores$scores), rep(1, nrow(nd)), tolerance = 1e-8)
  }
})

test_that("validation and guard behavior for clustering types", {
  skip_if_not_test_level("ci")
  skip_if_not(exists("dpmix.cluster", mode = "function"))

  set.seed(99)
  dat <- data.frame(
    y = abs(stats::rnorm(30)) + 0.1,
    x1 = stats::rnorm(30),
    x2 = stats::runif(30)
  )

  expect_error(
    dpmix.cluster(y ~ 1, data = dat, kernel = "normal", type = "weights", components = 4, mcmc = mcmc_fast(seed = 1L)),
    "requires covariates"
  )
  expect_error(
    dpmix.cluster(y ~ x1 + x2, data = dat, kernel = "normal", type = "weights", mcmc = mcmc_fast(seed = 1L)),
    "explicit 'components'"
  )
  expect_error(
    dpmix.cluster(y ~ x1 + x2, data = dat, kernel = "normal", type = "both", mcmc = mcmc_fast(seed = 1L)),
    "explicit 'components'"
  )

  expect_warning(
    fit_param <- dpmix.cluster(y ~ 1, data = dat, kernel = "normal", type = "param", mcmc = mcmc_fast(seed = 2L)),
    "using default components"
  )
  expect_s3_class(fit_param, "dpmixgpd_cluster_fit")

  expect_error(
    predict(fit_param, type = "psm", psm_max_n = 10L),
    "PSM is O\\(n\\^2\\)"
  )
})

test_that("cluster link and priors overrides are applied to spec", {
  skip_if_not_test_level("ci")
  skip_if_not(exists("build_cluster_bundle", mode = "function"))

  set.seed(111)
  dat <- data.frame(
    y = abs(stats::rnorm(20)) + 0.2,
    x1 = stats::rnorm(20),
    x2 = stats::runif(20)
  )

  b <- build_cluster_bundle(
    y ~ x1 + x2,
    data = dat,
    kernel = "normal",
    GPD = TRUE,
    type = "both",
    components = 4,
    link = list(
      bulk = list(mean = "identity"),
      gpd = list(tail_scale = list(link = "exp"))
    ),
    priors = list(
      bulk = list(sd = list(dist = "gamma", args = list(shape = 2, rate = 1))),
      gpd = list(
        tail_shape = list(dist = "normal", args = list(mean = 0, sd = 0.25)),
        tail_scale = list(dist = "normal", args = list(mean = 0, sd = 0.7))
      ),
      concentration = list(dist = "gamma", args = list(shape = 3, rate = 2))
    ),
    mcmc = mcmc_fast(seed = 7L)
  )

  expect_equal(b$spec$plan$bulk$mean$mode, "link")
  expect_equal(b$spec$plan$bulk$mean$link, "identity")
  expect_equal(b$spec$plan$bulk$sd$mode, "dist")
  expect_equal(b$spec$plan$bulk$sd$dist, "gamma")
  expect_equal(b$spec$plan$bulk$sd$args$shape, 2)
  expect_equal(b$spec$plan$gpd$tail_scale$mode, "link")
  expect_equal(b$spec$plan$gpd$tail_scale$link, "exp")
  expect_equal(b$spec$plan$gpd$tail_scale$beta_prior$dist, "normal")
  expect_equal(b$spec$plan$gpd$tail_scale$beta_prior$args$sd, 0.7)
  expect_equal(b$spec$plan$gpd$tail_shape$mode, "dist")
  expect_equal(b$spec$plan$gpd$tail_shape$dist, "normal")
  expect_equal(b$spec$plan$concentration$mode, "dist")
  expect_equal(b$spec$plan$concentration$dist, "gamma")
  expect_equal(b$spec$plan$concentration$args$shape, 3)
})

test_that("weights and both monitor gating while param does not", {
  skip_if_not_test_level("ci")
  skip_if_not(exists("dpmix.cluster", mode = "function"))

  set.seed(202)
  dat <- data.frame(
    y = abs(stats::rnorm(26)) + 0.2,
    x1 = stats::rnorm(26),
    x2 = stats::runif(26)
  )

  .to_mat <- function(smp) {
    if (inherits(smp, "mcmc.list")) {
      return(do.call(rbind, lapply(smp, as.matrix)))
    }
    as.matrix(smp)
  }

  fit_w <- dpmix.cluster(y ~ x1 + x2, data = dat, kernel = "normal", components = 4, type = "weights", mcmc = mcmc_fast(seed = 3L))
  fit_b <- dpmix.cluster(y ~ x1 + x2, data = dat, kernel = "normal", components = 4, type = "both", mcmc = mcmc_fast(seed = 4L))
  fit_p <- dpmix.cluster(y ~ x1 + x2, data = dat, kernel = "normal", components = 4, type = "param", mcmc = mcmc_fast(seed = 5L))

  cn_w <- colnames(.to_mat(fit_w$samples))
  cn_b <- colnames(.to_mat(fit_b$samples))
  cn_p <- colnames(.to_mat(fit_p$samples))

  expect_true(any(grepl("^eta\\[[0-9]+\\]$", cn_w)))
  expect_true(any(grepl("^B\\[[0-9]+,\\s*[0-9]+\\]$", cn_w)))
  expect_true(any(grepl("^eta\\[[0-9]+\\]$", cn_b)))
  expect_true(any(grepl("^B\\[[0-9]+,\\s*[0-9]+\\]$", cn_b)))
  expect_false(any(grepl("^eta\\[[0-9]+\\]$", cn_p)))
  expect_false(any(grepl("^B\\[[0-9]+,\\s*[0-9]+\\]$", cn_p)))
})

test_that("weights labels depend on x through gating scores", {
  skip_if_not_test_level("ci")
  skip_if_not(exists("dpmix.cluster", mode = "function"))

  set.seed(321)
  n <- 40
  x1 <- stats::rnorm(n)
  x2 <- stats::rnorm(n)
  y <- abs(stats::rnorm(n, mean = ifelse(x1 > 0, 2, 0.5), sd = 0.4)) + 0.1
  dat <- data.frame(y = y, x1 = x1, x2 = x2)

  fit <- dpmix.cluster(
    y ~ x1 + x2,
    data = dat,
    kernel = "normal",
    components = 4,
    type = "weights",
    mcmc = mcmc_fast(seed = 6L)
  )

  nd <- data.frame(
    y = c(stats::median(y), stats::median(y)),
    x1 = c(-2, 2),
    x2 = c(-1, 1)
  )
  pred <- predict(fit, newdata = nd, type = "label", return_scores = TRUE)
  expect_true(is.matrix(pred$scores))
  expect_gt(sum(abs(pred$scores[1, ] - pred$scores[2, ])), 1e-6)
})

test_that("component-level beta extraction is preferred when present", {
  skip_if_not_test_level("ci")
  fn <- getFromNamespace(".cluster_extract_beta_auto", "CausalMixGPD")

  draw_row <- c(
    "beta_tail_scale[1,1]" = 2.0,
    "beta_tail_scale[1,2]" = -1.0,
    "beta_tail_scale[2,1]" = 0.5,
    "beta_tail_scale[2,2]" = 0.25,
    "beta_tail_scale[1]" = 99.0,
    "beta_tail_scale[2]" = 98.0
  )

  b1 <- fn(draw_row = draw_row, base = "beta_tail_scale", comp = 1L, P = 2L)
  b2 <- fn(draw_row = draw_row, base = "beta_tail_scale", comp = 2L, P = 2L)
  expect_equal(as.numeric(b1), c(2.0, -1.0), tolerance = 1e-12)
  expect_equal(as.numeric(b2), c(0.5, 0.25), tolerance = 1e-12)
})

test_that("dpmgpd.cluster supports label and psm prediction", {
  skip_if_not_test_level("ci")
  skip_if_not(exists("dpmgpd.cluster", mode = "function"))

  set.seed(404)
  dat <- data.frame(
    y = abs(stats::rnorm(20)) + 0.5,
    x1 = stats::rnorm(20),
    x2 = stats::runif(20)
  )

  fit <- dpmgpd.cluster(
    y ~ x1 + x2,
    data = dat,
    kernel = "normal",
    components = 4,
    type = "weights",
    mcmc = mcmc_fast(seed = 20L)
  )
  expect_s3_class(fit, "dpmixgpd_cluster_fit")
  expect_s3_class(predict(fit, type = "psm"), "dpmixgpd_cluster_psm")
  expect_s3_class(predict(fit, type = "label"), "dpmixgpd_cluster_labels")
})

test_that("newdata unseen factor levels get explicit error", {
  fn <- getFromNamespace(".cluster_build_design", "CausalMixGPD")

  train <- data.frame(
    y = c(1, 2, 3),
    g = factor(c("a", "b", "a"))
  )
  trm <- stats::terms(y ~ g, data = train)
  mf <- stats::model.frame(trm, data = train)
  mm <- stats::model.matrix(stats::delete.response(trm), data = mf)
  X_cols <- setdiff(colnames(mm), "(Intercept)")
  meta <- list(
    terms = trm,
    xlevels = stats::.getXlevels(trm, mf),
    contrasts = attr(mm, "contrasts"),
    X_cols = X_cols,
    response = "y"
  )

  nd <- data.frame(
    y = 1,
    g = factor("c", levels = c("a", "b", "c"))
  )
  expect_error(
    fn(meta = meta, newdata = nd),
    "unseen factor levels"
  )
})
