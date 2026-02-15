# tests/testthat/test-internal-utils.R
# Unit tests for internal utility helpers (02-utilities-internal.R)

# Access internal helpers using :::
.validate_nimble_reserved_names <- CausalMixGPD:::.validate_nimble_reserved_names
.coerce_fit_df <- CausalMixGPD:::.coerce_fit_df
.compute_interval <- CausalMixGPD:::.compute_interval
.posterior_summarize <- CausalMixGPD:::.posterior_summarize
.truncate_components_one_draw <- CausalMixGPD:::.truncate_components_one_draw
.wrap_plotly <- CausalMixGPD:::.wrap_plotly
.plot_palette <- CausalMixGPD:::.plot_palette
.extract_nimble_code <- CausalMixGPD:::.extract_nimble_code
.wrap_nimble_code <- CausalMixGPD:::.wrap_nimble_code

# ======================================================================
# .validate_nimble_reserved_names
# ======================================================================

test_that(".validate_nimble_reserved_names accepts valid names", {
  expect_invisible(.validate_nimble_reserved_names(c("alpha", "beta", "sigma")))
  expect_invisible(.validate_nimble_reserved_names(character(0)))
  expect_invisible(.validate_nimble_reserved_names(NULL))
  expect_invisible(.validate_nimble_reserved_names(c("x_if", "myfor")))
})

test_that(".validate_nimble_reserved_names rejects reserved keywords", {
  expect_error(
    .validate_nimble_reserved_names(c("if", "alpha"), context = "test"),
    regexp = "reserved NIMBLE keywords"
  )
  expect_error(
    .validate_nimble_reserved_names(c("FOR", "WHILE")),
    regexp = "reserved NIMBLE keywords"
  )
  expect_error(
    .validate_nimble_reserved_names(c("T", "F")),
    regexp = "reserved NIMBLE keywords"
  )
  expect_error(
    .validate_nimble_reserved_names(c("na", "nan", "inf")),
    regexp = "reserved NIMBLE keywords"
  )
})

test_that(".validate_nimble_reserved_names handles edge cases", {
  expect_invisible(.validate_nimble_reserved_names(c("", NA_character_)))
  expect_invisible(.validate_nimble_reserved_names(c("iffy", "truly", "nullify")))
})

# ======================================================================
# Plot helpers / code wrappers
# ======================================================================

test_that(".plot_palette returns requested length", {
  expect_length(.plot_palette(3), 3)
  expect_length(.plot_palette(8), 8)
  expect_length(.plot_palette(12), 12)
})

test_that(".extract_nimble_code and .wrap_nimble_code handle wrapped/list code", {
  code_obj <- quote({ a <- 1 })
  wrapped <- .wrap_nimble_code(code_obj)
  expect_true(is.list(wrapped))
  expect_true(!is.null(wrapped$nimble))

  # extract returns $nimble when stored in a list wrapper
  extracted <- .extract_nimble_code(wrapped)
  expect_equal(deparse(extracted), deparse(code_obj))

  # if already a list, wrapper should pass-through
  pass <- .wrap_nimble_code(list(nimble = code_obj))
  expect_true(is.list(pass))
  expect_true(!is.null(pass$nimble))
})

test_that(".wrap_plotly returns plotly when available, otherwise passthrough", {
  p <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(1, 1))

  out <- .wrap_plotly(p)
  if (requireNamespace("plotly", quietly = TRUE)) {
    old_opt <- getOption("CausalMixGPD.plotly")
    options(CausalMixGPD.plotly = TRUE)
    on.exit(options(CausalMixGPD.plotly = old_opt), add = TRUE)
    out <- .wrap_plotly(p)
    expect_true(is.list(out))
    expect_true(inherits(out, "plotly") || inherits(out, "htmlwidget"))
  } else {
    expect_true(inherits(out, "ggplot"))
  }

  plots <- list(a = p, b = p)
  class(plots) <- c("mixgpd_fit_plots", "list")
  out2 <- .wrap_plotly(plots)
  expect_true(inherits(out2, "mixgpd_fit_plots"))
  expect_true(is.list(out2))
})

# ======================================================================
# .coerce_fit_df
# ======================================================================

test_that(".coerce_fit_df handles data.frame input", {
  df <- data.frame(estimate = 1:3, lower = 0:2, upper = 2:4)
  result <- .coerce_fit_df(df)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("estimate", "lower", "upper", "id") %in% names(result)))
  expect_equal(result$estimate, 1:3)
})

test_that(".coerce_fit_df handles data.frame missing estimate", {
  df <- data.frame(fit = c(1, 2), other = c(3, 4))
  result <- .coerce_fit_df(df)
  expect_equal(result$estimate, c(1, 2))
  expect_true("id" %in% names(result))
})

test_that(".coerce_fit_df handles data.frame missing fit/estimate column", {
  df <- data.frame(a = 1:3, b = 4:6)
  result <- .coerce_fit_df(df)
  expect_equal(result$estimate, 1:3)  # uses first numeric column
})

test_that(".coerce_fit_df handles numeric matrix with named columns", {
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  colnames(mat) <- c("estimate", "lower", "upper")
  result <- .coerce_fit_df(mat)
  expect_s3_class(result, "data.frame")
  expect_true("id" %in% names(result))
  expect_equal(nrow(result), 3)
})

test_that(".coerce_fit_df handles matrix with probs expansion", {
  mat <- matrix(1:6, nrow = 2, ncol = 3)
  probs <- c(0.25, 0.5, 0.75)
  result <- .coerce_fit_df(mat, n_pred = 2, probs = probs)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("id", "index", "estimate") %in% names(result)))
  expect_equal(nrow(result), 6)  # 2 rows x 3 probs
})

test_that(".coerce_fit_df handles matrix default (first column)", {

  mat <- matrix(1:6, nrow = 3, ncol = 2)
  result <- .coerce_fit_df(mat)
  expect_equal(result$estimate, 1:3)
  expect_equal(result$lower, 4:6)
})

test_that(".coerce_fit_df handles numeric vector", {
  vec <- c(1.0, 2.0, 3.0)
  result <- .coerce_fit_df(vec)
  expect_s3_class(result, "data.frame")
  expect_equal(result$estimate, vec)
  expect_equal(result$id, 1:3)
})

test_that(".coerce_fit_df errors on unsupported type", {
  expect_error(.coerce_fit_df(list(a = 1)), regexp = "Cannot coerce")
})

# ======================================================================
# .compute_interval
# ======================================================================

test_that(".compute_interval credible returns expected quantiles", {
  draws <- rnorm(1000, mean = 5, sd = 1)
  iv <- .compute_interval(draws, level = 0.95, type = "credible")
  expect_named(iv, c("lower", "upper"))
  expect_true(iv["lower"] < 5)

  expect_true(iv["upper"] > 5)
  expect_true(iv["lower"] < iv["upper"])
})

test_that(".compute_interval hpd returns valid interval", {
  skip_if_not_installed("coda")
  draws <- rnorm(1000, mean = 0, sd = 1)
  iv <- .compute_interval(draws, level = 0.95, type = "hpd")
  expect_named(iv, c("lower", "upper"))
  expect_true(iv["lower"] < iv["upper"])
})
test_that(".compute_interval handles insufficient draws", {
  expect_equal(.compute_interval(numeric(0)), c(lower = NA_real_, upper = NA_real_))
  expect_equal(.compute_interval(c(1)), c(lower = NA_real_, upper = NA_real_))
})

test_that(".compute_interval filters non-finite", {
  draws <- c(1, 2, 3, NA, Inf, -Inf, 4, 5)
  iv <- .compute_interval(draws, level = 0.95, type = "credible")
  expect_true(is.finite(iv["lower"]))
  expect_true(is.finite(iv["upper"]))
})

# ======================================================================
# .posterior_summarize
# ======================================================================

test_that(".posterior_summarize works on numeric vector", {
  draws <- rnorm(100, mean = 10, sd = 2)
  result <- .posterior_summarize(draws, probs = c(0.025, 0.5, 0.975), interval = "credible")
  expect_named(result, c("estimate", "lower", "upper", "q"))
  expect_length(result$estimate, 1)
  expect_true(result$lower < result$upper)
})

test_that(".posterior_summarize works on 2D matrix", {
  draws <- matrix(rnorm(300), nrow = 3, ncol = 100)
  result <- .posterior_summarize(draws, probs = c(0.025, 0.975), interval = "credible")
  expect_length(result$estimate, 3)
  expect_length(result$lower, 3)
  expect_length(result$upper, 3)
})

test_that(".posterior_summarize works on 3D array", {
  draws <- array(rnorm(600), dim = c(2, 3, 100))
  result <- .posterior_summarize(draws, probs = c(0.025, 0.975), interval = "credible")
  expect_equal(dim(result$estimate), c(2, 3))
  expect_equal(dim(result$lower), c(2, 3))
  expect_equal(dim(result$upper), c(2, 3))
})

test_that(".posterior_summarize handles interval = NULL", {
  draws <- rnorm(100)
  result <- .posterior_summarize(draws, probs = c(0.025, 0.975), interval = NULL)
  expect_true(is.na(result$lower))
  expect_true(is.na(result$upper))
})

test_that(".posterior_summarize hpd mode", {
  skip_if_not_installed("coda")
  draws <- rnorm(200)
  result <- .posterior_summarize(draws, probs = c(0.025, 0.975), interval = "hpd")
  expect_true(!is.na(result$lower))
  expect_true(!is.na(result$upper))
})

# ======================================================================
# .truncate_components_one_draw
# ======================================================================

test_that(".truncate_components_one_draw sorts by weight", {
  w <- c(0.1, 0.6, 0.3)
  params <- list(mu = c(1, 2, 3), sigma = c(0.5, 1, 1.5))
  result <- .truncate_components_one_draw(w, params, epsilon = 0.01)

  expect_equal(result$ord, c(2, 3, 1))  # sorted descending by weight
  expect_true(sum(result$weights) > 0.99)
})

test_that(".truncate_components_one_draw truncates small components", {
  w <- c(0.05, 0.9, 0.05)
  params <- list(mu = c(1, 2, 3))
  result <- .truncate_components_one_draw(w, params, epsilon = 0.1)

  expect_true(result$k <= 3)
  expect_true(length(result$weights) <= 3)
})

test_that(".truncate_components_one_draw errors on invalid epsilon", {
  w <- c(0.5, 0.5)
  params <- list(mu = c(1, 2))

  expect_error(.truncate_components_one_draw(w, params, epsilon = -0.1))
  expect_error(.truncate_components_one_draw(w, params, epsilon = 1.0))
  expect_error(.truncate_components_one_draw(w, params, epsilon = NA))
  expect_error(.truncate_components_one_draw(w, params, epsilon = c(0.1, 0.2)))
})

test_that(".truncate_components_one_draw handles empty params", {
  w <- c(0.3, 0.7)
  result <- .truncate_components_one_draw(w, params = list(), epsilon = 0.01)
  expect_true(result$k >= 1)
})

test_that(".truncate_components_one_draw validates params length", {
  w <- c(0.5, 0.5)
  params <- list(mu = c(1, 2, 3))  # wrong length

  expect_error(.truncate_components_one_draw(w, params, epsilon = 0.1))
})

test_that(".truncate_components_one_draw adjusts weights to sum to 1", {
  w <- c(0.6, 0.3, 0.1)
  params <- list(mu = c(1, 2, 3))
  result <- .truncate_components_one_draw(w, params, epsilon = 0.05)

  expect_equal(sum(result$weights), 1, tolerance = 1e-10)
})

# ======================================================================
# Additional internal utility helpers
# ======================================================================

# Access more internal helpers
.extract_nimble_code <- CausalMixGPD:::.extract_nimble_code
.wrap_nimble_code <- CausalMixGPD:::.wrap_nimble_code
.plot_palette <- CausalMixGPD:::.plot_palette
.backend_label <- CausalMixGPD:::.backend_label
.kernel_label <- CausalMixGPD:::.kernel_label
.get_epsilon <- CausalMixGPD:::.get_epsilon
.get_nobs <- CausalMixGPD:::.get_nobs

# ======================================================================
# .extract_nimble_code tests
# ======================================================================

test_that(".extract_nimble_code extracts from list with nimble", {
  code_obj <- list(nimble = quote(y ~ dnorm(0, 1)))
  result <- .extract_nimble_code(code_obj)
  expect_equal(result, quote(y ~ dnorm(0, 1)))
})

test_that(".extract_nimble_code extracts from list with code", {
  code_obj <- list(code = quote(y ~ dnorm(0, 1)))
  result <- .extract_nimble_code(code_obj)
  expect_equal(result, quote(y ~ dnorm(0, 1)))
})

test_that(".extract_nimble_code returns nimbleCode as-is", {
  code <- quote(y ~ dnorm(0, 1))
  class(code) <- c("nimbleCode", class(code))
  result <- .extract_nimble_code(code)
  expect_equal(result, code)
})

test_that(".extract_nimble_code returns non-list as-is", {
  result <- .extract_nimble_code("some_code")
  expect_equal(result, "some_code")
})

# ======================================================================
# .wrap_nimble_code tests
# ======================================================================

test_that(".wrap_nimble_code wraps non-list code", {
  code <- quote(y ~ dnorm(0, 1))
  result <- .wrap_nimble_code(code)
  expect_true(is.list(result))
  expect_equal(result$nimble, code)
})

test_that(".wrap_nimble_code returns list as-is", {
  code_list <- list(nimble = quote(y ~ dnorm(0, 1)))
  result <- .wrap_nimble_code(code_list)
  expect_equal(result, code_list)
})

# ======================================================================
# .backend_label tests
# ======================================================================

test_that(".backend_label returns correct labels", {
  expect_equal(.backend_label("sb"), "Stick-Breaking Process")
  expect_equal(.backend_label("crp"), "Chinese Restaurant Process")
  expect_equal(.backend_label("unknown"), "unknown")
})

# ======================================================================
# .kernel_label tests
# ======================================================================

test_that(".kernel_label returns correct labels", {
  expect_equal(.kernel_label("normal"), "Normal Distribution")
  expect_equal(.kernel_label("gamma"), "Gamma Distribution")
  expect_equal(.kernel_label("lognormal"), "Lognormal Distribution")
  expect_equal(.kernel_label("laplace"), "Laplace Distribution")
  expect_equal(.kernel_label("invgauss"), "Inverse Gaussian Distribution")
  expect_equal(.kernel_label("amoroso"), "Amoroso Distribution")
  expect_equal(.kernel_label("cauchy"), "Cauchy Distribution")
  expect_equal(.kernel_label("unknown"), "unknown")
})

# ======================================================================
# .plot_palette tests
# ======================================================================

test_that(".plot_palette returns valid hex colors", {
  pal <- .plot_palette(4)
  expect_length(pal, 4)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
})

test_that(".plot_palette recycles for n > 8", {
  pal <- .plot_palette(10)
  expect_length(pal, 10)
})

test_that(".plot_palette handles NULL", {
  pal <- .plot_palette(NULL)
  expect_length(pal, 8)
})

# ======================================================================
# .get_epsilon tests
# ======================================================================

test_that(".get_epsilon returns provided epsilon", {
  obj <- list(spec = list(meta = list(epsilon = 0.05)))
  result <- .get_epsilon(obj, epsilon = 0.1)
  expect_equal(result, 0.1)
})

test_that(".get_epsilon extracts from spec$meta", {
  obj <- list(spec = list(meta = list(epsilon = 0.05)))
  result <- .get_epsilon(obj, epsilon = NULL)
  expect_equal(result, 0.05)
})

test_that(".get_epsilon extracts from object$epsilon", {
  obj <- list(epsilon = 0.03)
  result <- .get_epsilon(obj, epsilon = NULL)
  expect_equal(result, 0.03)
})

test_that(".get_epsilon returns default 0.025", {
  obj <- list()
  result <- .get_epsilon(obj, epsilon = NULL)
  expect_equal(result, 0.025)
})

# ======================================================================
# .get_nobs tests
# ======================================================================

test_that(".get_nobs extracts from data$y", {
  obj <- list(data = list(y = 1:100))
  result <- .get_nobs(obj)
  expect_equal(result, 100)
})

test_that(".get_nobs extracts from y directly", {
  obj <- list(y = 1:50)
  result <- .get_nobs(obj)
  expect_equal(result, 50)
})

test_that(".get_nobs returns NA when not found", {
  obj <- list()
  result <- .get_nobs(obj)
  expect_true(is.na(result))
})
