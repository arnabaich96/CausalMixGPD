test_that("predict.mixgpd_fit accepts legacy x alias", {
  predict_method <- getFromNamespace("predict.mixgpd_fit", "CausalMixGPD")
  fake_fit <- structure(list(), class = "mixgpd_fit")
  seen <- new.env(parent = emptyenv())

  local_mocked_bindings(
    .validate_fit = function(object) TRUE,
    .predict_mixgpd = function(object, x = NULL, y = NULL, ...) {
      seen$x <- x
      seen$y <- y
      list(fit = data.frame(estimate = 1), type = "mean")
    },
    .package = "CausalMixGPD"
  )

  x_arg <- matrix(1, nrow = 1L, ncol = 1L)
  predict_method(fake_fit, x = x_arg, type = "mean", show_progress = FALSE)

  expect_equal(seen$x, x_arg)
  expect_null(seen$y)
  expect_error(
    predict_method(fake_fit, newdata = x_arg, x = x_arg, type = "mean", show_progress = FALSE),
    "Provide only one of 'newdata' or legacy 'x'"
  )
})

test_that("residuals plugin PIT forwards training X as newdata", {
  residuals_method <- getFromNamespace("residuals.mixgpd_fit", "CausalMixGPD")
  fake_fit <- structure(
    list(
      data = list(
        X = matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE),
        y = c(0.2, 0.8)
      )
    ),
    class = "mixgpd_fit"
  )
  seen <- new.env(parent = emptyenv())

  local_mocked_bindings(
    predict.mixgpd_fit = function(object, newdata = NULL, y = NULL, type = c("density", "survival", "quantile", "sample", "mean", "rmean", "median", "fit"), ...) {
      seen$newdata <- newdata
      seen$y <- y
      list(fit = data.frame(survival = c(0.8, 0.4)))
    },
    .package = "CausalMixGPD"
  )

  pit <- residuals_method(fake_fit, type = "pit", pit = "plugin")

  expect_equal(as.numeric(pit), c(0.2, 0.6))
  expect_equal(seen$newdata, fake_fit$data$X)
  expect_equal(seen$y, fake_fit$data$y)
})
