test_that("cluster summaries and print output use decreasing allocation order", {
  lbl <- structure(
    list(
      labels = c("10", "2", "2", "3", "3", "x", "x"),
      components = 4L,
      source = "train",
      data = data.frame(y = c(7, 6, 5, 4, 3, 2, 1))
    ),
    class = c("dpmixgpd_cluster_labels", "list")
  )

  sm <- summary(lbl)
  expect_equal(names(sm$cluster_sizes), c("2", "3", "x", "10"))
  expect_equal(as.integer(sm$cluster_sizes), c(2L, 2L, 2L, 1L))
  expect_match(paste(capture.output(print(lbl)), collapse = "\n"), "sizes     : 2:2, 3:2, x:2, 10:1", fixed = TRUE)
})

test_that("predicted cluster labels carry training reference for downstream methods", {
  fit <- structure(
    list(
      spec = list(meta = list()),
      samples = "fake_samples",
      cache_env = new.env(parent = emptyenv())
    ),
    class = c("dpmixgpd_cluster_fit", "list")
  )

  testthat::local_mocked_bindings(
    .cluster_samples_to_matrix = function(samples) {
      matrix(
        c(1, 1, 2, 1, 2, 2),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(NULL, c("z[1]", "z[2]", "z[3]"))
      )
    },
    .cluster_draw_indices = function(n_draws, burnin = NULL, thin = NULL) seq_len(n_draws),
    .cluster_extract_z_from_matrix = function(draw_sub) {
      matrix(c(1L, 1L, 2L, 1L, 2L, 2L), nrow = 2, byrow = TRUE)
    },
    compute_psm = function(z_draws) diag(3),
    dahl_labels = function(z_draws, psm) list(labels = c(1L, 2L, 2L), K = 2L, draw_index = 1L),
    .cluster_compute_scores = function(z_draws, labels, psm) {
      matrix(c(0.9, 0.1, 0.2, 0.8, 0.1, 0.9), nrow = 3, byrow = TRUE)
    },
    .cluster_training_data_frame = function(fit) data.frame(y = c(10, 11, 12)),
    predict_labels_newdata = function(fit, newdata, burnin = NULL, thin = NULL) {
      list(
        labels = c(1L, 2L),
        scores = matrix(c(0.7, 0.3, 0.4, 0.6), nrow = 2, byrow = TRUE),
        data = data.frame(y = c(20, 21)),
        K = 2L
      )
    },
    .package = "CausalMixGPD"
  )

  nd_lbl <- predict(fit, newdata = data.frame(y = c(0.1, 0.2)), type = "label", return_scores = TRUE)

  expect_true(is.list(nd_lbl$train_reference))
  expect_equal(nd_lbl$train_reference$labels, c(1L, 2L, 2L))
  expect_equal(nd_lbl$train_reference$data$y, c(10, 11, 12))
  expect_s3_class(plot(nd_lbl, type = "summary", plotly = FALSE), "ggplot")
  expect_s3_class(plot(fit, which = "summary", plotly = FALSE), "ggplot")
})

test_that("cluster summary plots use boxplots ordered by selected clusters", {
  skip_if_not_installed("ggplot2")

  lbl <- structure(
    list(
      labels = c("2", "2", "3", "3", "3", "10"),
      components = 3L,
      source = "train",
      data = data.frame(y = c(1.0, 1.2, 2.0, 2.2, 2.4, 3.1))
    ),
    class = c("dpmixgpd_cluster_labels", "list")
  )

  p <- plot(lbl, type = "summary", top_n = 2L, order_by = "size", plotly = FALSE)
  geom_classes <- unique(vapply(p$layers, function(layer) class(layer$geom)[1], character(1)))
  built <- ggplot2::ggplot_build(p)
  fill_scale <- built$plot$scales$get_scales("fill")
  color_scale <- built$plot$scales$get_scales("colour")

  expect_equal(geom_classes, "GeomBoxplot")
  expect_equal(levels(p$data$cluster), c("3", "2"))
  expect_setequal(fill_scale$range$range, c("3", "2"))
  expect_setequal(color_scale$range$range, c("3", "2"))
})

test_that("newdata summary plots use newdata-only boxplots for populated clusters", {
  skip_if_not_installed("ggplot2")

  lbl <- structure(
    list(
      labels = c("2", "3", "3"),
      components = 3L,
      source = "newdata",
      data = data.frame(y = c(1.5, 2.1, 2.4)),
      train_reference = list(
        labels = c("2", "2", "3", "3", "10"),
        data = data.frame(y = c(1.0, 1.1, 2.0, 2.2, 3.3))
      )
    ),
    class = c("dpmixgpd_cluster_labels", "list")
  )

  p <- plot(lbl, type = "summary", top_n = 2L, order_by = "size", plotly = FALSE)
  geom_classes <- unique(vapply(p$layers, function(layer) class(layer$geom)[1], character(1)))
  built <- ggplot2::ggplot_build(p)
  fill_scale <- built$plot$scales$get_scales("fill")
  color_scale <- built$plot$scales$get_scales("colour")

  expect_equal(geom_classes, "GeomBoxplot")
  expect_equal(levels(p$data$cluster), c("3", "2"))
  expect_setequal(fill_scale$range$range, c("3", "2"))
  expect_setequal(color_scale$range$range, c("3", "2"))
})
