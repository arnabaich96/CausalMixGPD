test_that("scalar vs vector dispatch separation is enforced", {

  spec <- list(
    meta = list(backend = "sb", kernel = "gamma", GPD = FALSE),
    dispatch = list(backend = "sb", GPD = FALSE),
    kernel = list(key = "gamma")
  )

  scalar <- DPmixGPD:::.get_dispatch_scalar(spec)
  expect_false(isTRUE(attr(scalar$d, "vectorized_wrapper")))
  expect_false(isTRUE(attr(scalar$p, "vectorized_wrapper")))
  expect_false(isTRUE(attr(scalar$q, "vectorized_wrapper")))
  expect_false(isTRUE(attr(scalar$r, "vectorized_wrapper")))

  vec <- DPmixGPD:::.get_dispatch(spec)
  expect_true(isTRUE(attr(vec$d, "vectorized_wrapper")))
  expect_true(isTRUE(attr(vec$p, "vectorized_wrapper")))
  expect_true(isTRUE(attr(vec$q, "vectorized_wrapper")))
  expect_true(isTRUE(attr(vec$r, "vectorized_wrapper")))
})
