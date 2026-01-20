# Tests for kernel registry functions (00-kernel-registry.R)

test_that("init_kernel_registry() returns TRUE", {

  # First call initializes the registry

  result <- init_kernel_registry()
  expect_true(result)


  # Second call returns TRUE (already initialized)
  result2 <- init_kernel_registry()
  expect_true(result2)
})

test_that("get_kernel_registry() returns a complete registry", {
 registry <- get_kernel_registry()

  # Check it's a list
  expect_true(is.list(registry))

  # Check all 7 kernels are present
  expected_kernels <- c("normal", "lognormal", "invgauss", "gamma", "laplace", "amoroso", "cauchy")
  expect_true(all(expected_kernels %in% names(registry)))
  expect_equal(length(registry), 7L)
})

test_that("Each kernel has required fields", {
  registry <- get_kernel_registry()

  for (kernel_name in names(registry)) {
    kernel <- registry[[kernel_name]]

    # Check required fields exist
    expect_true("key" %in% names(kernel),
                info = paste0("kernel '", kernel_name, "' missing 'key'"))
    expect_true("bulk_params" %in% names(kernel),
                info = paste0("kernel '", kernel_name, "' missing 'bulk_params'"))
    expect_true("bulk_support" %in% names(kernel),
                info = paste0("kernel '", kernel_name, "' missing 'bulk_support'"))
    expect_true("param_types" %in% names(kernel),
                info = paste0("kernel '", kernel_name, "' missing 'param_types'"))
    expect_true("allow_gpd" %in% names(kernel),
                info = paste0("kernel '", kernel_name, "' missing 'allow_gpd'"))
    expect_true("defaults_X" %in% names(kernel),
                info = paste0("kernel '", kernel_name, "' missing 'defaults_X'"))
    expect_true("sb" %in% names(kernel),
                info = paste0("kernel '", kernel_name, "' missing 'sb'"))
    expect_true("crp" %in% names(kernel),
                info = paste0("kernel '", kernel_name, "' missing 'crp'"))
    expect_true("signatures" %in% names(kernel),
                info = paste0("kernel '", kernel_name, "' missing 'signatures'"))

    # Check key matches the kernel name
    expect_equal(kernel$key, kernel_name,
                 info = paste0("kernel '", kernel_name, "' key mismatch"))
  }
})

test_that("Kernel bulk_params are character vectors", {
  registry <- get_kernel_registry()

  for (kernel_name in names(registry)) {
    kernel <- registry[[kernel_name]]
    expect_true(is.character(kernel$bulk_params),
                info = paste0("kernel '", kernel_name, "' bulk_params not character"))
    expect_true(length(kernel$bulk_params) >= 2L,
                info = paste0("kernel '", kernel_name, "' should have at least 2 bulk params"))
  }
})

test_that("Kernel SB signatures are properly structured", {
  registry <- get_kernel_registry()

  for (kernel_name in names(registry)) {
    kernel <- registry[[kernel_name]]
    sb <- kernel$sb

    expect_true(is.list(sb),
                info = paste0("kernel '", kernel_name, "' sb not a list"))
    expect_true("d" %in% names(sb),
                info = paste0("kernel '", kernel_name, "' sb missing 'd'"))
    expect_true("args" %in% names(sb),
                info = paste0("kernel '", kernel_name, "' sb missing 'args'"))
  }
})

test_that("Kernel CRP signatures are properly structured", {
  registry <- get_kernel_registry()

  for (kernel_name in names(registry)) {
    kernel <- registry[[kernel_name]]
    crp <- kernel$crp

    expect_true(is.list(crp),
                info = paste0("kernel '", kernel_name, "' crp not a list"))
    expect_true("d_base" %in% names(crp),
                info = paste0("kernel '", kernel_name, "' crp missing 'd_base'"))
  }
})

test_that("get_tail_registry() returns tail metadata", {
  tail_reg <- get_tail_registry()

  # Check it's a list
 expect_true(is.list(tail_reg))

  # Check required fields
  expect_true("params" %in% names(tail_reg))
  expect_true("support" %in% names(tail_reg))
  expect_true("indexed_by_cluster_in_crp" %in% names(tail_reg))

  # Check params are the GPD tail parameters
  expect_equal(tail_reg$params, c("threshold", "tail_scale", "tail_shape"))

  # Check support types
  expect_true(is.character(tail_reg$support))
  expect_equal(names(tail_reg$support), c("threshold", "tail_scale", "tail_shape"))

  # Check indexed_by_cluster_in_crp is logical
  expect_true(is.logical(tail_reg$indexed_by_cluster_in_crp))
})

test_that("Cauchy kernel does not allow GPD", {
  registry <- get_kernel_registry()
  expect_false(registry$cauchy$allow_gpd)
})

test_that("Most kernels allow GPD", {
  registry <- get_kernel_registry()
  gpd_kernels <- c("normal", "lognormal", "invgauss", "gamma", "laplace", "amoroso")

  for (k in gpd_kernels) {
    expect_true(registry[[k]]$allow_gpd,
                info = paste0("kernel '", k, "' should allow GPD"))
  }
})
