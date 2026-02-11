# testthat skeleton for a kernel (developer template)

test_that("<NAME> kernel: CDF monotone and quantile inversion", {
  skip_on_cran()

  u <- seq(0.05, 0.95, by = 0.05)

  # Example: check inversion p(q(u)) ≈ u
  # qq <- q<NAME>(u, <params>)
  # uu <- p<NAME>(qq, <params>)
  # expect_equal(uu, u, tolerance = 1e-3)
})
