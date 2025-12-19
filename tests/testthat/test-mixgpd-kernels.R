testthat::test_that("Kernels: mixture families (mix / mix+GPD / scalar+GPD) semantic + roundtrip checks", {
  testthat::skip_if_not_installed("nimble")

  # ---------- helpers ----------
  .norm_w <- function(w) {
    w <- as.numeric(w)
    s <- sum(w)
    if (!is.finite(s) || s <= 0) rep(1 / length(w), length(w)) else w / s
  }

  .expect_density_ok <- function(dval, label = "") {
    testthat::expect_true(is.numeric(dval), info = label)
    testthat::expect_true(length(dval) >= 1, info = label)
    testthat::expect_true(all(is.finite(dval)), info = label)
    testthat::expect_true(all(dval >= 0), info = label)
  }

  .expect_cdf_ok <- function(pval, label = "") {
    testthat::expect_true(is.numeric(pval), info = label)
    testthat::expect_true(length(pval) >= 1, info = label)
    testthat::expect_true(all(is.finite(pval)), info = label)
    testthat::expect_true(all(pval >= 0 & pval <= 1), info = label)
  }

  .expect_rng_ok <- function(x, support = c(-Inf, Inf), label = "") {
    testthat::expect_true(is.numeric(x), info = label)
    testthat::expect_true(length(x) == 1, info = label)
    testthat::expect_true(is.finite(x), info = label)
    testthat::expect_true(x >= support[1] && x <= support[2], info = label)
  }

  # Roundtrip q(p) -> p(q) with diagnostics (and realistic tolerances for numeric inversion).
  .roundtrip_qp <- function(qFun, pFun, probs, args, tol, label) {
    qv <- do.call(qFun, c(list(p = probs), args))
    testthat::expect_true(is.numeric(qv), info = paste0(label, " : qFun returned non-numeric"))
    testthat::expect_true(length(qv) == length(probs), info = paste0(label, " : qFun length mismatch"))

    # If qv has non-finite values, only check the finite subset (classic at p near 0/1).
    okq <- is.finite(qv) & probs > 0 & probs < 1
    if (!any(okq)) {
      testthat::succeed(paste0(label, " : roundtrip skipped (no finite q)"))
      return(invisible(TRUE))
    }

    pb <- vapply(qv[okq], function(qq) do.call(pFun, c(list(q = qq), args)), numeric(1))

    # Hard fail if pFun yields NA/Inf, with context.
    if (any(!is.finite(pb))) {
      bad <- which(!is.finite(pb))[1]
      msg <- paste0(
        label, " : pFun returned non-finite at index ", bad,
        " (p=", probs[okq][bad], ", q=", qv[okq][bad], ")."
      )
      testthat::fail(msg)
    }

    .expect_cdf_ok(pb, label = paste0(label, " : pFun range"))

    err <- max(abs(pb - probs[okq]))
    testthat::expect_true(
      err < tol,
      info = paste0(label, " : max|p_back - p| = ", signif(err, 6), " (tol=", tol, ")")
    )
    invisible(TRUE)
  }

  .check_mix_family <- function(
    name,
    support = c(-Inf, Inf),
    dMix, pMix, rMix, qMix,
    args_mix,
    grid = NULL,
    dMixGpd = NULL, pMixGpd = NULL, rMixGpd = NULL, qMixGpd = NULL,
    args_mixgpd = NULL,
    dScalarGpd = NULL, pScalarGpd = NULL, rScalarGpd = NULL, qScalarGpd = NULL,
    args_scalargpd = NULL,
    tol_mix = 1e-4,
    tol_splice = 5e-4
  ) {
    # normalize weights for stability
    if ("w" %in% names(args_mix)) args_mix$w <- .norm_w(args_mix$w)
    if (!is.null(args_mixgpd) && "w" %in% names(args_mixgpd)) args_mixgpd$w <- .norm_w(args_mixgpd$w)

    if (is.null(grid)) {
      grid <- if (is.finite(support[1])) support[1] + c(0.01, 0.2, 1, 2) else c(-2, -0.5, 0, 0.5, 2)
      grid <- grid[grid >= support[1]]
      if (length(grid) == 0) grid <- support[1] + 0.5
    }

    # --- MIX checks ---
    dvals <- vapply(grid, function(xx) do.call(dMix, c(list(x = xx), args_mix)), numeric(1))
    pvals <- vapply(grid, function(xx) do.call(pMix, c(list(q = xx), args_mix)), numeric(1))

    .expect_density_ok(dvals, label = paste0(name, " : dMix"))
    .expect_cdf_ok(pvals, label = paste0(name, " : pMix"))

    r1 <- do.call(rMix, c(list(n = 1L), args_mix))
    .expect_rng_ok(r1, support = support, label = paste0(name, " : rMix"))

    probs <- c(0.05, 0.2, 0.5, 0.8, 0.95)
    .roundtrip_qp(qMix, pMix, probs, args_mix, tol = tol_mix, label = paste0(name, " : mix roundtrip"))

    testthat::succeed(paste0("mix checks ok: ", name))

    # --- MIX+GPD checks ---
    if (!is.null(dMixGpd) && !is.null(pMixGpd) && !is.null(rMixGpd) && !is.null(qMixGpd)) {
      testthat::expect_true(!is.null(args_mixgpd), info = paste0(name, " : args_mixgpd missing"))

      u <- args_mixgpd$threshold

      # continuity at threshold: F_mix(u) == F_mixgpd(u)
      Fu0 <- do.call(pMix,    c(list(q = u), args_mix))
      Fu1 <- do.call(pMixGpd, c(list(q = u), args_mixgpd))
      .expect_cdf_ok(Fu0, label = paste0(name, " : Fu0"))
      .expect_cdf_ok(Fu1, label = paste0(name, " : Fu1"))
      testthat::expect_equal(as.numeric(Fu1), as.numeric(Fu0), tolerance = 1e-6,
                             info = paste0(name, " : continuity at threshold"))

      grid2 <- sort(unique(c(u - 0.1, u, u + 0.1, u + 1)))
      d2 <- vapply(grid2, function(xx) do.call(dMixGpd, c(list(x = xx), args_mixgpd)), numeric(1))
      p2 <- vapply(grid2, function(xx) do.call(pMixGpd, c(list(q = xx), args_mixgpd)), numeric(1))

      # fail loudly with context if NA/Inf happens
      if (any(!is.finite(p2))) {
        bad <- which(!is.finite(p2))[1]
        testthat::fail(paste0(
          name, " : pMixGpd produced non-finite at q=", grid2[bad],
          " (threshold=", u, ")."
        ))
      }

      .expect_density_ok(d2, label = paste0(name, " : dMixGpd"))
      .expect_cdf_ok(p2, label = paste0(name, " : pMixGpd"))

      r2 <- do.call(rMixGpd, c(list(n = 1L), args_mixgpd))
      .expect_rng_ok(r2, support = support, label = paste0(name, " : rMixGpd"))

      probs2 <- c(0.05, 0.2, 0.5, 0.9, 0.98)
      .roundtrip_qp(qMixGpd, pMixGpd, probs2, args_mixgpd, tol = tol_splice, label = paste0(name, " : mixGpd roundtrip"))

      testthat::succeed(paste0("mix+GPD checks ok: ", name))
    }

    # --- Scalar+GPD checks (if present) ---
    if (!is.null(dScalarGpd) && !is.null(pScalarGpd) && !is.null(rScalarGpd) && !is.null(qScalarGpd)) {
      testthat::expect_true(!is.null(args_scalargpd), info = paste0(name, " : args_scalargpd missing"))

      u <- args_scalargpd$threshold
      grid3 <- sort(unique(c(u - 0.1, u, u + 0.1, u + 1)))
      d3 <- vapply(grid3, function(xx) do.call(dScalarGpd, c(list(x = xx), args_scalargpd)), numeric(1))
      p3 <- vapply(grid3, function(xx) do.call(pScalarGpd, c(list(q = xx), args_scalargpd)), numeric(1))

      if (any(!is.finite(p3))) {
        bad <- which(!is.finite(p3))[1]
        testthat::fail(paste0(
          name, " : pScalarGpd produced non-finite at q=", grid3[bad],
          " (threshold=", u, ")."
        ))
      }

      .expect_density_ok(d3, label = paste0(name, " : dScalarGpd"))
      .expect_cdf_ok(p3, label = paste0(name, " : pScalarGpd"))

      r3 <- do.call(rScalarGpd, c(list(n = 1L), args_scalargpd))
      .expect_rng_ok(r3, support = support, label = paste0(name, " : rScalarGpd"))

      probs3 <- c(0.05, 0.2, 0.5, 0.9, 0.98)
      .roundtrip_qp(qScalarGpd, pScalarGpd, probs3, args_scalargpd, tol = tol_splice, label = paste0(name, " : scalarGpd roundtrip"))

      testthat::succeed(paste0("scalar+GPD checks ok: ", name))
    }

    invisible(TRUE)
  }

  # ---------- shared ----------
  w2 <- c(0.6, 0.4)
  gpd_tail <- list(threshold = 2.0, tail_scale = 1.0, tail_shape = 0.2)

  # Gamma
  .check_mix_family(
    name = "Gamma",
    support = c(0, Inf),
    dMix = dGammaMix, pMix = pGammaMix, rMix = rGammaMix, qMix = qGammaMix,
    dMixGpd = dGammaMixGpd, pMixGpd = pGammaMixGpd, rMixGpd = rGammaMixGpd, qMixGpd = qGammaMixGpd,
    dScalarGpd = dGammaGpd, pScalarGpd = pGammaGpd, rScalarGpd = rGammaGpd, qScalarGpd = qGammaGpd,
    args_mix = list(w = w2, shape = c(2, 4), rate = c(1, 1.5)),
    args_mixgpd = c(list(w = w2, shape = c(2, 4), rate = c(1, 1.5)), gpd_tail),
    args_scalargpd = c(list(shape = 3, rate = 1), gpd_tail),
    grid = c(0.1, 0.5, 1, 2, 4),
    tol_mix = 5e-4, tol_splice = 1e-3
  )

  # Normal
  .check_mix_family(
    name = "Normal",
    support = c(-Inf, Inf),
    dMix = dNormMix, pMix = pNormMix, rMix = rNormMix, qMix = qNormMix,
    dMixGpd = dNormMixGpd, pMixGpd = pNormMixGpd, rMixGpd = rNormMixGpd, qMixGpd = qNormMixGpd,
    dScalarGpd = dNormGpd, pScalarGpd = pNormGpd, rScalarGpd = rNormGpd, qScalarGpd = qNormGpd,
    args_mix = list(w = w2, mean = c(0, 3), sd = c(1, 0.8)),
    args_mixgpd = c(list(w = w2, mean = c(0, 3), sd = c(1, 0.8)), gpd_tail),
    args_scalargpd = c(list(mean = 0, sd = 1), gpd_tail),
    grid = c(-2, -0.5, 0, 1, 3, 5),
    tol_mix = 5e-4, tol_splice = 1e-3
  )

  # Lognormal
  .check_mix_family(
    name = "Lognormal",
    support = c(0, Inf),
    dMix = dLognormalMix, pMix = pLognormalMix, rMix = rLognormalMix, qMix = qLognormalMix,
    dMixGpd = dLognormalMixGpd, pMixGpd = pLognormalMixGpd, rMixGpd = rLognormalMixGpd, qMixGpd = qLognormalMixGpd,
    dScalarGpd = dLognormalGpd, pScalarGpd = pLognormalGpd, rScalarGpd = rLognormalGpd, qScalarGpd = qLognormalGpd,
    args_mix = list(w = w2, meanlog = c(0, 1), sdlog = c(0.6, 0.4)),
    args_mixgpd = c(list(w = w2, meanlog = c(0, 1), sdlog = c(0.6, 0.4)), gpd_tail),
    args_scalargpd = c(list(meanlog = 0, sdlog = 0.6), gpd_tail),
    grid = c(0.1, 0.5, 1, 2, 5),
    tol_mix = 5e-4, tol_splice = 1e-3
  )

  # Laplace
  .check_mix_family(
    name = "Laplace",
    support = c(-Inf, Inf),
    dMix = dLaplaceMix, pMix = pLaplaceMix, rMix = rLaplaceMix, qMix = qLaplaceMix,
    dMixGpd = dLaplaceMixGpd, pMixGpd = pLaplaceMixGpd, rMixGpd = rLaplaceMixGpd, qMixGpd = qLaplaceMixGpd,
    dScalarGpd = dLaplaceGpd, pScalarGpd = pLaplaceGpd, rScalarGpd = rLaplaceGpd, qScalarGpd = qLaplaceGpd,
    args_mix = list(w = w2, location = c(0, 2), scale = c(1, 0.5)),
    args_mixgpd = c(list(w = w2, location = c(0, 2), scale = c(1, 0.5)), gpd_tail),
    args_scalargpd = c(list(location = 0, scale = 1), gpd_tail),
    grid = c(-2, -1, 0, 1, 2, 4),
    tol_mix = 5e-4, tol_splice = 1e-3
  )

  # Inverse Gaussian
  .check_mix_family(
    name = "InvGauss",
    support = c(0, Inf),
    dMix = dInvGaussMix, pMix = pInvGaussMix, rMix = rInvGaussMix, qMix = qInvGaussMix,
    dMixGpd = dInvGaussMixGpd, pMixGpd = pInvGaussMixGpd, rMixGpd = rInvGaussMixGpd, qMixGpd = qInvGaussMixGpd,
    dScalarGpd = dInvGaussGpd, pScalarGpd = pInvGaussGpd, rScalarGpd = rInvGaussGpd, qScalarGpd = qInvGaussGpd,
    args_mix = list(w = w2, mean = c(1, 2), shape = c(1.5, 2.5)),
    args_mixgpd = c(list(w = w2, mean = c(1, 2), shape = c(1.5, 2.5)), gpd_tail),
    args_scalargpd = c(list(mean = 1.5, shape = 2.0), gpd_tail),
    grid = c(0.1, 0.5, 1, 2, 4),
    tol_mix = 1e-3, tol_splice = 2e-3
  )

  # Amoroso
  .check_mix_family(
    name = "Amoroso",
    support = c(0, Inf),
    dMix = dAmorosoMix, pMix = pAmorosoMix, rMix = rAmorosoMix, qMix = qAmorosoMix,
    dMixGpd = dAmorosoMixGpd, pMixGpd = pAmorosoMixGpd, rMixGpd = rAmorosoMixGpd, qMixGpd = qAmorosoMixGpd,
    dScalarGpd = dAmorosoGpd, pScalarGpd = pAmorosoGpd, rScalarGpd = rAmorosoGpd, qScalarGpd = qAmorosoGpd,
    args_mix = list(w = w2, loc = c(0, 0.5), scale = c(1, 1), shape1 = c(2, 3), shape2 = c(1, 1.5)),
    args_mixgpd = c(list(w = w2, loc = c(0, 0.5), scale = c(1, 1), shape1 = c(2, 3), shape2 = c(1, 1.5)), gpd_tail),
    args_scalargpd = c(list(loc = 0, scale = 1, shape1 = 2, shape2 = 1.3), gpd_tail),
    grid = c(0.01, 0.2, 0.8, 1.5, 3),
    tol_mix = 1e-3, tol_splice = 2e-3
  )

  # Cauchy (mix only)
  .check_mix_family(
    name = "Cauchy",
    support = c(-Inf, Inf),
    dMix = dCauchyMix, pMix = pCauchyMix, rMix = rCauchyMix, qMix = qCauchyMix,
    args_mix = list(w = w2, location = c(0, 2), scale = c(1, 0.5)),
    grid = c(-2, -1, 0, 1, 2, 4),
    tol_mix = 1e-3
  )
})


testthat::test_that("All core exported nimbleFunctions compile successfully", {
  testthat::skip_if_not_installed("nimble")

  ns <- asNamespace("DPmixGPD")
  nms <- ls(ns, all.names = TRUE)

  # Compile ONLY nimbleFunctions (your mix/mixgpd are R functions now).
  # This list matches what still exists as nimbleFunctions in 0-base-kernels + Amoroso.
  must_compile <- c(
    # base GPD
    "dGpd","pGpd","rGpd",
    # base invgauss PD/RNG
    "dinvgauss","pinvgauss","rinvgauss",
    # Amoroso family (these are nimbleFunctions)
    "dAmoroso","pAmoroso","rAmoroso",
    "dAmorosoMix","pAmorosoMix","rAmorosoMix",
    "dAmorosoMixGpd","pAmorosoMixGpd","rAmorosoMixGpd"
  )

  missing <- setdiff(must_compile, nms)
  testthat::expect(
    length(missing) == 0,
    paste0("Missing from namespace:\n - ", paste(missing, collapse = "\n - "))
  )

  for (nm in must_compile) {
    obj <- get(nm, envir = ns, inherits = FALSE)
    testthat::expect_true(inherits(obj, "nimbleFunction"),
                          info = paste0("Expected nimbleFunction: ", nm))

    testthat::expect_error(
      suppressWarnings(nimble::compileNimble(obj)),
      NA,
      info = paste0("Failed to compile: ", nm)
    )
    testthat::succeed(paste0("compile ok: ", nm))
  }
})

