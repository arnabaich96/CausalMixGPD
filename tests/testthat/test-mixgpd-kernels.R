testthat::test_that("Mixture kernels: semantic checks + roundtrip checks", {
  testthat::skip_if_not_installed("nimble")

  # ---------- helpers ----------
  .norm_w <- function(w) {
    w <- as.numeric(w)
    w / sum(w)
  }

  .expect_density_ok <- function(dval) {
    testthat::expect_true(is.numeric(dval))
    testthat::expect_true(length(dval) >= 1)
    testthat::expect_true(all(is.finite(dval)))
    testthat::expect_true(all(dval >= 0))
  }

  .expect_cdf_ok <- function(pval) {
    testthat::expect_true(is.numeric(pval))
    testthat::expect_true(length(pval) >= 1)
    testthat::expect_true(all(is.finite(pval)))
    testthat::expect_true(all(pval >= 0 & pval <= 1))
  }

  .expect_rng_ok <- function(x, support = c(-Inf, Inf)) {
    testthat::expect_true(is.numeric(x))
    testthat::expect_true(length(x) == 1)
    testthat::expect_true(is.finite(x))
    testthat::expect_true(x >= support[1] && x <= support[2])
  }

  # Add "empty defaults" only if a nimbleFunction signature includes them.
  .maybe_add_optional_args <- function(args, fn) {
    fmls <- tryCatch(formals(fn$run), error = function(e) NULL)
    if (is.null(fmls)) return(args)
    nms <- names(fmls)

    if ("rate"   %in% nms && !("rate"   %in% names(args))) args$rate   <- numeric(0)
    if ("var"    %in% nms && !("var"    %in% names(args))) args$var    <- numeric(0)
    if ("xm"     %in% nms && !("xm"     %in% names(args))) args$xm     <- numeric(0)
    if ("alpha"  %in% nms && !("alpha"  %in% names(args))) args$alpha  <- numeric(0)
    if ("param1" %in% nms && !("param1" %in% names(args))) args$param1 <- numeric(0)
    if ("param2" %in% nms && !("param2" %in% names(args))) args$param2 <- numeric(0)

    args
  }

  .roundtrip_qp <- function(qMix, pMix, probs, args_mix, tol = 1e-6) {
    q <- do.call(qMix, c(list(p = probs), args_mix))
    testthat::expect_true(is.numeric(q))
    testthat::expect_true(length(q) == length(probs))

    p_back <- vapply(q, function(qq) do.call(pMix, c(list(q = qq), args_mix)), numeric(1))
    .expect_cdf_ok(p_back)

    ok <- is.finite(q) & probs > 0 & probs < 1
    if (any(ok)) testthat::expect_true(max(abs(p_back[ok] - probs[ok])) < tol)
  }


  .check_mix_family <- function(
    name,
    support = c(-Inf, Inf),
    dMix, pMix, rMix, qMix,
    dMixGPD = NULL, pMixGPD = NULL, rMixGPD = NULL, qMixGPD = NULL,
    args_mix,
    args_gpd = NULL,
    grid = NULL
  ) {
    args_mix$w <- .norm_w(args_mix$w)

    args_mix <- .maybe_add_optional_args(args_mix, dMix)
    args_mix <- .maybe_add_optional_args(args_mix, pMix)
    args_mix <- .maybe_add_optional_args(args_mix, rMix)

    if (is.null(grid)) {
      grid <- if (is.finite(support[1])) support[1] + c(0.01, 0.2, 1, 2) else c(-1, 0, 0.5, 1, 2)
      grid <- grid[grid >= support[1]]
      if (length(grid) == 0) grid <- support[1] + 0.5
    }

    dvals <- vapply(grid, function(xx) do.call(dMix, c(list(x = xx), args_mix)), numeric(1))
    pvals <- vapply(grid, function(xx) do.call(pMix, c(list(q = xx), args_mix)), numeric(1))
    .expect_density_ok(dvals)
    .expect_cdf_ok(pvals)

    r1 <- do.call(rMix, c(list(n = 1L), args_mix))
    .expect_rng_ok(r1, support = support)

    probs <- c(0.05, 0.2, 0.5, 0.8, 0.95)
    .roundtrip_qp(qMix = qMix, pMix = pMix, probs = probs, args_mix = args_mix, tol = 1e-6)

    if (!is.null(dMixGPD) && !is.null(pMixGPD) && !is.null(rMixGPD) && !is.null(qMixGPD)) {
      testthat::expect_true(!is.null(args_gpd))
      args_gpd$w <- args_mix$w

      args_gpd <- .maybe_add_optional_args(args_gpd, dMixGPD)
      args_gpd <- .maybe_add_optional_args(args_gpd, pMixGPD)
      args_gpd <- .maybe_add_optional_args(args_gpd, rMixGPD)

      u <- args_gpd$u
      Fu0 <- do.call(pMix, c(list(q = u), args_mix))
      Fu1 <- do.call(pMixGPD, c(list(q = u), args_gpd))
      .expect_cdf_ok(Fu0); .expect_cdf_ok(Fu1)
      testthat::expect_equal(as.numeric(Fu1), as.numeric(Fu0), tolerance = 1e-6)

      grid2 <- sort(unique(c(u - 0.1, u, u + 0.1, u + 1)))
      dvals2 <- vapply(grid2, function(xx) do.call(dMixGPD, c(list(x = xx), args_gpd)), numeric(1))
      pvals2 <- vapply(grid2, function(xx) do.call(pMixGPD, c(list(q = xx), args_gpd)), numeric(1))
      .expect_density_ok(dvals2)
      .expect_cdf_ok(pvals2)

      r2 <- do.call(rMixGPD, c(list(n = 1L), args_gpd))
      .expect_rng_ok(r2, support = support)

      probs2 <- c(0.05, 0.2, 0.5, 0.9, 0.98)
      q2 <- do.call(qMixGPD, c(list(p = probs2), args_gpd))
      p2 <- vapply(q2, function(qq) do.call(pMixGPD, c(list(q = qq), args_gpd)), numeric(1))
      .expect_cdf_ok(p2)

      ok2 <- is.finite(q2) & probs2 > 0 & probs2 < 1
      if (any(ok2)) testthat::expect_true(max(abs(p2[ok2] - probs2[ok2])) < 5e-6)
    }

    testthat::succeed(paste("semantic checks ok:", name))
  }

  # ---------- shared ----------
  w2 <- c(0.6, 0.4)
  gpd_tail <- list(u = 2.0, sigma = 1.0, xi = 0.2)

  # Gamma
  .check_mix_family(
    name = "Gamma",
    support = c(0, Inf),
    dMix = dGammaMix, pMix = pGammaMix, rMix = rGammaMix, qMix = qGammaMix,
    dMixGPD = dGammaMixGPD, pMixGPD = pGammaMixGPD, rMixGPD = rGammaMixGPD, qMixGPD = qGammaMixGPD,
    args_mix = list(w = w2, shape = c(2, 4), scale = c(1, 0.7)),
    args_gpd = c(list(w = w2, shape = c(2, 4), scale = c(1, 0.7)), gpd_tail),
    grid = c(0.1, 0.5, 1, 2, 4)
  )

  # Normal
  .check_mix_family(
    name = "Normal",
    support = c(-Inf, Inf),
    dMix = dNormMix, pMix = pNormMix, rMix = rNormMix, qMix = qNormMix,
    dMixGPD = dNormMixGPD, pMixGPD = pNormMixGPD, rMixGPD = rNormMixGPD, qMixGPD = qNormMixGPD,
    args_mix = list(w = w2, mean = c(0, 3), sd = c(1, 0.8)),
    args_gpd = c(list(w = w2, mean = c(0, 3), sd = c(1, 0.8)), gpd_tail),
    grid = c(-2, -0.5, 0, 1, 3, 5)
  )

  # Lognormal
  .check_mix_family(
    name = "Lognormal",
    support = c(0, Inf),
    dMix = dLognormalMix, pMix = pLognormalMix, rMix = rLognormalMix, qMix = qLognormalMix,
    dMixGPD = dLognormalMixGPD, pMixGPD = pLognormalMixGPD, rMixGPD = rLognormalMixGPD, qMixGPD = qLognormalMixGPD,
    args_mix = list(w = w2, meanlog = c(0, 1), sdlog = c(0.6, 0.4)),
    args_gpd = c(list(w = w2, meanlog = c(0, 1), sdlog = c(0.6, 0.4)), gpd_tail),
    grid = c(0.1, 0.5, 1, 2, 5)
  )

  # Laplace
  .check_mix_family(
    name = "Laplace",
    support = c(-Inf, Inf),
    dMix = dLaplaceMix, pMix = pLaplaceMix, rMix = rLaplaceMix, qMix = qLaplaceMix,
    args_mix = list(w = w2, location = c(0, 2), scale = c(1, 0.5)),
    grid = c(-2, -1, 0, 1, 2, 4)
  )

  # Inverse Gaussian
  .check_mix_family(
    name = "InvGauss",
    support = c(0, Inf),
    dMix = dInvGaussMix, pMix = pInvGaussMix, rMix = rInvGaussMix, qMix = qInvGaussMix,
    args_mix = list(w = w2, mean = c(1, 2), shape = c(1.5, 2.5)),
    grid = c(0.1, 0.5, 1, 2, 4)
  )

  # Pareto (base + mix; no GPD splice in this file)
  .check_mix_family(
    name = "ParetoMix",
    support = c(0, Inf),
    dMix = dParetoMix, pMix = pParetoMix, rMix = rParetoMix, qMix = qParetoMix,
    args_mix = list(w = w2, scale = c(1, 2), shape = c(2, 3)),
    grid = c(1.01, 1.5, 2, 5, 10)
  )

  # Amoroso (mix + GPD splice)
  .check_mix_family(
    name = "AmorosoMix",
    support = c(0, Inf),
    dMix = dAmorosoMix, pMix = pAmorosoMix, rMix = rAmorosoMix, qMix = qAmorosoMix,
    dMixGPD = dAmorosoMixGPD, pMixGPD = pAmorosoMixGPD, rMixGPD = rAmorosoMixGPD, qMixGPD = qAmorosoMixGPD,
    args_mix = list(w = w2, loc = c(0, 0.5), scale = c(1, 1), shape1 = c(2, 3), shape2 = c(1, 1.5)),
    args_gpd = c(list(w = w2, loc = c(0, 0.5), scale = c(1, 1), shape1 = c(2, 3), shape2 = c(1, 1.5)), gpd_tail),
    grid = c(0.01, 0.2, 0.8, 1.5, 3)
  )

  # Cauchy (mix only; no GPD splice)
  .check_mix_family(
    name = "CauchyMix",
    support = c(-Inf, Inf),
    dMix = dCauchyMix, pMix = pCauchyMix, rMix = rCauchyMix, qMix = qCauchyMix,
    args_mix = list(w = w2, location = c(0, 2), scale = c(1, 0.5)),
    grid = c(-2, -1, 0, 1, 2, 4)
  )
})
testthat::test_that("All core exported nimbleFunctions compile successfully", {
  testthat::skip_if_not_installed("nimble")

  ns <- asNamespace("DPmixGPD")
  nms <- ls(ns, all.names = TRUE)

  must_compile <- c(
    "dGammaMix","pGammaMix","rGammaMix","dGammaMixGPD","pGammaMixGPD","rGammaMixGPD",
    "dNormMix","pNormMix","rNormMix","dNormMixGPD","pNormMixGPD","rNormMixGPD",
    "dLognormalMix","pLognormalMix","rLognormalMix","dLognormalMixGPD","pLognormalMixGPD","rLognormalMixGPD",
    "dInvGaussMix","pInvGaussMix","rInvGaussMix",
    "dLaplaceMix","pLaplaceMix","rLaplaceMix",
    "dParetoMix","pParetoMix","rParetoMix",
    "dAmoroso","pAmoroso","rAmoroso","dAmorosoMix","pAmorosoMix","rAmorosoMix",
    "dCauchyMix","pCauchyMix","rCauchyMix",
    "dPareto","pPareto","rPareto",
    "dinvgauss","pinvgauss","rinvgauss",
    "dGpd","pGpd","rGpd"
  )

  missing <- setdiff(must_compile, nms)
  testthat::expect(
    length(missing) == 0,
    paste0("Missing from namespace:\n - ", paste(missing, collapse = "\n - "))
  )

  for (nm in must_compile) {
    obj <- get(nm, envir = ns, inherits = FALSE)

    testthat::expect_error(
      suppressWarnings(nimble::compileNimble(obj)),
      NA,
      info = paste0("Failed to compile: ", nm)
    )
  }
})



