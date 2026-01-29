# test-kernels.R
# Kernel distribution tests (Tier A - pure math, no MCMC)
# These are fast unit tests that verify d/p/q/r functions work correctly

test_that(
  "Kernels: mixture families (mix / mix+GPD / scalar+GPD) semantic + roundtrip checks",
  {
    # This test runs kernel math checks - fast, no MCMC compilation

    # ---------- helpers ----------
    .norm_w <- function(w) {
      w <- as.numeric(w)
      s <- sum(w)
      if (!is.finite(s) || s <= 0) rep(1 / length(w), length(w)) else w / s
    }

    .expect_density_ok <- function(dval, label = "") {
      expect_true(is.numeric(dval), info = label)
      expect_true(length(dval) >= 1, info = label)
      expect_true(all(is.finite(dval)), info = label)
      expect_true(all(dval >= 0), info = label)
    }

    .expect_cdf_ok <- function(pval, label = "") {
      expect_true(is.numeric(pval), info = label)
      expect_true(length(pval) >= 1, info = label)
      expect_true(all(is.finite(pval)), info = label)
      expect_true(all(pval >= 0 & pval <= 1), info = label)
    }

    .expect_rng_ok <- function(x, support = c(-Inf, Inf), label = "") {
      expect_true(is.numeric(x), info = label)
      expect_true(length(x) == 1, info = label)
      expect_true(is.finite(x), info = label)
      expect_true(x >= support[1] && x <= support[2], info = label)
    }

    # Roundtrip q(p) -> p(q) with diagnostics (realistic tolerances for numeric inversion).
    .roundtrip_qp <- function(qFun, pFun, probs, args, tol, label) {
      qv <- do.call(qFun, c(list(p = probs), args))
      expect_true(is.numeric(qv), info = paste0(label, " : qFun returned non-numeric"))
      expect_true(length(qv) == length(probs), info = paste0(label, " : qFun length mismatch"))

      okq <- is.finite(qv) & probs > 0 & probs < 1
      if (!any(okq)) {
        succeed(paste0(label, " : roundtrip skipped (no finite q)"))
        return(invisible(TRUE))
      }

      # IMPORTANT: for nimbleFunctions, pass integer flags (1/0), not TRUE/FALSE.
      pb <- vapply(
        qv[okq],
        function(qq) do.call(pFun, c(list(q = qq), args, list(lower.tail = 1L, log.p = 0L))),
        numeric(1)
      )

      if (any(!is.finite(pb))) {
        bad <- which(!is.finite(pb))[1]
        msg <- paste0(
          label, " : pFun returned non-finite at index ", bad,
          " (p=", probs[okq][bad], ", q=", qv[okq][bad], ")."
        )
        fail(msg)
      }

      .expect_cdf_ok(pb, label = paste0(label, " : pFun range"))

      err <- max(abs(pb - probs[okq]))
      expect_true(
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
      dvals <- vapply(grid, function(xx) do.call(dMix, c(list(x = xx), args_mix, list(log = 0L))), numeric(1))
      pvals <- vapply(grid, function(xx) do.call(pMix, c(list(q = xx), args_mix, list(lower.tail = 1L, log.p = 0L))), numeric(1))

      .expect_density_ok(dvals, label = paste0(name, " : dMix"))
      .expect_cdf_ok(pvals, label = paste0(name, " : pMix"))

      r1 <- do.call(rMix, c(list(n = 1L), args_mix))
      .expect_rng_ok(r1, support = support, label = paste0(name, " : rMix"))

      probs <- c(0.05, 0.2, 0.5, 0.8, 0.95)
      .roundtrip_qp(qMix, pMix, probs, args_mix, tol = tol_mix, label = paste0(name, " : mix roundtrip"))

      succeed(paste0("mix checks ok: ", name))

      # --- MIX+GPD checks ---
      if (!is.null(dMixGpd) && !is.null(pMixGpd) && !is.null(rMixGpd) && !is.null(qMixGpd)) {
        expect_true(!is.null(args_mixgpd), info = paste0(name, " : args_mixgpd missing"))

        u <- args_mixgpd$threshold

        # continuity at threshold: F_mix(u) == F_mixgpd(u)
        Fu0 <- do.call(pMix,    c(list(q = u), args_mix,    list(lower.tail = 1L, log.p = 0L)))
        Fu1 <- do.call(pMixGpd, c(list(q = u), args_mixgpd, list(lower.tail = 1L, log.p = 0L)))

        .expect_cdf_ok(Fu0, label = paste0(name, " : Fu0"))
        .expect_cdf_ok(Fu1, label = paste0(name, " : Fu1"))

        expect_equal(
          as.numeric(Fu1), as.numeric(Fu0),
          tolerance = 1e-6,
          info = paste0(name, " : continuity at threshold")
        )

        grid2 <- sort(unique(c(u - 0.1, u, u + 0.1, u + 1)))
        d2 <- vapply(grid2, function(xx) do.call(dMixGpd, c(list(x = xx), args_mixgpd, list(log = 0L))), numeric(1))
        p2 <- vapply(grid2, function(xx) do.call(pMixGpd, c(list(q = xx), args_mixgpd, list(lower.tail = 1L, log.p = 0L))), numeric(1))

        if (any(!is.finite(p2))) {
          bad <- which(!is.finite(p2))[1]
          fail(paste0(
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

        succeed(paste0("mix+GPD checks ok: ", name))
      }

      # --- Scalar+GPD checks (if present) ---
      if (!is.null(dScalarGpd) && !is.null(pScalarGpd) && !is.null(rScalarGpd) && !is.null(qScalarGpd)) {
        expect_true(!is.null(args_scalargpd), info = paste0(name, " : args_scalargpd missing"))

        u <- args_scalargpd$threshold
        grid3 <- sort(unique(c(u - 0.1, u, u + 0.1, u + 1)))

        d3 <- vapply(grid3, function(xx) do.call(dScalarGpd, c(list(x = xx), args_scalargpd, list(log = 0L))), numeric(1))
        p3 <- vapply(grid3, function(xx) do.call(pScalarGpd, c(list(q = xx), args_scalargpd, list(lower.tail = 1L, log.p = 0L))), numeric(1))

        if (any(!is.finite(p3))) {
          bad <- which(!is.finite(p3))[1]
          fail(paste0(
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

        succeed(paste0("scalar+GPD checks ok: ", name))
      }

      invisible(TRUE)
    }

    # ---------- shared ----------
    # 3-component mixtures everywhere (per your rule)
    w3 <- c(0.50, 0.30, 0.20)

    gpd_tail <- list(
      threshold  = 2.0,
      tail_scale = 1.0,
      tail_shape = 0.2
    )

    # Gamma
    .check_mix_family(
      name = "Gamma",
      support = c(0, Inf),
      dMix = dGammaMix, pMix = pGammaMix, rMix = rGammaMix, qMix = qGammaMix,
      dMixGpd = dGammaMixGpd, pMixGpd = pGammaMixGpd, rMixGpd = rGammaMixGpd, qMixGpd = qGammaMixGpd,
      dScalarGpd = dGammaGpd, pScalarGpd = pGammaGpd, rScalarGpd = rGammaGpd, qScalarGpd = qGammaGpd,
      args_mix = list(w = w3, shape = c(2, 4, 6), scale = c(1, 1.5, 2.0)),
      args_mixgpd = c(list(w = w3, shape = c(2, 4, 6), scale = c(1, 1.5, 2.0)), gpd_tail),
      args_scalargpd = c(list(shape = 3, scale = 1), gpd_tail),
      grid = c(0.1, 0.5, 1, 2, 4, 6),
      tol_mix = 5e-4, tol_splice = 1e-3
    )

    # Normal
    .check_mix_family(
      name = "Normal",
      support = c(-Inf, Inf),
      dMix = dNormMix, pMix = pNormMix, rMix = rNormMix, qMix = qNormMix,
      dMixGpd = dNormMixGpd, pMixGpd = pNormMixGpd, rMixGpd = rNormMixGpd, qMixGpd = qNormMixGpd,
      dScalarGpd = dNormGpd, pScalarGpd = pNormGpd, rScalarGpd = rNormGpd, qScalarGpd = qNormGpd,
      args_mix = list(w = w3, mean = c(-1, 1, 3), sd = c(1, 0.8, 0.6)),
      args_mixgpd = c(list(w = w3, mean = c(-1, 1, 3), sd = c(1, 0.8, 0.6)), gpd_tail),
      args_scalargpd = c(list(mean = 0, sd = 1), gpd_tail),
      grid = c(-3, -1, 0, 1, 3, 5),
      tol_mix = 5e-4, tol_splice = 1e-3
    )

    # Lognormal
    .check_mix_family(
      name = "Lognormal",
      support = c(0, Inf),
      dMix = dLognormalMix, pMix = pLognormalMix, rMix = rLognormalMix, qMix = qLognormalMix,
      dMixGpd = dLognormalMixGpd, pMixGpd = pLognormalMixGpd, rMixGpd = rLognormalMixGpd, qMixGpd = qLognormalMixGpd,
      dScalarGpd = dLognormalGpd, pScalarGpd = pLognormalGpd, rScalarGpd = rLognormalGpd, qScalarGpd = qLognormalGpd,
      args_mix = list(w = w3, meanlog = c(-0.2, 0.6, 1.1), sdlog = c(0.7, 0.5, 0.4)),
      args_mixgpd = c(list(w = w3, meanlog = c(-0.2, 0.6, 1.1), sdlog = c(0.7, 0.5, 0.4)), gpd_tail),
      args_scalargpd = c(list(meanlog = 0, sdlog = 0.6), gpd_tail),
      grid = c(0.1, 0.5, 1, 2, 5, 10),
      tol_mix = 5e-4, tol_splice = 1e-3
    )

    # Laplace
    .check_mix_family(
      name = "Laplace",
      support = c(-Inf, Inf),
      dMix = dLaplaceMix, pMix = pLaplaceMix, rMix = rLaplaceMix, qMix = qLaplaceMix,
      dMixGpd = dLaplaceMixGpd, pMixGpd = pLaplaceMixGpd, rMixGpd = rLaplaceMixGpd, qMixGpd = qLaplaceMixGpd,
      dScalarGpd = dLaplaceGpd, pScalarGpd = pLaplaceGpd, rScalarGpd = rLaplaceGpd, qScalarGpd = qLaplaceGpd,
      args_mix = list(w = w3, location = c(-1, 1, 3), scale = c(1.2, 0.8, 0.6)),
      args_mixgpd = c(list(w = w3, location = c(-1, 1, 3), scale = c(1.2, 0.8, 0.6)), gpd_tail),
      args_scalargpd = c(list(location = 0, scale = 1), gpd_tail),
      grid = c(-3, -1, 0, 1, 3, 5),
      tol_mix = 5e-4, tol_splice = 1e-3
    )

    # Inverse Gaussian
    .check_mix_family(
      name = "InvGauss",
      support = c(0, Inf),
      dMix = dInvGaussMix, pMix = pInvGaussMix, rMix = rInvGaussMix, qMix = qInvGaussMix,
      dMixGpd = dInvGaussMixGpd, pMixGpd = pInvGaussMixGpd, rMixGpd = rInvGaussMixGpd, qMixGpd = qInvGaussMixGpd,
      dScalarGpd = dInvGaussGpd, pScalarGpd = pInvGaussGpd, rScalarGpd = rInvGaussGpd, qScalarGpd = qInvGaussGpd,
      args_mix = list(w = w3, mean = c(0.8, 1.6, 2.4), shape = c(1.5, 2.5, 3.5)),
      args_mixgpd = c(list(w = w3, mean = c(0.8, 1.6, 2.4), shape = c(1.5, 2.5, 3.5)), gpd_tail),
      args_scalargpd = c(list(mean = 1.5, shape = 2.0), gpd_tail),
      grid = c(0.1, 0.5, 1, 2, 4, 8),
      tol_mix = 1e-3, tol_splice = 2e-3
    )

    # Amoroso
    .check_mix_family(
      name = "Amoroso",
      support = c(0, Inf),
      dMix = dAmorosoMix, pMix = pAmorosoMix, rMix = rAmorosoMix, qMix = qAmorosoMix,
      dMixGpd = dAmorosoMixGpd, pMixGpd = pAmorosoMixGpd, rMixGpd = rAmorosoMixGpd, qMixGpd = qAmorosoMixGpd,
      dScalarGpd = dAmorosoGpd, pScalarGpd = pAmorosoGpd, rScalarGpd = rAmorosoGpd, qScalarGpd = qAmorosoGpd,
      args_mix = list(
        w = w3,
        loc = c(0, 0.3, 0.6),
        scale = c(1, 1, 1),
        shape1 = c(2, 3, 5),
        shape2 = c(1.0, 1.3, 1.6)
      ),
      args_mixgpd = c(list(
        w = w3,
        loc = c(0, 0.3, 0.6),
        scale = c(1, 1, 1),
        shape1 = c(2, 3, 5),
        shape2 = c(1.0, 1.3, 1.6)
      ), gpd_tail),
      args_scalargpd = c(list(loc = 0, scale = 1, shape1 = 2, shape2 = 1.3), gpd_tail),
      grid = c(0.01, 0.2, 0.8, 1.5, 3, 6),
      tol_mix = 1e-3, tol_splice = 2e-3
    )

    # Cauchy (mix only)
    .check_mix_family(
      name = "Cauchy",
      support = c(-Inf, Inf),
      dMix = dCauchyMix, pMix = pCauchyMix, rMix = rCauchyMix, qMix = qCauchyMix,
      args_mix = list(w = w3, location = c(-1, 1, 3), scale = c(1.2, 0.8, 0.6)),
      grid = c(-5, -2, -1, 0, 1, 3, 6),
      tol_mix = 1e-3
    )
  }
)
test_that("All nimble::nimbleFunction-defined objects in R/ compile", {
  # Keep this out of Tier A (cran) because it's expensive; allow it for CI + coverage.
  skip_if_not_test_level("ci")

  r_dir <- test_path("..", "..", "R")
  expect_true(dir.exists(r_dir), info = paste("Missing R dir:", r_dir))

  r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)
  expect_true(length(r_files) > 0, info = "No R files found under R/")

  # Find:  NAME <- nimble::nimbleFunction(   OR   NAME <- nimbleFunction(
  find_defs <- function(files) {
    rx <- "^\\s*([.A-Za-z][.A-Za-z0-9_]*)\\s*<-\\s*(nimble::)?nimbleFunction\\s*\\("
    out <- list()
    for (ff in files) {
      lines <- readLines(ff, warn = FALSE)
      hit <- grepl(rx, lines)
      if (any(hit)) {
        idx <- which(hit)
        nm  <- sub(rx, "\\1", lines[idx], perl = TRUE)
        out[[ff]] <- data.frame(
          name = nm,
          file = basename(ff),
          line = idx,
          stringsAsFactors = FALSE
        )
      }
    }
    if (!length(out)) {
      return(data.frame(name = character(), file = character(), line = integer()))
    }
    do.call(rbind, out)
  }

  defs <- find_defs(r_files)
  expect_true(nrow(defs) > 0, info = "No nimbleFunction definitions found in R/")

  # Use the package namespace so dependencies among nimbleFunctions are resolved.
  ns <- tryCatch(asNamespace("DPmixGPD"), error = function(e) NULL)
  if (is.null(ns)) {
    skip("DPmixGPD namespace not available for compile checks.")
  }

  # Pretty status symbols (colored if crayon is available)
  tick  <- if (requireNamespace("crayon", quietly = TRUE))
    crayon::green("\u2714") else "\u2714"
  cross <- if (requireNamespace("crayon", quietly = TRUE))
    crayon::red("\u2718") else "\u2718"

  failures <- character(0)
  results <- data.frame(
    status = character(),
    name   = character(),
    where  = character(),
    detail = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(defs))) {
    nm <- defs$name[i]
    where <- paste0(defs$file[i], ":", defs$line[i])

    if (!exists(nm, envir = ns, inherits = FALSE)) {
      msg <- paste0("object not found in package namespace")
      results <- rbind(results, data.frame(
        status = cross, name = nm, where = where, detail = msg,
        stringsAsFactors = FALSE
      ))
      failures <- c(failures, paste0("- ", nm, " @ ", where, " :: ", msg))
      next
    }

    obj <- get(nm, envir = ns, inherits = FALSE)

    # If a nimbleFunction was wrapped for vectorization, compile its *_nf alias.
    if (!inherits(obj, "nimbleFunction")) {
      nf_name <- paste0(nm, "_nf")
      if (exists(nf_name, envir = ns, inherits = FALSE)) {
        obj <- get(nf_name, envir = ns, inherits = FALSE)
      }
    }

    err <- tryCatch({
      suppressWarnings(nimble::compileNimble(obj))
      NULL
    }, error = function(e) e)

    if (is.null(err)) {
      # Per-object PASS entry in testthat output
      expect_true(TRUE, info = paste0("compileNimble succeeded for ", nm, " @ ", where))

      results <- rbind(results, data.frame(
        status = tick, name = nm, where = where, detail = "",
        stringsAsFactors = FALSE
      ))
    } else {
      e1 <- strsplit(conditionMessage(err), "\n", fixed = TRUE)[[1]][1]
      cls <- paste(class(obj), collapse = "/")
      msg <- paste0("class=", cls, " :: ", e1)

      results <- rbind(results, data.frame(
        status = cross, name = nm, where = where, detail = msg,
        stringsAsFactors = FALSE
      ))
      failures <- c(failures, paste0("- ", nm, " @ ", where, " :: ", msg))
    }
  }

  # Print a compact summary table
  cat("\nCompilation summary (nimbleFunction -> compileNimble):\n")
  print(results[, c("status", "name", "where")], row.names = FALSE)

  # If there are failures, print details and fail the test
  if (length(failures) > 0) {
    cat("\nFailure details:\n")
    cat(paste0(failures, collapse = "\n"), "\n")
    fail(paste(
      "The following nimbleFunction-defined objects failed to compile:",
      paste(failures, collapse = "\n"),
      sep = "\n"
    ))
  }
})




