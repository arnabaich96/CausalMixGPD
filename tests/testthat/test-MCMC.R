test_that("DPmixGPD: all model combinations build + run + S3 methods", {
  skip_if_not_installed("nimble")
  skip_if_not_installed("coda")

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # ---------- Fixed data (same across all tests) ----------
  set.seed(1)
  y_uncond <- abs(rnorm(80)) + 0.2

  set.seed(2)
  N <- 100
  X_full <- cbind(
    x1 = rnorm(N),
    x2 = rbinom(N, 1, 0.5),
    x3 = runif(N, -1, 1)
  )
  y_cond <- rexp(N) + 0.1

  # ---------- Grid knobs ----------
  Kmax_vec <- as.integer(c(4, 6, 8, 10, 12))
  backends <- c("sb", "crp")

  mcmc_cfg <- list(
    niter = 200,
    nburnin = 50,
    thin = 1,
    nchains = 1,
    seed = 1
  )

  # ---------- Registry ----------
  source(here::here("R/00-kernel-registry.R"))
  source(here::here("R/01-compile-spec.R"))
  source(here::here("R/02-build-bundle.R"))
  source(here::here("R/03-run-mcmc.R"))
  kreg <- get_kernel_registry()
  kernels <- names(kreg)

  # ---------- Conditional param_specs generator ----------
  make_param_specs_conditional <- function(kernel_key) {
    def <- kreg[[kernel_key]]
    bulk_params  <- def$bulk_params
    bulk_support <- def$bulk_support

    bulk <- list()
    for (nm in bulk_params) {
      sup <- bulk_support[[nm]]

      # requested special case:
      # exp link for location, square link for scale (if engine supports link specs)
      if (nm %in% c("loc", "location")) {
        bulk[[nm]] <- list(type = "link", link = list(name = "exp"))
      } else if (nm == "scale") {
        bulk[[nm]] <- list(type = "link", link = list(name = "power", k = 2))
      } else if (!is.null(sup) && sup %in% c("positive_scale", "positive_shape", "positive_mean", "positive_sd")) {
        bulk[[nm]] <- list(type = "link", link = list(name = "exp"))
      } else {
        bulk[[nm]] <- list(type = "link", link = list(name = "identity"))
      }
    }

    list(bulk = bulk)
  }

  # ---------- Run a single combo ----------
  quietly <- function(expr) {
    out <- character(0)
    msg <- character(0)

    val <- withCallingHandlers(
      {
        val2 <- utils::capture.output(
          {
            val1 <- suppressWarnings(
              suppressMessages(
                expr
              )
            )
            val1
          },
          type = "output"
        )
        out <<- c(out, val2)
        val1
      },
      message = function(m) {
        msg <<- c(msg, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )

    list(value = val, output = out, messages = msg)
  }

  run_combo <- function(kernel, backend, GPD, has_X, Kmax) {
    y <- if (has_X) y_cond else y_uncond
    X <- if (has_X) X_full else NULL
    ps <- if (has_X) make_param_specs_conditional(kernel) else NULL

    b <- quietly(
      DPmixGPD::build_nimble_bundle(
        y = y,
        X = X,
        backend = backend,
        kernel = kernel,
        GPD = isTRUE(GPD),
        Kmax = as.integer(Kmax),
        param_specs = ps,
        mcmc = mcmc_cfg
      )
    )$value

    f <- quietly(
      DPmixGPD:::run_mcmc_bundle_manual(b)
    )$value

    f
  }


  # ---------- S3 methods must not error & must "produce results" ----------
  check_s3 <- function(fit, test_name) {
    ok <- TRUE
    err <- NULL

    tryCatch({
      capture.output(print(fit))
    }, error = function(e) { ok <<- FALSE; err <<- paste0("print: ", conditionMessage(e)) })
    if (!ok) fail(paste0(test_name, " | ", err))

    s <- NULL
    ok <- TRUE; err <- NULL
    tryCatch({
      s <- summary(fit)
    }, error = function(e) { ok <<- FALSE; err <<- paste0("summary: ", conditionMessage(e)) })
    if (!ok) fail(paste0(test_name, " | ", err))
    if (is.null(s)) fail(paste0(test_name, " | summary returned NULL"))

    tmp_pdf <- tempfile(fileext = ".pdf")
    ok <- TRUE; err <- NULL
    tryCatch({
      grDevices::pdf(tmp_pdf)
      on.exit(grDevices::dev.off(), add = TRUE)
      plot(fit)
    }, error = function(e) { ok <<- FALSE; err <<- paste0("plot: ", conditionMessage(e)) })
    if (!ok) fail(paste0(test_name, " | ", err))

    invisible(TRUE)
  }


  # ---------- Summary collection ----------
  summary_env <- new.env(parent = emptyenv())
  summary_env$passed <- character(0)
  summary_env$failed <- character(0)

  on.exit({
    cat("\n\n========== DPmixGPD GRID SUMMARY ==========\n")
    cat("PASSED:", length(summary_env$passed), "\n")
    cat("FAILED:", length(summary_env$failed), "\n\n")
    if (length(summary_env$failed)) {
      cat("---- FAILURES ----\n")
      cat(paste0(summary_env$failed, collapse = "\n"), "\n")
    }
    cat("==========================================\n\n")
  }, add = TRUE)


  # ---------- Grid: each combination is its own named test inside one test_that ----------
  for (Kmax in Kmax_vec) {
    for (kernel in kernels) {
      allow_gpd <- isTRUE(kreg[[kernel]]$allow_gpd)
      gpd_grid <- if (allow_gpd) c(FALSE, TRUE) else c(FALSE)

      for (backend in backends) {
        for (GPD in gpd_grid) {
          for (has_X in c(FALSE, TRUE)) {

            test_name <- sprintf(
              "backend=%s kernel=%s GPD=%s X=%s Kmax=%d",
              backend, kernel, GPD, has_X, Kmax
            )

            ok <- TRUE
            err <- NULL
            fit <- NULL

            fit <- tryCatch(
              run_combo(kernel, backend, GPD, has_X, Kmax),
              error = function(e) {
                ok <<- FALSE
                err <<- conditionMessage(e)
                NULL
              }
            )

            if (!ok) {
              summary_env$failed <- c(summary_env$failed, paste0("FAIL: ", test_name, " | ", err))
              fail(paste0(test_name, " | ERROR: ", err))
            } else {
              # Must produce results: fit exists, samples exist
              expect_false(is.null(fit), info = paste0("fit is NULL: ", test_name))
              expect_true(!is.null(fit$mcmc$samples), info = paste0("missing mcmc samples: ", test_name))

              # Now S3 methods
              check_s3(fit, test_name)

              summary_env$passed <- c(summary_env$passed, paste0("PASS: ", test_name))
            }
          }
        }
      }
    }
  }
})
