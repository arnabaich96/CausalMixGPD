#!/usr/bin/env Rscript

suppressWarnings({
  suppressPackageStartupMessages({
    library(CausalMixGPD)
  })
})

is_covr_run <- isTRUE(getOption("covr", FALSE)) ||
  nzchar(Sys.getenv("DPMIXGPD_COVERAGE")) ||
  nzchar(Sys.getenv("COVERAGE")) ||
  nzchar(Sys.getenv("R_COVR")) ||
  any(grepl("covr", loadedNamespaces(), ignore.case = TRUE))

if (is_covr_run) {
  message("Skipping benchmark_acceptance.R under covr instrumentation.")
  quit(save = "no", status = 0L)
}

script_file <- tryCatch(normalizePath(commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))][1], winslash = "/", mustWork = FALSE), error = function(e) "")
if (!nzchar(script_file) || !file.exists(script_file)) {
  script_file <- normalizePath("tests/perf/benchmark_acceptance.R", winslash = "/", mustWork = TRUE)
}
script_dir <- dirname(script_file)
pkg_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = TRUE)
r_dir <- file.path(pkg_root, "R")
r_files <- list.files(r_dir, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
for (f in sort(r_files)) {
  txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
  if (length(txt) > 0L) txt[1] <- sub("^\\ufeff", "", txt[1])
  eval(parse(text = txt, keep.source = FALSE), envir = .GlobalEnv)
}
if (exists("init_kernel_registry", mode = "function", inherits = TRUE)) {
  try(init_kernel_registry(), silent = TRUE)
}

run_timer <- function(expr) {
  t0 <- proc.time()[["elapsed"]]
  force(expr)
  proc.time()[["elapsed"]] - t0
}

mk_unconditional <- function() {
  set.seed(2001)
  y <- abs(stats::rnorm(220)) + 0.1
  m <- list(niter = 220, nburnin = 60, thin = 2, nchains = 1, seed = 101, timing = TRUE)
  run_timer({
    dpmgpd(y = y, backend = "sb", kernel = "normal", components = 8, mcmc = m)
  })
}

mk_conditional <- function() {
  set.seed(2002)
  n <- 260
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n))
  y <- abs(0.5 * X[, 1] + stats::rnorm(n)) + 0.1
  m <- list(niter = 220, nburnin = 60, thin = 2, nchains = 1, seed = 102, timing = TRUE)
  run_timer({
    dpmgpd(y = y, X = X, backend = "sb", kernel = "normal", components = 8, mcmc = m)
  })
}

mk_causal <- function() {
  set.seed(2003)
  n <- 280
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n))
  A <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.3 * X[, 1]))
  y <- abs(0.7 * X[, 1] + A + stats::rnorm(n)) + 0.1
  cb <- build_causal_bundle(
    y = y, X = X, A = A,
    backend = "sb", kernel = "normal", GPD = FALSE, components = 8,
    mcmc_outcome = list(niter = 180, nburnin = 50, thin = 2, nchains = 1, seed = 103),
    mcmc_ps = list(niter = 140, nburnin = 40, thin = 1, nchains = 1, seed = 104)
  )
  run_timer({
    run_mcmc_causal(cb, show_progress = FALSE, parallel_arms = TRUE, workers = 2, timing = TRUE)
  })
}

mk_prediction_large <- function() {
  set.seed(2004)
  n <- 320
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n))
  y <- abs(0.5 * X[, 1] + stats::rnorm(n)) + 0.1
  fit <- dpmgpd(
    x = y, X = X, backend = "sb", kernel = "normal", components = 8,
    mcmc = list(niter = 180, nburnin = 50, thin = 2, nchains = 1, seed = 105),
    monitor_latent = TRUE
  )
  X_new <- X[rep(seq_len(nrow(X)), length.out = 10000), , drop = FALSE]
  run_timer({
    predict(
      fit,
      x = X_new,
      type = "quantile",
      index = c(0.5, 0.9),
      ndraws_pred = 120,
      chunk_size = 2000,
      parallel = TRUE,
      workers = 2
    )
  })
}

after <- data.frame(
  Task = c("Unconditional (N small)", "Conditional", "Causal", "Prediction (large)"),
  After = c(mk_unconditional(), mk_conditional(), mk_causal(), mk_prediction_large()),
  stringsAsFactors = FALSE
)

before_path <- Sys.getenv("DPMIXGPD_BENCH_BEFORE", unset = "")
if (nzchar(before_path) && file.exists(before_path)) {
  before <- utils::read.csv(before_path, stringsAsFactors = FALSE)
  out <- merge(before, after, by = "Task", all.y = TRUE)
  names(out)[names(out) == "Before"] <- "Before"
} else {
  out <- after
  out$Before <- NA_real_
}

out$Speedup <- ifelse(is.finite(out$Before) & out$After > 0, out$Before / out$After, NA_real_)
out <- out[, c("Task", "Before", "After", "Speedup")]

if (!dir.exists("tests/perf")) dir.create("tests/perf", recursive = TRUE)
utils::write.csv(out, "tests/perf/benchmark_acceptance.csv", row.names = FALSE)

fmt <- function(x) ifelse(is.na(x), "", sprintf("%.3f", x))
md <- c(
  "# Benchmark Acceptance Report",
  "",
  sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "| Task | Before | After | Speedup |",
  "| --- | ---: | ---: | ---: |"
)
for (i in seq_len(nrow(out))) {
  md <- c(md, sprintf("| %s | %s | %s | %s |",
                      out$Task[i], fmt(out$Before[i]), fmt(out$After[i]), fmt(out$Speedup[i])))
}
writeLines(md, con = "tests/perf/benchmark_acceptance_report.md")

cat("Wrote tests/perf/benchmark_acceptance.csv\n")
cat("Wrote tests/perf/benchmark_acceptance_report.md\n")
