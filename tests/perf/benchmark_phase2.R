#!/usr/bin/env Rscript

suppressWarnings({
  suppressPackageStartupMessages({
    library(CausalMixGPD)
  })
})

script_file <- tryCatch(normalizePath(commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))][1], winslash = "/", mustWork = FALSE), error = function(e) "")
if (!nzchar(script_file) || !file.exists(script_file)) {
  script_file <- normalizePath("tests/perf/benchmark_phase2.R", winslash = "/", mustWork = TRUE)
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
  val <- force(expr)
  list(seconds = proc.time()[["elapsed"]] - t0, value = val)
}

one_unconditional <- function() {
  set.seed(7001)
  y <- abs(stats::rnorm(220)) + 0.1
  out <- run_timer(dpmgpd(
    x = y, backend = "sb", kernel = "normal", components = 8,
    mcmc = list(niter = 260, nburnin = 80, thin = 2, nchains = 1, seed = 101, timing = TRUE)
  ))
  es <- ess_summary(out$value)
  pooled <- subset(es$table, chain == "pooled")
  list(
    sec = out$seconds,
    ess = pooled$ess[1] %||% NA_real_,
    eps = pooled$ess_per_sec[1] %||% NA_real_,
    params = paste(es$meta$params_used, collapse = ", ")
  )
}

one_conditional <- function() {
  set.seed(7002)
  n <- 260
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n))
  y <- abs(0.5 * X[, 1] + stats::rnorm(n)) + 0.1
  out <- run_timer(dpmgpd(
    x = y, X = X, backend = "sb", kernel = "normal", components = 8,
    mcmc = list(niter = 260, nburnin = 80, thin = 2, nchains = 1, seed = 102, timing = TRUE)
  ))
  es <- ess_summary(out$value)
  pooled <- subset(es$table, chain == "pooled")
  list(
    sec = out$seconds,
    ess = pooled$ess[1] %||% NA_real_,
    eps = pooled$ess_per_sec[1] %||% NA_real_,
    params = paste(es$meta$params_used, collapse = ", ")
  )
}

one_causal <- function() {
  set.seed(7003)
  n <- 260
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n))
  A <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.3 * X[, 1]))
  y <- abs(0.4 * X[, 1] + A + stats::rnorm(n)) + 0.1
  cb <- build_causal_bundle(
    y = y, X = X, A = A,
    backend = "sb", kernel = "normal", GPD = FALSE, components = 8,
    mcmc_outcome = list(niter = 220, nburnin = 60, thin = 2, nchains = 1, seed = 103),
    mcmc_ps = list(niter = 180, nburnin = 50, thin = 1, nchains = 1, seed = 104),
    PS = "logit"
  )
  out <- run_timer(run_mcmc_causal(cb, show_progress = FALSE, timing = TRUE))
  es <- ess_summary(out$value)
  pooled <- subset(es$table, chain == "pooled" & arm == "treated")
  list(
    sec = out$seconds,
    ess = pooled$ess[1] %||% NA_real_,
    eps = pooled$ess_per_sec[1] %||% NA_real_,
    params = paste(es$meta$params_used, collapse = ", ")
  )
}

one_prediction <- function() {
  set.seed(7004)
  n <- 320
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n))
  y <- abs(0.4 * X[, 1] + stats::rnorm(n)) + 0.1
  fit <- dpmgpd(
    x = y, X = X, backend = "sb", kernel = "normal", components = 8,
    mcmc = list(niter = 220, nburnin = 60, thin = 2, nchains = 1, seed = 105),
    monitor_latent = TRUE
  )
  X_new <- X[rep(seq_len(nrow(X)), length.out = 50000), , drop = FALSE]
  out <- run_timer(suppressMessages(
    predict(fit, x = X_new, type = "quantile", index = c(0.5, 0.9), parallel = TRUE, workers = 2)
  ))
  list(sec = out$seconds, ess = NA_real_, eps = NA_real_, params = "n/a")
}

`%||%` <- function(a, b) if (!is.null(a) && length(a)) a else b

rows <- list(
  "Unconditional (N small)" = one_unconditional(),
  "Conditional" = one_conditional(),
  "Causal" = one_causal(),
  "Prediction (large)" = one_prediction()
)

after <- data.frame(
  Task = names(rows),
  After = vapply(rows, function(z) z$sec, numeric(1)),
  ESS = vapply(rows, function(z) z$ess, numeric(1)),
  ESS_per_sec = vapply(rows, function(z) z$eps, numeric(1)),
  resolved_params = vapply(rows, function(z) z$params, character(1)),
  stringsAsFactors = FALSE
)

before_path <- Sys.getenv("DPMIXGPD_PHASE2_BEFORE", unset = "")
if (nzchar(before_path) && file.exists(before_path)) {
  before <- utils::read.csv(before_path, stringsAsFactors = FALSE)
  out <- merge(before, after, by = "Task", all.y = TRUE)
  if ("Before" %in% names(out)) {
    out$Speedup <- ifelse(is.finite(out$Before) & out$After > 0, out$Before / out$After, NA_real_)
  } else {
    out$Speedup <- NA_real_
  }
} else {
  out <- after
  out$Before <- NA_real_
  out$Speedup <- NA_real_
}

if (!dir.exists("tests/perf")) dir.create("tests/perf", recursive = TRUE)
utils::write.csv(out, "tests/perf/benchmark_phase2_results.csv", row.names = FALSE)

fmt <- function(x) ifelse(is.na(x), "", sprintf("%.3f", x))
md <- c(
  "# Phase 2 Benchmark Report",
  "",
  sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "| Task | Before | After | Speedup | ESS | ESS/sec | Resolved Params |",
  "| --- | ---: | ---: | ---: | ---: | ---: | --- |"
)
for (i in seq_len(nrow(out))) {
  md <- c(
    md,
    sprintf(
      "| %s | %s | %s | %s | %s | %s | %s |",
      out$Task[i], fmt(out$Before[i]), fmt(out$After[i]), fmt(out$Speedup[i]),
      fmt(out$ESS[i]), fmt(out$ESS_per_sec[i]), out$resolved_params[i]
    )
  )
}
writeLines(md, con = "tests/perf/benchmark_phase2_report.md")

cat("Wrote tests/perf/benchmark_phase2_results.csv\n")
cat("Wrote tests/perf/benchmark_phase2_report.md\n")
