# =========================
# 03-run-mcmc.R
# =========================

#' Run MCMC for a prepared bundle (manual runner; internal)
#'
#' @param bundle A \code{dpmixgpd_bundle} from \code{build_nimble_bundle()}.
#' @param show_progress Logical; passed to nimble.
#' @param compile Logical; whether to compile model and MCMC.
#' @return A list with samples and fitted objects.
#' @keywords internal
#' @noRd
run_mcmc_bundle_manual <- function(bundle, show_progress = TRUE, compile = TRUE) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  stopifnot(inherits(bundle, "dpmixgpd_bundle"))

  spec <- bundle$spec
  meta <- spec$meta
  m <- bundle$mcmc %||% list()

  code <- bundle$code
  constants <- bundle$constants %||% list()
  data <- bundle$data %||% list()
  inits_fun <- bundle$inits %||% function() list()
  dims <- bundle$dimensions %||% list()
  monitors <- bundle$monitors %||% character(0)

  # Build model
  Rmodel <- nimble::nimbleModel(
    code = code,
    data = data,
    constants = constants,
    inits = inits_fun(),
    dimensions = dims,
    check = TRUE,
    calculate = FALSE
  )

  # Configure MCMC
  conf <- nimble::configureMCMC(
    Rmodel,
    monitors = monitors,
    enableWAIC = FALSE
  )

  # ---- FIX: CRP sampler should cluster *stochastic* nodes only ----
  if (identical(meta$backend, "crp")) {
    # Remove any default sampler Nimble set for z and replace with CRP sampler
    if ("z" %in% conf$getSamplers()) {
      conf$removeSamplers("z")
    }
    stoch_nodes <- Rmodel$getNodeNames(stochOnly = TRUE, includeData = FALSE)
    cluster_nodes <- setdiff(stoch_nodes, "z")
    conf$addSampler(
      target = "z",
      type = "CRP",
      control = list(clusterNodes = cluster_nodes)
    )
  }

  # Build MCMC object
  Rmcmc <- nimble::buildMCMC(conf)

  # Compile if requested
  if (isTRUE(compile)) {
    Cmodel <- nimble::compileNimble(Rmodel, showCompilerOutput = FALSE)
    Cmcmc <- nimble::compileNimble(Rmcmc, project = Rmodel, showCompilerOutput = FALSE)
  } else {
    Cmodel <- NULL
    Cmcmc <- NULL
  }

  niter <- as.integer(m$niter %||% 2000)
  nburnin <- as.integer(m$nburnin %||% 500)
  thin <- as.integer(m$thin %||% 1)
  nchains <- as.integer(m$nchains %||% 1)

  # ---- FIX: setSeed must have length == nchains (nimble requirement) ----
  seed <- m$seed %||% NULL
  if (!is.null(seed)) {
    if (length(seed) == 1L && nchains > 1L) seed <- rep(as.integer(seed), nchains)
    if (length(seed) != nchains) {
      stop("mcmc$seed must be length 1 or length nchains.", call. = FALSE)
    }
  }

  samples <- nimble::runMCMC(
    if (isTRUE(compile)) Cmcmc else Rmcmc,
    niter = niter,
    nburnin = nburnin,
    thin = thin,
    nchains = nchains,
    setSeed = seed,
    progressBar = isTRUE(show_progress),
    samplesAsCodaMCMC = TRUE
  )

  list(
    spec = spec,
    model = if (isTRUE(compile)) Cmodel else Rmodel,
    mcmc_conf = conf,
    mcmc = if (isTRUE(compile)) Cmcmc else Rmcmc,
    samples = samples
  )
}

#' Build a prior table from a spec (internal)
#'
#' @param spec A compiled spec.
#' @return data.frame describing parameter support and prior.
#' @keywords internal
#' @noRd
#' @export
.dpmixgpd_build_prior_table_from_spec <- function(spec) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  kdef <- get_kernel_registry()[[spec$meta$kernel]]
  bulk_params <- kdef$bulk_params

  rows <- list()

  # bulk
  for (p in bulk_params) {
    d <- spec$node_plan$bulk[[p]]
    if (is.null(d)) next
    if (identical(d$type, "fixed")) {
      pr <- paste0("fixed = ", d$fixed_value %||% "<fixed>")
    } else {
      fam <- tolower(d$prior_family %||% "normal")
      if (fam == "normal") pr <- sprintf("normal(mean=%s, sd=%s)", d$mean %||% "normal_mean", d$sd %||% "normal_sd")
      if (fam == "gamma")  pr <- sprintf("gamma(shape=%s, rate=%s)", d$shape %||% "gamma_shape", d$rate %||% "gamma_rate")
      if (fam == "invgamma") pr <- sprintf("invgamma(shape=%s, rate=%s)", d$shape %||% "gamma_shape", d$rate %||% "gamma_rate")
    }

    support <- d$support %||% if (p %in% c("sd","scale","shape1","shape2")) "positive" else "real"
    rows[[length(rows) + 1]] <- data.frame(
      parameter = p,
      support = support,
      prior = pr,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

#' Print a DPmixGPD fit object
#'
#' Prints model description and prior table; does not print posterior samples.
#'
#' @param x A \code{dpmixgpd_fit}.
#' @param ... unused.
#' @export
print.dpmixgpd_fit <- function(x, ...) {
  spec <- x$spec
  meta <- spec$meta

  backend_full <- if (meta$backend == "sb") "Stick-breaking" else "Chinese Restaurant Process"
  lik <- paste0("DP with ", toupper(meta$kernel), " mixture", if (isTRUE(meta$GPD)) " and GPD tail" else "")
  cat("Model Description\n")
  cat("  Likelihood: ", lik, "\n", sep = "")
  cat("  Backend:    ", backend_full, "\n", sep = "")
  cat("  N:          ", meta$N, "\n", sep = "")
  cat("  X:          ", if (isTRUE(meta$has_X)) sprintf("%d covariates", meta$p) else "None", "\n", sep = "")
  if (meta$backend == "sb") cat("  Components: ", meta$Kmax, "\n", sep = "")

  cat("\nParameter Priors\n")
  pt <- .dpmixgpd_build_prior_table_from_spec(spec)
  # print as simple table
  print(pt, row.names = FALSE)

  invisible(x)
}

#' Summary of DPmixGPD fit
#'
#' Prints model description, parameter summary (from MCMC samples), and WAIC if available.
#'
#' @param object A \code{dpmixgpd_fit}.
#' @param ... unused.
#' @export
summary.dpmixgpd_fit <- function(object, ...) {
  out <- list(
    print = object,
    posterior = NULL,
    waic = object$mcmc$waic
  )

  # basic posterior summaries via coda if available
  sm <- NULL
  if (inherits(object$mcmc$samples, "mcmc") || inherits(object$mcmc$samples, "mcmc.list")) {
    sm <- tryCatch(coda::summary(object$mcmc$samples), error = function(e) NULL)
  }
  out$posterior <- sm

  class(out) <- "summary.dpmixgpd_fit"
  out
}

#' Print summary.dpmixgpd_fit
#'
#' @param x Summary object.
#' @param ... unused.
#' @export
print.summary.dpmixgpd_fit <- function(x, ...) {
  print(x$print)

  if (!is.null(x$posterior)) {
    cat("\nPosterior Summary (coda)\n")
    # print only statistics matrix
    print(x$posterior$statistics)
  }

  if (!is.null(x$waic)) {
    cat("\nWAIC\n")
    print(x$waic)
  }

  invisible(x)
}

#' Plot method for DPmixGPD fit (ggmcmc diagnostics)
#'
#' @param x A \code{dpmixgpd_fit}.
#' @param params Optional character vector of parameters to plot (default: all).
#' @param family Optional subset for plot families: any of
#'   c("trace","density","running","autocorr","rhat","ess")
#' @param ... passed to ggmcmc geoms when applicable
#' @export
plot.dpmixgpd_fit <- function(x, params = NULL,
                              family = c("trace","density","running","autocorr","rhat","ess"),
                              ...) {
  if (!requireNamespace("ggmcmc", quietly = TRUE)) {
    stop("Package 'ggmcmc' is required for plot.dpmixgpd_fit().", call. = FALSE)
  }
  if (!requireNamespace("coda", quietly = TRUE)) {
    stop("Package 'coda' is required for plot.dpmixgpd_fit().", call. = FALSE)
  }

  s <- x$mcmc$samples
  if (is.null(s)) stop("fit$mcmc$samples is missing.", call. = FALSE)

  # Coerce to coda::mcmc.list
  if (is.matrix(s)) {
    s <- coda::as.mcmc(s)
  }
  if (inherits(s, "mcmc")) {
    s <- coda::mcmc.list(s)
  }
  if (!inherits(s, "mcmc.list")) {
    stop("fit$mcmc$samples must be matrix, mcmc, or mcmc.list.", call. = FALSE)
  }

  n_chains <- length(s)

  # Convert to ggmcmc long format
  g <- ggmcmc::ggs(s)

  # Optional parameter filtering
  if (!is.null(params)) {
    g <- g[g$Parameter %in% params, , drop = FALSE]
    if (!nrow(g)) stop("No matching parameters found in samples for 'params'.", call. = FALSE)
  }

  family <- match.arg(family, several.ok = TRUE)

  # Build plots
  plots <- list()

  if ("trace" %in% family) {
    plots$trace <- ggmcmc::ggs_traceplot(g, ...)
    print(plots$trace)
  }

  if ("density" %in% family) {
    plots$density <- ggmcmc::ggs_density(g, ...)
    print(plots$density)
  }

  if ("running" %in% family) {
    plots$running <- ggmcmc::ggs_running(g, ...)
    print(plots$running)
  }

  if ("autocorr" %in% family) {
    plots$autocorr <- ggmcmc::ggs_autocorrelation(g, ...)
    print(plots$autocorr)
  }

  # Multi-chain only diagnostics
  if (n_chains > 1 && "rhat" %in% family) {
    plots$rhat <- ggmcmc::ggs_Rhat(s, ...)
    print(plots$rhat)
  }

  if (n_chains > 1 && "ess" %in% family) {
    plots$ess <- ggmcmc::ggs_effective(s, ...)
    print(plots$ess)
  }

  # Return plots invisibly so you can save them if you want
  invisible(plots)
}

