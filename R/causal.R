#' Build a causal bundle (design + two outcome arms)
#'
#' \code{build_causal_bundle()} is the detailed constructor behind
#' \code{\link{bundle}} for causal analyses. It prepares:
#' \itemize{
#'   \item a propensity score (PS) design block for \eqn{A \mid X},
#'   \item a control-arm outcome bundle for \eqn{Y(0)},
#'   \item a treated-arm outcome bundle for \eqn{Y(1)}.
#' }
#'
#' The outcome bundles reuse the one-arm DPM plus optional GPD machinery. The
#' PS block provides a shared adjustment object used by
#' \code{\link{run_mcmc_causal}} and
#' \code{\link{predict.causalmixgpd_causal_fit}}.
#'
#' @details
#' The causal bundle encodes the two arm-specific predictive laws
#' \eqn{F_0(y \mid x)} and \eqn{F_1(y \mid x)}. Downstream causal estimands are
#' functionals of these two distributions:
#' \deqn{\mathrm{ATE} = E\{Y(1)\} - E\{Y(0)\}, \qquad
#' \mathrm{QTE}(\tau) = Q_1(\tau) - Q_0(\tau).}
#'
#' When \code{PS} is enabled, the package estimates a propensity score model
#' \eqn{e(x) = \Pr(A = 1 \mid X = x)} and uses a posterior summary of that score
#' as an augmented covariate in the arm-specific outcome models. This mirrors
#' the workflow described in the manuscript vignette.
#'
#' @param y Numeric outcome vector.
#' @param X Design matrix or data.frame of covariates (N x P).
#' @param A Binary treatment indicator (length N, values 0/1).
#' @param backend Character; \code{"sb"}, \code{"crp"}, or \code{"spliced"} for
#'   outcome models. If length 2,
#'   the first entry is used for treated (\code{A=1}) and the second for control (\code{A=0}).
#' @param kernel Character kernel name for outcome models. If length 2,
#'   the first entry is used for treated (\code{A=1}) and the second for control (\code{A=0}).
#' @param GPD Logical; include GPD tail for outcomes if TRUE. If length 2,
#'   the first entry is used for treated (\code{A=1}) and the second for control (\code{A=0}).
#' @param components Integer >= 2; truncation parameter for outcome mixtures. If length 2,
#'   the first entry is used for treated (\code{A=1}) and the second for control (\code{A=0}).
#' @param param_specs Outcome parameter overrides (same structure as \code{build_nimble_bundle()}).
#'   You can pass a single list used for both arms or a list with \code{con} and \code{trt} entries.
#' @param mcmc_outcome MCMC settings list for the outcome bundles.
#' @param mcmc_ps MCMC settings list for the PS model.
#' @param epsilon Numeric in [0,1) used by outcome bundles for posterior truncation summaries.
#'   If length 2, the first entry is used for treated (\code{A=1}) and the second for control (\code{A=0}).
#' @param alpha_random Logical; whether the outcome-model DP concentration parameter \eqn{\kappa} is stochastic.
#' @param ps_prior Normal prior for PS coefficients. List with \code{mean} and \code{sd}.
#' @param include_intercept Logical; if TRUE, an intercept column is prepended to \code{X}
#'   in the PS model.
#' @param PS Character or logical; controls propensity score estimation:
#'   \itemize{
#'     \item \code{"logit"} (default): Logistic regression PS model
#'     \item \code{"probit"}: Probit regression PS model
#'     \item \code{"naive"}: Gaussian naive Bayes PS model
#'     \item \code{FALSE}: No PS estimation; outcome models use only \code{X}
#'   }
#'   The PS model choice is stored in bundle metadata for downstream use in prediction
#'   and summaries, enabling seamless integration of future PS estimation methods.
#' @param ps_scale Scale used when augmenting outcomes with PS: \code{"logit"} or \code{"prob"}.
#' @param ps_summary Posterior summary for PS: \code{"mean"} or \code{"median"}.
#' @param ps_clamp Numeric epsilon for clamping PS values to \eqn{(\epsilon, 1-\epsilon)}.
#' @param monitor Character monitor profile: \code{"core"} (default) or \code{"full"}.
#' @param monitor_latent Logical; whether to monitor latent allocations (\code{z}) in outcome arms.
#' @param monitor_v Logical; whether to monitor stick-breaking \code{v} terms for SB outcomes.
#' @return A list of class \code{"causalmixgpd_causal_bundle"} containing the
#'   design bundle, two outcome bundles, training data, arm indices, and
#'   metadata required for posterior prediction and causal effect summaries.
#' @seealso \code{\link{bundle}}, \code{\link{run_mcmc_causal}},
#'   \code{\link{predict.causalmixgpd_causal_fit}}, \code{\link{ate}},
#'   \code{\link{qte}}, \code{\link{cate}}, \code{\link{cqte}}.
#' @examples
#' \dontrun{
#' set.seed(1)
#' N <- 100
#' X <- cbind(x1 = rnorm(N), x2 = runif(N))
#' A <- rbinom(N, 1, plogis(0.3 + 0.5 * X[, 1]))
#' y <- rexp(N) + 0.1
#'
#' cb <- build_causal_bundle(
#'   y = y,
#'   X = X,
#'   A = A,
#'   backend = "sb",
#'   kernel = "gamma",
#'   GPD = TRUE,
#'   components = 10,
#'   PS = "probit"
#' )
#' }
#' @export
build_causal_bundle <- function(
    y,
    X,
    A,
    backend = c("sb", "crp", "spliced"),
    kernel,
    GPD = FALSE,
    components = NULL,
    param_specs = NULL,
    mcmc_outcome = list(niter = 2000, nburnin = 500, thin = 1, nchains = 1, seed = 1),
    mcmc_ps = list(niter = 2000, nburnin = 500, thin = 1, nchains = 1, seed = 1),
    epsilon = 0.025,
    alpha_random = TRUE,
    ps_prior = list(mean = 0, sd = 2),
    include_intercept = TRUE,
    PS = "logit",
    ps_scale = c("logit", "prob"),
    ps_summary = c("mean", "median"),
    ps_clamp = 1e-6,
    monitor = c("core", "full"),
    monitor_latent = FALSE,
    monitor_v = FALSE
) {

  .arm_value <- function(val, name) {
    if (length(val) == 1L) return(list(trt = val, con = val))
    if (length(val) == 2L) return(list(trt = val[[1]], con = val[[2]]))
    stop(sprintf("%s must be length 1 or length 2.", name), call. = FALSE)
  }

  backend <- .arm_value(backend, "backend")
  backend$trt <- match.arg(backend$trt, choices = c("sb", "crp", "spliced"))
  backend$con <- match.arg(backend$con, choices = c("sb", "crp", "spliced"))
  monitor <- match.arg(monitor)
  ps_scale <- match.arg(ps_scale)
  ps_summary <- match.arg(ps_summary)

  if (identical(monitor, "full")) {
    monitor_latent <- TRUE
    monitor_v <- TRUE
  }

  y <- as.numeric(y)
  if (!length(y)) stop("y must be a non-empty numeric vector.", call. = FALSE)

  has_x <- !is.null(X)
  if (has_x) {
    if (!is.matrix(X)) X <- as.matrix(X)
    if (ncol(X) == 0L) {
      X <- NULL
      has_x <- FALSE
    }
  }
  if (has_x && nrow(X) != length(y)) {
    stop("X must have the same number of rows as length(y).", call. = FALSE)
  }

  A <- as.integer(A)
  if (length(A) != length(y)) stop("A must have the same length as y.", call. = FALSE)
  if (anyNA(A) || !all(A %in% c(0L, 1L))) stop("A must be binary (0/1) with no NA.", call. = FALSE)

  if (is.null(components)) components <- length(y)
  components <- .arm_value(components, "components")
  components$trt <- as.integer(components$trt)
  components$con <- as.integer(components$con)
  if (!is.finite(components$trt) || components$trt < 2L) {
    stop("components (treated) must be an integer >= 2.", call. = FALSE)
  }
  if (!is.finite(components$con) || components$con < 2L) {
    stop("components (control) must be an integer >= 2.", call. = FALSE)
  }

  # Validate and normalize PS parameter (disable PS when X is missing/empty)
  ps_model_type <- FALSE
  ps_choices <- c("logit", "probit", "naive")
  if (!has_x) {
    ps_model_type <- FALSE
  } else if (isFALSE(PS) || is.null(PS)) {
    ps_model_type <- FALSE
  } else if (isTRUE(PS)) {
    ps_model_type <- "logit"
  } else {
    ps_model_type <- match.arg(as.character(PS), choices = ps_choices)
  }

  idx_con <- which(A == 0L)
  idx_trt <- which(A == 1L)
  if (!length(idx_con) || !length(idx_trt)) {
    stop("Both treatment arms must have at least one observation.", call. = FALSE)
  }

  # Only build PS bundle if PS model is specified (not FALSE)
  ps_bundle <- NULL
  ps_placeholder <- NULL
  if (!isFALSE(ps_model_type)) {
    ps_spec <- list(
      model = ps_model_type,
      prior = list(
        mean = ps_prior$mean %||% 0,
        sd = ps_prior$sd %||% 2
      ),
      include_intercept = isTRUE(include_intercept)
    )
    ps_bundle <- .build_ps_bundle(A = A, X = X, spec = ps_spec, mcmc = mcmc_ps)
    ps_placeholder <- rep(0, length(y))
  }

  ps_con <- param_specs$con %||% param_specs
  ps_trt <- param_specs$trt %||% param_specs

  kernel <- .arm_value(kernel, "kernel")
  kchoices <- names(get_kernel_registry())
  kernel$trt <- match.arg(kernel$trt, choices = kchoices)
  kernel$con <- match.arg(kernel$con, choices = kchoices)

  GPD <- .arm_value(GPD, "GPD")
  GPD$trt <- isTRUE(GPD$trt)
  GPD$con <- isTRUE(GPD$con)

  epsilon <- .arm_value(epsilon, "epsilon")
  epsilon$trt <- as.numeric(epsilon$trt)
  epsilon$con <- as.numeric(epsilon$con)
  if (!is.finite(epsilon$trt) || epsilon$trt < 0 || epsilon$trt >= 1) {
    stop("epsilon (treated) must be in [0,1).", call. = FALSE)
  }
  if (!is.finite(epsilon$con) || epsilon$con < 0 || epsilon$con >= 1) {
    stop("epsilon (control) must be in [0,1).", call. = FALSE)
  }

  bundle_con <- build_nimble_bundle(
    y = y[idx_con],
    X = if (has_x) X[idx_con, , drop = FALSE] else NULL,
    ps = if (!is.null(ps_placeholder)) ps_placeholder[idx_con] else NULL,
    backend = backend$con,
    kernel = kernel$con,
    GPD = GPD$con,
    components = components$con,
    param_specs = ps_con,
    mcmc = mcmc_outcome,
    epsilon = epsilon$con,
    alpha_random = alpha_random,
    monitor = monitor,
    monitor_latent = monitor_latent,
    monitor_v = monitor_v
  )

  bundle_trt <- build_nimble_bundle(
    y = y[idx_trt],
    X = if (has_x) X[idx_trt, , drop = FALSE] else NULL,
    ps = if (!is.null(ps_placeholder)) ps_placeholder[idx_trt] else NULL,
    backend = backend$trt,
    kernel = kernel$trt,
    GPD = GPD$trt,
    components = components$trt,
    param_specs = ps_trt,
    mcmc = mcmc_outcome,
    epsilon = epsilon$trt,
    alpha_random = alpha_random,
    monitor = monitor,
    monitor_latent = monitor_latent,
    monitor_v = monitor_v
  )

  out <- list(
    design = ps_bundle,
    outcome = list(con = bundle_con, trt = bundle_trt),
    data = list(y = y, X = X, A = A),
    index = list(con = idx_con, trt = idx_trt),
    meta = list(
      backend = backend,
      kernel = kernel,
      GPD = GPD,
      components = components,
      epsilon = epsilon,
      has_x = has_x,
      needs_ps = !isFALSE(ps_model_type),
      ps_scale = ps_scale,
      ps_summary = ps_summary,
      ps_clamp = ps_clamp,
      ps = list(
        enabled = !isFALSE(ps_model_type),
        include_in_outcome = !isFALSE(ps_model_type),
        model_type = ps_model_type
      ),
      monitor_policy = list(
        monitor = monitor,
        monitor_latent = isTRUE(monitor_latent),
        monitor_v = isTRUE(monitor_v)
      )
    ),
    call = match.call()
  )
  class(out) <- "causalmixgpd_causal_bundle"
  out
}

.run_ps_mcmc_bundle <- function(bundle, show_progress = TRUE, quiet = FALSE) {
  nimble_quiet <- isTRUE(show_progress) || isTRUE(quiet)

  stopifnot(inherits(bundle, "causalmixgpd_ps_bundle"))
  progress_ctx <- .cmgpd_progress_start(
    total_steps = 5L,
    enabled = isTRUE(show_progress),
    quiet = isTRUE(quiet),
    label = "run_ps_mcmc_bundle"
  )
  on.exit(.cmgpd_progress_done(progress_ctx, final_label = NULL), add = TRUE)
  .cmgpd_progress_step(progress_ctx, "Validating PS MCMC inputs")

  if (!"package:nimble" %in% search()) {
    suppressPackageStartupMessages(base::require("nimble", quietly = TRUE, warn.conflicts = FALSE))
  }

  code <- .extract_nimble_code(bundle$code)
  constants <- bundle$constants %||% list()
  data <- bundle$data %||% list()
  inits <- bundle$inits %||% list()
  m <- bundle$mcmc %||% list()
  monitors <- bundle$monitors %||% "beta"

  .cmgpd_progress_step(progress_ctx, "Building PS NIMBLE model")
  Rmodel <- .cmgpd_capture_nimble(
    nimble::nimbleModel(
      code = code,
      data = data,
      constants = constants,
      inits = inits,
      check = TRUE,
      calculate = FALSE
    ),
    suppress = nimble_quiet
  )

  conf <- .cmgpd_capture_nimble(
    nimble::configureMCMC(
      Rmodel,
      monitors = monitors,
      enableWAIC = FALSE
    ),
    suppress = nimble_quiet
  )

  Rmcmc <- .cmgpd_capture_nimble(nimble::buildMCMC(conf), suppress = nimble_quiet)

  .cmgpd_progress_step(progress_ctx, "Compiling PS model")
  Cmodel <- .cmgpd_capture_nimble(
    nimble::compileNimble(Rmodel, showCompilerOutput = FALSE),
    suppress = nimble_quiet
  )
  Cmcmc  <- .cmgpd_capture_nimble(
    nimble::compileNimble(Rmcmc, project = Rmodel, showCompilerOutput = FALSE),
    suppress = nimble_quiet
  )

  niter   <- as.integer(m$niter   %||% 2000)
  nburnin <- as.integer(m$nburnin %||% 500)
  thin    <- as.integer(m$thin    %||% 1)
  nchains <- as.integer(m$nchains %||% 1)

  seed <- m$seed %||% NULL
  if (!is.null(seed)) {
    seed <- as.integer(seed)
    if (length(seed) == 1L && nchains > 1L) seed <- seed + seq_len(nchains) - 1L
    if (length(seed) != nchains) stop("mcmc$seed must be length 1 or length nchains.", call. = FALSE)
  }

  if (nchains > 1L) {
    inits_list <- vector("list", nchains)
    for (ch in seq_len(nchains)) {
      if (!is.null(seed)) set.seed(seed[ch])
      inits_list[[ch]] <- inits
    }
  } else {
    if (!is.null(seed)) set.seed(seed[1])
    inits_list <- inits
  }

  .cmgpd_progress_step(progress_ctx, "Running PS MCMC")
  res <- .cmgpd_capture_nimble(
    nimble::runMCMC(
      Cmcmc,
      niter = niter,
      nburnin = nburnin,
      thin = thin,
      nchains = nchains,
      inits = inits_list,
      setSeed = seed,
      progressBar = FALSE,
      samplesAsCodaMCMC = TRUE
    ),
    suppress = nimble_quiet
  )

  .cmgpd_progress_step(progress_ctx, "Assembling PS fit")
  fit <- list(
    mcmc = list(samples = res),
    bundle = bundle,
    call = match.call()
  )
  class(fit) <- "causalmixgpd_ps_fit"
  fit
}

#' Run posterior sampling for a causal bundle
#'
#' \code{run_mcmc_causal()} executes the PS block (when enabled) and the two
#' arm-specific outcome models prepared by \code{\link{build_causal_bundle}},
#' then returns a single \code{"causalmixgpd_causal_fit"} object.
#'
#' @details
#' The fitted object contains the posterior draws needed to evaluate arm-level
#' predictive distributions \eqn{F_1(y \mid x, \mathcal{D})} and
#' \eqn{F_0(y \mid x, \mathcal{D})}, followed by marginal or conditional causal
#' contrasts. When \code{PS = FALSE} in the bundle, the PS block is skipped and
#' outcome prediction uses only the original covariates.
#'
#' @param bundle A \code{"causalmixgpd_causal_bundle"} from \code{build_causal_bundle()}.
#' @param show_progress Logical; if TRUE, print step messages and render progress where supported.
#' @param quiet Logical; if TRUE, suppress step messages and progress display.
#' @param parallel_arms Logical; if TRUE, run control and treated outcome arms in parallel.
#' @param workers Optional integer workers for parallel arm execution.
#' @param timing Logical; if TRUE, return arm and total timings in \code{$timing}.
#' @param z_update_every Integer >= 1 passed to arm-level outcome MCMC.
#' @return A list of class \code{"causalmixgpd_causal_fit"} containing the
#'   fitted treated/control outcome models, optional PS fit, the original
#'   bundle, and timing metadata when requested.
#' @seealso \code{\link{build_causal_bundle}}, \code{\link{mcmc}},
#'   \code{\link{predict.causalmixgpd_causal_fit}}, \code{\link{ate}},
#'   \code{\link{qte}}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal")
#' fit <- run_mcmc_causal(cb)
#' }
#' @export
run_mcmc_causal <- function(bundle, show_progress = TRUE, quiet = FALSE,
                            parallel_arms = FALSE, workers = NULL, timing = FALSE,
                            z_update_every = NULL) {
  stopifnot(inherits(bundle, "causalmixgpd_causal_bundle"))
  progress_ctx <- .cmgpd_progress_start(
    total_steps = 6L,
    enabled = isTRUE(show_progress),
    quiet = isTRUE(quiet),
    label = "run_mcmc_causal"
  )
  on.exit(.cmgpd_progress_done(progress_ctx, final_label = NULL), add = TRUE)
  .cmgpd_progress_step(progress_ctx, "Validating causal MCMC configuration")

  z_update_every <- as.integer(
    z_update_every %||%
      bundle$outcome$con$mcmc$z_update_every %||%
      bundle$outcome$trt$mcmc$z_update_every %||%
      1L
  )
  if (!is.finite(z_update_every) || z_update_every < 1L) {
    stop("'z_update_every' must be an integer >= 1.", call. = FALSE)
  }

  # Check if propensity scores are enabled
  ps_meta <- bundle$meta$ps %||% list()
  ps_enabled <- isTRUE(ps_meta$enabled) && isTRUE(bundle$meta$has_x)
  ps_summary <- bundle$meta$ps_summary %||% "mean"
  ps_scale <- bundle$meta$ps_scale %||% "logit"
  ps_clamp <- bundle$meta$ps_clamp %||% 1e-6

  ps_fit <- NULL
  ps_hat <- NULL
  ps_cov <- NULL
  ps_model <- NULL

  if (ps_enabled) {
    .cmgpd_progress_step(progress_ctx, "Running propensity score block")
    ps_fit <- .run_ps_mcmc_bundle(bundle$design, show_progress = show_progress, quiet = quiet)
    ps_training_X <- bundle$data$X
    if (is.null(ps_training_X)) {
      stop("Training design matrix 'X' is missing from causal bundle.", call. = FALSE)
    }
    ps_training_X <- if (is.matrix(ps_training_X)) ps_training_X else as.matrix(ps_training_X)
    storage.mode(ps_training_X) <- "double"

    # Compute propensity scores and assign to outcome data
    ps_hat <- .compute_ps_from_fit(
      ps_fit = ps_fit,
      ps_bundle = bundle$design,
      X_new = ps_training_X,
      summary = ps_summary,
      clamp = ps_clamp
    )
    ps_cov <- .apply_ps_scale(ps_hat, scale = ps_scale, clamp = ps_clamp)
    idx_con <- bundle$index$con
    idx_trt <- bundle$index$trt
    bundle$outcome$con$data$ps <- ps_cov[idx_con]
    bundle$outcome$trt$data$ps <- ps_cov[idx_trt]

    # Prepare PS model for downstream prediction
    ps_model <- list(
      fit = ps_fit,
      bundle = bundle$design,
      scale = ps_scale,
      summary = ps_summary,
      clamp = ps_clamp
    )
  }
  if (!ps_enabled) {
    .cmgpd_progress_step(progress_ctx, "Skipping propensity score block")
  }

  .cmgpd_progress_step(progress_ctx, "Rebuilding outcome-arm code/monitors")
  # Regenerate code/constants/monitors (with or without PS)
  for (arm in c("con", "trt")) {
    b <- bundle$outcome[[arm]]
    b$code <- build_code_from_spec(b$spec)
    b$constants <- build_constants_from_spec(b$spec)
    pol <- b$monitor_policy %||% bundle$meta$monitor_policy %||% list()
    b$monitors <- build_monitors_from_spec(
      b$spec,
      monitor_v = isTRUE(pol$monitor_v),
      monitor_latent = isTRUE(pol$monitor_latent)
    )
    bundle$outcome[[arm]] <- b
  }
  timing_info <- list(total = NA_real_, con = NA_real_, trt = NA_real_, parallel_arms = FALSE)
  t0_total <- proc.time()[["elapsed"]]
  parallel_arms <- isTRUE(parallel_arms)
  workers <- as.integer(workers %||% 2L)
  if (!is.finite(workers) || workers < 1L) workers <- 1L
  if (parallel_arms) {
    warning("parallel_arms currently falls back to sequential execution in this environment.",
            call. = FALSE)
    parallel_arms <- FALSE
  }

  if (parallel_arms &&
      requireNamespace("future", quietly = TRUE) &&
      requireNamespace("future.apply", quietly = TRUE)) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = min(workers, 2L))
    fit_list <- future.apply::future_lapply(c("con", "trt"), function(arm) {
      run_mcmc_bundle_manual(
        bundle$outcome[[arm]],
        show_progress = FALSE,
        quiet = TRUE,
        timing = timing,
        z_update_every = z_update_every
      )
    }, future.seed = TRUE)
    timing_info$parallel_arms <- TRUE
    timing_info$con <- fit_list[[1]]$timing$mcmc %||% NA_real_
    timing_info$trt <- fit_list[[2]]$timing$mcmc %||% NA_real_
    con_fit <- fit_list[[1]]
    trt_fit <- fit_list[[2]]
  } else {
    if (parallel_arms) {
      warning("parallel_arms=TRUE requested but 'future'/'future.apply' are unavailable; running sequentially.",
              call. = FALSE)
    }
    .cmgpd_progress_step(progress_ctx, "Running control-arm outcome MCMC")
    t_con <- proc.time()[["elapsed"]]
    con_fit <- run_mcmc_bundle_manual(
      bundle$outcome$con,
      show_progress = show_progress,
      quiet = quiet,
      timing = timing,
      z_update_every = z_update_every
    )
    timing_info$con <- proc.time()[["elapsed"]] - t_con
    .cmgpd_progress_step(progress_ctx, "Running treated-arm outcome MCMC")
    t_trt <- proc.time()[["elapsed"]]
    trt_fit <- run_mcmc_bundle_manual(
      bundle$outcome$trt,
      show_progress = show_progress,
      quiet = quiet,
      timing = timing,
      z_update_every = z_update_every
    )
    timing_info$trt <- proc.time()[["elapsed"]] - t_trt
  }
  timing_info$total <- proc.time()[["elapsed"]] - t0_total

  # Attach PS model if available
  if (!is.null(ps_model)) {
    con_fit$ps_model <- ps_model
    trt_fit$ps_model <- ps_model
  }

  .cmgpd_progress_step(progress_ctx, "Assembling causal fit object")
  out <- list(
    ps_fit = ps_fit,
    outcome_fit = list(con = con_fit, trt = trt_fit),
    bundle = bundle,
    ps_hat = ps_hat,
    ps_cov = ps_cov,
    timing = timing_info,
    call = match.call()
  )
  class(out) <- "causalmixgpd_causal_fit"
  out
}

.causal_is_conditional_model <- function(fit) {
  meta <- fit$bundle$meta %||% list()
  has_x <- meta$has_x %||% NULL
  if (is.logical(has_x) && length(has_x) == 1L && !is.na(has_x)) {
    return(isTRUE(has_x))
  }
  X <- fit$bundle$data$X %||% NULL
  !is.null(X)
}

.causal_require_conditional <- function(fit, fn = c("cate", "cqte")) {
  fn <- match.arg(fn)
  if (.causal_is_conditional_model(fit)) return(invisible(TRUE))
  if (identical(fn, "cate")) {
    stop("cate() is available only for conditional causal models with covariates (X). For unconditional models, use ate()/att().", call. = FALSE)
  }
  stop("cqte() is available only for conditional causal models with covariates (X). For unconditional models, use qte()/qtt().", call. = FALSE)
}

.causal_validate_interval <- function(interval = "credible", level = 0.95) {
  compute_interval <- TRUE
  interval_use <- interval
  if (is.character(interval_use) && length(interval_use) == 1L && identical(tolower(interval_use), "none")) {
    compute_interval <- FALSE
    interval_use <- "credible"
  } else if (is.null(interval_use)) {
    compute_interval <- FALSE
    interval_use <- "credible"
  } else {
    interval_use <- match.arg(interval_use, choices = c("credible", "hpd"))
  }
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) {
    stop("'level' must be a numeric value between 0 and 1.", call. = FALSE)
  }
  list(compute_interval = compute_interval, interval = interval_use, level = as.numeric(level))
}

.causal_warn_ignored_marginal_inputs <- function(fn, newdata = NULL, y = NULL, conditional_fn) {
  ignored <- character(0)
  if (!is.null(newdata)) ignored <- c(ignored, "'newdata'")
  if (!is.null(y)) ignored <- c(ignored, "'y'")
  if (!length(ignored)) return(invisible(FALSE))
  verb <- if (length(ignored) == 1L) "is" else "are"
  warning(
    sprintf(
      "%s() computes a marginal estimand over the training covariate distribution, so %s %s ignored. Use %s() for conditional effects at specified covariate values.",
      fn,
      paste(ignored, collapse = " and "),
      verb,
      conditional_fn
    ),
    call. = FALSE
  )
  invisible(TRUE)
}

.causal_resolve_conditional_x <- function(fit, newdata = NULL, fn = c("cate", "cqte")) {
  fn <- match.arg(fn)
  x_train <- fit$bundle$data$X %||% NULL
  x_pred <- newdata %||% x_train
  if (is.null(x_pred)) {
    stop(sprintf("%s() requires covariates in 'newdata' or training X in the fitted model.", fn), call. = FALSE)
  }
  x_pred
}

#' Conditional quantile treatment effects
#'
#' \code{cqte()} evaluates treated-minus-control predictive quantiles at
#' user-supplied covariate rows.
#'
#' @details
#' For each prediction row \eqn{x}, the conditional quantile treatment effect is
#' \deqn{\mathrm{CQTE}(\tau, x) = Q_1(\tau \mid x, \mathcal{D}) -
#' Q_0(\tau \mid x, \mathcal{D}).}
#'
#' This estimand is available only for conditional causal models with
#' covariates. For marginal quantile contrasts over the empirical covariate
#' distribution, use \code{\link{qte}} or \code{\link{qtt}}.
#'
#' If the fit includes a PS block, the same PS adjustment is applied to both arm
#' predictions before differencing.
#'
#' @param fit A \code{"causalmixgpd_causal_fit"} object from \code{run_mcmc_causal()}.
#' @param probs Numeric vector of probabilities in (0, 1) specifying the quantile levels
#'   of the outcome distribution to estimate treatment effects at.
#' @param newdata Optional data.frame or matrix of covariates for prediction.
#'   If \code{NULL}, uses the training covariates stored in \code{fit}.
#' @param interval Character or NULL; type of credible interval: \code{NULL} for no interval,
#'   \code{"credible"} for equal-tailed quantile intervals (default), or \code{"hpd"} for
#'   highest posterior density intervals.
#' @param level Numeric credible level for intervals (default 0.95 for 95 percent CI).
#' @param show_progress Logical; if TRUE, print step messages and render progress where supported.
#' @return An object of class \code{"causalmixgpd_qte"} containing the CQTE
#'   summary, the probability grid, and the treated/control prediction objects
#'   used to construct the effect.
#' @seealso \code{\link{qte}}, \code{\link{qtt}}, \code{\link{cate}},
#'   \code{\link{predict.causalmixgpd_causal_fit}}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' cqte(fit, probs = c(0.5, 0.9), newdata = X[1:5, ])
#' cqte(fit, probs = c(0.5, 0.9), interval = "credible", level = 0.90)  # 90% CI
#' cqte(fit, probs = c(0.5, 0.9), interval = "hpd")  # HPD intervals
#' cqte(fit, probs = c(0.5, 0.9), interval = NULL)   # No intervals
#' }
#' @export
cqte <- function(fit,
                probs = c(0.1, 0.5, 0.9),
                newdata = NULL,
                interval = "credible",
                level = 0.95,
                show_progress = TRUE) {
  stopifnot(inherits(fit, "causalmixgpd_causal_fit"))
  progress_ctx <- .cmgpd_progress_start(
    total_steps = 5L,
    enabled = isTRUE(show_progress),
    quiet = FALSE,
    label = "cqte"
  )
  on.exit(.cmgpd_progress_done(progress_ctx, final_label = NULL), add = TRUE)
  .cmgpd_progress_step(progress_ctx, "Preparing CQTE inputs")

  .causal_require_conditional(fit, fn = "cqte")
  iv <- .causal_validate_interval(interval = interval, level = level)
  compute_interval <- iv$compute_interval
  interval <- iv$interval
  level <- iv$level
  x_pred <- .causal_resolve_conditional_x(fit = fit, newdata = newdata, fn = "cqte")

  ps_meta <- fit$bundle$meta$ps %||% list()
  ps_enabled <- isTRUE(ps_meta$enabled) && isTRUE(fit$bundle$meta$has_x)
  ps_scale <- fit$bundle$meta$ps_scale %||% "logit"
  ps_summary <- fit$bundle$meta$ps_summary %||% "mean"
  ps_clamp <- fit$bundle$meta$ps_clamp %||% 1e-6

  ps_prob <- NULL
  ps_cov <- NULL
  if (!is.null(x_pred) && ps_enabled) {
    .cmgpd_progress_step(progress_ctx, "Preparing propensity-score adjustment")
    ps_fit_use <- fit$ps_fit
    ps_bundle_use <- fit$bundle$design

    # Fallback: try to retrieve PS model from outcome fits if causal fit missing it
    if (is.null(ps_fit_use)) {
      ps_model_try <- (fit$outcome_fit$trt$ps_model %||% fit$outcome_fit$con$ps_model %||% NULL)
      if (!is.null(ps_model_try)) {
        ps_fit_use <- ps_model_try$fit
        ps_bundle_use <- ps_model_try$bundle
      }
    }

    # If PS model is still unavailable, warn and proceed without PS
    if (is.null(ps_fit_use) || is.null(ps_bundle_use)) {
      warning("Causal fit missing PS model; proceeding without PS adjustment.", call. = FALSE)
      ps_prob <- NULL
    } else {
      ps_prob <- .compute_ps_from_fit(
        ps_fit = ps_fit_use,
        ps_bundle = ps_bundle_use,
        X_new = x_pred,
        summary = ps_summary,
        clamp = ps_clamp
      )
      ps_cov <- .apply_ps_scale(ps_prob, scale = ps_scale, clamp = ps_clamp)
    }
  } else {
    .cmgpd_progress_step(progress_ctx, "Skipping propensity-score adjustment")
  }

  .cmgpd_progress_step(progress_ctx, "Predicting treated-arm quantiles")
  pr_trt <- predict(fit$outcome_fit$trt, x = x_pred, type = "quantile", index = probs,
                    ps = ps_cov,
                    interval = if (compute_interval) interval else NULL,
                    store_draws = TRUE,
                    show_progress = FALSE)
  .cmgpd_progress_step(progress_ctx, "Predicting control-arm quantiles")
  pr_con <- predict(fit$outcome_fit$con, x = x_pred, type = "quantile", index = probs,
                    ps = ps_cov,
                    interval = if (compute_interval) interval else NULL,
                    store_draws = TRUE,
                    show_progress = FALSE)

  if (is.null(pr_trt$draws) || is.null(pr_con$draws)) {
    stop("CQTE requires stored posterior quantile draws; set store_draws=TRUE in predict().", call. = FALSE)
  }
  # Coerce unconditional quantile draws (M x S) into array (S x 1 x M)
  if (is.matrix(pr_trt$draws) && length(dim(pr_trt$draws)) == 2L) {
    M <- nrow(pr_trt$draws)
    S <- ncol(pr_trt$draws)
    arr <- array(NA_real_, dim = c(S, 1L, M))
    for (s in seq_len(S)) arr[s, 1L, ] <- pr_trt$draws[, s]
    pr_trt$draws <- arr
  }
  if (is.matrix(pr_con$draws) && length(dim(pr_con$draws)) == 2L) {
    M <- nrow(pr_con$draws)
    S <- ncol(pr_con$draws)
    arr <- array(NA_real_, dim = c(S, 1L, M))
    for (s in seq_len(S)) arr[s, 1L, ] <- pr_con$draws[, s]
    pr_con$draws <- arr
  }
  if (is.null(dim(pr_trt$draws))) pr_trt$draws <- array(pr_trt$draws, dim = c(length(pr_trt$draws), 1L, 1L))
  if (is.null(dim(pr_con$draws))) pr_con$draws <- array(pr_con$draws, dim = c(length(pr_con$draws), 1L, 1L))
  d_tr <- dim(pr_trt$draws)
  d_co <- dim(pr_con$draws)
  if (!identical(d_tr, d_co)) {
    common <- pmin(d_tr, d_co)
    if (any(common < 1L)) {
      stop("Treated and control posterior draws must have matching dimensions for CQTE.", call. = FALSE)
    }
    pr_trt$draws <- pr_trt$draws[
      seq_len(common[1]),
      seq_len(common[2]),
      seq_len(common[3]),
      drop = FALSE
    ]
    pr_con$draws <- pr_con$draws[
      seq_len(common[1]),
      seq_len(common[2]),
      seq_len(common[3]),
      drop = FALSE
    ]
  }

  diff_draws <- pr_trt$draws - pr_con$draws  # dims: S x n_pred x length(probs)
  .cmgpd_progress_step(progress_ctx, "Aggregating CQTE estimates")
  fit_mat <- apply(diff_draws, c(2, 3), mean, na.rm = TRUE)
  n_pred <- if (!is.null(dim(pr_trt$draws))) dim(pr_trt$draws)[2] else length(pr_trt$fit)
  n_prob <- length(probs)
  fit_mat <- matrix(fit_mat, nrow = n_pred, ncol = n_prob)
  lower <- upper <- NULL
  if (compute_interval) {
    # Compute intervals for each (prediction, quantile) combination
    lower <- matrix(NA_real_, nrow = n_pred, ncol = n_prob)
    upper <- matrix(NA_real_, nrow = n_pred, ncol = n_prob)
    for (i in seq_len(n_pred)) {
      for (j in seq_len(n_prob)) {
        iv <- .compute_interval(diff_draws[, i, j], level = level, type = interval)
        lower[i, j] <- iv["lower"]
        upper[i, j] <- iv["upper"]
      }
    }
  }

  # Create CQTE fit data frame for convenience
  qte_fit <- data.frame(
    id = if (n_pred > 1L) rep(seq_len(n_pred), each = n_prob) else rep(1L, n_prob),
    index = rep(probs, times = n_pred),
    estimate = as.vector(t(fit_mat)),
    lower = if (!is.null(lower)) as.vector(t(lower)) else NA_real_,
    upper = if (!is.null(upper)) as.vector(t(upper)) else NA_real_
  )
  qte_fit <- .reorder_predict_cols(qte_fit)

  # Extract meta from causal fit
  meta <- fit$bundle$meta %||% list()

  out <- list(
    fit = fit_mat,
    lower = lower,
    upper = upper,
    qte = list(fit = qte_fit, draws = diff_draws),
    probs = probs,
    grid = probs,
    trt = pr_trt,
    con = pr_con,
    x = x_pred,
    ps = ps_prob,
    n_pred = n_pred,
    level = level,
    interval = if (compute_interval) interval else "none",
    type = "cqte",
    meta = list(
      ps_enabled = ps_enabled,
      ps_scale = ps_scale,
      ps_summary = ps_summary,
      backend = meta$backend,
      kernel = meta$kernel,
      GPD = meta$GPD
    )
  )
  class(out) <- "causalmixgpd_qte"
  out
}


#' Conditional average treatment effects
#'
#' \code{cate()} evaluates treated-minus-control predictive means, or restricted
#' means, at user-supplied covariate rows.
#'
#' @details
#' For each prediction row \eqn{x}, the conditional average treatment effect is
#' \deqn{\mathrm{CATE}(x) = E\{Y(1) \mid x, \mathcal{D}\} -
#' E\{Y(0) \mid x, \mathcal{D}\}.}
#'
#' With \code{type = "rmean"}, the estimand becomes the conditional restricted
#' mean contrast
#' \deqn{E\{\min(Y(1), c) \mid x, \mathcal{D}\} -
#' E\{\min(Y(0), c) \mid x, \mathcal{D}\},}
#' which remains finite even when the ordinary mean is unstable under a heavy
#' GPD tail.
#'
#' This estimand is available only for conditional causal models with
#' covariates. For marginal mean contrasts, use \code{\link{ate}} or
#' \code{\link{att}}.
#'
#' @param fit A \code{"causalmixgpd_causal_fit"} object from \code{run_mcmc_causal()}.
#' @param newdata Optional data.frame or matrix of covariates for prediction.
#'   If \code{NULL}, uses the training covariates stored in \code{fit}.
#' @param type Character; \code{"mean"} (default) for ordinary mean CATE or
#'   \code{"rmean"} for restricted-mean CATE.
#' @param cutoff Finite numeric cutoff for restricted mean; required for
#'   \code{type = "rmean"}, ignored otherwise.
#' @param interval Character or NULL; type of credible interval: \code{NULL} for no interval,
#'   \code{"credible"} for equal-tailed quantile intervals (default), or \code{"hpd"} for
#'   highest posterior density intervals.
#' @param level Numeric credible level for intervals (default 0.95 for 95 percent CI).
#' @param nsim_mean Number of posterior predictive draws to approximate the mean.
#' @param show_progress Logical; if TRUE, print step messages and render progress where supported.
#' @return An object of class \code{"causalmixgpd_ate"} containing the CATE
#'   summary, optional intervals, and the treated/control prediction objects used
#'   to construct the effect.
#' @seealso \code{\link{ate}}, \code{\link{att}}, \code{\link{cqte}},
#'   \code{\link{ate_rmean}}, \code{\link{predict.causalmixgpd_causal_fit}}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' cate(fit, newdata = X[1:5, ])
#' cate(fit, interval = "credible", level = 0.90)  # 90% CI
#' cate(fit, interval = "hpd")  # HPD intervals
#' cate(fit, interval = NULL)   # No intervals
#' }
#' @export
cate <- function(fit,
                newdata = NULL,
                type = c("mean", "rmean"),
                cutoff = NULL,
                interval = "credible",
                level = 0.95,
                nsim_mean = 200L,
                show_progress = TRUE) {
  stopifnot(inherits(fit, "causalmixgpd_causal_fit"))
  progress_ctx <- .cmgpd_progress_start(
    total_steps = 5L,
    enabled = isTRUE(show_progress),
    quiet = FALSE,
    label = "cate"
  )
  on.exit(.cmgpd_progress_done(progress_ctx, final_label = NULL), add = TRUE)
  .cmgpd_progress_step(progress_ctx, "Preparing CATE inputs")

  .causal_require_conditional(fit, fn = "cate")

  type <- match.arg(type)
  iv <- .causal_validate_interval(interval = interval, level = level)
  compute_interval <- iv$compute_interval
  interval <- iv$interval
  level <- iv$level
  x_pred <- .causal_resolve_conditional_x(fit = fit, newdata = newdata, fn = "cate")

  ps_meta <- fit$bundle$meta$ps %||% list()
  ps_enabled <- isTRUE(ps_meta$enabled) && isTRUE(fit$bundle$meta$has_x)
  ps_scale <- fit$bundle$meta$ps_scale %||% "logit"
  ps_summary <- fit$bundle$meta$ps_summary %||% "mean"
  ps_clamp <- fit$bundle$meta$ps_clamp %||% 1e-6

  ps_prob <- NULL
  ps_cov <- NULL
  if (!is.null(x_pred) && ps_enabled) {
    .cmgpd_progress_step(progress_ctx, "Preparing propensity-score adjustment")
    ps_fit_use <- fit$ps_fit
    ps_bundle_use <- fit$bundle$design

    # Fallback: try to retrieve PS model from outcome fits if causal fit missing it
    if (is.null(ps_fit_use)) {
      ps_model_try <- (fit$outcome_fit$trt$ps_model %||% fit$outcome_fit$con$ps_model %||% NULL)
      if (!is.null(ps_model_try)) {
        ps_fit_use <- ps_model_try$fit
        ps_bundle_use <- ps_model_try$bundle
      }
    }

    # If PS model is still unavailable, warn and proceed without PS
    if (is.null(ps_fit_use) || is.null(ps_bundle_use)) {
      warning("Causal fit missing PS model; proceeding without PS adjustment.", call. = FALSE)
      ps_prob <- NULL
    } else {
      ps_prob <- .compute_ps_from_fit(
        ps_fit = ps_fit_use,
        ps_bundle = ps_bundle_use,
        X_new = x_pred,
        summary = ps_summary,
        clamp = ps_clamp
      )
      ps_cov <- .apply_ps_scale(ps_prob, scale = ps_scale, clamp = ps_clamp)
    }
  } else {
    .cmgpd_progress_step(progress_ctx, "Skipping propensity-score adjustment")
  }

  .cmgpd_progress_step(progress_ctx, "Predicting treated-arm effects")
  pr_trt <- predict(fit$outcome_fit$trt, x = x_pred, type = type,
                    cutoff = cutoff,
                    ps = ps_cov, interval = if (compute_interval) interval else NULL,
                    level = level, nsim_mean = nsim_mean, store_draws = TRUE,
                    show_progress = FALSE)
  .cmgpd_progress_step(progress_ctx, "Predicting control-arm effects")
  pr_con <- predict(fit$outcome_fit$con, x = x_pred, type = type,
                    cutoff = cutoff,
                    ps = ps_cov, interval = if (compute_interval) interval else NULL,
                    level = level, nsim_mean = nsim_mean, store_draws = TRUE,
                    show_progress = FALSE)

  if (is.null(pr_trt$draws) || is.null(pr_con$draws)) {
    stop("CATE requires stored posterior mean draws; set store_draws=TRUE in predict().", call. = FALSE)
  }
  if (is.null(dim(pr_trt$draws))) {
    pr_trt$draws <- matrix(pr_trt$draws, ncol = 1L)
  }
  if (is.null(dim(pr_con$draws))) {
    pr_con$draws <- matrix(pr_con$draws, ncol = 1L)
  }
  d_tr <- dim(pr_trt$draws)
  d_co <- dim(pr_con$draws)
  if (!identical(d_tr, d_co)) {
    common <- pmin(d_tr, d_co)
    if (any(common < 1L)) {
      stop("Treated and control posterior draws must have matching dimensions for CATE.", call. = FALSE)
    }
    pr_trt$draws <- pr_trt$draws[seq_len(common[1]), seq_len(common[2]), drop = FALSE]
    pr_con$draws <- pr_con$draws[seq_len(common[1]), seq_len(common[2]), drop = FALSE]
  }

  diff_draws <- pr_trt$draws - pr_con$draws  # dims: S x n_pred
  .cmgpd_progress_step(progress_ctx, "Aggregating CATE estimates")
  fit_vec <- colMeans(diff_draws, na.rm = TRUE)
  n_pred <- length(fit_vec)
  lower <- upper <- NULL
  if (compute_interval) {
    # Compute intervals for each prediction point
    lower <- numeric(n_pred)
    upper <- numeric(n_pred)
    for (i in seq_len(n_pred)) {
      iv <- .compute_interval(diff_draws[, i], level = level, type = interval)
      lower[i] <- iv["lower"]
      upper[i] <- iv["upper"]
    }
  }

  # Create CATE fit data frame for convenience
  ate_fit <- data.frame(
    id = seq_len(n_pred),
    estimate = fit_vec,
    lower = if (!is.null(lower)) lower else NA_real_,
    upper = if (!is.null(upper)) upper else NA_real_
  )

  # Extract meta from causal fit
  meta <- fit$bundle$meta %||% list()
  ps_enabled <- isTRUE(ps_meta$enabled) && isTRUE(meta$has_x)

  out <- list(
    fit = fit_vec,
    lower = lower,
    upper = upper,
    ate = list(fit = ate_fit, draws = diff_draws),
    grid = NULL,
    trt = pr_trt,
    con = pr_con,
    x = x_pred,
    ps = ps_prob,
    n_pred = n_pred,
    nsim_mean = nsim_mean,
    level = level,
    interval = if (compute_interval) interval else "none",
    type = "cate",
    meta = list(
      ps_enabled = ps_enabled,
      ps_scale = ps_scale,
      ps_summary = ps_summary,
      backend = meta$backend,
      kernel = meta$kernel,
      GPD = meta$GPD
    )
  )
  class(out) <- "causalmixgpd_ate"
  out
}

.causal_ate_draws_matrix <- function(draws) {
  if (is.null(draws)) return(NULL)
  if (is.null(dim(draws))) return(matrix(as.numeric(draws), ncol = 1L))
  if (length(dim(draws)) == 2L) return(draws)
  stop("Unexpected mean draw dimensions in causal treatment effects.", call. = FALSE)
}

.causal_effect_subset_index <- function(fit, n_pred, subset = c("all", "treated")) {
  subset <- match.arg(subset)
  if (n_pred <= 1L) return(1L)
  if (subset == "all") return(seq_len(n_pred))
  idx <- as.integer(fit$bundle$index$trt %||% integer(0))
  idx <- idx[is.finite(idx) & idx >= 1L & idx <= n_pred]
  idx <- unique(idx)
  if (!length(idx)) {
    stop("No treated rows available for treated-only treatment effect aggregation.", call. = FALSE)
  }
  idx
}

.causal_summarize_draw_cols <- function(draw_mat, compute_interval, interval, level) {
  draw_mat <- as.matrix(draw_mat)
  estimate <- colMeans(draw_mat, na.rm = TRUE)
  lower <- upper <- rep(NA_real_, ncol(draw_mat))
  if (compute_interval) {
    for (j in seq_len(ncol(draw_mat))) {
      iv <- .compute_interval(draw_mat[, j], level = level, type = interval)
      lower[j] <- iv["lower"]
      upper[j] <- iv["upper"]
    }
  }
  list(estimate = estimate, lower = lower, upper = upper)
}

.causal_aggregate_qte <- function(obj, idx, effect_type = c("qte", "qtt")) {
  effect_type <- match.arg(effect_type)
  probs <- obj$probs %||% obj$grid %||% numeric(0)
  level <- obj$level %||% 0.95
  interval <- obj$interval %||% "none"
  compute_interval <- interval %in% c("credible", "hpd")

  trt_fit_draws <- .causal_ate_draws_matrix(obj$trt$draws %||% NULL)
  con_fit_draws <- .causal_ate_draws_matrix(obj$con$draws %||% NULL)
  if (is.null(trt_fit_draws) || is.null(con_fit_draws)) {
    stop("QTE aggregation requires treated/control posterior predictive draws.", call. = FALSE)
  }
  if (nrow(trt_fit_draws) != nrow(con_fit_draws)) {
    stop("Treated and control posterior draws must have the same number of rows.", call. = FALSE)
  }

  n_pred <- as.integer(obj$n_pred %||% 1L)
  if (n_pred > 1L) {
    if (ncol(trt_fit_draws) != ncol(con_fit_draws)) {
      stop("Treated and control posterior predictive draws must align by covariate row.", call. = FALSE)
    }
    idx <- unique(as.integer(idx))
    idx <- idx[idx >= 1L & idx <= ncol(trt_fit_draws)]
    if (!length(idx)) stop("No valid rows selected for QTE aggregation.", call. = FALSE)
    trt_pool <- trt_fit_draws[, idx, drop = FALSE]
    con_pool <- con_fit_draws[, idx, drop = FALSE]
  } else {
    # Unconditional case: each row already samples from the arm-level marginal.
    trt_pool <- trt_fit_draws
    con_pool <- con_fit_draws
  }

  .row_quantiles <- function(draw_mat, probs) {
    S <- nrow(draw_mat)
    M <- length(probs)
    out <- matrix(NA_real_, nrow = S, ncol = M)
    for (s in seq_len(S)) {
      xs <- as.numeric(draw_mat[s, ])
      xs <- xs[is.finite(xs)]
      if (!length(xs)) next
      out[s, ] <- as.numeric(stats::quantile(xs, probs = probs, names = FALSE, type = 7))
    }
    out
  }

  trt_q <- .row_quantiles(trt_pool, probs = probs)
  con_q <- .row_quantiles(con_pool, probs = probs)
  diff_q <- trt_q - con_q

  diff_summ <- .causal_summarize_draw_cols(diff_q, compute_interval = compute_interval, interval = interval, level = level)
  trt_summ <- .causal_summarize_draw_cols(trt_q, compute_interval = compute_interval, interval = interval, level = level)
  con_summ <- .causal_summarize_draw_cols(con_q, compute_interval = compute_interval, interval = interval, level = level)

  lower <- upper <- NULL
  if (compute_interval) {
    lower <- as.numeric(diff_summ$lower)
    upper <- as.numeric(diff_summ$upper)
  }

  qte_fit <- data.frame(
    id = rep(1L, length(probs)),
    index = probs,
    estimate = as.numeric(diff_summ$estimate),
    lower = if (!is.null(lower)) as.numeric(lower) else NA_real_,
    upper = if (!is.null(upper)) as.numeric(upper) else NA_real_
  )
  qte_fit <- .reorder_predict_cols(qte_fit)
  fit_tbl <- qte_fit[, c("index", "estimate", "lower", "upper"), drop = FALSE]

  trt_fit <- data.frame(
    id = rep(1L, length(probs)),
    index = probs,
    estimate = as.numeric(trt_summ$estimate),
    lower = as.numeric(trt_summ$lower),
    upper = as.numeric(trt_summ$upper)
  )
  trt_fit <- .reorder_predict_cols(trt_fit)
  con_fit <- data.frame(
    id = rep(1L, length(probs)),
    index = probs,
    estimate = as.numeric(con_summ$estimate),
    lower = as.numeric(con_summ$lower),
    upper = as.numeric(con_summ$upper)
  )
  con_fit <- .reorder_predict_cols(con_fit)

  S <- nrow(diff_q)
  M <- ncol(diff_q)
  trt_draws_out <- array(NA_real_, dim = c(S, 1L, M))
  con_draws_out <- array(NA_real_, dim = c(S, 1L, M))
  diff_draws_out <- array(NA_real_, dim = c(S, 1L, M))
  for (j in seq_len(M)) {
    trt_draws_out[, 1L, j] <- trt_q[, j]
    con_draws_out[, 1L, j] <- con_q[, j]
    diff_draws_out[, 1L, j] <- diff_q[, j]
  }

  pr_trt <- obj$trt
  pr_con <- obj$con
  pr_trt$fit <- trt_fit
  pr_con$fit <- con_fit
  pr_trt$draws <- trt_draws_out
  pr_con$draws <- con_draws_out

  out <- list(
    fit = fit_tbl,
    lower = lower,
    upper = upper,
    qte = list(fit = qte_fit, draws = diff_draws_out),
    probs = probs,
    grid = probs,
    trt = pr_trt,
    con = pr_con,
    x = NULL,
    ps = NULL,
    n_pred = 1L,
    level = level,
    interval = interval,
    type = effect_type,
    meta = obj$meta %||% list()
  )
  class(out) <- "causalmixgpd_qte"
  out
}

.causal_aggregate_ate <- function(obj, idx, effect_type = c("ate", "att")) {
  effect_type <- match.arg(effect_type)
  level <- obj$level %||% 0.95
  interval <- obj$interval %||% "none"
  compute_interval <- interval %in% c("credible", "hpd")

  trt_draws <- .causal_ate_draws_matrix(obj$trt$draws %||% NULL)
  con_draws <- .causal_ate_draws_matrix(obj$con$draws %||% NULL)
  if (is.null(trt_draws) || is.null(con_draws)) {
    stop("ATE aggregation requires treated/control mean draws.", call. = FALSE)
  }
  if (!identical(dim(trt_draws), dim(con_draws))) {
    stop("Treated and control mean draws must match for aggregation.", call. = FALSE)
  }

  idx <- unique(as.integer(idx))
  idx <- idx[idx >= 1L & idx <= ncol(trt_draws)]
  if (!length(idx)) stop("No valid rows selected for ATE aggregation.", call. = FALSE)

  trt_avg <- rowMeans(trt_draws[, idx, drop = FALSE], na.rm = TRUE)
  con_avg <- rowMeans(con_draws[, idx, drop = FALSE], na.rm = TRUE)
  diff_avg <- trt_avg - con_avg

  diff_summ <- .causal_summarize_draw_cols(matrix(diff_avg, ncol = 1L), compute_interval = compute_interval, interval = interval, level = level)
  trt_summ <- .causal_summarize_draw_cols(matrix(trt_avg, ncol = 1L), compute_interval = compute_interval, interval = interval, level = level)
  con_summ <- .causal_summarize_draw_cols(matrix(con_avg, ncol = 1L), compute_interval = compute_interval, interval = interval, level = level)

  fit_vec <- as.numeric(diff_summ$estimate)
  lower <- upper <- NULL
  if (compute_interval) {
    lower <- as.numeric(diff_summ$lower)
    upper <- as.numeric(diff_summ$upper)
  }

  ate_fit <- data.frame(
    id = 1L,
    estimate = fit_vec,
    lower = if (!is.null(lower)) lower else NA_real_,
    upper = if (!is.null(upper)) upper else NA_real_
  )
  trt_fit <- data.frame(
    id = 1L,
    estimate = as.numeric(trt_summ$estimate),
    lower = as.numeric(trt_summ$lower),
    upper = as.numeric(trt_summ$upper)
  )
  con_fit <- data.frame(
    id = 1L,
    estimate = as.numeric(con_summ$estimate),
    lower = as.numeric(con_summ$lower),
    upper = as.numeric(con_summ$upper)
  )

  pr_trt <- obj$trt
  pr_con <- obj$con
  pr_trt$fit <- trt_fit
  pr_con$fit <- con_fit
  pr_trt$draws <- matrix(trt_avg, ncol = 1L)
  pr_con$draws <- matrix(con_avg, ncol = 1L)

  out <- list(
    fit = fit_vec,
    lower = lower,
    upper = upper,
    ate = list(fit = ate_fit, draws = matrix(diff_avg, ncol = 1L)),
    grid = NULL,
    trt = pr_trt,
    con = pr_con,
    x = NULL,
    ps = NULL,
    n_pred = 1L,
    nsim_mean = obj$nsim_mean %||% NA_integer_,
    level = level,
    interval = interval,
    type = effect_type,
    meta = obj$meta %||% list()
  )
  class(out) <- "causalmixgpd_ate"
  out
}

#' Quantile treatment effects, marginal over the empirical covariate distribution
#'
#' \code{qte()} returns the marginal quantile treatment effect implied by the
#' causal fit.
#'
#' @details
#' The package computes
#' \deqn{\mathrm{QTE}(\tau) = Q_1^{m}(\tau) - Q_0^{m}(\tau),}
#' where \eqn{Q_a^{m}(\tau)} is the arm-\eqn{a} posterior predictive marginal
#' quantile obtained by averaging over the empirical training covariate
#' distribution.
#'
#' For unconditional causal models (\code{X = NULL}), this reduces to a direct
#' contrast of the arm-level unconditional predictive distributions.
#'
#' @param fit A \code{"causalmixgpd_causal_fit"} object from \code{run_mcmc_causal()}.
#' @param probs Numeric vector of probabilities in (0, 1) specifying the quantile levels
#'   of the outcome distribution to estimate treatment effects at.
#' @param newdata Ignored for marginal estimands. If supplied, a warning is issued
#'   and training data are used.
#' @param y Ignored for marginal estimands. If supplied, a warning is issued
#'   and training data are used.
#' @param interval Character or NULL; type of credible interval: \code{NULL} for no interval,
#'   \code{"credible"} for equal-tailed quantile intervals (default), or \code{"hpd"} for
#'   highest posterior density intervals.
#' @param level Numeric credible level for intervals (default 0.95 for 95 percent CI).
#' @param show_progress Logical; if TRUE, print step messages and render progress where supported.
#' @return An object of class \code{"causalmixgpd_qte"} containing the
#'   marginal QTE summary, the probability grid, and the arm-specific predictive
#'   objects used in the aggregation.
#' @seealso \code{\link{qtt}}, \code{\link{cqte}}, \code{\link{ate}},
#'   \code{\link{predict.causalmixgpd_causal_fit}}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' qte(fit, probs = c(0.5, 0.9))
#' }
#' @export
qte <- function(fit,
                probs = c(0.1, 0.5, 0.9),
                newdata = NULL,
                y = NULL,
                interval = "credible",
                level = 0.95,
                show_progress = TRUE) {
  stopifnot(inherits(fit, "causalmixgpd_causal_fit"))
  progress_ctx <- .cmgpd_progress_start(
    total_steps = 5L,
    enabled = isTRUE(show_progress),
    quiet = FALSE,
    label = "qte"
  )
  on.exit(.cmgpd_progress_done(progress_ctx, final_label = NULL), add = TRUE)
  .cmgpd_progress_step(progress_ctx, "Preparing QTE inputs")

  .causal_warn_ignored_marginal_inputs(fn = "qte", newdata = newdata, y = y, conditional_fn = "cqte")
  probs <- as.numeric(probs)
  if (!length(probs) || anyNA(probs) || any(!is.finite(probs)) || any(probs <= 0 | probs >= 1)) {
    stop("'probs' must be a numeric vector with values strictly between 0 and 1.", call. = FALSE)
  }
  iv <- .causal_validate_interval(interval = interval, level = level)
  compute_interval <- iv$compute_interval
  interval <- iv$interval
  level <- iv$level

  x_pred <- fit$bundle$data$X %||% NULL
  n_pred <- if (!is.null(x_pred)) nrow(as.matrix(x_pred)) else 1L

  ps_meta <- fit$bundle$meta$ps %||% list()
  ps_enabled <- isTRUE(ps_meta$enabled) && .causal_is_conditional_model(fit) && !is.null(x_pred)
  ps_scale <- fit$bundle$meta$ps_scale %||% "logit"
  ps_summary <- fit$bundle$meta$ps_summary %||% "mean"
  ps_clamp <- fit$bundle$meta$ps_clamp %||% 1e-6

  ps_prob <- NULL
  ps_cov <- NULL
  if (ps_enabled) {
    .cmgpd_progress_step(progress_ctx, "Preparing propensity-score adjustment")
    ps_fit_use <- fit$ps_fit
    ps_bundle_use <- fit$bundle$design
    if (is.null(ps_fit_use)) {
      ps_model_try <- (fit$outcome_fit$trt$ps_model %||% fit$outcome_fit$con$ps_model %||% NULL)
      if (!is.null(ps_model_try)) {
        ps_fit_use <- ps_model_try$fit
        ps_bundle_use <- ps_model_try$bundle
      }
    }
    if (is.null(ps_fit_use) || is.null(ps_bundle_use)) {
      warning("Causal fit missing PS model; proceeding without PS adjustment.", call. = FALSE)
      ps_prob <- NULL
    } else {
      ps_prob <- .compute_ps_from_fit(
        ps_fit = ps_fit_use,
        ps_bundle = ps_bundle_use,
        X_new = x_pred,
        summary = ps_summary,
        clamp = ps_clamp
      )
      ps_cov <- .apply_ps_scale(ps_prob, scale = ps_scale, clamp = ps_clamp)
    }
  } else {
    .cmgpd_progress_step(progress_ctx, "Skipping propensity-score adjustment")
  }

  .cmgpd_progress_step(progress_ctx, "Predicting treated-arm draws")
  pr_trt <- predict(
    fit$outcome_fit$trt,
    x = x_pred,
    type = "fit",
    ps = ps_cov,
    interval = NULL,
    store_draws = TRUE,
    show_progress = FALSE
  )
  .cmgpd_progress_step(progress_ctx, "Predicting control-arm draws")
  pr_con <- predict(
    fit$outcome_fit$con,
    x = x_pred,
    type = "fit",
    ps = ps_cov,
    interval = NULL,
    store_draws = TRUE,
    show_progress = FALSE
  )

  meta <- fit$bundle$meta %||% list()
  cq <- list(
    trt = pr_trt,
    con = pr_con,
    probs = probs,
    grid = probs,
    level = level,
    interval = if (compute_interval) interval else "none",
    n_pred = n_pred,
    x = NULL,
    ps = NULL,
    meta = list(
      ps_enabled = ps_enabled,
      ps_scale = ps_scale,
      ps_summary = ps_summary,
      backend = meta$backend,
      kernel = meta$kernel,
      GPD = meta$GPD
    )
  )
  idx <- .causal_effect_subset_index(fit = fit, n_pred = cq$n_pred %||% 1L, subset = "all")
  .cmgpd_progress_step(progress_ctx, "Aggregating QTE estimates")
  .causal_aggregate_qte(cq, idx = idx, effect_type = "qte")
}

#' Quantile treatment effects standardized to treated covariates
#'
#' \code{qtt()} computes the quantile treatment effect on the treated.
#'
#' @details
#' The estimand is
#' \deqn{\mathrm{QTT}(\tau) = Q_1^{t}(\tau) - Q_0^{t}(\tau),}
#' where marginalization is over the empirical covariate distribution of the
#' treated units only.
#'
#' @inheritParams qte
#' @return An object of class \code{"causalmixgpd_qte"} containing the QTT
#'   summary, the probability grid, and the arm-specific predictive objects used
#'   in the aggregation.
#' @seealso \code{\link{qte}}, \code{\link{cqte}}, \code{\link{att}}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' qtt(fit, probs = c(0.5, 0.9))
#' }
#' @export
qtt <- function(fit,
                probs = c(0.1, 0.5, 0.9),
                newdata = NULL,
                y = NULL,
                interval = "credible",
                level = 0.95,
                show_progress = TRUE) {
  stopifnot(inherits(fit, "causalmixgpd_causal_fit"))
  progress_ctx <- .cmgpd_progress_start(
    total_steps = 5L,
    enabled = isTRUE(show_progress),
    quiet = FALSE,
    label = "qtt"
  )
  on.exit(.cmgpd_progress_done(progress_ctx, final_label = NULL), add = TRUE)
  .cmgpd_progress_step(progress_ctx, "Preparing QTT inputs")

  .causal_warn_ignored_marginal_inputs(fn = "qtt", newdata = newdata, y = y, conditional_fn = "cqte")
  probs <- as.numeric(probs)
  if (!length(probs) || anyNA(probs) || any(!is.finite(probs)) || any(probs <= 0 | probs >= 1)) {
    stop("'probs' must be a numeric vector with values strictly between 0 and 1.", call. = FALSE)
  }
  iv <- .causal_validate_interval(interval = interval, level = level)
  compute_interval <- iv$compute_interval
  interval <- iv$interval
  level <- iv$level

  x_pred <- fit$bundle$data$X %||% NULL
  n_pred <- if (!is.null(x_pred)) nrow(as.matrix(x_pred)) else 1L

  ps_meta <- fit$bundle$meta$ps %||% list()
  ps_enabled <- isTRUE(ps_meta$enabled) && .causal_is_conditional_model(fit) && !is.null(x_pred)
  ps_scale <- fit$bundle$meta$ps_scale %||% "logit"
  ps_summary <- fit$bundle$meta$ps_summary %||% "mean"
  ps_clamp <- fit$bundle$meta$ps_clamp %||% 1e-6

  ps_prob <- NULL
  ps_cov <- NULL
  if (ps_enabled) {
    .cmgpd_progress_step(progress_ctx, "Preparing propensity-score adjustment")
    ps_fit_use <- fit$ps_fit
    ps_bundle_use <- fit$bundle$design
    if (is.null(ps_fit_use)) {
      ps_model_try <- (fit$outcome_fit$trt$ps_model %||% fit$outcome_fit$con$ps_model %||% NULL)
      if (!is.null(ps_model_try)) {
        ps_fit_use <- ps_model_try$fit
        ps_bundle_use <- ps_model_try$bundle
      }
    }
    if (is.null(ps_fit_use) || is.null(ps_bundle_use)) {
      warning("Causal fit missing PS model; proceeding without PS adjustment.", call. = FALSE)
      ps_prob <- NULL
    } else {
      ps_prob <- .compute_ps_from_fit(
        ps_fit = ps_fit_use,
        ps_bundle = ps_bundle_use,
        X_new = x_pred,
        summary = ps_summary,
        clamp = ps_clamp
      )
      ps_cov <- .apply_ps_scale(ps_prob, scale = ps_scale, clamp = ps_clamp)
    }
  } else {
    .cmgpd_progress_step(progress_ctx, "Skipping propensity-score adjustment")
  }

  .cmgpd_progress_step(progress_ctx, "Predicting treated-arm draws")
  pr_trt <- predict(
    fit$outcome_fit$trt,
    x = x_pred,
    type = "fit",
    ps = ps_cov,
    interval = NULL,
    store_draws = TRUE,
    show_progress = FALSE
  )
  .cmgpd_progress_step(progress_ctx, "Predicting control-arm draws")
  pr_con <- predict(
    fit$outcome_fit$con,
    x = x_pred,
    type = "fit",
    ps = ps_cov,
    interval = NULL,
    store_draws = TRUE,
    show_progress = FALSE
  )

  meta <- fit$bundle$meta %||% list()
  cq <- list(
    trt = pr_trt,
    con = pr_con,
    probs = probs,
    grid = probs,
    level = level,
    interval = if (compute_interval) interval else "none",
    n_pred = n_pred,
    x = NULL,
    ps = NULL,
    meta = list(
      ps_enabled = ps_enabled,
      ps_scale = ps_scale,
      ps_summary = ps_summary,
      backend = meta$backend,
      kernel = meta$kernel,
      GPD = meta$GPD
    )
  )
  idx <- .causal_effect_subset_index(fit = fit, n_pred = cq$n_pred %||% 1L, subset = "treated")
  .cmgpd_progress_step(progress_ctx, "Aggregating QTT estimates")
  .causal_aggregate_qte(cq, idx = idx, effect_type = "qtt")
}

#' Average treatment effects, marginal over the empirical covariate distribution
#'
#' \code{ate()} computes the posterior predictive average treatment effect.
#'
#' @details
#' The default mean-scale estimand is
#' \deqn{\mathrm{ATE} = E\{Y(1)\} - E\{Y(0)\},}
#' where the expectation is taken with respect to the empirical training
#' covariate distribution for conditional models.
#'
#' When \code{type = "rmean"}, the function instead computes a restricted-mean
#' ATE using \eqn{E\{\min(Y(a), c)\}} for each arm.
#'
#' For unconditional causal models (\code{X = NULL}), the computation reduces to
#' a direct contrast of the unconditional treated and control predictive laws.
#'
#' @param fit A \code{"causalmixgpd_causal_fit"} object from \code{run_mcmc_causal()}.
#' @param newdata Ignored for marginal estimands. If supplied, a warning is issued
#'   and training data are used.
#' @param y Ignored for marginal estimands. If supplied, a warning is issued
#'   and training data are used.
#' @param type Character; \code{"mean"} (default) for ordinary mean ATE or
#'   \code{"rmean"} for restricted-mean ATE.
#' @param cutoff Finite numeric cutoff for restricted mean; required for
#'   \code{type = "rmean"}, ignored otherwise.
#' @param interval Character or NULL; type of credible interval: \code{NULL} for no interval,
#'   \code{"credible"} for equal-tailed quantile intervals (default), or \code{"hpd"} for
#'   highest posterior density intervals.
#' @param level Numeric credible level for intervals (default 0.95 for 95 percent CI).
#' @param nsim_mean Number of posterior predictive draws to approximate the mean.
#' @param show_progress Logical; if TRUE, print step messages and render progress where supported.
#' @return An object of class \code{"causalmixgpd_ate"} containing the
#'   marginal ATE summary, optional intervals, and the arm-specific predictive
#'   objects used in the aggregation.
#' @seealso \code{\link{att}}, \code{\link{cate}}, \code{\link{qte}},
#'   \code{\link{ate_rmean}}, \code{\link{predict.causalmixgpd_causal_fit}}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' ate(fit, interval = "credible", level = 0.90, nsim_mean = 100)
#' }
#' @export
ate <- function(fit,
                newdata = NULL,
                y = NULL,
                type = c("mean", "rmean"),
                cutoff = NULL,
                interval = "credible",
                level = 0.95,
                nsim_mean = 200L,
                show_progress = TRUE) {
  stopifnot(inherits(fit, "causalmixgpd_causal_fit"))
  progress_ctx <- .cmgpd_progress_start(
    total_steps = 5L,
    enabled = isTRUE(show_progress),
    quiet = FALSE,
    label = "ate"
  )
  on.exit(.cmgpd_progress_done(progress_ctx, final_label = NULL), add = TRUE)
  .cmgpd_progress_step(progress_ctx, "Preparing ATE inputs")

  .causal_warn_ignored_marginal_inputs(fn = "ate", newdata = newdata, y = y, conditional_fn = "cate")
  type <- match.arg(type)
  iv <- .causal_validate_interval(interval = interval, level = level)
  compute_interval <- iv$compute_interval
  interval <- iv$interval
  level <- iv$level

  x_pred <- fit$bundle$data$X %||% NULL
  n_pred <- if (!is.null(x_pred)) nrow(as.matrix(x_pred)) else 1L

  ps_meta <- fit$bundle$meta$ps %||% list()
  ps_enabled <- isTRUE(ps_meta$enabled) && .causal_is_conditional_model(fit) && !is.null(x_pred)
  ps_scale <- fit$bundle$meta$ps_scale %||% "logit"
  ps_summary <- fit$bundle$meta$ps_summary %||% "mean"
  ps_clamp <- fit$bundle$meta$ps_clamp %||% 1e-6

  ps_prob <- NULL
  ps_cov <- NULL
  if (ps_enabled) {
    .cmgpd_progress_step(progress_ctx, "Preparing propensity-score adjustment")
    ps_fit_use <- fit$ps_fit
    ps_bundle_use <- fit$bundle$design
    if (is.null(ps_fit_use)) {
      ps_model_try <- (fit$outcome_fit$trt$ps_model %||% fit$outcome_fit$con$ps_model %||% NULL)
      if (!is.null(ps_model_try)) {
        ps_fit_use <- ps_model_try$fit
        ps_bundle_use <- ps_model_try$bundle
      }
    }
    if (is.null(ps_fit_use) || is.null(ps_bundle_use)) {
      warning("Causal fit missing PS model; proceeding without PS adjustment.", call. = FALSE)
      ps_prob <- NULL
    } else {
      ps_prob <- .compute_ps_from_fit(
        ps_fit = ps_fit_use,
        ps_bundle = ps_bundle_use,
        X_new = x_pred,
        summary = ps_summary,
        clamp = ps_clamp
      )
      ps_cov <- .apply_ps_scale(ps_prob, scale = ps_scale, clamp = ps_clamp)
    }
  } else {
    .cmgpd_progress_step(progress_ctx, "Skipping propensity-score adjustment")
  }

  .cmgpd_progress_step(progress_ctx, "Predicting treated-arm effects")
  pr_trt <- predict(
    fit$outcome_fit$trt,
    x = x_pred,
    type = type,
    cutoff = cutoff,
    ps = ps_cov,
    interval = if (compute_interval) interval else NULL,
    level = level,
    nsim_mean = nsim_mean,
    store_draws = TRUE,
    show_progress = FALSE
  )
  .cmgpd_progress_step(progress_ctx, "Predicting control-arm effects")
  pr_con <- predict(
    fit$outcome_fit$con,
    x = x_pred,
    type = type,
    cutoff = cutoff,
    ps = ps_cov,
    interval = if (compute_interval) interval else NULL,
    level = level,
    nsim_mean = nsim_mean,
    store_draws = TRUE,
    show_progress = FALSE
  )

  meta <- fit$bundle$meta %||% list()
  ca <- list(
    trt = pr_trt,
    con = pr_con,
    level = level,
    interval = if (compute_interval) interval else "none",
    n_pred = n_pred,
    nsim_mean = nsim_mean,
    x = NULL,
    ps = NULL,
    meta = list(
      ps_enabled = ps_enabled,
      ps_scale = ps_scale,
      ps_summary = ps_summary,
      backend = meta$backend,
      kernel = meta$kernel,
      GPD = meta$GPD
    )
  )
  idx <- .causal_effect_subset_index(fit = fit, n_pred = ca$n_pred %||% 1L, subset = "all")
  .cmgpd_progress_step(progress_ctx, "Aggregating ATE estimates")
  .causal_aggregate_ate(ca, idx = idx, effect_type = "ate")
}

#' Average treatment effects standardized to treated covariates
#'
#' \code{att()} computes the average treatment effect on the treated.
#'
#' @details
#' The estimand is
#' \deqn{\mathrm{ATT} = E\{Y(1) - Y(0) \mid A = 1\},}
#' approximated by marginalizing over the empirical covariate distribution of
#' treated units.
#'
#' @inheritParams ate
#' @return An object of class \code{"causalmixgpd_ate"} containing the ATT
#'   summary, optional intervals, and the arm-specific predictive objects used
#'   in the aggregation.
#' @seealso \code{\link{ate}}, \code{\link{qtt}}, \code{\link{cate}}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' att(fit, interval = "credible", nsim_mean = 100)
#' }
#' @export
att <- function(fit,
                newdata = NULL,
                y = NULL,
                type = c("mean", "rmean"),
                cutoff = NULL,
                interval = "credible",
                level = 0.95,
                nsim_mean = 200L,
                show_progress = TRUE) {
  stopifnot(inherits(fit, "causalmixgpd_causal_fit"))
  progress_ctx <- .cmgpd_progress_start(
    total_steps = 5L,
    enabled = isTRUE(show_progress),
    quiet = FALSE,
    label = "att"
  )
  on.exit(.cmgpd_progress_done(progress_ctx, final_label = NULL), add = TRUE)
  .cmgpd_progress_step(progress_ctx, "Preparing ATT inputs")

  .causal_warn_ignored_marginal_inputs(fn = "att", newdata = newdata, y = y, conditional_fn = "cate")
  type <- match.arg(type)
  iv <- .causal_validate_interval(interval = interval, level = level)
  compute_interval <- iv$compute_interval
  interval <- iv$interval
  level <- iv$level

  x_pred <- fit$bundle$data$X %||% NULL
  n_pred <- if (!is.null(x_pred)) nrow(as.matrix(x_pred)) else 1L

  ps_meta <- fit$bundle$meta$ps %||% list()
  ps_enabled <- isTRUE(ps_meta$enabled) && .causal_is_conditional_model(fit) && !is.null(x_pred)
  ps_scale <- fit$bundle$meta$ps_scale %||% "logit"
  ps_summary <- fit$bundle$meta$ps_summary %||% "mean"
  ps_clamp <- fit$bundle$meta$ps_clamp %||% 1e-6

  ps_prob <- NULL
  ps_cov <- NULL
  if (ps_enabled) {
    .cmgpd_progress_step(progress_ctx, "Preparing propensity-score adjustment")
    ps_fit_use <- fit$ps_fit
    ps_bundle_use <- fit$bundle$design
    if (is.null(ps_fit_use)) {
      ps_model_try <- (fit$outcome_fit$trt$ps_model %||% fit$outcome_fit$con$ps_model %||% NULL)
      if (!is.null(ps_model_try)) {
        ps_fit_use <- ps_model_try$fit
        ps_bundle_use <- ps_model_try$bundle
      }
    }
    if (is.null(ps_fit_use) || is.null(ps_bundle_use)) {
      warning("Causal fit missing PS model; proceeding without PS adjustment.", call. = FALSE)
      ps_prob <- NULL
    } else {
      ps_prob <- .compute_ps_from_fit(
        ps_fit = ps_fit_use,
        ps_bundle = ps_bundle_use,
        X_new = x_pred,
        summary = ps_summary,
        clamp = ps_clamp
      )
      ps_cov <- .apply_ps_scale(ps_prob, scale = ps_scale, clamp = ps_clamp)
    }
  } else {
    .cmgpd_progress_step(progress_ctx, "Skipping propensity-score adjustment")
  }

  .cmgpd_progress_step(progress_ctx, "Predicting treated-arm effects")
  pr_trt <- predict(
    fit$outcome_fit$trt,
    x = x_pred,
    type = type,
    cutoff = cutoff,
    ps = ps_cov,
    interval = if (compute_interval) interval else NULL,
    level = level,
    nsim_mean = nsim_mean,
    store_draws = TRUE,
    show_progress = FALSE
  )
  .cmgpd_progress_step(progress_ctx, "Predicting control-arm effects")
  pr_con <- predict(
    fit$outcome_fit$con,
    x = x_pred,
    type = type,
    cutoff = cutoff,
    ps = ps_cov,
    interval = if (compute_interval) interval else NULL,
    level = level,
    nsim_mean = nsim_mean,
    store_draws = TRUE,
    show_progress = FALSE
  )

  meta <- fit$bundle$meta %||% list()
  ca <- list(
    trt = pr_trt,
    con = pr_con,
    level = level,
    interval = if (compute_interval) interval else "none",
    n_pred = n_pred,
    nsim_mean = nsim_mean,
    x = NULL,
    ps = NULL,
    meta = list(
      ps_enabled = ps_enabled,
      ps_scale = ps_scale,
      ps_summary = ps_summary,
      backend = meta$backend,
      kernel = meta$kernel,
      GPD = meta$GPD
    )
  )
  idx <- .causal_effect_subset_index(fit = fit, n_pred = ca$n_pred %||% 1L, subset = "treated")
  .cmgpd_progress_step(progress_ctx, "Aggregating ATT estimates")
  .causal_aggregate_ate(ca, idx = idx, effect_type = "att")
}


#' Restricted-mean ATE helper
#'
#' \code{ate_rmean()} is a convenience wrapper for restricted-mean treatment
#' effects when the ordinary mean is unstable or undefined.
#'
#' @details
#' The restricted-mean estimand replaces \eqn{Y(a)} by
#' \eqn{\min\{Y(a), c\}}, so the contrast remains finite even when the fitted
#' GPD tail implies \eqn{\xi \ge 1}.
#'
#' @inheritParams cate
#' @param cutoff Finite numeric cutoff for the restricted mean.
#' @return A \code{"causalmixgpd_ate"} object computed via \code{\link{ate}}
#'   for unconditional fits or \code{\link{cate}} for conditional fits.
#' @seealso \code{\link{ate}}, \code{\link{cate}}, \code{\link{predict.mixgpd_fit}}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal",
#'                          GPD = TRUE, components = 6)
#' fit <- run_mcmc_causal(cb)
#' ate_rm <- ate_rmean(fit, cutoff = 10, interval = "credible")
#' }
#' @export
ate_rmean <- function(fit,
                      newdata = NULL,
                      cutoff,
                      interval = "credible",
                      level = 0.95,
                      nsim_mean = 200L,
                      show_progress = TRUE) {
  if (.causal_is_conditional_model(fit)) {
    cate(fit = fit,
         newdata = newdata,
         type = "rmean",
         cutoff = cutoff,
         interval = interval,
         level = level,
         nsim_mean = nsim_mean,
         show_progress = show_progress)
  } else {
    ate(fit = fit,
        newdata = newdata,
        y = NULL,
        type = "rmean",
        cutoff = cutoff,
        interval = interval,
        level = level,
        nsim_mean = nsim_mean,
        show_progress = show_progress)
  }
}


#' Predict arm-specific and contrast-scale quantities from a causal fit
#'
#' \code{predict.causalmixgpd_causal_fit()} is the causal counterpart to
#' \code{\link{predict.mixgpd_fit}}. It coordinates the treated and control arm
#' predictions so that both sides use the same covariate rows and the same PS
#' adjustment.
#'
#' @details
#' For each prediction row \eqn{x}, the function evaluates arm-specific
#' posterior predictive quantities based on
#' \eqn{F_1(y \mid x, \mathcal{D})} and \eqn{F_0(y \mid x, \mathcal{D})}. Mean
#' and quantile outputs are returned on the treatment-effect scale, while
#' density, survival, and probability outputs retain both arm-specific curves.
#'
#' If a PS model is stored in the fit, the same estimated score is supplied to
#' both arms unless the user overrides it with \code{ps}. This is the main
#' prediction entry point used internally by \code{\link{ate}}, \code{\link{qte}},
#' \code{\link{cate}}, and \code{\link{cqte}}.
#'
#' @inheritParams predict.mixgpd_fit
#' @param object A \code{"causalmixgpd_causal_fit"} object returned by
#'   \code{run_mcmc_causal()}.
#' @param ps Optional numeric vector of propensity scores aligned with \code{x}
#'   / \code{newdata}. When provided, the supplied scores are used instead of
#'   recomputing them from the stored PS model (needed only for custom inputs).
#' @param id Optional identifier for prediction rows. Provide either a column name
#'   in \code{x}/\code{newdata} or a vector of length \code{nrow(x)}. The id column
#'   is excluded from analysis.
#' @param type Prediction type. Supported: \code{"mean"}, \code{"quantile"},
#'   \code{"density"}, \code{"survival"}, \code{"prob"}.
#' @param interval Character or NULL; type of credible interval: \code{NULL} for no interval,
#'   \code{"credible"} for equal-tailed quantile intervals (default), or \code{"hpd"} for
#'   highest posterior density intervals.
#' @return For \code{"mean"} and \code{"quantile"}, a causal prediction object
#'   whose \code{$fit} component reports treated-minus-control posterior
#'   summaries. For \code{"density"}, \code{"survival"}, and \code{"prob"},
#'   the \code{$fit} component contains side-by-side treated and control
#'   summaries evaluated on the supplied \code{y} grid.
#' @seealso \code{\link{predict.mixgpd_fit}}, \code{\link{ate}},
#'   \code{\link{qte}}, \code{\link{cate}}, \code{\link{cqte}}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal")
#' fit <- run_mcmc_causal(cb)
#' predict(fit, x = X[1:10, ], type = "quantile", index = c(0.25, 0.5, 0.75))
#' predict(fit, x = X[1:10, ], type = "mean", interval = "hpd")  # HPD intervals
#' predict(fit, x = X[1:10, ], type = "mean", interval = NULL)   # No intervals
#' }
#' @export
predict.causalmixgpd_causal_fit <- function(object,
                                        x = NULL,
                                        y = NULL,
                                        ps = NULL,
                                        id = NULL,
                                        newdata = NULL,
                                        type = c("mean", "quantile", "density", "survival", "prob", "location"),
                                        p = NULL,
                                        index = NULL,
                                        nsim = NULL,
                                        interval = "credible",
                                        probs = c(0.025, 0.5, 0.975),
                                        store_draws = TRUE,
                                        nsim_mean = 200L,
                                        ncores = 1L,
                                        show_progress = TRUE,
                                        ...) {
  stopifnot(inherits(object, "causalmixgpd_causal_fit"))
  progress_ctx <- .cmgpd_progress_start(
    total_steps = 5L,
    enabled = isTRUE(show_progress),
    quiet = FALSE,
    label = "predict.causalmixgpd_causal_fit"
  )
  on.exit(.cmgpd_progress_done(progress_ctx, final_label = NULL), add = TRUE)
  .cmgpd_progress_step(progress_ctx, "Resolving causal prediction inputs")

  if (!is.null(newdata) && !is.null(x)) {
    stop("Provide only one of 'x' or 'newdata' (they are aliases).", call. = FALSE)
  }
  if (!is.null(newdata)) x <- newdata

  id_info <- .resolve_predict_id(x, id = id)
  x <- id_info$x
  id_vec <- id_info$id

  type <- match.arg(type)

  # Handle interval: NULL/"none" means no interval, otherwise match to credible/hpd
  compute_interval <- TRUE
  if (is.character(interval) && length(interval) == 1L && identical(tolower(interval), "none")) {
    interval <- NULL
  }
  if (is.null(interval)) {
    compute_interval <- FALSE
    interval <- "credible"  # placeholder for downstream calls
  } else {
    interval <- match.arg(interval, choices = c("credible", "hpd"))
  }

  bundle <- object$bundle %||% list()

  X_train <- bundle$data$X %||% NULL
  has_X <- !is.null(X_train)
  x_mat <- if (!is.null(x)) as.matrix(x) else NULL
  n_pred_default <- if (has_X) {
    nrow(X_train)
  } else if (type %in% c("density", "survival", "prob") && !is.null(y)) {
    length(as.numeric(y))
  } else {
    1L
  }
  n_pred <- if (!is.null(x_mat)) nrow(x_mat) else n_pred_default
  if (!is.null(id_vec) && length(id_vec) != n_pred) {
    stop("Length of 'id' must match the number of prediction rows.", call. = FALSE)
  }
  id_vals <- if (!is.null(id_vec)) id_vec else seq_len(n_pred)

  ps_enabled <- isTRUE(bundle$meta$ps$enabled) && has_X
  ps_include_in_outcome <- isTRUE(bundle$meta$ps$include_in_outcome)
  if (is.null(ps_include_in_outcome)) ps_include_in_outcome <- ps_enabled
  ps_model_type <- bundle$meta$ps$model_type %||% FALSE
  ps_scale <- bundle$meta$ps_scale %||% "logit"
  ps_summary <- bundle$meta$ps_summary %||% "mean"
  ps_clamp <- bundle$meta$ps_clamp %||% 1e-6

  if (type == "quantile") {
    # Alias index <-> p for consistency with predict.mixgpd_fit()
    if (!is.null(index) && is.null(p)) {
      p <- index
    } else if (!is.null(index) && !is.null(p)) {
      if (!isTRUE(all.equal(as.numeric(index), as.numeric(p)))) {
        stop("Provide only one of 'p' or 'index' for causal quantile prediction.", call. = FALSE)
      }
    }
    if (is.null(p) || length(p) == 0 || any(!is.finite(as.numeric(p)))) {
      stop("Causal predict for quantile requires one or more finite probabilities in 'p' or 'index'.", call. = FALSE)
    }
    p <- as.numeric(p)
    if (any(p <= 0 | p >= 1)) {
      stop("Probabilities in 'p'/'index' must be in (0, 1).", call. = FALSE)
    }
  }

  if (type %in% c("density", "survival", "prob")) {
    if (is.null(y)) stop("Causal predict for density/survival/prob requires 'y'.", call. = FALSE)
    y <- as.numeric(y)
    if (!length(y) || any(!is.finite(y))) stop("'y' must be a finite numeric vector.", call. = FALSE)
    if (length(y) != n_pred) {
      stop("Length of 'y' must match the number of prediction rows (nrow(x) or training X).", call. = FALSE)
    }
  }

  ps_full <- NULL
  ps_cov <- NULL
  .cmgpd_progress_step(progress_ctx, "Preparing propensity-score inputs")
  if (ps_enabled) {
    if (!is.null(ps)) {
      ps_full <- as.numeric(ps)
      eps <- as.numeric(ps_clamp)[1]
      if (is.finite(eps) && eps > 0) {
        ps_full <- pmin(pmax(ps_full, eps), 1 - eps)
      }
    } else if (!is.null(x_mat)) {
      if (is.null(object$ps_fit)) {
        stop(sprintf("Causal fit missing PS model (%s); cannot compute propensity scores for newdata.", ps_model_type), call. = FALSE)
      }
      ps_full <- .compute_ps_from_fit(
        ps_fit = object$ps_fit,
        ps_bundle = bundle$design,
        X_new = x_mat,
        summary = ps_summary,
        clamp = ps_clamp
      )
    } else {
      ps_full <- object$ps_hat %||% NULL
    }

    if (!is.null(ps_full) && length(ps_full) != n_pred) {
      stop("Length of 'ps' must equal the number of prediction rows (nrow(x)).", call. = FALSE)
    }
    if (!is.null(ps_full)) {
      ps_cov <- .apply_ps_scale(ps_full, scale = ps_scale, clamp = ps_clamp)
    }
  }

  ps_trt <- if (ps_enabled && ps_include_in_outcome) ps_cov else NULL
  ps_con <- if (ps_enabled && ps_include_in_outcome) ps_cov else NULL

  .return_out <- function(obj) {
    .cmgpd_progress_step(progress_ctx, "Assembling causal prediction output")
    obj
  }

  .extract_stats <- function(pr, n_pred) {
    fit <- pr$fit
    if (is.data.frame(fit)) {
      if ("id" %in% names(fit)) {
        fit <- fit[order(fit$id), , drop = FALSE]
      }
      est <- if ("estimate" %in% names(fit)) fit$estimate else as.numeric(fit[[1]])
      lower <- if ("lower" %in% names(fit)) fit$lower else rep(NA_real_, length(est))
      upper <- if ("upper" %in% names(fit)) fit$upper else rep(NA_real_, length(est))
    } else if (is.matrix(fit)) {
      est <- as.numeric(fit[, 1])
      lower <- rep(NA_real_, length(est))
      upper <- rep(NA_real_, length(est))
    } else {
      est <- as.numeric(fit)
      lower <- rep(NA_real_, length(est))
      upper <- rep(NA_real_, length(est))
    }
    if (length(est) == 1L && n_pred > 1L) {
      est <- rep(est, n_pred)
      lower <- rep(lower, n_pred)
      upper <- rep(upper, n_pred)
    }
    if (length(est) != n_pred) {
      stop("Unexpected prediction length in causal predict.", call. = FALSE)
    }
    list(estimate = est, lower = lower, upper = upper)
  }

  if (type %in% c("density", "survival", "prob")) {
    .cmgpd_progress_step(progress_ctx, "Predicting treated and control arms")
    pred_type <- if (type == "density") "density" else "survival"
    x_pred <- if (!is.null(x_mat)) x_mat else if (has_X) X_train else NULL
    y_vec <- y

    .predict_pairwise <- function(fit, ps_vec) {
      est <- lower <- upper <- numeric(n_pred)
      for (i in seq_len(n_pred)) {
        xi <- if (!is.null(x_pred)) x_pred[i, , drop = FALSE] else NULL
        psi <- if (!is.null(ps_vec)) ps_vec[i] else NULL
        pr <- predict(
          fit,
          x = xi,
          y = y_vec[i],
          ps = psi,
          id = if (!is.null(xi)) id_vals[i] else NULL,
          type = pred_type,
          interval = if (compute_interval) interval else NULL,
          probs = probs,
          store_draws = FALSE,
          show_progress = FALSE,
          ncores = 1L,
          ...
        )
        stats <- .extract_stats(pr, 1L)
        est[i] <- stats$estimate[1]
        lower[i] <- stats$lower[1]
        upper[i] <- stats$upper[1]
      }
      list(estimate = est, lower = lower, upper = upper)
    }

    trt_stats <- .predict_pairwise(object$outcome_fit$trt, ps_trt)
    con_stats <- .predict_pairwise(object$outcome_fit$con, ps_con)

    if (type == "prob") {
      trt_stats <- list(
        estimate = 1 - trt_stats$estimate,
        lower = 1 - trt_stats$upper,
        upper = 1 - trt_stats$lower
      )
      con_stats <- list(
        estimate = 1 - con_stats$estimate,
        lower = 1 - con_stats$upper,
        upper = 1 - con_stats$lower
      )
    }

    ps_col <- if (!is.null(ps_full)) ps_full else rep(NA_real_, n_pred)
    out <- data.frame(
      id = id_vals,
      y = y_vec,
      ps = ps_col,
      trt_estimate = trt_stats$estimate,
      trt_lower = trt_stats$lower,
      trt_upper = trt_stats$upper,
      con_estimate = con_stats$estimate,
      con_lower = con_stats$lower,
      con_upper = con_stats$upper,
      row.names = NULL
    )
    attr(out, "type") <- type
    class(out) <- c("causalmixgpd_causal_predict", class(out))
    return(.return_out(out))
  }

  id_arg <- if (!is.null(x)) id_vals else NULL

  .cmgpd_progress_step(progress_ctx, "Predicting treated arm")
  pr_trt <- predict(
    object$outcome_fit$trt,
    x = x,
    y = y,
    ps = ps_trt,
    id = id_arg,
    type = type,
    index = if (type == "quantile") p else NULL,
    nsim = nsim,
    interval = if (compute_interval) interval else NULL,
    probs = probs,
    store_draws = store_draws,
    nsim_mean = nsim_mean,
    show_progress = FALSE,
    ncores = ncores,
    ...
  )

  .cmgpd_progress_step(progress_ctx, "Predicting control arm")
  pr_con <- predict(
    object$outcome_fit$con,
    x = x,
    y = y,
    ps = ps_con,
    id = id_arg,
    type = type,
    index = if (type == "quantile") p else NULL,
    nsim = nsim,
    interval = if (compute_interval) interval else NULL,
    probs = probs,
    store_draws = store_draws,
    nsim_mean = nsim_mean,
    show_progress = FALSE,
    ncores = ncores,
    ...
  )
  # If quantile draws are available, compute intervals on the treatment effect directly.
  if (type == "quantile" && !is.null(pr_trt$draws) && !is.null(pr_con$draws)) {
    normalize_draws <- function(draws) {
      if (is.null(dim(draws))) {
        return(array(as.numeric(draws), dim = c(length(draws), 1L, 1L)))
      }
      d <- dim(draws)
      if (length(d) == 2L) {
        # Unconditional case: draws is M x S -> convert to S x 1 x M
        M <- d[1]
        S <- d[2]
        arr <- array(NA_real_, dim = c(S, 1L, M))
        for (s in seq_len(S)) arr[s, 1L, ] <- draws[, s]
        return(arr)
      }
      if (length(d) == 3L) return(draws) # S x n_pred x M
      stop("Unexpected draw dimensions in causal quantile prediction.", call. = FALSE)
    }

    trt_draws <- normalize_draws(pr_trt$draws)
    con_draws <- normalize_draws(pr_con$draws)
    d_tr <- dim(trt_draws)
    d_co <- dim(con_draws)
    if (!identical(d_tr, d_co)) {
      common <- pmin(d_tr, d_co)
      if (any(common < 1L)) {
        stop("Treated and control posterior draws must have matching dimensions.", call. = FALSE)
      }
      trt_draws <- trt_draws[
        seq_len(common[1]),
        seq_len(common[2]),
        seq_len(common[3]),
        drop = FALSE
      ]
      con_draws <- con_draws[
        seq_len(common[1]),
        seq_len(common[2]),
        seq_len(common[3]),
        drop = FALSE
      ]
    }

    diff_draws <- trt_draws - con_draws  # S x n_pred x M
    # .posterior_summarize expects draws in last dimension
    diff_for_sum <- aperm(diff_draws, c(2, 3, 1))  # n_pred x M x S
    summ <- .posterior_summarize(
      diff_for_sum,
      probs = probs,
      interval = if (compute_interval) interval else NULL
    )

    est <- summ$estimate
    lower <- summ$lower
    upper <- summ$upper
    n_pred_eff <- if (is.matrix(est)) nrow(est) else length(est)
    id_eff <- id_vals[seq_len(min(length(id_vals), n_pred_eff))]
    if (length(id_eff) < n_pred_eff) {
      id_eff <- seq_len(n_pred_eff)
    }
    ps_col <- if (!is.null(ps_full)) {
      ps_full[seq_len(min(length(ps_full), n_pred_eff))]
    } else {
      rep(NA_real_, n_pred_eff)
    }

    if (length(p) > 1L) {
      out_df <- data.frame(
        id = rep(id_eff, each = length(p)),
        index = rep(p, times = n_pred_eff),
        ps = rep(ps_col, each = length(p)),
        estimate = as.vector(t(est)),
        lower = if (compute_interval) as.vector(t(lower)) else NA_real_,
        upper = if (compute_interval) as.vector(t(upper)) else NA_real_,
        row.names = NULL
      )
      out_df <- .reorder_predict_cols(out_df)
      attr(out_df, "type") <- type
      attr(out_df, "index") <- p
      attr(out_df, "trt") <- pr_trt
      attr(out_df, "con") <- pr_con
      class(out_df) <- c("causalmixgpd_causal_predict", class(out_df))
      return(.return_out(out_df))
    }

    est_vec <- as.numeric(est)
    lower_vec <- if (compute_interval) as.numeric(lower) else rep(NA_real_, n_pred_eff)
    upper_vec <- if (compute_interval) as.numeric(upper) else rep(NA_real_, n_pred_eff)
    out <- data.frame(
      id = id_eff,
      index = rep(p, length.out = n_pred_eff),
      ps = ps_col,
      estimate = est_vec,
      lower = lower_vec,
      upper = upper_vec,
      row.names = NULL
    )
    out <- .reorder_predict_cols(out)
    attr(out, "type") <- type
    attr(out, "index") <- p
    attr(out, "trt") <- pr_trt
    attr(out, "con") <- pr_con
    class(out) <- c("causalmixgpd_causal_predict", class(out))
    return(.return_out(out))
  }
  # Special handling for quantile with possibly multiple probabilities
  if (type == "quantile" && length(p) > 1L) {
    # Expect pr_trt$fit and pr_con$fit to be data.frames with rows ordered per index block:
    # index varies slowest in returned fit: index repeated each n_pred, with ids within block.
    fit_tr <- pr_trt$fit
    fit_co <- pr_con$fit
    if (!is.data.frame(fit_tr) || !is.data.frame(fit_co) ||
        !("index" %in% names(fit_tr)) || !("id" %in% names(fit_tr))) {
      stop("Unexpected format from underlying predict() for multiple quantiles.", call. = FALSE)
    }
    M <- length(p)
    # Ensure expected row count
    if (nrow(fit_tr) != n_pred * M || nrow(fit_co) != n_pred * M) {
      stop("Unexpected prediction length in causal predict.", call. = FALSE)
    }
    # Build matrices: columns = probs, rows = prediction rows
    trt_mat <- matrix(NA_real_, nrow = n_pred, ncol = M)
    trt_lower <- matrix(NA_real_, nrow = n_pred, ncol = M)
    trt_upper <- matrix(NA_real_, nrow = n_pred, ncol = M)
    con_mat <- matrix(NA_real_, nrow = n_pred, ncol = M)
    con_lower <- matrix(NA_real_, nrow = n_pred, ncol = M)
    con_upper <- matrix(NA_real_, nrow = n_pred, ncol = M)
    key_tr <- paste(fit_tr$id, fit_tr$index, sep = "|")
    key_co <- paste(fit_co$id, fit_co$index, sep = "|")
    key_ref <- paste(rep(id_vals, each = M), rep(p, times = n_pred), sep = "|")
    idx_tr <- match(key_ref, key_tr)
    idx_co <- match(key_ref, key_co)
    if (anyNA(idx_tr) || anyNA(idx_co)) {
      stop("Unexpected prediction ordering in causal predict.", call. = FALSE)
    }
    fit_tr <- fit_tr[idx_tr, , drop = FALSE]
    fit_co <- fit_co[idx_co, , drop = FALSE]
    trt_mat <- matrix(as.numeric(fit_tr$estimate), nrow = n_pred, ncol = M, byrow = TRUE)
    con_mat <- matrix(as.numeric(fit_co$estimate), nrow = n_pred, ncol = M, byrow = TRUE)
    trt_lower <- if ("lower" %in% names(fit_tr)) matrix(as.numeric(fit_tr$lower), nrow = n_pred, ncol = M, byrow = TRUE) else matrix(NA_real_, nrow = n_pred, ncol = M)
    trt_upper <- if ("upper" %in% names(fit_tr)) matrix(as.numeric(fit_tr$upper), nrow = n_pred, ncol = M, byrow = TRUE) else matrix(NA_real_, nrow = n_pred, ncol = M)
    con_lower <- if ("lower" %in% names(fit_co)) matrix(as.numeric(fit_co$lower), nrow = n_pred, ncol = M, byrow = TRUE) else matrix(NA_real_, nrow = n_pred, ncol = M)
    con_upper <- if ("upper" %in% names(fit_co)) matrix(as.numeric(fit_co$upper), nrow = n_pred, ncol = M, byrow = TRUE) else matrix(NA_real_, nrow = n_pred, ncol = M)
    diff_mat <- trt_mat - con_mat
    diff_lower <- trt_lower - con_upper
    diff_upper <- trt_upper - con_lower

    ps_col <- if (!is.null(ps_full)) ps_full else rep(NA_real_, n_pred)
    out_df <- data.frame(
      id = rep(id_vals, each = M),
      index = rep(p, times = n_pred),
      ps = rep(ps_col, each = M),
      estimate = as.vector(t(diff_mat)),
      lower = as.vector(t(diff_lower)),
      upper = as.vector(t(diff_upper)),
      row.names = NULL
    )
    out_df <- .reorder_predict_cols(out_df)
    attr(out_df, "type") <- type
    attr(out_df, "index") <- p
    attr(out_df, "trt") <- pr_trt
    attr(out_df, "con") <- pr_con
    class(out_df) <- c("causalmixgpd_causal_predict", class(out_df))
    return(.return_out(out_df))
  }

  trt_stats <- .extract_stats(pr_trt, n_pred)
  con_stats <- .extract_stats(pr_con, n_pred)

  diff_est <- trt_stats$estimate - con_stats$estimate
  diff_lower <- trt_stats$lower - con_stats$lower
  diff_upper <- trt_stats$upper - con_stats$upper

  ps_col <- if (!is.null(ps_full)) ps_full else rep(NA_real_, n_pred)
  out <- data.frame(
    id = id_vals,
    ps = ps_col,
    estimate = diff_est,
    lower = diff_lower,
    upper = diff_upper,
    row.names = NULL
  )
  out <- .reorder_predict_cols(out)
  attr(out, "type") <- type
  if (type == "quantile") attr(out, "index") <- p
  attr(out, "trt") <- pr_trt
  attr(out, "con") <- pr_con
  class(out) <- c("causalmixgpd_causal_predict", class(out))
  .return_out(out)
}


.build_ps_bundle <- function(A, X, spec, mcmc) {
  if (!is.matrix(X)) X <- as.matrix(X)
  N <- nrow(X)
  P <- ncol(X)

  if (isTRUE(spec$include_intercept)) {
    X <- cbind(`(Intercept)` = 1, X)
    P <- ncol(X)
  }

  model <- spec$model %||% "logit"
  if (!model %in% c("logit", "probit", "naive")) {
    stop("Unsupported PS model. Supported: logit, probit, naive.", call. = FALSE)
  }

  # Generate NIMBLE code based on model type
  if (model == "logit") {
    code <- nimble::nimbleCode({
      for (i in 1:N) {
        A[i] ~ dbern(pi[i])
        logit(pi[i]) <- inprod(X[i, 1:P], beta[1:P])
      }
      for (j in 1:P) {
        beta[j] ~ dnorm(beta_mean, sd = beta_sd)
      }
    })
  } else if (model == "probit") {
    code <- nimble::nimbleCode({
      for (i in 1:N) {
        A[i] ~ dbern(pi[i])
        probit(pi[i]) <- inprod(X[i, 1:P], beta[1:P])
      }
      for (j in 1:P) {
        beta[j] ~ dnorm(beta_mean, sd = beta_sd)
      }
    })
  } else if (model == "naive") {
    # Naive Bayes: model feature distributions given treatment status
    code <- nimble::nimbleCode({
      for (i in 1:N) {
        A[i] ~ dbern(pi_prior)
        for (j in 1:P) {
          X[i, j] ~ dnorm(mu[A[i] + 1, j], sd = sigma[A[i] + 1, j])
        }
      }
      pi_prior ~ dbeta(1, 1)
      for (k in 1:2) {
        for (j in 1:P) {
          mu[k, j] ~ dnorm(mu_mean, sd = mu_sd)
          sigma[k, j] ~ dunif(sigma_min, sigma_max)
        }
      }
    })
  }

  constants <- list(N = N, P = P)

  # Set model-specific constants, data, inits, and monitors
  if (model %in% c("logit", "probit")) {
    constants$beta_mean <- as.numeric(spec$prior$mean %||% 0)
    constants$beta_sd <- as.numeric(spec$prior$sd %||% 2)
    data <- list(A = as.integer(A), X = X)
    inits <- list(beta = rep(0, P))
    monitors <- c("beta")
  } else if (model == "naive") {
    # Naive Bayes constants
    X_mean <- colMeans(X, na.rm = TRUE)
    X_sd <- apply(X, 2, sd, na.rm = TRUE)
    X_sd <- pmax(X_sd, 0.1)  # Prevent zero SD
    constants$mu_mean <- mean(X_mean, na.rm = TRUE)
    constants$mu_sd <- mean(X_sd, na.rm = TRUE)
    constants$sigma_min <- 0.05
    constants$sigma_max <- 5 * mean(X_sd, na.rm = TRUE)
    data <- list(A = as.integer(A), X = X)
    # Initialize with sample means/sds by treatment group
    init_mu <- matrix(0, nrow = 2, ncol = P)
    init_sigma <- matrix(1, nrow = 2, ncol = P)
    for (k in 0:1) {
      X_k <- X[A == k, , drop = FALSE]
      if (nrow(X_k) > 0) {
        init_mu[k + 1, ] <- colMeans(X_k, na.rm = TRUE)
        init_sigma[k + 1, ] <- pmax(apply(X_k, 2, sd, na.rm = TRUE), 0.1)
      }
    }
    inits <- list(pi_prior = 0.5, mu = init_mu, sigma = init_sigma)
    monitors <- c("pi_prior", "mu", "sigma")
  }

  # Store model type for downstream PS computation
  model_type_name <- if (model == "logit") "ps_logit"
                     else if (model == "probit") "ps_probit"
                     else if (model == "naive") "ps_naive"
                     else "ps_unknown"

  bundle <- list(
    spec = list(
      meta = list(type = model_type_name, include_intercept = isTRUE(spec$include_intercept)),
      model = model,
      prior = spec$prior
    ),
    code = code,
    constants = constants,
    dimensions = list(),
    data = data,
    inits = inits,
    monitors = monitors,
    mcmc = mcmc
  )
  class(bundle) <- "causalmixgpd_ps_bundle"
  bundle
}

.ps_design_matrix <- function(ps_bundle, X_new) {
  stopifnot(inherits(ps_bundle, "causalmixgpd_ps_bundle"))
  X_train <- ps_bundle$data$X
  if (is.null(X_train)) stop("PS bundle missing design matrix.", call. = FALSE)

  include_intercept <- isTRUE(ps_bundle$spec$meta$include_intercept)
  train_cols <- if (!is.null(colnames(X_train))) colnames(X_train) else character(ncol(X_train))
  base_cols <- if (include_intercept) setdiff(train_cols, "(Intercept)") else train_cols
  base_cols <- base_cols[!is.na(base_cols)]

  X_new <- as.matrix(X_new)
  storage.mode(X_new) <- "double"
  if (is.null(nrow(X_new)) || nrow(X_new) < 1) {
    stop("New data must have one or more rows.", call. = FALSE)
  }

  if (length(base_cols) > 0) {
    if (!is.null(colnames(X_new))) {
      if (!setequal(colnames(X_new), base_cols)) {
        stop("Column names of newdata do not match PS design.", call. = FALSE)
      }
      X_new <- X_new[, base_cols, drop = FALSE]
    } else {
      if (ncol(X_new) != length(base_cols)) {
        stop("Newdata must have the same number of columns as the original design.", call. = FALSE)
      }
    }
  } else {
    if (ncol(X_new) != 0L) {
      stop("No covariates expected in newdata for PS-only intercept models.", call. = FALSE)
    }
  }

  if (include_intercept) {
    out <- cbind(`(Intercept)` = 1, X_new)
    colnames(out) <- train_cols
  } else {
    out <- X_new
  }
  storage.mode(out) <- "double"
  out
}

.apply_ps_scale <- function(ps_prob, scale = c("prob", "logit"), clamp = 1e-6) {
  scale <- match.arg(scale)
  ps_prob <- as.numeric(ps_prob)
  if (scale == "prob") return(ps_prob)

  eps <- as.numeric(clamp)[1]
  if (is.finite(eps) && eps > 0) {
    ps_prob <- pmin(pmax(ps_prob, eps), 1 - eps)
  }
  qlogis(ps_prob)
}

.compute_ps_from_fit <- function(ps_fit, ps_bundle, X_new,
                                 summary = c("mean", "median"),
                                 clamp = 1e-6) {
  summary <- match.arg(summary)
  model_type <- ps_bundle$spec$model %||% "logit"
  samples <- as.matrix(ps_fit$mcmc$samples)

  if (model_type %in% c("logit", "probit")) {
    # Linear model PS: logit or probit
    design <- .ps_design_matrix(ps_bundle, X_new)
    beta_cols <- grep("^beta\\[[0-9]+\\]$", colnames(samples), value = TRUE)
    if (!length(beta_cols)) stop("PS beta draws not found.", call. = FALSE)

    beta_inds <- as.integer(sub("^beta\\[([0-9]+)\\]$", "\\1", beta_cols))
    order_idx <- order(beta_inds, na.last = NA)
    beta_mat <- samples[, beta_cols, drop = FALSE]
    beta_mat <- beta_mat[, order_idx, drop = FALSE]  # S x P

    if (ncol(design) != ncol(beta_mat)) {
      stop("Mismatch between PS design columns and beta coefficients.", call. = FALSE)
    }

    # Determine inverse link function
    inv_link <- if (model_type == "logit") plogis else if (model_type == "probit") pnorm else plogis

    # Posterior predictive PS: average inverse link over beta draws
    eta <- beta_mat %*% t(design)  # S x n_pred
    ps_draws <- inv_link(eta)
    ps_vec <- if (summary == "mean") {
      colMeans(ps_draws)
    } else {
      apply(ps_draws, 2, stats::median)
    }
    eps <- as.numeric(clamp)[1]
    if (is.finite(eps) && eps > 0) {
      ps_vec <- pmin(pmax(ps_vec, eps), 1 - eps)
    }
    ps_vec

  } else if (model_type == "naive") {
    # Naive Bayes: use Bayes rule with learned feature distributions
    X_new <- as.matrix(X_new)
    storage.mode(X_new) <- "double"
    X_train <- ps_bundle$data$X %||% NULL
    if (is.null(X_train)) stop("PS bundle missing training design matrix.", call. = FALSE)

    include_intercept <- isTRUE(ps_bundle$spec$meta$include_intercept)
    if (include_intercept && ncol(X_new) == (ncol(X_train) - 1L)) {
      X_new <- cbind(`(Intercept)` = 1, X_new)
    }

    train_cols <- colnames(X_train)
    if (!is.null(train_cols) && length(train_cols) == ncol(X_new)) {
      new_cols <- colnames(X_new)
      if (!is.null(new_cols) && all(train_cols %in% new_cols)) {
        X_new <- X_new[, train_cols, drop = FALSE]
      }
    }

    if (ncol(X_new) != ncol(X_train)) {
      stop("Mismatch between PS training features and newdata columns.", call. = FALSE)
    }

    n_pred <- nrow(X_new)
    S <- nrow(samples)  # Number of MCMC iterations

    # Extract posterior samples
    pi_cols <- grep("^pi_prior$", colnames(samples), value = TRUE)
    mu_cols <- grep("^mu\\[", colnames(samples), value = TRUE)
    sigma_cols <- grep("^sigma\\[", colnames(samples), value = TRUE)

    if (!length(pi_cols) || !length(mu_cols) || !length(sigma_cols)) {
      stop("Naive Bayes PS samples missing (pi_prior, mu, or sigma).", call. = FALSE)
    }

    # Extract samples as matrices
    pi_samples <- samples[, pi_cols, drop = FALSE]
    mu_samples <- samples[, mu_cols, drop = FALSE]
    sigma_samples <- samples[, sigma_cols, drop = FALSE]

    # Initialize PS matrix
    ps_draws <- matrix(0, nrow = S, ncol = n_pred)

    # For each posterior sample, compute P(A=1|X) using Bayes rule
    for (s in 1:S) {
      pi_prior <- pi_samples[s, 1]  # P(A=1)

      # Reshape mu and sigma from samples to (K=2, P) matrices
      # mu and sigma are stored as mu[1,1], mu[1,2], ..., mu[2,1], mu[2,2], ...
      P <- ncol(X_new)
      mu_mat <- matrix(mu_samples[s, ], nrow = 2, ncol = P, byrow = FALSE)
      sigma_mat <- matrix(sigma_samples[s, ], nrow = 2, ncol = P, byrow = FALSE)

      # Compute likelihood P(X|A=k) for each treatment group
      # Using independence: P(X|A) = prod_j P(X_j|A)
      log_lik_t0 <- matrix(0, nrow = n_pred, ncol = P)
      log_lik_t1 <- matrix(0, nrow = n_pred, ncol = P)

      for (j in 1:P) {
        log_lik_t0[, j] <- dnorm(X_new[, j], mean = mu_mat[1, j], sd = sigma_mat[1, j], log = TRUE)
        log_lik_t1[, j] <- dnorm(X_new[, j], mean = mu_mat[2, j], sd = sigma_mat[2, j], log = TRUE)
      }

      # Sum across features (log scale)
      log_lik_t0_sum <- rowSums(log_lik_t0)
      log_lik_t1_sum <- rowSums(log_lik_t1)

      # Apply Bayes rule: P(A=1|X) = P(X|A=1)P(A=1) / [P(X|A=1)P(A=1) + P(X|A=0)P(A=0)]
      # Using log-sum-exp trick for numerical stability
      log_post_t0 <- log(1 - pi_prior) + log_lik_t0_sum
      log_post_t1 <- log(pi_prior) + log_lik_t1_sum

      # Convert back from log scale
      max_log <- pmax(log_post_t0, log_post_t1)
      ps_draws[s, ] <- exp(log_post_t1 - max_log) / (exp(log_post_t0 - max_log) + exp(log_post_t1 - max_log))
    }

    ps_vec <- if (summary == "mean") {
      colMeans(ps_draws)
    } else {
      apply(ps_draws, 2, stats::median)
    }
    eps <- as.numeric(clamp)[1]
    if (is.finite(eps) && eps > 0) {
      ps_vec <- pmin(pmax(ps_vec, eps), 1 - eps)
    }
    ps_vec
  }
}

