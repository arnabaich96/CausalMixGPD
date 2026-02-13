#' Build a causal bundle (design + two outcome arms)
#'
#' Creates a causal bundle with:
#' \itemize{
#'   \item a propensity score (PS) design model (logit/probit regression of \code{T} on \code{X} or a naive Bayes classifier)
#'   \item an outcome bundle for the control arm (\code{T = 0})
#'   \item an outcome bundle for the treated arm (\code{T = 1})
#' }
#'
#' The outcome bundles reuse the existing DPM + optional GPD tail machinery. The PS model is a
#' lightweight NIMBLE estimator supporting logit/probit regression or a naive Bayes classifier with
#' simple priors.
#'
#' @param y Numeric outcome vector.
#' @param X Design matrix or data.frame of covariates (N x P).
#' @param T Binary treatment indicator (length N, values 0/1).
#' @param backend Character; \code{"sb"} or \code{"crp"} for outcome models. If length 2,
#'   the first entry is used for treated (\code{T=1}) and the second for control (\code{T=0}).
#' @param kernel Character kernel name for outcome models. If length 2,
#'   the first entry is used for treated (\code{T=1}) and the second for control (\code{T=0}).
#' @param GPD Logical; include GPD tail for outcomes if TRUE. If length 2,
#'   the first entry is used for treated (\code{T=1}) and the second for control (\code{T=0}).
#' @param components Integer >= 2; truncation parameter for outcome mixtures. If length 2,
#'   the first entry is used for treated (\code{T=1}) and the second for control (\code{T=0}).
#' @param param_specs Outcome parameter overrides (same structure as \code{build_nimble_bundle()}).
#'   You can pass a single list used for both arms or a list with \code{con} and \code{trt} entries.
#' @param mcmc_outcome MCMC settings list for the outcome bundles.
#' @param mcmc_ps MCMC settings list for the PS model.
#' @param epsilon Numeric in [0,1) used by outcome bundles for posterior truncation summaries.
#'   If length 2, the first entry is used for treated (\code{T=1}) and the second for control (\code{T=0}).
#' @param alpha_random Logical; whether outcome concentration \code{alpha} is stochastic.
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
#' @return A list of class \code{"dpmixgpd_causal_bundle"}.
#' @examples
#' \dontrun{
#' set.seed(1)
#' N <- 100
#' X <- cbind(x1 = rnorm(N), x2 = runif(N))
#' T <- rbinom(N, 1, plogis(0.3 + 0.5 * X[, 1]))
#' y <- rexp(N) + 0.1
#'
#' cb <- build_causal_bundle(
#'   y = y,
#'   X = X,
#'   T = T,
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
    T,
    backend = c("sb", "crp"),
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
    ps_clamp = 1e-6
) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  .arm_value <- function(val, name) {
    if (length(val) == 1L) return(list(trt = val, con = val))
    if (length(val) == 2L) return(list(trt = val[[1]], con = val[[2]]))
    stop(sprintf("%s must be length 1 or length 2.", name), call. = FALSE)
  }

  backend <- .arm_value(backend, "backend")
  backend$trt <- match.arg(backend$trt, choices = c("sb", "crp"))
  backend$con <- match.arg(backend$con, choices = c("sb", "crp"))
  ps_scale <- match.arg(ps_scale)
  ps_summary <- match.arg(ps_summary)

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

  T <- as.integer(T)
  if (length(T) != length(y)) stop("T must have the same length as y.", call. = FALSE)
  if (anyNA(T) || !all(T %in% c(0L, 1L))) stop("T must be binary (0/1) with no NA.", call. = FALSE)

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

  idx_con <- which(T == 0L)
  idx_trt <- which(T == 1L)
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
    ps_bundle <- .build_ps_bundle(T = T, X = X, spec = ps_spec, mcmc = mcmc_ps)
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
    alpha_random = alpha_random
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
    alpha_random = alpha_random
  )

  out <- list(
    design = ps_bundle,
    outcome = list(con = bundle_con, trt = bundle_trt),
    data = list(y = y, X = X, T = T),
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
      )
    ),
    call = match.call()
  )
  class(out) <- "dpmixgpd_causal_bundle"
  out
}

.run_ps_mcmc_bundle <- function(bundle, show_progress = TRUE) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  stopifnot(inherits(bundle, "dpmixgpd_ps_bundle"))
  if (!"package:nimble" %in% search()) {
    suppressPackageStartupMessages(base::require("nimble", quietly = TRUE, warn.conflicts = FALSE))
  }

  code <- .extract_nimble_code(bundle$code)
  constants <- bundle$constants %||% list()
  data <- bundle$data %||% list()
  inits <- bundle$inits %||% list()
  m <- bundle$mcmc %||% list()
  monitors <- bundle$monitors %||% "beta"

  Rmodel <- nimble::nimbleModel(
    code = code,
    data = data,
    constants = constants,
    inits = inits,
    check = TRUE,
    calculate = FALSE
  )

  conf <- nimble::configureMCMC(
    Rmodel,
    monitors = monitors,
    enableWAIC = FALSE
  )

  Rmcmc <- nimble::buildMCMC(conf)

  Cmodel <- nimble::compileNimble(Rmodel, showCompilerOutput = FALSE)
  Cmcmc  <- nimble::compileNimble(Rmcmc, project = Rmodel, showCompilerOutput = FALSE)

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

  res <- nimble::runMCMC(
    Cmcmc,
    niter = niter,
    nburnin = nburnin,
    thin = thin,
    nchains = nchains,
    inits = inits_list,
    setSeed = seed,
    progressBar = isTRUE(show_progress),
    samplesAsCodaMCMC = TRUE
  )

  fit <- list(
    mcmc = list(samples = res),
    bundle = bundle,
    call = match.call()
  )
  class(fit) <- "dpmixgpd_ps_fit"
  fit
}

#' Run MCMC for a causal bundle
#'
#' Executes the outcome arms (and PS model if enabled), returning a single causal fit.
#' When \code{PS=FALSE} in the bundle, the PS model is skipped entirely.
#'
#' @param bundle A \code{"dpmixgpd_causal_bundle"} from \code{build_causal_bundle()}.
#' @param show_progress Logical; passed to nimble for each block.
#' @return A list of class \code{"dpmixgpd_causal_fit"}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal")
#' fit <- run_mcmc_causal(cb)
#' }
#' @export
run_mcmc_causal <- function(bundle, show_progress = TRUE) {
  stopifnot(inherits(bundle, "dpmixgpd_causal_bundle"))

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
    ps_fit <- .run_ps_mcmc_bundle(bundle$design, show_progress = show_progress)
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

  # Regenerate code/constants/monitors (with or without PS)
  for (arm in c("con", "trt")) {
    b <- bundle$outcome[[arm]]
    b$code <- build_code_from_spec(b$spec)
    b$constants <- build_constants_from_spec(b$spec)
    b$monitors <- build_monitors_from_spec(b$spec)
    bundle$outcome[[arm]] <- b
  }

  con_fit <- run_mcmc_bundle_manual(bundle$outcome$con, show_progress = show_progress)
  trt_fit <- run_mcmc_bundle_manual(bundle$outcome$trt, show_progress = show_progress)

  # Attach PS model if available
  if (!is.null(ps_model)) {
    con_fit$ps_model <- ps_model
    trt_fit$ps_model <- ps_model
  }

  out <- list(
    ps_fit = ps_fit,
    outcome_fit = list(con = con_fit, trt = trt_fit),
    bundle = bundle,
    ps_hat = ps_hat,
    ps_cov = ps_cov,
    call = match.call()
  )
  class(out) <- "dpmixgpd_causal_fit"
  out
}

#' Conditional quantile treatment effects (CQTE)
#'
#' Computes treated-minus-control quantiles from a causal fit.
#'
#' @param fit A \code{"dpmixgpd_causal_fit"} object from \code{run_mcmc_causal()}.
#' @param probs Numeric vector of probabilities in (0, 1) specifying the quantile levels
#'   of the outcome distribution to estimate treatment effects at.
#' @param newdata Optional data.frame or matrix of covariates for prediction.
#' @param interval Character or NULL; type of credible interval: \code{NULL} for no interval,
#'   \code{"credible"} for equal-tailed quantile intervals (default), or \code{"hpd"} for
#'   highest posterior density intervals.
#' @param level Numeric credible level for intervals (default 0.95 for 95 percent CI).
#' @return A list with elements \code{fit} (CQTE), \code{grid} (probabilities),
#'   and the treated/control prediction objects.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", components = 6)
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
                level = 0.95) {
  stopifnot(inherits(fit, "dpmixgpd_causal_fit"))

  # Handle interval: NULL or "none" mean no interval; otherwise match to credible/hpd
  compute_interval <- TRUE
  if (is.character(interval) && length(interval) == 1L && identical(tolower(interval), "none")) {
    compute_interval <- FALSE
    interval <- "credible"  # placeholder for downstream
  } else if (is.null(interval)) {
    compute_interval <- FALSE
    interval <- "credible"  # placeholder for downstream
  } else {
    interval <- match.arg(interval, choices = c("credible", "hpd"))
  }

  # Validate level parameter
  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    stop("'level' must be a numeric value between 0 and 1.", call. = FALSE)
  }
  x_pred <- newdata %||% (fit$bundle$data$X %||% NULL)

  ps_meta <- fit$bundle$meta$ps %||% list()
  ps_enabled <- isTRUE(ps_meta$enabled) && isTRUE(fit$bundle$meta$has_x)
  ps_scale <- fit$bundle$meta$ps_scale %||% "logit"
  ps_summary <- fit$bundle$meta$ps_summary %||% "mean"
  ps_clamp <- fit$bundle$meta$ps_clamp %||% 1e-6

  ps_prob <- NULL
  ps_cov <- NULL
  if (!is.null(x_pred) && ps_enabled) {
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
  }

  pr_trt <- predict(fit$outcome_fit$trt, x = x_pred, type = "quantile", index = probs,
                    ps = ps_cov,
                    interval = if (compute_interval) interval else NULL,
                    store_draws = TRUE)
  pr_con <- predict(fit$outcome_fit$con, x = x_pred, type = "quantile", index = probs,
                    ps = ps_cov,
                    interval = if (compute_interval) interval else NULL,
                    store_draws = TRUE)

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
  if (!identical(dim(pr_trt$draws), dim(pr_con$draws))) {
    stop("Treated and control posterior draws must have matching dimensions for CQTE.", call. = FALSE)
  }

  diff_draws <- pr_trt$draws - pr_con$draws  # dims: S x n_pred x length(probs)
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
    index = rep(probs, each = n_pred),
    id = if (n_pred > 1L) rep(seq_len(n_pred), times = n_prob) else rep(1L, n_prob),
    estimate = as.vector(fit_mat),
    lower = if (!is.null(lower)) as.vector(lower) else NA_real_,
    upper = if (!is.null(upper)) as.vector(upper) else NA_real_
  )

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
  class(out) <- "dpmixgpd_qte"
  out
}


#' Conditional average treatment effects (CATE)
#'
#' Computes treated-minus-control posterior means from a causal fit.
#'
#' @param fit A \code{"dpmixgpd_causal_fit"} object from \code{run_mcmc_causal()}.
#' @param newdata Optional data.frame or matrix of covariates for prediction.
#' @param type Character; \code{"mean"} (default) for ordinary mean CATE or
#'   \code{"rmean"} for restricted-mean CATE.
#' @param cutoff Finite numeric cutoff for restricted mean; required for
#'   \code{type = "rmean"}, ignored otherwise.
#' @param interval Character or NULL; type of credible interval: \code{NULL} for no interval,
#'   \code{"credible"} for equal-tailed quantile intervals (default), or \code{"hpd"} for
#'   highest posterior density intervals.
#' @param level Numeric credible level for intervals (default 0.95 for 95 percent CI).
#' @param nsim_mean Number of posterior predictive draws to approximate the mean.
#' @return A list with elements \code{fit} (CATE), optional \code{lower}/\code{upper},
#'   and the treated/control prediction objects.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", components = 6)
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
                nsim_mean = 200L) {
  stopifnot(inherits(fit, "dpmixgpd_causal_fit"))

  type <- match.arg(type)

  # Handle interval: NULL or "none" means no interval, otherwise match to credible/hpd
  compute_interval <- TRUE
  if (is.character(interval) && length(interval) == 1L && identical(tolower(interval), "none")) {
    compute_interval <- FALSE
    interval <- "credible"  # placeholder for downstream
  } else if (is.null(interval)) {
    compute_interval <- FALSE
    interval <- "credible"  # placeholder for downstream
  } else {
    interval <- match.arg(interval, choices = c("credible", "hpd"))
  }

  # Validate level parameter
  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    stop("'level' must be a numeric value between 0 and 1.", call. = FALSE)
  }

  x_pred <- newdata %||% (fit$bundle$data$X %||% NULL)

  ps_meta <- fit$bundle$meta$ps %||% list()
  ps_enabled <- isTRUE(ps_meta$enabled) && isTRUE(fit$bundle$meta$has_x)
  ps_scale <- fit$bundle$meta$ps_scale %||% "logit"
  ps_summary <- fit$bundle$meta$ps_summary %||% "mean"
  ps_clamp <- fit$bundle$meta$ps_clamp %||% 1e-6

  ps_prob <- NULL
  ps_cov <- NULL
  if (!is.null(x_pred) && ps_enabled) {
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
  }

  pr_trt <- predict(fit$outcome_fit$trt, x = x_pred, type = type,
                    cutoff = cutoff,
                    ps = ps_cov, interval = if (compute_interval) interval else NULL,
                    cred.level = level, nsim_mean = nsim_mean, store_draws = TRUE)
  pr_con <- predict(fit$outcome_fit$con, x = x_pred, type = type,
                    cutoff = cutoff,
                    ps = ps_cov, interval = if (compute_interval) interval else NULL,
                    cred.level = level, nsim_mean = nsim_mean, store_draws = TRUE)

  if (is.null(pr_trt$draws) || is.null(pr_con$draws)) {
    stop("CATE requires stored posterior mean draws; set store_draws=TRUE in predict().", call. = FALSE)
  }
  if (is.null(dim(pr_trt$draws))) {
    pr_trt$draws <- matrix(pr_trt$draws, ncol = 1L)
  }
  if (is.null(dim(pr_con$draws))) {
    pr_con$draws <- matrix(pr_con$draws, ncol = 1L)
  }
  if (!identical(dim(pr_trt$draws), dim(pr_con$draws))) {
    stop("Treated and control posterior draws must have matching dimensions for CATE.", call. = FALSE)
  }

  diff_draws <- pr_trt$draws - pr_con$draws  # dims: S x n_pred
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
  class(out) <- "dpmixgpd_ate"
  out
}

.causal_qte_draws_array <- function(draws) {
  if (is.null(draws)) return(NULL)
  if (is.null(dim(draws))) return(array(as.numeric(draws), dim = c(length(draws), 1L, 1L)))
  d <- dim(draws)
  if (length(d) == 2L) {
    M <- d[1]
    S <- d[2]
    arr <- array(NA_real_, dim = c(S, 1L, M))
    for (s in seq_len(S)) arr[s, 1L, ] <- draws[, s]
    return(arr)
  }
  if (length(d) == 3L) return(draws)
  stop("Unexpected quantile draw dimensions in causal treatment effects.", call. = FALSE)
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

  trt_draws <- .causal_qte_draws_array(obj$trt$draws %||% NULL)
  con_draws <- .causal_qte_draws_array(obj$con$draws %||% NULL)
  if (is.null(trt_draws) || is.null(con_draws)) {
    stop("QTE aggregation requires treated/control quantile draws.", call. = FALSE)
  }
  if (!identical(dim(trt_draws), dim(con_draws))) {
    stop("Treated and control quantile draws must match for aggregation.", call. = FALSE)
  }

  idx <- unique(as.integer(idx))
  idx <- idx[idx >= 1L & idx <= dim(trt_draws)[2]]
  if (!length(idx)) stop("No valid rows selected for QTE aggregation.", call. = FALSE)

  S <- dim(trt_draws)[1]
  M <- dim(trt_draws)[3]
  trt_avg <- matrix(apply(trt_draws[, idx, , drop = FALSE], c(1, 3), mean, na.rm = TRUE), nrow = S, ncol = M)
  con_avg <- matrix(apply(con_draws[, idx, , drop = FALSE], c(1, 3), mean, na.rm = TRUE), nrow = S, ncol = M)
  diff_avg <- trt_avg - con_avg

  diff_summ <- .causal_summarize_draw_cols(diff_avg, compute_interval = compute_interval, interval = interval, level = level)
  trt_summ <- .causal_summarize_draw_cols(trt_avg, compute_interval = compute_interval, interval = interval, level = level)
  con_summ <- .causal_summarize_draw_cols(con_avg, compute_interval = compute_interval, interval = interval, level = level)

  fit_mat <- matrix(diff_summ$estimate, nrow = 1L, ncol = M)
  lower <- upper <- NULL
  if (compute_interval) {
    lower <- matrix(diff_summ$lower, nrow = 1L, ncol = M)
    upper <- matrix(diff_summ$upper, nrow = 1L, ncol = M)
  }

  qte_fit <- data.frame(
    index = probs,
    id = rep(1L, length(probs)),
    estimate = as.vector(fit_mat),
    lower = if (!is.null(lower)) as.vector(lower) else NA_real_,
    upper = if (!is.null(upper)) as.vector(upper) else NA_real_
  )

  trt_fit <- data.frame(
    estimate = as.numeric(trt_summ$estimate),
    index = probs,
    id = rep(1L, length(probs)),
    lower = as.numeric(trt_summ$lower),
    upper = as.numeric(trt_summ$upper)
  )
  con_fit <- data.frame(
    estimate = as.numeric(con_summ$estimate),
    index = probs,
    id = rep(1L, length(probs)),
    lower = as.numeric(con_summ$lower),
    upper = as.numeric(con_summ$upper)
  )

  trt_draws_out <- array(NA_real_, dim = c(S, 1L, M))
  con_draws_out <- array(NA_real_, dim = c(S, 1L, M))
  diff_draws_out <- array(NA_real_, dim = c(S, 1L, M))
  for (j in seq_len(M)) {
    trt_draws_out[, 1L, j] <- trt_avg[, j]
    con_draws_out[, 1L, j] <- con_avg[, j]
    diff_draws_out[, 1L, j] <- diff_avg[, j]
  }

  pr_trt <- obj$trt
  pr_con <- obj$con
  pr_trt$fit <- trt_fit
  pr_con$fit <- con_fit
  pr_trt$draws <- trt_draws_out
  pr_con$draws <- con_draws_out

  out <- list(
    fit = fit_mat,
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
  class(out) <- "dpmixgpd_qte"
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
  class(out) <- "dpmixgpd_ate"
  out
}

#' Quantile treatment effects (QTE), marginal over training covariates
#'
#' Computes a marginal quantile treatment effect by averaging conditional
#' quantile effects over the training covariate rows.
#'
#' @param fit A \code{"dpmixgpd_causal_fit"} object from \code{run_mcmc_causal()}.
#' @param probs Numeric vector of probabilities in (0, 1) specifying the quantile levels
#'   of the outcome distribution to estimate treatment effects at.
#' @param newdata Deprecated placeholder for marginal estimands; must be \code{NULL}.
#' @param y Deprecated placeholder for marginal estimands; must be \code{NULL}.
#' @param interval Character or NULL; type of credible interval: \code{NULL} for no interval,
#'   \code{"credible"} for equal-tailed quantile intervals (default), or \code{"hpd"} for
#'   highest posterior density intervals.
#' @param level Numeric credible level for intervals (default 0.95 for 95 percent CI).
#' @return A list with elements \code{fit} (QTE), \code{grid} (probabilities),
#'   and aggregated treated/control prediction objects.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", components = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' qte(fit, probs = c(0.5, 0.9))
#' }
#' @export
qte <- function(fit,
                probs = c(0.1, 0.5, 0.9),
                newdata = NULL,
                y = NULL,
                interval = "credible",
                level = 0.95) {
  stopifnot(inherits(fit, "dpmixgpd_causal_fit"))
  if (!is.null(newdata) || !is.null(y)) {
    stop("qte() computes a marginal estimand on training data only. Use cqte() for conditional effects with 'newdata'.", call. = FALSE)
  }

  cq <- cqte(
    fit = fit,
    probs = probs,
    newdata = fit$bundle$data$X %||% NULL,
    interval = interval,
    level = level
  )
  idx <- .causal_effect_subset_index(fit = fit, n_pred = cq$n_pred %||% 1L, subset = "all")
  .causal_aggregate_qte(cq, idx = idx, effect_type = "qte")
}

#' Quantile treatment effect on the treated (QTT)
#'
#' Computes a treated-only marginal quantile treatment effect by averaging
#' conditional quantile effects over rows with assigned treatment \code{T=1}.
#'
#' @inheritParams qte
#' @return A list with elements \code{fit} (QTT), \code{grid} (probabilities),
#'   and aggregated treated/control prediction objects.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", components = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' qtt(fit, probs = c(0.5, 0.9))
#' }
#' @export
qtt <- function(fit,
                probs = c(0.1, 0.5, 0.9),
                newdata = NULL,
                y = NULL,
                interval = "credible",
                level = 0.95) {
  stopifnot(inherits(fit, "dpmixgpd_causal_fit"))
  if (!is.null(newdata) || !is.null(y)) {
    stop("qtt() computes a treated-only marginal estimand on training data only. Use cqte() for conditional effects with 'newdata'.", call. = FALSE)
  }

  cq <- cqte(
    fit = fit,
    probs = probs,
    newdata = fit$bundle$data$X %||% NULL,
    interval = interval,
    level = level
  )
  idx <- .causal_effect_subset_index(fit = fit, n_pred = cq$n_pred %||% 1L, subset = "treated")
  .causal_aggregate_qte(cq, idx = idx, effect_type = "qtt")
}

#' Average treatment effects (ATE), marginal over training covariates
#'
#' Computes a marginal average treatment effect by averaging conditional
#' treatment effects over the training covariate rows.
#'
#' @param fit A \code{"dpmixgpd_causal_fit"} object from \code{run_mcmc_causal()}.
#' @param newdata Deprecated placeholder for marginal estimands; must be \code{NULL}.
#' @param y Deprecated placeholder for marginal estimands; must be \code{NULL}.
#' @param type Character; \code{"mean"} (default) for ordinary mean ATE or
#'   \code{"rmean"} for restricted-mean ATE.
#' @param cutoff Finite numeric cutoff for restricted mean; required for
#'   \code{type = "rmean"}, ignored otherwise.
#' @param interval Character or NULL; type of credible interval: \code{NULL} for no interval,
#'   \code{"credible"} for equal-tailed quantile intervals (default), or \code{"hpd"} for
#'   highest posterior density intervals.
#' @param level Numeric credible level for intervals (default 0.95 for 95 percent CI).
#' @param nsim_mean Number of posterior predictive draws to approximate the mean.
#' @return A list with elements \code{fit} (ATE), optional \code{lower}/\code{upper},
#'   and aggregated treated/control prediction objects.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", components = 6)
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
                nsim_mean = 200L) {
  stopifnot(inherits(fit, "dpmixgpd_causal_fit"))
  if (!is.null(newdata) || !is.null(y)) {
    stop("ate() computes a marginal estimand on training data only. Use cate() for conditional effects with 'newdata'.", call. = FALSE)
  }

  ca <- cate(
    fit = fit,
    newdata = fit$bundle$data$X %||% NULL,
    type = type,
    cutoff = cutoff,
    interval = interval,
    level = level,
    nsim_mean = nsim_mean
  )
  idx <- .causal_effect_subset_index(fit = fit, n_pred = ca$n_pred %||% 1L, subset = "all")
  .causal_aggregate_ate(ca, idx = idx, effect_type = "ate")
}

#' Average treatment effect on the treated (ATT)
#'
#' Computes a treated-only marginal average treatment effect by averaging
#' conditional treatment effects over rows with assigned treatment \code{T=1}.
#'
#' @inheritParams ate
#' @return A list with elements \code{fit} (ATT), optional \code{lower}/\code{upper},
#'   and aggregated treated/control prediction objects.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", components = 6)
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
                nsim_mean = 200L) {
  stopifnot(inherits(fit, "dpmixgpd_causal_fit"))
  if (!is.null(newdata) || !is.null(y)) {
    stop("att() computes a treated-only marginal estimand on training data only. Use cate() for conditional effects with 'newdata'.", call. = FALSE)
  }

  ca <- cate(
    fit = fit,
    newdata = fit$bundle$data$X %||% NULL,
    type = type,
    cutoff = cutoff,
    interval = interval,
    level = level,
    nsim_mean = nsim_mean
  )
  idx <- .causal_effect_subset_index(fit = fit, n_pred = ca$n_pred %||% 1L, subset = "treated")
  .causal_aggregate_ate(ca, idx = idx, effect_type = "att")
}


#' Restricted-mean ATE (finite under heavy tails)
#'
#' Computes an average treatment effect on the restricted mean scale,
#' \eqn{E[min(Y, cutoff)]}, which is finite even when the ordinary mean does not exist
#' under heavy-tailed GPD regimes.
#'
#' @inheritParams cate
#' @param cutoff Finite numeric cutoff for the restricted mean.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal",
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
                      nsim_mean = 200L) {
  cate(fit = fit,
       newdata = newdata,
       type = "rmean",
       cutoff = cutoff,
       interval = interval,
       level = level,
       nsim_mean = nsim_mean)
}


#' Predict from a causal fit
#'
#' Provides a unified interface to the treated and control outcome models while
#' guaranteeing that the same propensity scores are used for both arms. For new
#' covariates, the PS model stored in \code{object$ps_fit} is used to estimate
#' the required scores unless the user supplies their own via \code{ps}. If the
#' bundle was built with \code{PS=FALSE}, the PS model does not exist and outcome
#' predictions use only the original covariates \code{X}.
#'
#' @inheritParams predict.mixgpd_fit
#' @param object A \code{"dpmixgpd_causal_fit"} object returned by
#'   \code{run_mcmc_causal()}.
#' @param ps Optional numeric vector of propensity scores aligned with \code{x}
#'   / \code{newdata}. When provided, the supplied scores are used instead of
#'   recomputing them from the stored PS model (needed only for custom inputs).
#' @param type Prediction type. Supported: \code{"mean"}, \code{"quantile"},
#'   \code{"density"}, \code{"survival"}, \code{"prob"}.
#' @param interval Character or NULL; type of credible interval: \code{NULL} for no interval,
#'   \code{"credible"} for equal-tailed quantile intervals (default), or \code{"hpd"} for
#'   highest posterior density intervals.
#' @return For \code{"mean"} or \code{"quantile"}, a numeric matrix with columns
#'   \code{ps}, \code{estimate}, \code{lower}, \code{upper}, representing
#'   treated-minus-control posterior summaries. When PS is disabled or X is absent,
#'   \code{ps} is \code{NA} and no PS is used. For \code{"density"}, \code{"survival"},
#'   and \code{"prob"},
#'   a data frame with columns \code{y}, \code{ps}, \code{trt_estimate}, \code{trt_lower},
#'   \code{trt_upper}, \code{con_estimate}, \code{con_lower}, \code{con_upper}.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal")
#' fit <- run_mcmc_causal(cb)
#' predict(fit, x = X[1:10, ], type = "quantile", p = c(0.25, 0.5, 0.75))
#' predict(fit, x = X[1:10, ], type = "mean", interval = "hpd")  # HPD intervals
#' predict(fit, x = X[1:10, ], type = "mean", interval = NULL)   # No intervals
#' }
#' @export
predict.dpmixgpd_causal_fit <- function(object,
                                        x = NULL,
                                        y = NULL,
                                        ps = NULL,
                                        newdata = NULL,
                                        type = c("mean", "quantile", "density", "survival", "prob", "location"),
                                        p = NULL,
                                        nsim = NULL,
                                        interval = "credible",
                                        probs = c(0.025, 0.5, 0.975),
                                        store_draws = TRUE,
                                        nsim_mean = 200L,
                                        ncores = 1L,
                                        ...) {
  stopifnot(inherits(object, "dpmixgpd_causal_fit"))

  if (!is.null(newdata) && !is.null(x)) {
    stop("Provide only one of 'x' or 'newdata' (they are aliases).", call. = FALSE)
  }
  if (!is.null(newdata)) x <- newdata

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

  ps_enabled <- isTRUE(bundle$meta$ps$enabled) && has_X
  ps_include_in_outcome <- isTRUE(bundle$meta$ps$include_in_outcome)
  if (is.null(ps_include_in_outcome)) ps_include_in_outcome <- ps_enabled
  ps_model_type <- bundle$meta$ps$model_type %||% FALSE
  ps_scale <- bundle$meta$ps_scale %||% "logit"
  ps_summary <- bundle$meta$ps_summary %||% "mean"
  ps_clamp <- bundle$meta$ps_clamp %||% 1e-6

  if (type == "quantile") {
    if (is.null(p) || length(p) == 0 || any(!is.finite(as.numeric(p)))) {
      stop("Causal predict for quantile requires one or more finite probabilities in 'p'.", call. = FALSE)
    }
    p <- as.numeric(p)
    if (any(p <= 0 | p >= 1)) {
      stop("Probabilities in 'p' must be in (0, 1).", call. = FALSE)
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
          type = pred_type,
          interval = if (compute_interval) interval else NULL,
          probs = probs,
          store_draws = FALSE,
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
    class(out) <- c("dpmixgpd_causal_predict", class(out))
    return(out)
  }

  pr_trt <- predict(
    object$outcome_fit$trt,
    x = x,
    y = y,
    ps = ps_trt,
    type = type,
    index = if (type == "quantile") p else NULL,
    nsim = nsim,
    interval = if (compute_interval) interval else NULL,
    probs = probs,
    store_draws = store_draws,
    nsim_mean = nsim_mean,
    ncores = ncores,
    ...
  )

  pr_con <- predict(
    object$outcome_fit$con,
    x = x,
    y = y,
    ps = ps_con,
    type = type,
    index = if (type == "quantile") p else NULL,
    nsim = nsim,
    interval = if (compute_interval) interval else NULL,
    probs = probs,
    store_draws = store_draws,
    nsim_mean = nsim_mean,
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
    if (!identical(dim(trt_draws), dim(con_draws))) {
      stop("Treated and control posterior draws must have matching dimensions.", call. = FALSE)
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
    ps_col <- if (!is.null(ps_full)) ps_full else rep(NA_real_, n_pred)

    if (length(p) > 1L) {
      out_df <- data.frame(
        id = rep(seq_len(n_pred), times = length(p)),
        index = rep(p, each = n_pred),
        ps = rep(ps_col, times = length(p)),
        estimate = as.vector(est),
        lower = if (compute_interval) as.vector(lower) else NA_real_,
        upper = if (compute_interval) as.vector(upper) else NA_real_,
        row.names = NULL
      )
      attr(out_df, "type") <- type
      attr(out_df, "index") <- p
      attr(out_df, "trt") <- pr_trt
      attr(out_df, "con") <- pr_con
      class(out_df) <- c("dpmixgpd_causal_predict", class(out_df))
      return(out_df)
    }

    est_vec <- as.numeric(est)
    lower_vec <- if (compute_interval) as.numeric(lower) else rep(NA_real_, n_pred)
    upper_vec <- if (compute_interval) as.numeric(upper) else rep(NA_real_, n_pred)
    out <- cbind(ps = ps_col, estimate = est_vec, lower = lower_vec, upper = upper_vec)
    attr(out, "type") <- type
    attr(out, "index") <- p
    attr(out, "trt") <- pr_trt
    attr(out, "con") <- pr_con
    class(out) <- c("dpmixgpd_causal_predict", class(out))
    return(out)
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
    for (j in seq_len(M)) {
      rows <- ((j - 1L) * n_pred + 1L):(j * n_pred)
      trt_mat[, j] <- as.numeric(fit_tr$estimate[rows])
      trt_lower[, j] <- if ("lower" %in% names(fit_tr)) as.numeric(fit_tr$lower[rows]) else NA_real_
      trt_upper[, j] <- if ("upper" %in% names(fit_tr)) as.numeric(fit_tr$upper[rows]) else NA_real_
      con_mat[, j] <- as.numeric(fit_co$estimate[rows])
      con_lower[, j] <- if ("lower" %in% names(fit_co)) as.numeric(fit_co$lower[rows]) else NA_real_
      con_upper[, j] <- if ("upper" %in% names(fit_co)) as.numeric(fit_co$upper[rows]) else NA_real_
    }
    diff_mat <- trt_mat - con_mat
    diff_lower <- trt_lower - con_upper
    diff_upper <- trt_upper - con_lower

    ps_col <- if (!is.null(ps_full)) ps_full else rep(NA_real_, n_pred)
    out_df <- data.frame(
      id = rep(seq_len(n_pred), times = M),
      index = rep(p, each = n_pred),
      ps = rep(ps_col, times = M),
      estimate = as.vector(diff_mat),
      lower = as.vector(diff_lower),
      upper = as.vector(diff_upper),
      row.names = NULL
    )
    attr(out_df, "type") <- type
    attr(out_df, "index") <- p
    attr(out_df, "trt") <- pr_trt
    attr(out_df, "con") <- pr_con
    class(out_df) <- c("dpmixgpd_causal_predict", class(out_df))
    return(out_df)
  }

  trt_stats <- .extract_stats(pr_trt, n_pred)
  con_stats <- .extract_stats(pr_con, n_pred)

  diff_est <- trt_stats$estimate - con_stats$estimate
  diff_lower <- trt_stats$lower - con_stats$lower
  diff_upper <- trt_stats$upper - con_stats$upper

  ps_col <- if (!is.null(ps_full)) ps_full else rep(NA_real_, n_pred)
  out <- cbind(ps = ps_col, estimate = diff_est, lower = diff_lower, upper = diff_upper)
  attr(out, "type") <- type
  if (type == "quantile") attr(out, "index") <- p
  attr(out, "trt") <- pr_trt
  attr(out, "con") <- pr_con
  class(out) <- c("dpmixgpd_causal_predict", class(out))
  out
}


.build_ps_bundle <- function(T, X, spec, mcmc) {
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
        T[i] ~ dbern(pi[i])
        logit(pi[i]) <- inprod(X[i, 1:P], beta[1:P])
      }
      for (j in 1:P) {
        beta[j] ~ dnorm(beta_mean, sd = beta_sd)
      }
    })
  } else if (model == "probit") {
    code <- nimble::nimbleCode({
      for (i in 1:N) {
        T[i] ~ dbern(pi[i])
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
        T[i] ~ dbern(pi_prior)
        for (j in 1:P) {
          X[i, j] ~ dnorm(mu[T[i] + 1, j], sd = sigma[T[i] + 1, j])
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
    data <- list(T = as.integer(T), X = X)
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
    data <- list(T = as.integer(T), X = X)
    # Initialize with sample means/sds by treatment group
    init_mu <- matrix(0, nrow = 2, ncol = P)
    init_sigma <- matrix(1, nrow = 2, ncol = P)
    for (k in 0:1) {
      X_k <- X[T == k, , drop = FALSE]
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
  class(bundle) <- "dpmixgpd_ps_bundle"
  bundle
}

.ps_design_matrix <- function(ps_bundle, X_new) {
  stopifnot(inherits(ps_bundle, "dpmixgpd_ps_bundle"))
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

    # For each posterior sample, compute P(T=1|X) using Bayes rule
    for (s in 1:S) {
      pi_prior <- pi_samples[s, 1]  # P(T=1)

      # Reshape mu and sigma from samples to (K=2, P) matrices
      # mu and sigma are stored as mu[1,1], mu[1,2], ..., mu[2,1], mu[2,2], ...
      P <- ncol(X_new)
      mu_mat <- matrix(mu_samples[s, ], nrow = 2, ncol = P, byrow = FALSE)
      sigma_mat <- matrix(sigma_samples[s, ], nrow = 2, ncol = P, byrow = FALSE)

      # Compute likelihood P(X|T=k) for each treatment group
      # Using independence: P(X|T) = prod_j P(X_j|T)
      log_lik_t0 <- matrix(0, nrow = n_pred, ncol = P)
      log_lik_t1 <- matrix(0, nrow = n_pred, ncol = P)

      for (j in 1:P) {
        log_lik_t0[, j] <- dnorm(X_new[, j], mean = mu_mat[1, j], sd = sigma_mat[1, j], log = TRUE)
        log_lik_t1[, j] <- dnorm(X_new[, j], mean = mu_mat[2, j], sd = sigma_mat[2, j], log = TRUE)
      }

      # Sum across features (log scale)
      log_lik_t0_sum <- rowSums(log_lik_t0)
      log_lik_t1_sum <- rowSums(log_lik_t1)

      # Apply Bayes rule: P(T=1|X) = P(X|T=1)P(T=1) / [P(X|T=1)P(T=1) + P(X|T=0)P(T=0)]
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
