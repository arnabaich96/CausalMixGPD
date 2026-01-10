#' Build a causal bundle (design + two outcome arms)
#'
#' Creates a causal bundle with:
#' \itemize{
#'   \item a propensity score (PS) design model (logistic regression of \code{T} on \code{X})
#'   \item an outcome bundle for the control arm (\code{T = 0})
#'   \item an outcome bundle for the treated arm (\code{T = 1})
#' }
#'
#' The outcome bundles reuse the existing DPM + optional GPD tail machinery. The PS model is a
#' lightweight NIMBLE logistic regression with normal priors on coefficients.
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
#' @param J Integer >= 2; truncation parameter for outcome mixtures. If length 2,
#'   the first entry is used for treated (\code{T=1}) and the second for control (\code{T=0}).
#' @param components Deprecated alias for \code{J}. Only one of \code{J} or \code{components}
#'   should be supplied.
#' @param param_specs Outcome parameter overrides (same structure as \code{build_nimble_bundle()}).
#'   You can pass a single list used for both arms or a list with \code{con} and \code{trt} entries.
#' @param mcmc_outcome MCMC settings list for the outcome bundles.
#' @param mcmc_ps MCMC settings list for the PS model.
#' @param epsilon Numeric in [0,1) used by outcome bundles for posterior truncation summaries.
#'   If length 2, the first entry is used for treated (\code{T=1}) and the second for control (\code{T=0}).
#' @param alpha_random Logical; whether outcome concentration \code{alpha} is stochastic.
#' @param ps_model PS model family. Currently supports: \code{"logit"}.
#' @param ps_prior Normal prior for PS coefficients. List with \code{mean} and \code{sd}.
#' @param include_intercept Logical; if TRUE, an intercept column is prepended to \code{X}
#'   in the PS model.
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
#'   J = 10
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
    J = NULL,
    param_specs = NULL,
    mcmc_outcome = list(niter = 2000, nburnin = 500, thin = 1, nchains = 1, seed = 1),
    mcmc_ps = list(niter = 2000, nburnin = 500, thin = 1, nchains = 1, seed = 1),
    epsilon = 0.025,
    alpha_random = TRUE,
    ps_model = c("logit"),
    ps_prior = list(mean = 0, sd = 2),
    include_intercept = TRUE
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
  ps_model <- match.arg(ps_model, choices = c("logit"))

  y <- as.numeric(y)
  if (!length(y)) stop("y must be a non-empty numeric vector.", call. = FALSE)

  if (is.null(X)) stop("X is required for causal bundles.", call. = FALSE)
  if (!is.matrix(X)) X <- as.matrix(X)
  if (nrow(X) != length(y)) stop("X must have the same number of rows as length(y).", call. = FALSE)

  T <- as.integer(T)
  if (length(T) != length(y)) stop("T must have the same length as y.", call. = FALSE)
  if (anyNA(T) || !all(T %in% c(0L, 1L))) stop("T must be binary (0/1) with no NA.", call. = FALSE)

  if (!is.null(J) && !is.null(components)) {
    stop("Provide only one of 'J' or 'components'.", call. = FALSE)
  }
  if (!is.null(J)) components <- J
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

  idx_con <- which(T == 0L)
  idx_trt <- which(T == 1L)
  if (!length(idx_con) || !length(idx_trt)) {
    stop("Both treatment arms must have at least one observation.", call. = FALSE)
  }

  ps_spec <- list(
    model = ps_model,
    prior = list(
      mean = ps_prior$mean %||% 0,
      sd = ps_prior$sd %||% 2
    ),
    include_intercept = isTRUE(include_intercept)
  )

  ps_bundle <- .build_ps_bundle(T = T, X = X, spec = ps_spec, mcmc = mcmc_ps)

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
    X = X[idx_con, , drop = FALSE],
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
    X = X[idx_trt, , drop = FALSE],
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
      epsilon = epsilon
    ),
    call = match.call()
  )
  class(out) <- "dpmixgpd_causal_bundle"
  out
}

.run_ps_mcmc_bundle <- function(bundle, show_progress = TRUE) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  stopifnot(inherits(bundle, "dpmixgpd_ps_bundle"))

  code <- bundle$code
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
#' Executes the PS model and both outcome arms, returning a single causal fit.
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

  ps_fit <- .run_ps_mcmc_bundle(bundle$design, show_progress = show_progress)
  con_fit <- run_mcmc_bundle_manual(bundle$outcome$con, show_progress = show_progress)
  trt_fit <- run_mcmc_bundle_manual(bundle$outcome$trt, show_progress = show_progress)

  out <- list(
    ps_fit = ps_fit,
    outcome_fit = list(con = con_fit, trt = trt_fit),
    bundle = bundle,
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
#' @param probs Numeric vector of probabilities in (0, 1).
#' @param newdata Optional data.frame or matrix of covariates for prediction.
#' @param interval Credible interval type passed to \code{predict()}.
#' @return A list with elements \code{fit} (CQTE), \code{grid} (probabilities),
#'   and the treated/control prediction objects.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", J = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' cqte(fit, probs = c(0.5, 0.9), newdata = X[1:5, ])
#' }
#' @export
cqte <- function(fit,
                 probs = c(0.1, 0.5, 0.9),
                 newdata = NULL,
                 interval = c("none", "credible")) {
  stopifnot(inherits(fit, "dpmixgpd_causal_fit"))
  interval <- match.arg(interval)
  pr_trt <- predict(fit$outcome_fit$trt, newdata = newdata, type = "quantile", p = probs,
                    interval = interval)
  pr_con <- predict(fit$outcome_fit$con, newdata = newdata, type = "quantile", p = probs,
                    interval = interval)
  out <- list(
    fit = pr_trt$fit - pr_con$fit,
    lower = if (!is.null(pr_trt$lower) && !is.null(pr_con$lower)) pr_trt$lower - pr_con$upper else NULL,
    upper = if (!is.null(pr_trt$upper) && !is.null(pr_con$upper)) pr_trt$upper - pr_con$lower else NULL,
    grid = probs,
    trt = pr_trt,
    con = pr_con,
    type = "cqte"
  )
  class(out) <- "dpmixgpd_cqte"
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
  if (!model %in% c("logit")) {
    stop("Unsupported PS model. Supported: logit.", call. = FALSE)
  }

  code <- nimble::nimbleCode({
    for (i in 1:N) {
      T[i] ~ dbern(pi[i])
      logit(pi[i]) <- inprod(X[i, 1:P], beta[1:P])
    }
    for (j in 1:P) {
      beta[j] ~ dnorm(beta_mean, sd = beta_sd)
    }
  })

  constants <- list(
    N = N,
    P = P,
    beta_mean = as.numeric(spec$prior$mean %||% 0),
    beta_sd = as.numeric(spec$prior$sd %||% 2)
  )
  data <- list(
    T = as.integer(T),
    X = X
  )
  inits <- list(beta = rep(0, P))
  monitors <- c("beta")

  bundle <- list(
    spec = list(
      meta = list(type = "ps_logit", include_intercept = isTRUE(spec$include_intercept)),
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
