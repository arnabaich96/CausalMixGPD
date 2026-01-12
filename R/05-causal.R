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
  ps_placeholder <- rep(0, length(y))

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
    ps = ps_placeholder[idx_con],
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
    ps = ps_placeholder[idx_trt],
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
      ps = list(enabled = TRUE)
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
  ps_training_X <- bundle$data$X
  if (is.null(ps_training_X)) {
    stop("Training design matrix 'X' is missing from causal bundle.", call. = FALSE)
  }
  ps_training_X <- if (is.matrix(ps_training_X)) ps_training_X else as.matrix(ps_training_X)
  storage.mode(ps_training_X) <- "double"
  ps_hat <- .compute_ps_from_fit(ps_fit = ps_fit, ps_bundle = bundle$design, X_new = ps_training_X)
  idx_con <- bundle$index$con
  idx_trt <- bundle$index$trt
  bundle$outcome$con$data$ps <- ps_hat[idx_con]
  bundle$outcome$trt$data$ps <- ps_hat[idx_trt]

  # Regenerate code/constants/monitors now that PS data is attached
  for (arm in c("con", "trt")) {
    b <- bundle$outcome[[arm]]
    b$code <- build_code_from_spec(b$spec)
    b$constants <- build_constants_from_spec(b$spec)
    b$monitors <- build_monitors_from_spec(b$spec)
    bundle$outcome[[arm]] <- b
  }

  con_fit <- run_mcmc_bundle_manual(bundle$outcome$con, show_progress = show_progress)
  trt_fit <- run_mcmc_bundle_manual(bundle$outcome$trt, show_progress = show_progress)

  # Attach PS model for downstream prediction on new covariates
  ps_model <- list(fit = ps_fit, bundle = bundle$design)
  con_fit$ps_model <- ps_model
  trt_fit$ps_model <- ps_model

  out <- list(
    ps_fit = ps_fit,
    outcome_fit = list(con = con_fit, trt = trt_fit),
    bundle = bundle,
    ps_hat = ps_hat,
    call = match.call()
  )
  class(out) <- "dpmixgpd_causal_fit"
  out
}

#' Quantile treatment effects (QTE)
#'
#' Computes treated-minus-control quantiles from a causal fit.
#'
#' @param fit A \code{"dpmixgpd_causal_fit"} object from \code{run_mcmc_causal()}.
#' @param probs Numeric vector of probabilities in (0, 1).
#' @param newdata Optional data.frame or matrix of covariates for prediction.
#' @param interval Credible interval type passed to \code{predict()}.
#' @return A list with elements \code{fit} (QTE), \code{grid} (probabilities),
#'   and the treated/control prediction objects.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", J = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' qte(fit, probs = c(0.5, 0.9), newdata = X[1:5, ])
#' }
qte <- function(fit,
                probs = c(0.1, 0.5, 0.9),
                newdata = NULL,
                interval = c("none", "credible")) {
  stopifnot(inherits(fit, "dpmixgpd_causal_fit"))
  interval <- match.arg(interval)
  x_pred <- newdata %||% (fit$bundle$data$X %||% NULL)

  ps_new <- NULL
  if (!is.null(x_pred)) {
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
      ps_new <- NULL
    } else {
      ps_new <- .compute_ps_from_fit(ps_fit = ps_fit_use, ps_bundle = ps_bundle_use, X_new = x_pred)
    }
  }

  pr_trt <- predict(fit$outcome_fit$trt, x = x_pred, type = "quantile", p = probs,
                    ps = ps_new,
                    interval = interval,
                    store_draws = TRUE)
  pr_con <- predict(fit$outcome_fit$con, x = x_pred, type = "quantile", p = probs,
                    ps = ps_new,
                    interval = interval,
                    store_draws = TRUE)

  if (is.null(pr_trt$draws) || is.null(pr_con$draws)) {
    stop("QTE requires stored posterior quantile draws; set store_draws=TRUE in predict().", call. = FALSE)
  }
  if (!identical(dim(pr_trt$draws), dim(pr_con$draws))) {
    stop("Treated and control posterior draws must have matching dimensions for QTE.", call. = FALSE)
  }

  diff_draws <- pr_trt$draws - pr_con$draws  # dims: S x n_pred x length(probs)
  fit_mat <- apply(diff_draws, c(2, 3), mean, na.rm = TRUE)
  lower <- upper <- NULL
  if (interval == "credible") {
    qarr <- apply(diff_draws, c(2, 3), stats::quantile, probs = c(0.025, 0.975), na.rm = TRUE)
    lower <- qarr[1, , , drop = TRUE]
    upper <- qarr[2, , , drop = TRUE]
  }

  out <- list(
    fit = fit_mat,
    lower = lower,
    upper = upper,
    grid = probs,
    trt = pr_trt,
    con = pr_con,
    type = "qte"
  )
  class(out) <- "dpmixgpd_qte"
  out
}


#' Average treatment effects (ATE)
#'
#' Computes treated-minus-control posterior means from a causal fit.
#'
#' @param fit A \code{"dpmixgpd_causal_fit"} object from \code{run_mcmc_causal()}.
#' @param newdata Optional data.frame or matrix of covariates for prediction.
#' @param interval Credible interval type passed to \code{predict()}.
#' @param nsim_mean Number of posterior predictive draws to approximate the mean.
#' @param probs Quantiles for credible intervals when \code{interval="credible"}.
#' @return A list with elements \code{fit} (ATE), optional \code{lower}/\code{upper},
#'   and the treated/control prediction objects.
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", J = 6)
#' fit <- run_mcmc_causal(cb, show_progress = FALSE)
#' ate(fit, newdata = X[1:5, ])
#' }
#' @export
ate <- function(fit,
                newdata = NULL,
                interval = c("none", "credible"),
                nsim_mean = 200L,
                probs = c(0.025, 0.5, 0.975)) {
  stopifnot(inherits(fit, "dpmixgpd_causal_fit"))
  interval <- match.arg(interval)
  x_pred <- newdata %||% (fit$bundle$data$X %||% NULL)

  ps_new <- NULL
  if (!is.null(x_pred)) {
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
      ps_new <- NULL
    } else {
      ps_new <- .compute_ps_from_fit(ps_fit = ps_fit_use, ps_bundle = ps_bundle_use, X_new = x_pred)
    }
  }

  pr_trt <- predict(fit$outcome_fit$trt, x = x_pred, type = "mean",
                    ps = ps_new, interval = interval, probs = probs,
                    nsim_mean = nsim_mean, store_draws = TRUE)
  pr_con <- predict(fit$outcome_fit$con, x = x_pred, type = "mean",
                    ps = ps_new, interval = interval, probs = probs,
                    nsim_mean = nsim_mean, store_draws = TRUE)

  if (is.null(pr_trt$draws) || is.null(pr_con$draws)) {
    stop("ATE requires stored posterior mean draws; set store_draws=TRUE in predict().", call. = FALSE)
  }
  if (!identical(dim(pr_trt$draws), dim(pr_con$draws))) {
    stop("Treated and control posterior draws must have matching dimensions for ATE.", call. = FALSE)
  }

  diff_draws <- pr_trt$draws - pr_con$draws  # dims: S x n_pred
  fit_vec <- colMeans(diff_draws, na.rm = TRUE)
  lower <- upper <- NULL
  if (interval == "credible") {
    qmat <- t(apply(diff_draws, 2, stats::quantile, probs = probs, na.rm = TRUE))
    lower <- qmat[, 1]
    upper <- qmat[, length(probs)]
  }

  out <- list(
    fit = fit_vec,
    lower = lower,
    upper = upper,
    grid = NULL,
    trt = pr_trt,
    con = pr_con,
    type = "ate"
  )
  class(out) <- "dpmixgpd_ate"
  out
}


#' Predict from a causal fit
#'
#' Provides a unified interface to the treated and control outcome models while
#' guaranteeing that the same propensity scores are used for both arms. For new
#' covariates, the PS model stored in \code{object$ps_fit} is used to estimate
#' the required scores unless the user supplies their own via \code{ps}.
#'
#' @inheritParams predict.mixgpd_fit
#' @param object A \code{"dpmixgpd_causal_fit"} object returned by
#'   \code{run_mcmc_causal()}.
#' @param ps Optional numeric vector of propensity scores aligned with \code{x}
#'   / \code{newdata}. When provided, the supplied scores are used instead of
#'   recomputing them from the stored PS model (needed only for custom inputs).
#' @return A list with components \code{ps} (estimated propensity scores used for
#'   prediction), \code{trt} (treated-arm prediction output), \code{con}
#'   (control-arm prediction output), \code{type} (the requested prediction type),
#'   and \code{grid} (matching grid from the outcome predictions, e.g., \code{p}
#'   for quantiles or \code{y} for densities).
#' @examples
#' \dontrun{
#' cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal")
#' fit <- run_mcmc_causal(cb)
#' predict(fit, x = X[1:10, ], type = "quantile", p = c(0.25, 0.5, 0.75))
#' }
#' @export
#' @method predict dpmixgpd_causal_fit
predict.dpmixgpd_causal_fit <- function(object,
                                        x = NULL,
                                        y = NULL,
                                        ps = NULL,
                                        newdata = NULL,
                                        type = c("density", "survival", "quantile", "sample", "mean"),
                                        p = NULL,
                                        nsim = NULL,
                                        interval = c("none", "credible"),
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
  interval <- match.arg(interval)

  bundle <- object$bundle %||% list()
  X_train <- bundle$data$X %||% NULL
  has_X <- !is.null(X_train)
  x_mat <- if (!is.null(x)) as.matrix(x) else NULL
  n_pred_default <- if (has_X) nrow(X_train) else 1L
  n_pred <- if (!is.null(x_mat)) nrow(x_mat) else n_pred_default

  ps_full <- NULL
  ps_trt <- NULL
  ps_con <- NULL
  if (!is.null(ps)) {
    ps_full <- as.numeric(ps)
  } else if (!is.null(x_mat)) {
    if (is.null(object$ps_fit)) {
      stop("Causal fit missing PS model; cannot compute propensity scores for newdata.", call. = FALSE)
    }
    ps_full <- .compute_ps_from_fit(
      ps_fit = object$ps_fit,
      ps_bundle = bundle$design,
      X_new = x_mat
    )
  } else {
    ps_full <- object$ps_hat %||% bundle$data$ps
    ps_trt <- object$outcome_fit$trt$data$ps %||% NULL
    ps_con <- object$outcome_fit$con$data$ps %||% NULL
    if (is.null(ps_trt) || is.null(ps_con)) {
      idx_trt <- bundle$index$trt %||% integer(0)
      idx_con <- bundle$index$con %||% integer(0)
      if (!is.null(ps_full) && length(idx_trt) && length(idx_con)) {
        ps_trt <- ps_full[idx_trt]
        ps_con <- ps_full[idx_con]
      }
    }
  }

  if (!is.null(ps_full) && !is.null(x_mat)) {
    if (length(ps_full) != n_pred) {
      stop("Length of 'ps' must equal the number of prediction rows (nrow(x)).", call. = FALSE)
    }
  } else if (!is.null(x_mat) && is.null(ps_full)) {
    stop("PS-augmented causal fit requires propensity scores when predicting on new data.", call. = FALSE)
  }

  if (!is.null(x_mat)) {
    ps_trt <- ps_full
    ps_con <- ps_full
  }

  pr_trt <- predict(
    object$outcome_fit$trt,
    x = x,
    y = y,
    ps = ps_trt,
    type = type,
    p = p,
    nsim = nsim,
    interval = interval,
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
    p = p,
    nsim = nsim,
    interval = interval,
    probs = probs,
    store_draws = store_draws,
    nsim_mean = nsim_mean,
    ncores = ncores,
    ...
  )

  out <- list(
    ps = ps_full,
    ps_trt = ps_trt,
    ps_con = ps_con,
    trt = pr_trt,
    con = pr_con,
    type = type,
    grid = pr_trt$grid
  )
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


 .ps_beta_means <- function(ps_fit) {
  stopifnot(inherits(ps_fit, "dpmixgpd_ps_fit"))
  samples <- as.matrix(ps_fit$mcmc$samples)
  beta_cols <- grep("^beta\\[[0-9]+\\]$", colnames(samples), value = TRUE)
  if (!length(beta_cols)) stop("PS beta draws not found.", call. = FALSE)
  beta_inds <- as.integer(sub("^beta\\[([0-9]+)\\]$", "\\1", beta_cols))
  order_idx <- order(beta_inds, na.last = NA)
  beta_means <- colMeans(samples[, beta_cols, drop = FALSE])
  beta_means <- as.numeric(beta_means[order_idx])
  beta_means
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

.compute_ps_from_fit <- function(ps_fit, ps_bundle, X_new) {
  design <- .ps_design_matrix(ps_bundle, X_new)
  samples <- as.matrix(ps_fit$mcmc$samples)
  beta_cols <- grep("^beta\\[[0-9]+\\]$", colnames(samples), value = TRUE)
  if (!length(beta_cols)) stop("PS beta draws not found.", call. = FALSE)

  beta_inds <- as.integer(sub("^beta\\[([0-9]+)\\]$", "\\1", beta_cols))
  order_idx <- order(beta_inds, na.last = NA)
  beta_mat <- samples[, beta_cols, drop = FALSE]
  beta_mat <- beta_mat[, order_idx, drop = FALSE]  # S x P

  if (ncol(design) != ncol(beta_mat)) {
    stop("Mismatch between PS design columns and beta coefficients.", call. = FALSE)
  }

  # Posterior predictive PS: average plogis over beta draws
  eta <- beta_mat %*% t(design)  # S x n_pred
  ps_draws <- plogis(eta)
  colMeans(ps_draws)
}
