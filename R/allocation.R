#' Cluster allocation and posterior similarity matrix (internal)
#'
#' Internal helper functions for cluster allocation analysis, including
#' posterior similarity matrix (PSM) computation and Dahl's method for
#' finding representative clusterings.
#'
#' @name allocation
#' @keywords internal
#' @noRd
#' @references
#' Dahl, D. B. (2006). Model-based clustering for expression data via a
#' Dirichlet process mixture model. In M. Vannucci, et al. (Eds.), Bayesian
#' Inference for Gene Expression and Proteomics (pp. 201-218). Cambridge
#' University Press.
NULL

#' Extract cluster assignment matrix from MCMC samples
#'
#' Extracts the posterior draws of cluster assignments \code{z[1:N]} from a fitted
#' mixgpd_fit object and returns them as an integer matrix (iterations x N).
#'
#' @param object A \code{mixgpd_fit} object.
#' @return Integer matrix with rows = posterior draws, cols = observations.
#' @keywords internal
#' @noRd
.extract_z_matrix <- function(object) {
  smp <- .get_samples_mcmclist(object)
  if (is.null(smp)) {
    stop("No MCMC samples found in object.", call. = FALSE)
  }

  # Stack all chains
  zmat_list <- vector("list", length(smp))
  for (i in seq_along(smp)) {
    M <- as.matrix(smp[[i]])
    z_cols <- grep("^z\\[[0-9]+\\]$", colnames(M))
    if (length(z_cols) == 0) {
      stop("No cluster assignment variables 'z[i]' found in MCMC samples.", call. = FALSE)
    }
    # Sort z columns numerically by index
    z_colnames <- colnames(M)[z_cols]
    z_indices <- as.integer(sub("^z\\[([0-9]+)\\]$", "\\1", z_colnames))
    z_cols <- z_cols[order(z_indices)]
    zmat_list[[i]] <- M[, z_cols, drop = FALSE]
  }

  zmat <- do.call(rbind, zmat_list)
  # Convert to integer matrix
  mode(zmat) <- "integer"
  return(zmat)
}

#' Compute posterior similarity matrix
#'
#' Computes the posterior similarity matrix (PSM) from a matrix of cluster
#' assignments. \code{PSM[i,j]} = probability that observations i and j are in the
#' same cluster.
#'
#' @param z_matrix Integer matrix (iterations x N) of cluster assignments.
#' @return Symmetric N x N matrix of co-clustering probabilities.
#' @keywords internal
#' @noRd
.compute_psm <- function(z_matrix) {
  n_iter <- nrow(z_matrix)
  n_obs <- ncol(z_matrix)
  PSM <- matrix(0, n_obs, n_obs)

  for (s in seq_len(n_iter)) {
    z <- z_matrix[s, ]
    # Indicator matrix: same cluster
    A <- outer(z, z, "==") * 1.0
    PSM <- PSM + A
  }
  PSM <- PSM / n_iter
  return(PSM)
}

#' Find Dahl representative clustering
#'
#' Identifies the posterior draw that minimizes squared distance to the
#' posterior similarity matrix, following Dahl (2006). Returns relabeled
#' cluster assignments as consecutive integers 1, 2, ..., K.
#'
#' @param z_matrix Integer matrix (iterations x N) of cluster assignments.
#' @param PSM Posterior similarity matrix (N x N).
#' @return List with components: draw_index (integer), labels (integer vector),
#'   K (number of clusters).
#' @keywords internal
#' @noRd
.dahl_representative <- function(z_matrix, PSM) {
  n_iter <- nrow(z_matrix)
  ssq <- numeric(n_iter)

  for (s in seq_len(n_iter)) {
    z <- z_matrix[s, ]
    A <- outer(z, z, "==") * 1.0
    ssq[s] <- sum((A - PSM)^2)
  }

  s_star <- which.min(ssq)
  z_hat <- as.integer(z_matrix[s_star, ])

  # Relabel to consecutive integers
  labels <- match(z_hat, unique(z_hat))
  K <- length(unique(labels))

  return(list(
    draw_index = s_star,
    labels = labels,
    K = K
  ))
}

#' Compute cluster membership probabilities from PSM
#'
#' For each observation, computes the probability of membership in each cluster
#' defined by the representative clustering, derived from the posterior
#' similarity matrix.
#'
#' @param z_matrix Integer matrix (iterations x N) of cluster assignments.
#' @param labels_representative Integer vector of representative cluster labels.
#' @param PSM Posterior similarity matrix (N x N).
#' @return N x K matrix of cluster membership probabilities.
#' @keywords internal
#' @noRd
.compute_cluster_probs <- function(z_matrix, labels_representative, PSM) {
  n_obs <- length(labels_representative)
  K <- length(unique(labels_representative))

  probs <- matrix(0, nrow = n_obs, ncol = K)

  for (k in seq_len(K)) {
    # Observations in cluster k in the representative partition
    idx_k <- which(labels_representative == k)
    # For each observation i, average PSM with all members of cluster k
    for (i in seq_len(n_obs)) {
      probs[i, k] <- mean(PSM[i, idx_k])
    }
  }

  # Normalize rows to sum to 1
  row_sums <- rowSums(probs)
  for (i in seq_len(n_obs)) {
    if (row_sums[i] > 0) {
      probs[i, ] <- probs[i, ] / row_sums[i]
    } else {
      # Fallback: uniform distribution
      probs[i, ] <- 1 / K
    }
  }

  return(probs)
}
