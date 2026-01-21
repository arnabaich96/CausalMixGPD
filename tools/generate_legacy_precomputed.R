# ============================================================================
# Generate Precomputed Results for Legacy Vignettes
# ============================================================================
#
# This script generates the .rds files needed by legacy vignettes.
# Running this script exercises the causal, kernel, and S3 method code paths
# that contribute to package coverage.
#
# Usage:
#   source("tools/generate_legacy_precomputed.R")
#   generate_all_precomputed()
#
# Or run specific sections:
#   generate_v00_precomputed()  # Basic unconditional/conditional
#   generate_unconditional_precomputed()  # v05-v09
#   generate_conditional_precomputed()    # v10-v13
#   generate_causal_precomputed()         # v14-v19
#
# ============================================================================

# Output directory for precomputed files
PRECOMP_DIR <- file.path("vignettes", "legacy", "articles", "legacy-precomputed")

# Ensure output directory exists
if (!dir.exists(PRECOMP_DIR)) {
  dir.create(PRECOMP_DIR, recursive = TRUE, showWarnings = FALSE)
}

# Helper to save precomputed result
save_precomp <- function(obj, tag) {
  path <- file.path(PRECOMP_DIR, paste0(tag, ".rds"))
  saveRDS(obj, path)
  cat("Saved:", path, "\n")
}

# Helper to check if file exists
precomp_exists <- function(tag) {
  path <- file.path(PRECOMP_DIR, paste0(tag, ".rds"))
  file.exists(path)
}

# Suppress MCMC output
quiet_mcmc <- function(expr) {
  nullfile <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  utils::capture.output(result <- force(expr), file = nullfile)
  result
}

# Default MCMC settings (short for precomputation)
mcmc_default <- list(niter = 400, nburnin = 100, thin = 2, nchains = 1, seed = 1)

# ============================================================================
# v00: Start Here - Basic unconditional and conditional
# ============================================================================

generate_v00_precomputed <- function(force = FALSE) {
  cat("\n=== Generating v00: Start Here ===\n")
  
  # Unconditional fit
  tag <- "v00-start-here-fit_uncond"
  if (force || !precomp_exists(tag)) {
    cat("Generating:", tag, "\n")
    data("nc_pos200_k3", package = "DPmixGPD")
    y <- nc_pos200_k3$y
    
    bundle <- build_nimble_bundle(
      y = y,
      backend = "crp",
      kernel = "gamma",
      GPD = FALSE,
      components = 5,
      mcmc = mcmc_default
    )
    fit <- quiet_mcmc(run_mcmc_bundle_manual(bundle, show_progress = FALSE))
    save_precomp(fit, tag)
  } else {
    cat("Skipping (exists):", tag, "\n")
  }
  
  # Conditional fit
  tag <- "v00-start-here-fit_cond"
  if (force || !precomp_exists(tag)) {
    cat("Generating:", tag, "\n")
    data("nc_posX100_p3_k2", package = "DPmixGPD")
    yc <- nc_posX100_p3_k2$y
    X <- as.matrix(nc_posX100_p3_k2$X)
    
    bundle <- build_nimble_bundle(
      y = yc,
      X = X,
      backend = "sb",
      kernel = "lognormal",
      GPD = FALSE,
      components = 5,
      mcmc = mcmc_default
    )
    fit <- quiet_mcmc(run_mcmc_bundle_manual(bundle, show_progress = FALSE))
    save_precomp(fit, tag)
  } else {
    cat("Skipping (exists):", tag, "\n")
  }
  
  invisible(TRUE)
}

# ============================================================================
# v05-v09: Unconditional models
# ============================================================================

generate_unconditional_precomputed <- function(force = FALSE) {
  cat("\n=== Generating Unconditional Models (v05-v09) ===\n")
  
  # Load data
  data("nc_pos200_k3", package = "DPmixGPD")
  y <- nc_pos200_k3$y
  
  configs <- list(
    list(tag = "v05-uncond-crp-bulk", backend = "crp", GPD = FALSE, kernel = "gamma"),
    list(tag = "v06-uncond-crp-bulk-lognormal", backend = "crp", GPD = FALSE, kernel = "lognormal"),
    list(tag = "v07-uncond-sb-bulk", backend = "sb", GPD = FALSE, kernel = "gamma"),
    list(tag = "v08-uncond-crp-gpd", backend = "crp", GPD = TRUE, kernel = "gamma"),
    list(tag = "v09-uncond-sb-gpd", backend = "sb", GPD = TRUE, kernel = "gamma")
  )
  
  for (cfg in configs) {
    if (force || !precomp_exists(cfg$tag)) {
      cat("Generating:", cfg$tag, "\n")
      
      tryCatch({
        bundle <- build_nimble_bundle(
          y = y,
          backend = cfg$backend,
          kernel = cfg$kernel,
          GPD = cfg$GPD,
          components = 5,
          mcmc = mcmc_default
        )
        fit <- quiet_mcmc(run_mcmc_bundle_manual(bundle, show_progress = FALSE))
        save_precomp(fit, cfg$tag)
      }, error = function(e) {
        cat("  Error:", conditionMessage(e), "\n")
      })
    } else {
      cat("Skipping (exists):", cfg$tag, "\n")
    }
  }
  
  invisible(TRUE)
}

# ============================================================================
# v10-v13: Conditional models
# ============================================================================

generate_conditional_precomputed <- function(force = FALSE) {
  cat("\n=== Generating Conditional Models (v10-v13) ===\n")
  
  # Load data
  data("nc_posX100_p3_k2", package = "DPmixGPD")
  y <- nc_posX100_p3_k2$y
  X <- as.matrix(nc_posX100_p3_k2$X)
  
  configs <- list(
    list(tag = "v10-cond-crp-bulk", backend = "crp", GPD = FALSE, kernel = "gamma"),
    list(tag = "v11-cond-sb-bulk", backend = "sb", GPD = FALSE, kernel = "lognormal"),
    list(tag = "v12-cond-crp-gpd", backend = "crp", GPD = TRUE, kernel = "gamma"),
    list(tag = "v13-cond-sb-gpd", backend = "sb", GPD = TRUE, kernel = "lognormal")
  )
  
  for (cfg in configs) {
    if (force || !precomp_exists(cfg$tag)) {
      cat("Generating:", cfg$tag, "\n")
      
      tryCatch({
        bundle <- build_nimble_bundle(
          y = y,
          X = X,
          backend = cfg$backend,
          kernel = cfg$kernel,
          GPD = cfg$GPD,
          components = 5,
          mcmc = mcmc_default
        )
        fit <- quiet_mcmc(run_mcmc_bundle_manual(bundle, show_progress = FALSE))
        save_precomp(fit, cfg$tag)
      }, error = function(e) {
        cat("  Error:", conditionMessage(e), "\n")
      })
    } else {
      cat("Skipping (exists):", cfg$tag, "\n")
    }
  }
  
  invisible(TRUE)
}

# ============================================================================
# v14-v19: Causal models
# ============================================================================

generate_causal_precomputed <- function(force = FALSE) {
  cat("\n=== Generating Causal Models (v14-v19) ===\n")
  
  # Load causal data
  data("causal_alt_real500_p4_k2", package = "DPmixGPD")
  y <- abs(causal_alt_real500_p4_k2$y) + 0.01
  T_vec <- causal_alt_real500_p4_k2$T
  X <- as.matrix(causal_alt_real500_p4_k2$X)
  
  mcmc_causal <- list(niter = 300, nburnin = 80, nchains = 1, thin = 1, seed = 1)
  
  # v14: Causal no-X CRP
  tag <- "v14-causal-no-x-crp"
  if (force || !precomp_exists(tag)) {
    cat("Generating:", tag, "\n")
    tryCatch({
      bundle <- build_causal_bundle(
        y = y,
        T = T_vec,
        X = NULL,
        kernel = "gamma",
        backend = "crp",
        PS = FALSE,
        GPD = FALSE,
        components = 6,
        mcmc_outcome = mcmc_causal
      )
      fit <- quiet_mcmc(run_mcmc_causal(bundle, show_progress = FALSE))
      save_precomp(fit, tag)
      
      # Also generate QTE and ATE for this fit
      qte_result <- qte(fit, probs = c(0.25, 0.5, 0.75), interval = "credible")
      save_precomp(qte_result, paste0(tag, "-qte"))
      
      ate_result <- ate(fit, interval = "credible", nsim_mean = 100)
      save_precomp(ate_result, paste0(tag, "-ate"))
    }, error = function(e) {
      cat("  Error:", conditionMessage(e), "\n")
    })
  } else {
    cat("Skipping (exists):", tag, "\n")
  }
  
  # v15: Causal X no-PS SB
  tag <- "v15-causal-x-no-ps-sb"
  if (force || !precomp_exists(tag)) {
    cat("Generating:", tag, "\n")
    tryCatch({
      bundle <- build_causal_bundle(
        y = y,
        T = T_vec,
        X = X,
        kernel = "lognormal",
        backend = "sb",
        PS = FALSE,
        GPD = FALSE,
        components = 5,
        mcmc_outcome = mcmc_causal
      )
      fit <- quiet_mcmc(run_mcmc_causal(bundle, show_progress = FALSE))
      save_precomp(fit, tag)
    }, error = function(e) {
      cat("  Error:", conditionMessage(e), "\n")
    })
  } else {
    cat("Skipping (exists):", tag, "\n")
  }
  
  # v16: Causal same-backend CRP
  tag <- "v16-causal-same-backend-crp"
  if (force || !precomp_exists(tag)) {
    cat("Generating:", tag, "\n")
    tryCatch({
      bundle <- build_causal_bundle(
        y = y,
        T = T_vec,
        X = X,
        kernel = "gamma",
        backend = "crp",
        PS = TRUE,
        GPD = FALSE,
        components = 6,
        mcmc_outcome = mcmc_causal,
        mcmc_ps = list(niter = 200, nburnin = 50, nchains = 1, thin = 1, seed = 2)
      )
      fit <- quiet_mcmc(run_mcmc_causal(bundle, show_progress = FALSE))
      save_precomp(fit, tag)
    }, error = function(e) {
      cat("  Error:", conditionMessage(e), "\n")
    })
  } else {
    cat("Skipping (exists):", tag, "\n")
  }
  
  # v17: Causal same-backend SB
  tag <- "v17-causal-same-backend-sb"
  if (force || !precomp_exists(tag)) {
    cat("Generating:", tag, "\n")
    tryCatch({
      bundle <- build_causal_bundle(
        y = y,
        T = T_vec,
        X = X,
        kernel = "lognormal",
        backend = "sb",
        PS = TRUE,
        GPD = FALSE,
        components = 5,
        mcmc_outcome = mcmc_causal,
        mcmc_ps = list(niter = 200, nburnin = 50, nchains = 1, thin = 1, seed = 2)
      )
      fit <- quiet_mcmc(run_mcmc_causal(bundle, show_progress = FALSE))
      save_precomp(fit, tag)
    }, error = function(e) {
      cat("  Error:", conditionMessage(e), "\n")
    })
  } else {
    cat("Skipping (exists):", tag, "\n")
  }
  
  invisible(TRUE)
}

# ============================================================================
# Generate all kernel variants for coverage
# ============================================================================

generate_kernel_coverage <- function(force = FALSE) {
  cat("\n=== Generating Kernel Coverage Tests ===\n")
  
  data("nc_pos200_k3", package = "DPmixGPD")
  y <- nc_pos200_k3$y
  
  # Test all kernels with SB backend
  kernels <- c("gamma", "lognormal", "invgauss", "normal", "laplace", "cauchy", "amoroso")
  
  for (kernel in kernels) {
    tag <- paste0("kernel-coverage-", kernel)
    if (force || !precomp_exists(tag)) {
      cat("Generating:", tag, "\n")
      
      # Adjust y for real-support kernels
      y_use <- if (kernel %in% c("normal", "laplace", "cauchy")) {
        y - mean(y)  # Center for real-support kernels
      } else {
        y
      }
      
      tryCatch({
        bundle <- build_nimble_bundle(
          y = y_use,
          backend = "sb",
          kernel = kernel,
          GPD = FALSE,
          components = 4,
          mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
        )
        fit <- quiet_mcmc(run_mcmc_bundle_manual(bundle, show_progress = FALSE))
        save_precomp(fit, tag)
        
        # Exercise predict methods
        pred_q <- predict(fit, type = "quantile", index = c(0.5, 0.9))
        pred_d <- predict(fit, y = y_use[1:10], type = "density")
        pred_s <- predict(fit, y = y_use[1:10], type = "survival")
      }, error = function(e) {
        cat("  Error:", conditionMessage(e), "\n")
      })
    } else {
      cat("Skipping (exists):", tag, "\n")
    }
  }
  
  invisible(TRUE)
}

# ============================================================================
# Main entry point
# ============================================================================

generate_all_precomputed <- function(force = FALSE) {
  cat("============================================================\n")
  cat("Generating Precomputed Results for Legacy Vignettes\n")
  cat("============================================================\n")
  cat("Output directory:", normalizePath(PRECOMP_DIR, mustWork = FALSE), "\n")
  cat("Force regenerate:", force, "\n")
  
  # Load package
  if (!requireNamespace("DPmixGPD", quietly = TRUE)) {
    stop("DPmixGPD package must be installed. Run: devtools::install()")
  }
  library(DPmixGPD)
  
  # Generate all sections
  generate_v00_precomputed(force = force)
  generate_unconditional_precomputed(force = force)
  generate_conditional_precomputed(force = force)
  generate_causal_precomputed(force = force)
  generate_kernel_coverage(force = force)
  
  cat("\n============================================================\n")
  cat("Precomputation complete!\n")
  cat("============================================================\n")
  
  # List generated files
  files <- list.files(PRECOMP_DIR, pattern = "\\.rds$", full.names = FALSE)
  cat("\nGenerated files (", length(files), "):\n", sep = "")
  for (f in files) {
    size <- file.size(file.path(PRECOMP_DIR, f))
    cat("  ", f, " (", round(size / 1024, 1), " KB)\n", sep = "")
  }
  
  invisible(TRUE)
}

# Print usage when sourced
cat("Legacy Vignette Precomputation Script loaded.\n\n")
cat("Usage:\n")
cat("  generate_all_precomputed()           # Generate all precomputed files\n")
cat("  generate_all_precomputed(force=TRUE) # Force regenerate all\n")
cat("  generate_v00_precomputed()           # Just v00 (start here)\n")
cat("  generate_unconditional_precomputed() # Just unconditional models\n")
cat("  generate_conditional_precomputed()   # Just conditional models\n")
cat("  generate_causal_precomputed()        # Just causal models\n")
cat("  generate_kernel_coverage()           # All kernel types\n")
