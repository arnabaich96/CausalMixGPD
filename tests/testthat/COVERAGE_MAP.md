# DPmixGPD Test Coverage Map

This document maps test coverage by feature area to help identify gaps and guide test development.

**Legend:**
- ✅ Covered
- ⚠️ Partial coverage
- ❌ Not covered / Missing
- 🔧 Needs refactoring

---

## 1. Kernel Library (d/p/q/r functions)

| Feature | Test File | Status |
|---------|-----------|--------|
| Mix d/p/q/r functions | `test-kernels.R` | ✅ |
| Mix+GPD splice behavior | `test-kernels.R` | ✅ |
| Scalar+GPD behavior | `test-kernels.R` | ✅ |
| Roundtrip q(p) -> p(q) | `test-kernels.R` | ✅ |
| Edge cases (bounds, NA) | `test-kernels.R` | ⚠️ Some covered |

**Tier:** A (cran) - Pure math, no MCMC

---

## 2. Bundle Build Layer

| Feature | Test File | Status |
|---------|-----------|--------|
| Spec validation | `test-bundle.R` | ✅ |
| CRP vs SB code assembly | `test-dispatch-separation.R` | ✅ |
| Kernel registry | `test-smoke-core-workflows.R` | ✅ |
| Input validation errors | `test-bundle.R` | ✅ |
| Conditional (with X) | `test-smoke-core-workflows.R` | ✅ |
| GPD flag handling | `test-smoke-core-workflows.R` | ✅ |

**Tier:** A (cran) - No MCMC compilation

---

## 3. MCMC Run Layer

| Feature | Test File | Status |
|---------|-----------|--------|
| Compile + run returns structure | Per-kernel tests (`test-normal.R`, etc.) | ✅ |
| SB backend workflows | Per-kernel tests | ✅ |
| CRP backend workflows | Per-kernel tests | ✅ |
| GPD vs non-GPD | Per-kernel tests | ✅ |
| Conditional vs unconditional | Per-kernel tests | ✅ |
| Caching behavior | `helper-cache.R` (infrastructure) | ⚠️ Indirect |

**Tier:** B (ci) - Requires MCMC compilation

---

## 4. Prediction Layer

| Feature | Test File | Status |
|---------|-----------|--------|
| type="mean" | `test-predict-unconditional.R`, `helper-predict-distribution.R` | ✅ |
| type="median" | `test-predict-unconditional.R` | ✅ |
| type="quantile" | `test-predict-contracts.R`, `helper-predict-distribution.R` | ✅ |
| type="sample" | `helper-predict-distribution.R` | ✅ |
| type="density" | `helper-predict-distribution.R` | ✅ |
| type="survival" | `helper-predict-distribution.R` | ✅ |
| type="location" | `test-predict-contracts.R` | ⚠️ Basic |
| Conditional x/newdata contracts | `test-predict-contracts.R` | ✅ |
| Parallel ncores determinism | `test-predict-contracts.R` | ✅ |
| Credible intervals | `test-predict-contracts.R` | ✅ |

**Tier:** B (ci) - Requires fitted model

---

## 5. S3 Methods & UX

| Feature | Test File | Status |
|---------|-----------|--------|
| print.mixgpd_fit | `helper-predict-distribution.R` | ✅ |
| summary.mixgpd_fit | `helper-predict-distribution.R` | ✅ |
| plot.mixgpd_fit | `helper-predict-distribution.R` | ✅ |
| fitted.mixgpd_fit | `test-fitted.R` | ✅ |
| residuals.mixgpd_fit (raw) | `test-fitted.R` | ✅ |
| residuals.mixgpd_fit (PIT) | `helper-predict-distribution.R` | ✅ |
| plot.mixgpd_predict | `test-predict-unconditional.R` | ✅ |
| plot.mixgpd_fitted | `test-fitted.R` | ✅ |
| params() extractor | `test-fitted.R`, `test-causal-predict.R` | ✅ |

**Tier:** A/B mixed

---

## 6. Causal Inference

### 6a. Causal Mechanics (build/run/PS)

| Feature | Test File | Status |
|---------|-----------|--------|
| build_causal_bundle | `test-smoke-core-workflows.R`, `test-causal.R` | ✅ |
| run_mcmc_causal | `test-causal.R` | ✅ |
| PS models: logit | `test-causal.R` | ✅ |
| PS models: probit | `test-causal.R` | ✅ |
| PS models: naive | `test-causal.R` | ✅ |
| PS models: FALSE (disabled) | `test-causal.R` | ✅ |
| design="observational" | `test-causal.R` | ✅ |
| design="rct" | `test-causal.R` | ✅ |
| design enforcement errors | `test-causal.R` | ✅ |
| Mixed backends (trt/con) | `test-causal.R` | ✅ |

**Tier:** A (design tests), B (workflow tests)

### 6b. Causal Estimands (ATE/QTE)

| Feature | Test File | Status |
|---------|-----------|--------|
| ate() basic | `test-causal-ate.R` | ✅ |
| qte() basic | `test-causal-ate.R` | ✅ |
| Draw alignment across arms | `test-causal-ate.R` | ✅ |
| Credible intervals | `test-causal-ate.R` | ✅ |

**Tier:** B (ci)

### 6c. Causal Prediction Types

| Feature | Test File | Status |
|---------|-----------|--------|
| predict(causal, type="mean") | `test-causal.R` | ✅ |
| predict(causal, type="quantile", p=...) | `test-causal-predict.R` | ✅ |
| predict(causal, type="density", y=...) | `test-causal-predict.R` | ✅ |
| predict(causal, type="survival", y=...) | `test-causal-predict.R` | ✅ |
| predict(causal, type="prob", y=...) | `test-causal-predict.R` | ✅ |
| ps_scale variants ("prob" vs "logit") | `test-causal-predict.R` | ✅ |
| ps_summary variants ("mean" vs "median") | `test-causal-predict.R` | ✅ |
| ps_clamp behavior | `test-causal-predict.R` | ✅ |
| User-supplied ps= bypass | `test-causal-predict.R` | ✅ |
| "x vs newdata both provided" error | `test-causal-predict.R` | ✅ |
| Missing p error for quantile | `test-causal-predict.R` | ✅ |
| y length mismatch error | `test-causal-predict.R` | ✅ |

**Tier:** A (error paths), B (integration)

### 6d. Causal S3/Plotting

| Feature | Test File | Status |
|---------|-----------|--------|
| print.dpmixgpd_causal_fit | `test-causal-predict.R` | ✅ |
| summary.dpmixgpd_causal_fit | `test-causal-predict.R` | ✅ |
| print.dpmixgpd_causal_bundle | `test-causal-predict.R` | ✅ |
| plot.dpmixgpd_causal_fit | `test-causal-predict.R` | ✅ |
| plot.dpmixgpd_qte | `test-causal-predict.R` | ✅ |
| plot.dpmixgpd_ate | `test-causal-predict.R` | ✅ |
| plot.dpmixgpd_causal_predict | `test-causal-predict.R` | ✅ |

**Tier:** B (ci)

---

## 7. Theory / Mathematical Correctness

| Feature | Test File | Status |
|---------|-----------|--------|
| GPD splice continuity | `test-theory.R` | ✅ |
| Vectorization contracts | `test-vectorization.R`, `test-vectorization-contract.R` | ✅ |

**Tier:** A (cran)

---

## Summary: Known Gaps

### All High Priority Gaps Now Covered ✅

The following gaps were identified and have been addressed with tests:
1. ~~**ps_scale/ps_summary/ps_clamp variants**~~ → Now in `test-causal-predict.R`
2. ~~**ncores parallel prediction**~~ → Already in `test-predict-contracts.R`
3. ~~**params() extractor**~~ → Now in `test-fitted.R` and `test-causal-predict.R`
4. ~~**plot.mixgpd_fitted**~~ → Now in `test-fitted.R`

### Medium Priority (nice to have)
1. More edge case coverage in kernel math
2. Caching behavior explicit tests
3. WAIC computation tests

---

## Test File Organization

### Tier A (cran) - Fast, no MCMC
- `test-bundle.R`
- `test-dispatch-separation.R`
- `test-kernels.R`
- `test-predict-contracts.R` (input validation only)
- `test-theory.R`
- `test-vectorization.R`
- `test-vectorization-contract.R`
- `test-smoke-core-workflows.R`
- `test-causal.R` (PS parameter tests, design enforcement only)

### Tier B (ci) - Integration, requires MCMC
- `test-normal.R`, `test-gamma.R`, `test-lognormal.R`, `test-laplace.R`, `test-invgauss.R`, `test-amoroso.R`, `test-cauchy.R`
- `test-predict-unconditional.R`
- `test-predict-contracts.R` (prediction behavior tests)
- `test-fitted.R` (fitted, params, plot.mixgpd_fitted)
- `test-causal.R` (representative combos)
- `test-causal-ate.R`
- `test-causal-predict.R` (predict types, PS options, params, plotting)

### Tier C (full) - Exhaustive
- `test-causal.R` (exhaustive kernel x backend x GPD grid)
