# DPmixGPD function map
This file lists **what each function does** and **where it lives** after the reorg.
> Legend: **exported** = in NAMESPACE / user-facing; **internal** = helper.

## `R/00-utils.R`
- `getNimbleOption()` — *internal*. —

## `R/01-kernel-registry.R`
- `get_kernel()` — **exported**. Get kernel
- `get_kernel_def()` — *internal*. Get kernel definition
- `init_kernel_registry()` — *internal*. Init kernel registry
- `register_kernel()` — **exported**. Register kernel
- `register_kernel_def()` — *internal*. Register kernel definition

## `R/02-modelspec-dp.R`
- `Fmix()` — *internal*. —
- `build_model_spec()` — *internal*. Build model spec
- `build_model_spec_xy()` — *internal*. Build model spec xy
- `f()` — *internal*. —

## `R/03-engine.R`
- `build_nimble_model_gamma_reg()` — *internal*. Internal: run Nimble MCMC for unconditional Gamma SB mixture
- `build_nimble_model_gamma_uncond()` — *internal*. Build nimble model gamma uncond
- `run_mcmc_engine()` — *internal*. Run mcmc engine
- `run_mcmc_nimble_gamma()` — *internal*. Run mcmc nimble gamma
- `simulate_chain()` — *internal*. Simulate chain

## `R/04-model-methods.R`
- `F()` — *internal*. —
- `as_mcmc()` — **exported**. As mcmc
- `as_mcmc.mixgpd_fit()` — **exported**. As mcmc mixgpd fit
- `coef.mixgpd_fit()` — **exported**. Coef mixgpd fit
- `f_root()` — *internal*. —
- `fit.dpm()` — **exported**. Fit dpm
- `fit.dpmgpd()` — **exported**. Fit dpmgpd
- `fit_mixgpd()` — **exported**. Fit mixgpd
- `fit_mixgpd_gpd()` — **exported**. Fit mixgpd gpd
- `fit_mixgpd_gpd_xy()` — **exported**. Fit mixgpd gpd xy
- `fit_mixgpd_xy()` — **exported**. Fit mixgpd xy
- `fitted.mixgpd_fit()` — **exported**. Fitted mixgpd fit
- `get_j()` — *internal*. —
- `mcmc_ggdiag()` — **exported**. Mcmc ggdiag
- `predict.mixgpd_fit()` — **exported**. Predict mixgpd fit
- `print.mixgpd_fit()` — **exported**. Print summary of a MixGPD fit
- `print.summary.mixgpd_fit()` — **exported**. Print summary mixgpd fit
- `summary.mixgpd_fit()` — **exported**. Summarize a MixGPD fit

## `R/05-causal.R`
- `ate()` — **exported**. Average Treatment Effect (ATE)
- `ate.mixgpd_te_fit()` — **exported**. —
- `fit.TE()` — **exported**. Fit TE
- `get_q()` — *internal*. —
- `predict.mixgpd_te_fit()` — **exported**. Predict mixgpd te fit
- `plot.mixgpd_te_fit()` — **exported**. Plot treatment effects
- `plot_te_curve()` — **exported**. Plot treatment-effect curve vs propensity score
- `print.mixgpd_te()` — **exported**. Print mixgpd te
- `qte()` — **exported**. Quantile Treatment Effect (QTE)
- `qte.mixgpd_te_fit()` — **exported**. —
- `te_curve()` — **exported**. Treatment-effect curve vs propensity score
- `te_curve.mixgpd_te_fit()` — **exported**. —

## `R/06-zzz.R`
_No top-level `<- function(...)` definitions found._

## `inst/kernels/` (kernel modules)
Each kernel file defines a `kernel_*_def` list with component-level `d/p/q/r` functions and registers it at load time.
- `inst/kernels/01-kernel-gamma.R` — kernel **gamma** (`kernel_gamma_def`), params: `c("shape", "scale")`
- `inst/kernels/02-kernel-normal.R` — kernel **normal** (`kernel_normal_def`), params: `c("mean", "sd")`
- `inst/kernels/03-kernel-lognormal.R` — kernel **lognormal** (`kernel_lognormal_def`), params: `c("meanlog", "sdlog")`
- `inst/kernels/04-kernel-laplace.R` — kernel **laplace** (`kernel_laplace_def`), params: `c("location", "scale")`
- `inst/kernels/05-kernel-inverse-gaussian.R` — kernel **inverse_gaussian** (`kernel_inverse_gaussian_def`), params: `c("mean", "shape")`
- `inst/kernels/06-kernel-amoroso.R` — kernel **amoroso** (`kernel_amoroso_def`), params: `c("location", "scale", "shape1", "shape2")`
- `inst/kernels/07-kernel-pareto.R` — kernel **pareto** (`kernel_pareto_def`), params: `c("scale", "shape")`

## Where to look for common tasks
- **Add a new bulk kernel:** add a new file under `inst/kernels/` that defines `kernel_<name>_def` (with `d/p/q/r`) and calls `register_kernel_def()`.
- **Fit model / run MCMC:** see `R/03-engine.R` and `R/04-model-methods.R`.
- **Treatment effects (ATE/QTE/CQTE):** see `R/05-causal.R`.
- **DP + model specification:** see `R/02-modelspec-dp.R`.
- **Kernel registry:** see `R/01-kernel-registry.R`.
