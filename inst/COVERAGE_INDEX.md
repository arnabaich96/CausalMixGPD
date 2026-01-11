# Coverage Index (Frozen)

This index prevents drift by listing datasets, kernels, backends, GPD usage,
and S3 methods expected in vignettes. Update only by explicit design change.

## Dataset -> vignette mapping

### Non-causal (no X)
- nc_real200_k2: quickstart, backends, kernels, prediction
- nc_pos200_k3: kernels, quickstart
- nc_pos_tail200_k4: gpd-tail, prediction

### Non-causal (with X)
- nc_posX100_p3_k2: covariates, prediction
- nc_posX100_p4_k3: covariates
- nc_posX100_p5_k4: covariates
- nc_realX100_p3_k2: covariates
- nc_realX100_p5_k3: covariates

### Causal
- causal_pos500_p3_k2: causal-quickstart, same-vs-different-kernels
- causal_alt_pos500_p3_k3: same-vs-different-kernels
- causal_alt_real500_p4_k2: same-vs-different-kernels
- causal_alt_pos500_p5_k4_tail: causal-tail

## Kernel x backend x GPD coverage

### No-X, real-line (nc_real200_k2, GPD=FALSE)
- normal: CRP, SB
- laplace: CRP, SB
- cauchy: CRP, SB

### No-X, positive bulk-only (nc_pos200_k3, GPD=FALSE)
- gamma: CRP, SB
- lognormal: CRP, SB
- invgauss: CRP, SB
- amoroso (shape1=1): CRP, SB

### No-X, positive tail-designed (nc_pos_tail200_k4, GPD=TRUE)
- gamma: CRP, SB
- lognormal: CRP, SB
- invgauss: CRP, SB
- amoroso (shape1=1): CRP, SB

### With-X, positive (each of nc_posX100_p3_k2/p4_k3/p5_k4, GPD=FALSE)
- gamma: CRP, SB
- lognormal: CRP, SB
- invgauss: CRP, SB
- amoroso (shape1=1): CRP, SB

### With-X, real-line (each of nc_realX100_p3_k2/p5_k3, GPD=FALSE)
- normal: CRP, SB
- laplace: CRP, SB
- cauchy: CRP, SB

## Causal kernel-by-arm combinations

### Same kernel by arm (causal_pos500_p3_k2, GPD=FALSE)
- gamma-gamma, lognormal-lognormal, invgauss-invgauss, amoroso-amoroso

### Different kernels by arm
- causal_alt_pos500_p3_k3: lognormal vs gamma (GPD=FALSE)
- causal_alt_real500_p4_k2: normal vs laplace (or cauchy without GPD)
- causal_alt_pos500_p5_k4_tail: invgauss vs amoroso (GPD=TRUE, tail-designed)

## S3 methods demonstrated
- print(), summary(), plot(), predict() in bundle/S3 vignette.
- Additional generics (coef(), logLik(), fitted(), residuals()) where applicable.

## DQRP clubs
- Real-line club: normal, laplace, cauchy + one bulk+GPD curve.
- Positive club: gamma, lognormal, invgauss, amoroso + one bulk+GPD curve.
