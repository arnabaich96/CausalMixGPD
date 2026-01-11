# DPmixGPD Dataset Inventory (Frozen)

## Non-causal (no covariates, n = 200)
- nc_real200_k2 (real-line, bulk-only, K=2)
- nc_pos200_k3 (positive support, bulk-only, K=3)
- nc_pos_tail200_k4 (positive support, tail-designed, K=4)

## Non-causal with covariates (n = 100)
- nc_posX100_p3_k2
- nc_posX100_p4_k3
- nc_posX100_p5_k4
- nc_realX100_p3_k2
- nc_realX100_p5_k3

## Causal (N = 500)
- causal_pos500_p3_k2 (same kernel by arm)
- causal_alt_pos500_p3_k3 (different positive kernels by arm)
- causal_alt_real500_p4_k2 (different real-line kernels by arm)
- causal_alt_pos500_p5_k4_tail (different kernels by arm + tail-designed)
