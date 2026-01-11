# Dataset Object Structure (Frozen)

## Non-causal datasets
Each object is a list:
```
list(
  y,
  X = NULL or data.frame,
  meta = list(n, support, p, K_true, tail, exceed_frac, seed),
  truth = list(kernel, weights, params, threshold, tail_params)
)
```

## Causal datasets
Each object is a list:
```
list(
  y, T, X,
  meta = list(N, support, p, K0, K1, tail, exceed_frac),
  truth = list(kernel0, kernel1, params0, params1, tail_params)
)
```
