# Performance Refactor Acceptance Checklist Map

This file maps merge-gate checklist items to concrete automated checks.

1. Functional equivalence:
- `tests/testthat/test-integration.R`:
  `functional equivalence and contracts on seeded runs` (full level),
  plus contract checks in CI-level tests.

2. Compile cache:
- `tests/testthat/test-integration.R`:
  `compile cache exposes timing and reuses build/compile`.

3. Parallel chains:
- `tests/testthat/test-integration.R`:
  `parallel chains path preserves output contract`.

4. Causal arm parallelism:
- `tests/testthat/test-integration.R`:
  `causal arms can run in parallel and preserve contract`.

5. Prediction scaling:
- `tests/testthat/test-integration.R`:
  `predict supports ndraws_pred/chunk_size/parallel aliases`.
- `tests/perf/benchmark_acceptance.R`:
  `Prediction (large)` benchmark row.

6. Monitoring policy:
- `tests/testthat/test-integration.R`:
  `monitoring policy defaults exclude latent z and opt-in works`.

7. No global side effects:
- `tests/testthat/test-integration.R`:
  `parallel runtime does not leave global future plan modified`.

8. CRAN safety:
- Parallel tests fall back to sequential when `future`/`future.apply` missing;
  this is asserted by runtime warning/fallback branches in
  `R/build-run.R`, `R/causal.R`, `R/internal.R`, and `R/engine.R`.
- CI baseline should run with parallel off.

9. Benchmark report:
- Run `tests/perf/benchmark_acceptance.R` to generate:
  `tests/perf/benchmark_acceptance.csv` and
  `tests/perf/benchmark_acceptance_report.md`.

10. Phase 2 benchmark report:
- Run `tests/perf/benchmark_phase2.R` to generate:
  `tests/perf/benchmark_phase2_results.csv` and
  `tests/perf/benchmark_phase2_report.md`.
- Includes ESS and ESS/sec columns plus resolved canonical parameters.
