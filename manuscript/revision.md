# Review Revision Suggestions and Resolutions

This file lists the review comments point by point, in the order given in the original review note, together with the resolutions already made in the package, manuscript, documentation, or replication material.

## General Review Points

1. **Original comment:** The package description refers to `https://doi.org/10.5281/zenodo.19620523`, but the record appears to be retracted and cannot be accessed.

   **Resolution taken:** The removed Zenodo reference was replaced with the updated methodology DOI `10.5281/zenodo.19672760` in `DESCRIPTION`, `README.md`, `inst/CITATION`, the citation page, and the manuscript citation text.

2. **Original comment:** JSS requires that the methodology be published first in a journal focused on statistical methodology before reviewing the software article.

   **Resolution taken:** The manuscript now separates software contribution from methodological development, cites the methodology work explicitly, and states that methodological development beyond the software article should be evaluated separately. Final journal publication of the methodology remains an external submission requirement rather than a package-code change.

3. **Original comment:** The package and replication material need to be improved before the work can be considered for review.

   **Resolution taken:** The package version was advanced to `0.8.0`, the documentation and examples were regenerated, replication workflows were consolidated, reviewer-facing tests were added, and the manuscript was rebuilt with updated figures and outputs.

## Detailed Comments

1. **Original comment:** In the article, citations of other packages must use the standard JSS format.

   **Resolution taken:** Package references in the JSS manuscript were converted to standard JSS-style `\pkg{}` package markup with normal citation commands, and the manuscript was rebuilt.

2. **Original comment:** The discussion of state-of-the-art implementations is restricted to R and should include software implementations in other languages.

   **Resolution taken:** The introduction was expanded to discuss non-R implementations, including Python tools such as `pyrichlet`, `DoWhy`, `EconML`, and `scikit-learn`, the Python/R `BayesMix` interface, and Julia's `DPMMSubClusters.jl`.

3. **Original comment:** The replication material is not prepared as requested by JSS because five replication scripts are provided, while JSS requires a single replication script.

   **Resolution taken:** A single package-level replication entry point was added at `inst/replication/replicate.R`, and the manuscript replication workflow was consolidated into `manuscript/replication/replicate_all.R`. The prior section-specific scripts were moved under `build_helpers` and are no longer separate replication entry points.

4. **Original comment:** `help(package = "CausalMixGPD")` shows that not all man page titles are in title style.

   **Resolution taken:** Manual-page titles were revised and regenerated. A reviewer-facing test now checks that generated `.Rd` titles do not start with a lowercase letter.

5. **Original comment:** `help(package = "CausalMixGPD")` shows that some listed functions are not intended for users and should be removed from the list using the `internal` keyword.

   **Resolution taken:** Internal builder/code-generation helpers were marked with `@keywords internal`, removed from the exported namespace where appropriate, and regenerated in the manual with the `\keyword{internal}` marker.

6. **Original comment:** Some groups of functions listed in the help index are redundant, especially several plot methods that could be grouped on a single man page or handled by one function with different arguments.

   **Resolution taken:** Redundant plot documentation was consolidated with shared `@rdname` pages. Causal effect plots now share `plot.causalmixgpd_qte.Rd`, one-arm and causal diagnostic plots share `plot.mixgpd_fit.Rd`, prediction plots share `plot.mixgpd_predict.Rd`, and cluster plot methods share `plot.dpmixgpd_cluster_fit.Rd`.

7. **Original comment:** Main functions such as `dpmix.cluster` and `dpmgpd.causal` have help files but no examples.

   **Resolution taken:** Examples were added to the main wrapper documentation, including `dpmix.cluster`, `dpmgpd.cluster`, `dpmix.causal`, and `dpmgpd.causal`, and pkgdown/reference pages were regenerated.

8. **Original comment:** The article does not give a clear overview of all functions and only illustrates a few examples; an overview of package structure and classes is mandatory.

   **Resolution taken:** The manuscript now includes a package overview section with a table of core exported functions grouped by workflow stage and a table of principal object classes with their main public methods/accessors.

9. **Original comment:** Several model classes have similar structures and methods but distinct names and no common ground; inheritance would improve modularity and maintenance.

   **Resolution taken:** Fitted model objects now include common S3 base classes such as `causalmixgpd_fit`, and treatment-effect result objects include `causalmixgpd_effect`. Reviewer-facing tests verify these class relationships.

10. **Original comment:** Some methods appear to be missing.

    **Resolution taken:** Additional S3 methods and registrations were added, including causal estimand methods for `ate`, `cate`, `qte`, and `cqte`, a `print.mixgpd_predict` method, and shared method documentation for prediction, plotting, summaries, fitted values, residuals, and cluster profile accessors.

11. **Original comment:** Calling `plot(fit)` after `example("cate")` produces all possible plots, erasing each other on the default plotting device. Calling `plot` without arguments should produce a single plot. The requirement for `ggmcmc` should be limited to plot types that need it.

    **Resolution taken:** The default diagnostic plot family was changed to a single `traceplot`. Users can request all diagnostics explicitly with `family = "all"`. Causal-fit plotting now defaults to the treated arm and uses `arm = "both"` only when both arms are explicitly requested. The diagnostic `ggmcmc` dependency is limited to MCMC diagnostic plotting, while treatment-effect and prediction plots use their own plotting methods.

12. **Original comment:** Running `example(ate)` and printing the fit object shows `NA` timing values.

    **Resolution taken:** Timing capture was revised for propensity-score, control-arm, treated-arm, parallel, and sequential causal fits. Print and summary methods now omit unavailable timing values rather than printing `NA` fields, and reviewer-facing tests cover this behavior.

13. **Original comment:** `plot(fit)` produces repeated ggplot scale warnings and many diagnostic warnings, and without `par(ask = TRUE)` the plots cannot be inspected.

    **Resolution taken:** Diagnostic plotting now returns named plot objects instead of forcing every plot directly onto the device. Print methods render plot collections with section labels, the default plot set is a single trace plot, redundant ggplot scale warnings are muffled where possible, and multi-plot output is requested explicitly.

14. **Original comment:** Methods are missing for `causalmixgpd_causal_fit`; more methods are expected for model classes.

    **Resolution taken:** The causal fit class now participates in a broader S3 structure through `causalmixgpd_fit`, and effect-estimation functions were converted to S3 methods where appropriate. The manuscript class overview now lists the intended public methods and accessors.

15. **Original comment:** `cate` is defined as a function but begins by checking that `fit` inherits from `causalmixgpd_causal_fit`; making it a method would be clearer.

    **Resolution taken:** `cate` was converted to an S3 generic with `cate.causalmixgpd_causal_fit` and `cate.default`. The same S3 pattern was applied to `ate`, `qte`, and `cqte`.

16. **Original comment:** Several functions do not check inputs as expected, and progress output can obscure an error, as shown by `ate(fit, level = 0, nsim_mean = 100)`.

    **Resolution taken:** Causal effect helpers now validate `fit`, `level`, `probs`, `nsim_mean`, and required covariate inputs before progress output begins. Wrapper MCMC arguments now reject unknown and malformed inputs with explicit messages.

17. **Original comment:** The replication material directly queries object internals with `summary(z_test)$cluster_profiles`; this should be avoided. Summary output should have its own class and accessors.

    **Resolution taken:** A public `cluster_profiles()` accessor was added and exported for cluster fits, labels, and summary objects. The manuscript and replication scripts now use `cluster_profiles(z_test)` rather than reaching into `summary()` internals.

18. **Original comment:** The implementation repeatedly uses `Xpred %*% t(beta_mat)`, but `crossprod` or `tcrossprod` is usually faster.

    **Resolution taken:** Matrix multiplication hot paths were updated to use `tcrossprod()` where applicable, including prediction and causal-design calculations.

19. **Original comment:** Loading the package also loads `nimble`, which masks objects from `stats` and `base`; this raises the question why `nimble` is not only in `Imports`.

    **Resolution taken:** `nimble` remains in `Imports` and is no longer listed in `Depends`, so loading `CausalMixGPD` should not attach `nimble` to the search path.

20. **Original comment:** `DESCRIPTION` links to `https://zenodo.org/records/19620523`, which is removed from Zenodo.

    **Resolution taken:** The removed record was replaced by `10.5281/zenodo.19672760` in package metadata and related citation surfaces.

21. **Original comment:** `citation(package = "CausalMixGPD")` is not the same as the manuscript citation and appears outdated.

    **Resolution taken:** `inst/CITATION` was updated to provide separate citation entries for the package and the methodology paper, and README/citation guidance was updated to match the manuscript.

22. **Original comment:** `methods(class = "mixgpd_predict")` shows only `plot`; a print method is missing. Plotting `rmean` predictions returns `NULL` with an unknown prediction type warning.

    **Resolution taken:** `print.mixgpd_predict` was added and registered. `plot.mixgpd_predict` now handles `rmean` prediction objects and returns a structured `mixgpd_predict_plots` object.

23. **Original comment:** Running manuscript code gives results that differ from those shown in the manuscript, including the ATE summary and Figures 9 and 10.

    **Resolution taken:** The manuscript was rebuilt from the updated code and consolidated replication scripts. The regenerated PDFs, figures, cache objects, and pkgdown outputs now reflect the current computations and package version.

24. **Original comment:** The manuscript states that the posterior similarity matrix in Figure 5 shows a clear block structure, but this was not visible; row/column re-ordering may be required.

    **Resolution taken:** The clustering PSM figure and caption were updated to use ordered rows and columns so block structure is visible. The manuscript text and regenerated figure now describe the ordered PSM display.

