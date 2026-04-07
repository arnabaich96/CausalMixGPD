"""
One-off assembler: rebuild vignettes/CausalMixGPD_JSS_article.Rmd from logical sections.
Run from package root: python tools/restructure_jss_vignette.py
"""
from __future__ import annotations

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
RMD = ROOT / "vignettes" / "CausalMixGPD_JSS_article.Rmd"


def find_header(lines: list[str], header: str) -> int:
    for i, line in enumerate(lines):
        if line.strip() == header:
            return i
    raise ValueError(f"Missing header: {header!r}")


def extract_range(lines: list[str], start: int, end: int) -> list[str]:
    return lines[start:end]


INTRO = r'''# Introduction

Causal questions are often framed through average treatment effects, yet many applications require **distributional** comparisons between arms—including behavior in the upper tail, where events are rare but consequences can be large [@ImbensRubin2015].

Flexible Bayesian causal tools (e.g. [bcf](https://cran.r-project.org/package=bcf), [bartCause](https://cran.r-project.org/package=bartCause)) excel at mean and heterogeneous effects, but tail-focused contrasts are easier when the **full outcome distribution** is modeled coherently rather than stitched from separate marginal summaries.

**CausalMixGPD** unifies (i) Dirichlet-process-mixture (**DPM**) bulk modeling, (ii) optional **generalized Pareto** (**GPD**) tails for threshold exceedances, (iii) **predictor-dependent clustering** via posterior similarity matrices and representative partitions [@Binder1978; @Dahl2006], and (iv) **distributional causal estimands** as posterior functionals of arm-specific models, with optional propensity-score augmentation in observational settings [@RosenbaumRubin1983]. Computation uses **nimble** [@nimble_pkg].

Below: **Package overview** (workflow and interface), **Customization** of common arguments, two **case studies** (clustering; causal inference), **Discussion**, **References**, then an **appendix** with background notation and model detail.


'''

POSTERIOR_CODE_CHUNK = """```{r posterior-code, echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE}
if (!is.null(fit$timing)) {
  knitr::kable(as.data.frame(fit$timing), format = \"html\", row.names = FALSE)
} else {
  invisible(NULL)
}
```

"""

BOSTON_SUMMARY_CHUNK = """```{r app_cluster_eda_summary, echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE}
knitr::kable(
  data.frame(
    Statistic = c(\"n\", \"min medv\", \"median medv\", \"mean medv\", \"max medv\"),
    Value = c(
      nrow(dat),
      signif(min(dat$medv), 6),
      signif(stats::median(dat$medv), 6),
      signif(mean(dat$medv), 6),
      signif(max(dat$medv), 6)
    )
  ),
  format = \"html\",
  row.names = FALSE
)
```

"""

LALONDE_SUMMARY_CHUNK = r"""```{r app_lalonde_eda_summary, echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE}
eda_lalonde <- data.frame(
  arm = c("control", "treated"),
  n = c(
    sum(app_lalonde_covars$treat == 0),
    sum(app_lalonde_covars$treat == 1)
  ),
  mean_re78 = c(
    mean(app_lalonde_covars$re78[app_lalonde_covars$treat == 0]),
    mean(app_lalonde_covars$re78[app_lalonde_covars$treat == 1])
  ),
  median_re78 = c(
    stats::median(app_lalonde_covars$re78[app_lalonde_covars$treat == 0]),
    stats::median(app_lalonde_covars$re78[app_lalonde_covars$treat == 1])
  )
)
eda_lalonde[, c("mean_re78", "median_re78")] <- signif(eda_lalonde[, c("mean_re78", "median_re78")], 6)
knitr::kable(eda_lalonde, format = "html", row.names = FALSE)
```

"""

QTE_RESULTS_CHUNK = """```{r qte-results, echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE}
summary(qte_fit)
```

"""

CATE_CHUNK_FIXED = """```{r cate-call, echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE}
cate_fit <- cate(fit, newdata = xnew, interval = \"hpd\", show_progress = FALSE)
summary(cate_fit)
```

"""

CQTE_CHUNK_FIXED = """```{r cqte-call, echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE}
cqte_fit <- cqte(
  fit,
  probs = c(0.25, 0.50, 0.75, 0.90, 0.95),
  newdata = xnew,
  interval = \"credible\"
)
knitr::kable(as.data.frame(cqte_fit$fit_df), format = \"html\", row.names = FALSE)
```

"""


def strip_fig_caps(s: str) -> str:
    s = re.sub(r",\s*fig\.cap='[^']*'", "", s)
    s = re.sub(r",\s*fig\.cap=\"[^\"]*\"", "", s)
    return s


def remove_plot_chunks(text: str, labels: list[str]) -> str:
    lines = text.splitlines(keepends=True)
    out: list[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        skip = False
        for lab in labels:
            if line.startswith("```{r " + lab + ",") or line.startswith("```{r " + lab + "}"):
                i += 1
                while i < len(lines) and not lines[i].strip().startswith("```"):
                    i += 1
                if i < len(lines):
                    i += 1  # closing fence
                skip = True
                break
        if not skip:
            out.append(line)
            i += 1
    return "".join(out)


def remove_kable_caption_arg(block: str) -> str:
    # remove caption = ... from knitr::kable( ... )
    return re.sub(r",\s*caption\s*=\s*[^\n)]+(\)|,)", r"\1", block, count=1)


def process_background_appendix(blob: str) -> str:
    # remove figure build + plot chunk groups
    labels_build_plot = [
        ("gpdDensityFigure-build", "gpdDensityFigure-plot"),
        ("splicedDensityFigure-build", "splicedDensityFigure-plot"),
        ("causalEstimandFigure-build", "causalEstimandFigure-plot"),
    ]
    for a, b in labels_build_plot:
        blob = remove_plot_chunks(blob, [a, b])
    blob = blob.replace(
        "The figure above illustrates the GPD density for",
        "The GPD density for",
    )
    blob = blob.replace(
        "The figure above provides a visual\ncomparison",
        "A conceptual comparison",
    )
    blob = blob.replace(
        "The figure above depicts two treatment-specific conditional density curves at a representative covariate value.  The dashed vertical lines correspond to the 90th conditional quantile for each arm, while the double-ended arrow shows the conditional quantile treatment effect (CQTE).",
        "At a fixed covariate profile $x$, contrasts such as the **conditional quantile treatment effect** $\\mathrm{CQTE}(\\tau\\mid x)=Q_1(\\tau\\mid x)-Q_0(\\tau\\mid x)$ are defined from the arm-specific conditional CDFs.",
    )
    blob = re.sub(
        r"Differentiating\nDifferentiating the spliced CDF stated earlier",
        "Differentiating the spliced CDF stated earlier",
        blob,
    )
    blob = blob.replace(
        "# Background and Preliminaries\n",
        "# Appendix: Background and preliminaries\n",
    )
    blob = blob.replace(
        "The package reports the causal estimands listed in the table below,",
        "The package reports the causal estimands in the following table.",
    )
    blob = blob.replace(
        "The next section describes how the package fits the treatment-specific\nconditional distributions and computes these estimands from posterior\npredictive output.\n\n\n",
        "The **Package overview** section describes how the package fits the treatment-specific "
        "conditional distributions and computes these estimands from posterior predictive output.\n\n\n",
    )
    # strip caption from causal-estimands kable
    blob = re.sub(
        r"\n  caption = paste\([\s\S]*?\)\n\)",
        "\n)",
        blob,
        count=1,
    )
    return blob


def process_package_overview(blob: str) -> str:
    blob = blob.replace(
        "spliced construction are provided in *Background and Preliminaries*,",
        "spliced construction are provided in the **appendix** (*Background and preliminaries*),",
    )
    blob = blob.replace(
        "*Background and Preliminaries*: the density $f(y\\mid x)$, survival function",
        "the appendix (*Background and preliminaries*): the density $f(y\\mid x)$, survival function",
    )
    blob = blob.replace(
        "*Customization of Package and Additional Features*. Specifically,",
        "**Customization** (below). Specifically,",
    )
    blob = blob.replace(
        "Other causal estimands, PS adjustments, and plots are documented in\n*Workflow-specific extensions* in the appendix.\n\n\n",
        "Other causal estimands and PS adjustments are summarized under **Workflow-specific extensions** in the **Customization** section.\n\n\n",
    )
    return blob


def process_customization(blob: str) -> str:
    blob = blob.replace(
        "This appendix details the customizable features",
        "This section details the customizable features",
    )
    blob = blob.replace(
        "Besides the customizable features illustrated in *Generalized Pareto distribution for threshold exceedances*, *Dirichlet process mixture model for conditional densities*, *Causal Inference: Notation, Identification, and Reported Estimands*, and *Spliced DPM--GPD model, Likelihood, and Covariate Dependence*",
        "Besides the theory sections in the **appendix** (*Background and preliminaries*) that motivate the GPD tail, DPM bulk, causal estimands, and spliced likelihood",
    )
    blob = blob.replace(
        "Representative outputs include the cluster overlay plot, test-sample cluster assignment sizes, the standardized QTE figure, and the mean-based CATE figure together with the CQTE table.",
        "Representative outputs in the case studies include numeric summaries of clustering (e.g. similarity summaries and partition sizes) and causal estimands (ATE/QTE/CATE/CQTE tables).",
    )
    return blob


def process_data1(blob: str) -> str:
    blob = blob.replace("# Data Analysis~I: Clustering\n", "# Data analysis I: Clustering\n")
    blob = blob.replace(
        "The following figure shows the descriptive analyses of the Boston housing price data as represented by the median home value (medv). The variability in the response variable is quite large, and there seems to be an elongated tail toward higher values. The implication of these observations is that there would be a more accurate model of structure when using a flexible mixture approach, rather than a simple regression.\n\n",
        "Across all census tracts, **medv** is right-skewed with a long upper tail, motivating flexible mixture modeling rather than a simple location-shift specification.\n\n",
    )
    blob = remove_plot_chunks(blob, ["app_cluster_response_panels"])
    blob = BOSTON_SUMMARY_CHUNK + "\n" + blob
    blob = remove_plot_chunks(blob, ["app_cluster_psm_plot-code", "app_cluster_overlay", "app_cluster_test_sizes"])
    blob = blob.replace(
        "As seen from the PSM summary figure above, the PSM\nreveals",
        "The posterior similarity matrix (PSM) reveals",
    )
    blob = blob.replace(
        "The following figure shows the representative training\ncluster summary produced by the package plot() method applied\nto the training labels object.\n\n",
        "Representative training-cluster sizes and profiles are summarized numerically below.\n\n",
    )
    blob = blob.replace(
        "The following figure displays the distribution of\npredicted cluster assignments across the test sample, produced by the\npackage plot() method applied to the test labels object.\n\n",
        "Test-set assignment counts by representative training cluster:\n\n",
    )
    # After z_test predict chunk, inject table of assignment counts
    blob = blob.replace(
        "```\n\n```{r app_cluster_test_profiles",
        "```\n\n```{r app_cluster_test_assignment_table, echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE}\n"
        "tab_assign <- as.data.frame(table(Predicted_cluster = z_test$labels))\n"
        "names(tab_assign)[2] <- \"n_test\"\n"
        "knitr::kable(tab_assign, format = \"html\", row.names = FALSE)\n"
        "```\n\n```{r app_cluster_test_profiles",
    )
    return blob


QTE_BLOCK_OLD = """```{r qte-call, echo=TRUE, message=FALSE, warning=FALSE, results='hide', eval=TRUE}
qte_fit <- qte(fit, probs = c(0.25, 0.50, 0.75), interval = "credible")
```

The following figure displays two standardized QTE views side by
side from the spliced causal model: the effect curve and the
treated-versus-control quantile curves with uncertainty intervals.

```{r qte_plot, echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE, out.width='95%', fig.cap='Standardized quantile treatment effect (QTE) summaries for Lalonde 1978 earnings, returned by qte(fit, probs = c(0.25, 0.50, 0.75), interval = "credible"). Left panel: effect curve $Q_1^{\\\\mathrm{m}}(\\\\tau)-Q_0^{\\\\mathrm{m}}(\\\\tau)$ versus $\\\\tau$ with pointwise $95\\\\%$ credible band. Right panel: standardized treated and control quantile curves $Q_1^{\\\\mathrm{m}}(\\\\tau)$ and $Q_0^{\\\\mathrm{m}}(\\\\tau)$ on the original earnings scale, with pointwise credible bands.'}
qte_effect_plot <- plot(qte_fit, type = "effect")
qte_arms_plot <- plot(qte_fit, type = "arms")
qte_effect_plot + qte_arms_plot + plot_layout(ncol = 2)
```

"""

QTE_BLOCK_NEW = """```{r qte-call, echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE}
qte_fit <- qte(fit, probs = c(0.25, 0.50, 0.75), interval = "credible")
```

```{r qte-results, echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE}
summary(qte_fit)
```

"""

CATE_MULTI_OLD = """```{r cate-call, echo=TRUE, message=FALSE, warning=FALSE, results='hide', eval=TRUE}
cate_fit <- cate(fit, newdata = xnew, interval = "hpd", show_progress = FALSE)
summary(cate_fit)
# plot(cate_fit, type = "effect") # Displays mean-based CATE plot by profile
```

The mean-based conditional average treatment effects are generally
modest and uncertain. Although some profiles show positive point
estimates, the corresponding intervals are broad and typically overlap
zero. Thus, at the level of conditional means, the program does not
appear to induce a strong and uniform shift in earnings across all
participant types.

```{r cqte-call, echo=TRUE, message=FALSE, warning=FALSE, results='hide', eval=TRUE}
cqte_fit <- cqte(fit, probs = c(0.25, 0.50, 0.75, 0.90, 0.95),
                 newdata = xnew, interval = "credible")
# cqte_fit$fit_df # Displays the full CQTE posterior summary data frame
```


The figure below shows the mean-based CATE summary, and the CQTE
table below reports posterior estimates with 95% credible intervals.

```{r cate_plot, echo=FALSE, message=FALSE, warning=FALSE, eval=TRUE, fig.width=7, fig.height=4, out.width='90%', fig.cap='Conditional average treatment effect summaries for Lalonde 1978 earnings at representative covariate profiles, returned by cate(fit, newdata = xnew, interval = "hpd").' }
plot(cate_fit, type = "effect")
```

```{r cqte_table, echo=FALSE, message=FALSE, warning=FALSE, eval=TRUE, results='asis'}
knitr::kable(
  as.data.frame(cqte_fit$fit_df),
  format = "html",
  caption = "CQTE posterior summaries across profiles and quantile indices.",
  row.names = FALSE
)
```

"""

CATE_MULTI_NEW = """```{r cate-call, echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE}
cate_fit <- cate(fit, newdata = xnew, interval = "hpd", show_progress = FALSE)
summary(cate_fit)
```

The mean-based conditional average treatment effects are generally
modest and uncertain. Although some profiles show positive point
estimates, the corresponding intervals are broad and typically overlap
zero. Thus, at the level of conditional means, the program does not
appear to induce a strong and uniform shift in earnings across all
participant types.

```{r cqte-call, echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE}
cqte_fit <- cqte(fit, probs = c(0.25, 0.50, 0.75, 0.90, 0.95),
                 newdata = xnew, interval = "credible")
knitr::kable(as.data.frame(cqte_fit$fit_df), format = "html", row.names = FALSE)
```

Posterior summaries for mean-based CATE appear above; the CQTE table appears above as well.

"""


def process_data2(blob: str) -> str:
    blob = blob.replace("# Data Analysis~II: Causal Inference\n", "# Data analysis II: Causal inference\n")
    blob = blob.replace(
        "We estimate the causal estimands defined in the table shown earlier under *Causal Inference: Notation, Identification, and Reported Estimands*, using the notation and setup from that section.",
        "We estimate the causal estimands defined in the **appendix** (*Causal Inference: Notation, Identification, and Reported estimands*).",
    )
    blob = blob.replace(
        "The following figure provides full-sample\ndescriptive views of observed 1978 earnings (re78) by treatment\nassignment. Both groups exhibit substantial mass near zero together\nwith pronounced right-skewness, and the arm-specific distributions\ndiffer in ways that are not well summarized by a simple location shift.\nThese features motivate modeling the full treatment-specific outcome\ndistributions directly rather than relying solely on average effects.\n\n",
        "Earnings are heavily skewed with substantial mass near zero for both arms; arm-specific distributions differ in ways not captured by a simple mean shift, motivating full distributional modeling.\n\n",
    )
    blob = remove_plot_chunks(blob, ["app_lalonde_response_panels"])
    blob = LALONDE_SUMMARY_CHUNK + "\n" + blob
    if QTE_BLOCK_OLD in blob:
        blob = blob.replace(QTE_BLOCK_OLD, QTE_BLOCK_NEW, 1)
    else:
        blob = remove_plot_chunks(blob, ["qte_plot"])
        blob = blob.replace("results='hide', eval=TRUE}", "eval=TRUE}", 1)
        blob = blob.replace("results='hide', eval=TRUE}", "eval=TRUE}", 1)
    blob = strip_fig_caps(blob)
    blob = remove_plot_chunks(blob, ["qte_plot"])
    blob = blob.replace(
 'knitr::kable(\n  profile_t,\n  format = "html",\n  caption = "Representative Lalonde participant profiles used as new-data inputs for the conditional causal contrasts shown in the mean-based CATE figure below. Columns (Profile 1--Profile 4) give the covariate values for one synthetic individual on the original data scale: age (years), educ (years of schooling), race, married, nodegree, and prior annual earnings re74 and re75 (US dollars).",\n  row.names = TRUE\n)',
 'knitr::kable(\n  profile_t,\n  format = "html",\n  row.names = TRUE\n)',
    )
    if CATE_MULTI_OLD in blob:
        blob = blob.replace(CATE_MULTI_OLD, CATE_MULTI_NEW, 1)
    else:
        blob = remove_plot_chunks(blob, ["cate_plot", "cqte_table"])
    blob = blob.replace(
        "The figure below shows the mean-based CATE summary, and the CQTE\ntable below reports posterior estimates with 95% credible intervals.\n\n",
        "",
    )
    return blob


def process_discussion(blob: str) -> str:
    return blob


def main() -> None:
    text = RMD.read_text(encoding="utf-8")
    lines = text.splitlines(keepends=True)

    yaml_end = 0
    seen = 0
    for i, ln in enumerate(lines):
        if ln.strip() == "---":
            seen += 1
            if seen >= 2:
                yaml_end = i + 1
                break

    setup_start = find_header(lines, "```{r setup, include=FALSE, eval=TRUE}")
    # find closing ``` of setup chunk
    setup_end = setup_start + 1
    while setup_end < len(lines) and lines[setup_end].strip() != "```":
        setup_end += 1
    setup_end += 1

    i_bg = find_header(lines, "# Background and Preliminaries")
    i_pkg = find_header(lines, "# Package Overview: Workflow, Model, and Inference")
    i_d1 = find_header(lines, "# Data Analysis~I: Clustering")
    i_d2 = find_header(lines, "# Data Analysis~II: Causal Inference")
    i_disc = find_header(lines, "# Discussion")
    i_cust = find_header(lines, "# Customization of Package and Additional Features")

    yaml_block = "".join(lines[:yaml_end])
    setup_chunk = "".join(lines[setup_start:setup_end])

    background = "".join(lines[i_bg:i_pkg])
    overview = "".join(lines[i_pkg:i_d1])
    data1 = "".join(lines[i_d1:i_d2])
    data2 = "".join(lines[i_d2:i_disc])
    discussion = "".join(lines[i_disc:i_cust])
    customization = "".join(lines[i_cust:])

    background = process_background_appendix(background)
    overview = process_package_overview(overview)
    customization = process_customization(customization)
    data1 = process_data1(data1)
    data2 = process_data2(data2)
    discussion = process_discussion(discussion)

    # setup tweaks: cache off, fig caps globally none
    setup_chunk = setup_chunk.replace("cache = TRUE", "cache = FALSE")
    setup_chunk = setup_chunk.replace(
        "library(ggplot2)\nknitr::opts_knit$set(concordance = FALSE)\n",
        "library(ggplot2)\nlibrary(knitr)\nknitr::opts_knit$set(concordance = FALSE)\n",
    )

    refs = """

# References

::: {#refs}
:::


"""

    out = (
        yaml_block
        + "\n"
        + setup_chunk
        + "\n"
        + INTRO
        + overview
        + customization
        + data1
        + data2
        + discussion
        + refs
        + background
    )

    RMD.write_text(out, encoding="utf-8", newline="\n")
    print("Wrote", RMD)


if __name__ == "__main__":
    main()
