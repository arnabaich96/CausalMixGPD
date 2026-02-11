# DPmixGPD: Model fundamentals (bulk DPM + GPD tail)

``` r

library(DPmixGPD)
```

This vignette defines the statistical model underlying DPmixGPD. The
three ingredients are:

1.  a Dirichlet process mixture (DPM) for the *bulk* (central) part of
    the distribution,
2.  a generalized Pareto distribution (GPD) model for the *tail*
    (extremes), and
3.  a *proper* splicing construction that yields a valid density/CDF on
    the full support.

Other vignettes (unconditional, conditional, causal) refer back here
rather than repeating these definitions.

## Bulk model: Dirichlet process mixture

Let $`y_1,\dots,y_n`$ denote outcomes. DPmixGPD uses a mixture
representation
``` math
  y_i \mid z_i=j \sim k(y_i \mid \Theta_j), \qquad
  z_i \sim \mathrm{Categorical}(w_1,w_2,\dots),
```
so the bulk density is
``` math
  f_{\mathrm{DPM}}(y)
  =
  \sum_{j=1}^\infty w_j \, k(y \mid \Theta_j).
```

A Dirichlet process prior is imposed on the random mixing measure $`G`$
underlying the parameters $`\Theta_j`$:
``` math
  G \sim \mathrm{DP}(\alpha, G_0), \qquad \Theta_j \overset{iid}{\sim} G.
```
Here $`\alpha>0`$ is the concentration parameter controlling expected
clustering, and $`G_0`$ is the base measure encoding prior beliefs about
kernel parameters ([Ferguson 1973](#ref-ferguson1973)).

DPmixGPD supports two equivalent DP representations:

- stick-breaking (“SB”) ([Sethuraman 1994](#ref-sethuraman1994)),
  implemented with a finite truncation at $`J`$ components ([Ishwaran
  and James 2001](#ref-ishwaran2001));
- Chinese restaurant process (“CRP”), a partition-based formulation
  ([**blackwell1973?**](#ref-blackwell1973); [Neal
  2000](#ref-neal2000)).

In SB form (truncated at $`J`$),
``` math
  v_j \sim \mathrm{Beta}(1,\alpha),\quad
  w_1=v_1,\quad
  w_j=v_j\prod_{\ell<j}(1-v_\ell)\ (j\ge 2),\quad
  w_J = 1-\sum_{j=1}^{J-1} w_j.
```

## Tail model: generalized Pareto distribution

For upper-tail modeling, fix a threshold $`u`$ and define exceedances
$`x=y-u>0`$ for $`y>u`$. The GPD density for exceedances is
``` math
  g(x\mid \sigma,\xi)
  =
  \frac{1}{\sigma}
  \left(1+\xi\frac{x}{\sigma}\right)^{-1/\xi - 1},
  \qquad x>0,\ \sigma>0,
```
with support constraint $`1+\xi x/\sigma>0`$. The corresponding CDF is
``` math
  G(x\mid\sigma,\xi)
  =
  1-\left(1+\xi\frac{x}{\sigma}\right)^{-1/\xi}
  \quad (\xi\ne 0),
```
and in the limit $`\xi\to 0`$,
``` math
  G(x\mid\sigma,0)=1-\exp(-x/\sigma).
```
This GPD limit result for threshold exceedances is central in extreme
value theory ([Balkema and Haan 1974](#ref-balkema1974); [Pickands
1975](#ref-pickands1975); [Coles 2001](#ref-coles2001)).

## Splicing: a proper bulk–tail distribution

DPmixGPD combines a bulk model $`F_{\mathrm{DPM}}`$ with a GPD
exceedance model while ensuring the resulting full distribution
integrates to 1.

Let $`u`$ denote the tail threshold. Define:
``` math
  p_u = F_{\mathrm{DPM}}(u), \qquad \bar p_u = 1 - p_u.
```
The spliced CDF is defined by:
``` math
F(y) =
\begin{cases}
F_{\mathrm{DPM}}(y), & y \le u,\\[6pt]
p_u + \bar p_u \, G(y-u\mid\sigma,\xi), & y>u.
\end{cases}
```
By construction, $`F`$ is nondecreasing, right-continuous, and satisfies
$`\lim_{y\to-\infty}F(y)=0`$, $`\lim_{y\to\infty}F(y)=1`$. The
corresponding density is:
``` math
f(y) =
\begin{cases}
f_{\mathrm{DPM}}(y), & y \le u,\\[6pt]
\bar p_u \, g(y-u\mid\sigma,\xi), & y>u.
\end{cases}
```
This “bulk + tail using all observations” Bayesian philosophy matches
earlier semiparametric bulk–tail constructions ([Frigessi et al.
2002](#ref-frigessi2002); [Behrens et al. 2004](#ref-behrens2004);
[Nascimento et al. 2012](#ref-doNascimento2012)).

## Posterior computation in DPmixGPD

DPmixGPD implements these models through NIMBLE-backed MCMC. At a high
level, posterior inference alternates between:

1.  allocation updates $`z_i`$ (component membership),
2.  component parameter updates $`\Theta_j`$,
3.  mixture weight updates (SB/CRP),
4.  tail updates $`(u,\sigma,\xi)`$ if `GPD = TRUE`,
5.  hyperparameter updates (e.g., $`\alpha`$) where stochastic.

All user-facing workflows produce a `mixgpd_fit` object (or a causal fit
object wrapping two `mixgpd_fit`s), which then supports
[`predict()`](https://rdrr.io/r/stats/predict.html),
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html), and related S3
methods.

## References

Balkema, August A., and Laurens de Haan. 1974. “Residual Life Time at
Great Age.” *The Annals of Probability* 2 (5): 792–804.
<https://doi.org/10.1214/aop/1176996548>.

Behrens, Cibele N., Hedibert F. Lopes, and Dani Gamerman. 2004.
“Bayesian Analysis of Extreme Events with Threshold Estimation.”
*Statistical Modelling* 4 (3): 227–44.
<https://doi.org/10.1191/1471082x04st075oa>.

Coles, Stuart. 2001. *An Introduction to Statistical Modeling of Extreme
Values*. Springer. <https://doi.org/10.1007/978-1-4471-3675-0>.

Ferguson, Thomas S. 1973. “A Bayesian Analysis of Some Nonparametric
Problems.” *The Annals of Statistics* 1 (2): 209–30.
<https://doi.org/10.1214/aos/1176342360>.

Frigessi, Arnoldo, Ola Haug, and Håvard Rue. 2002. “A Dynamic Mixture
Model for Unsupervised Tail Estimation Without Threshold Selection.”
*Extremes* 5 (3): 219–35. <https://doi.org/10.1023/A:1024072610684>.

Ishwaran, Hemant, and Lancelot F. James. 2001. “Gibbs Sampling Methods
for Stick-Breaking Priors.” *Journal of the American Statistical
Association* 96 (453): 161–73.
<https://doi.org/10.1198/016214501750332758>.

Nascimento, Fernando Ferraz do, Dani Gamerman, and Hedibert Freitas
Lopes. 2012. “A Semiparametric Bayesian Approach to Extreme Value
Estimation.” *Statistics and Computing* 22 (2): 661–75.
<https://doi.org/10.1007/s11222-011-9270-z>.

Neal, Radford M. 2000. “Markov Chain Sampling Methods for Dirichlet
Process Mixture Models.” *Journal of Computational and Graphical
Statistics* 9 (2): 249–65.
<https://doi.org/10.1080/10618600.2000.10474879>.

Pickands, James. 1975. “Statistical Inference Using Extreme Order
Statistics.” *The Annals of Statistics* 3 (1): 119–31.
<https://doi.org/10.1214/aos/1176343003>.

Sethuraman, Jayaram. 1994. “A Constructive Definition of Dirichlet
Priors.” *Statistica Sinica* 4 (2): 639–50.
