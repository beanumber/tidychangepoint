
# tidychangepoint

<!-- badges: start -->
[![R-CMD-check](https://github.com/beanumber/tidychangepoint/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/beanumber/tidychangepoint/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/tidychangepoint)](https://CRAN.R-project.org/package=tidychangepoint)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/tidychangepoint)](https://www.r-pkg.org/pkg/tidychangepoint)
<!-- badges: end -->

## Usage

To install `tidychangepoint` from CRAN:

``` r
install.packages("tidychangepoint")
```

To install the development version of `tidychangepoint`:

``` r
remotes::install_github("beanumber/tidychangepoint")
```

To load it:

``` r
library(tidychangepoint)
```

## Tidy methods for changepoint analysis

The `tidychangepoint` package allows you to use any number of algorithms
for detecting changepoint sets in univariate time series with a common,
`tidyverse`-compliant interface. Currently, algorithms from
`changepoint`, `wbs`, and several genetic algorithms made accessible via
`GA` are supported. It also provides model-fitting procedures for
commonly-used parametric models, tools for computing various penalty
functions, and graphical diagnostic displays.

Changepoint sets are computed using the `segment()` function, which
takes a numeric vector that is coercible into a `ts` object, and a
string indicating the algorithm you wish you use. `segment()` always
returns a `tidycpt` object.

``` r
x <- segment(CET, method = "pelt", minseglen = 3)
class(x)
```

    ## [1] "tidycpt"

Various methods are available for `tidycpt` objects. For example,
`as.ts()` returns the original data as `ts` object, and `changepoints()`
returns the set of changepoint indices.

``` r
changepoints(x)
```

    ## [1] 237 330

If the original time series has time labels, we can also retrieve that
information.

``` r
changepoints(x, use_labels = TRUE)
```

    ## [1] "1895-01-01" "1988-01-01"

The `fitness()` function returns the both the value and the name of the
objective function that the algorithm used to find the optimal
changepoint set.

``` r
fitness(x)
```

    ##     MBIC 
    ## 643.5292

## Algorithmic coverage

``` r
tidycpt_coverage() |>
  knitr::kable()
```

| method | pkg | version | segmenter_class | models | penalties | helper | wraps |
|:---|:---|:---|:---|:---|:---|:---|:---|
| ga | GA | 3.2.4 | tidyga | fit_lmshift, fit_lmshift_ar1, fit_meanshift, fit_meanshift_lnorm, fit_meanshift_norm, fit_meanshift_norm_ar1, fit_meanshift_region, fit_meanshift2, fit_meanvar, fit_nhpp, fit_nhpp_region, fit_trendshift, fit_trendshift_ar1 | AIC, BIC, MBIC, MDL | segment_ga() | GA::ga() |
| ga-coen | GA | 3.2.4 | tidyga | fit_nhpp | BMDL | segment_ga_coen() | segment_ga() |
| ga-shi | GA | 3.2.4 | tidyga | fit_meanshift_norm_ar1 | BIC | segment_ga_shi() | segment_ga() |
| random | GA | 3.2.4 | tidyga | fit_lmshift, fit_lmshift_ar1, fit_meanshift, fit_meanshift_lnorm, fit_meanshift_norm, fit_meanshift_norm_ar1, fit_meanshift_region, fit_meanshift2, fit_meanvar, fit_nhpp, fit_nhpp_region, fit_trendshift, fit_trendshift_ar1 | AIC, BIC, MBIC, MDL | segment_ga_random() | segment_ga() |
| binseg | changepoint | 2.3 | cpt | fit_meanvar | None, SIC, BIC, MBIC, AIC, Hannan-Quinn, Asymptotic, Manual, CROPS | NA | changepoint::cpt.meanvar() |
| pelt | changepoint | 2.3 | cpt | fit_meanshift_norm, fit_meanvar | None, SIC, BIC, MBIC, AIC, Hannan-Quinn, Asymptotic, Manual, CROPS | segment_pelt() | changepoint::cpt.mean() or changepoint::cpt.meanvar() |
| segneigh | changepoint | 2.3 | cpt | fit_meanvar | None, SIC, BIC, MBIC, AIC, Hannan-Quinn, Asymptotic, Manual, CROPS | NA | changepoint::cpt.meanvar() |
| single-best | changepoint | 2.3 | cpt | fit_meanvar | None, SIC, BIC, MBIC, AIC, Hannan-Quinn, Asymptotic, Manual, CROPS | NA | changepoint::cpt.meanvar() |
| wbs | wbs | 1.4.1 | wbs | NA | NA | NA | wbs::wbs(). |

## References

Please read [the full
paper](https://beanumber.github.io/changepoint-paper/) for more details.

To cite the package, use the following information:

``` r
citation("tidychangepoint")
```

    ## Warning in citation("tidychangepoint"): could not determine year for
    ## 'tidychangepoint' from package DESCRIPTION file

    ## To cite package 'tidychangepoint' in publications use:
    ## 
    ##   Baumer B, Suarez Sierra B, Coen A, Taimal C (????). _tidychangepoint:
    ##   A Tidy Framework for Changepoint Detection Analysis_. R package
    ##   version 0.0.1, <https://beanumber.github.io/tidychangepoint/>.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {tidychangepoint: A Tidy Framework for Changepoint Detection Analysis},
    ##     author = {Benjamin S. Baumer and Biviana Marcela {Suarez Sierra} and Arrigo Coen and Carlos A. Taimal},
    ##     note = {R package version 0.0.1},
    ##     url = {https://beanumber.github.io/tidychangepoint/},
    ##   }
