
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
x <- segment(CET, method = "pelt", model = fit_meanshift_norm, minseglen = 3)
class(x)
```

    ## [1] "tidycpt"

Various methods are available for `tidycpt` objects. For example,
`as.ts()` returns the original data as `ts` object, and `changepoints()`
returns the set of changepoint indices.

``` r
changepoints(x)
```

    ## [1] 330

If the original time series has time labels, we can also retrieve that
information.

``` r
changepoints(x, use_labels = TRUE)
```

    ## [1] "1988-01-01"

The `fitness()` function returns the both the value and the name of the
objective function that the algorithm used to find the optimal
changepoint set.

``` r
fitness(x)
```

    ##    MBIC 
    ## 688.331

The `tidy()` method shows the fitted parameters values for each region.

``` r
tidy(x)
```

    ## Registered S3 method overwritten by 'tsibble':
    ##   method               from 
    ##   as_tibble.grouped_df dplyr

    ## # A tibble: 2 × 9
    ##   region    num_obs   min   max  mean    sd begin   end param_mu
    ##   <chr>       <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 [1,330)       329  6.86  10.6  9.17 0.615     1   330     9.17
    ## 2 [330,367)      37  8.95  11.2 10.3  0.528   330   367    10.3

## Algorithmic coverage

``` r
ls_methods()
```

    ## # A tibble: 15 × 5
    ##    method      pkg             segmenter_class helper              wraps        
    ##    <chr>       <chr>           <chr>           <chr>               <chr>        
    ##  1 pelt        changepoint     cpt             segment_pelt()      changepoint:…
    ##  2 binseg      changepoint     cpt             <NA>                changepoint:…
    ##  3 segneigh    changepoint     cpt             <NA>                changepoint:…
    ##  4 single-best changepoint     cpt             <NA>                changepoint:…
    ##  5 wbs         wbs             wbs             <NA>                wbs::wbs()   
    ##  6 ga          GA              tidyga          segment_ga()        GA::ga()     
    ##  7 ga-shi      GA              tidyga          segment_ga_shi()    segment_ga() 
    ##  8 ga-coen     GA              tidyga          segment_ga_coen()   segment_ga() 
    ##  9 coen        tidychangepoint seg_basket      segment_coen()      <NA>         
    ## 10 random      GA              tidyga          segment_ga_random() segment_ga() 
    ## 11 manual      tidychangepoint seg_cpt         segment_manual()    <NA>         
    ## 12 null        tidychangepoint seg_cpt         segment_manual()    <NA>         
    ## 13 strucchange strucchange     breakpointsfull <NA>                strucchange:…
    ## 14 segmented   segmented       segmented       <NA>                segmented::s…
    ## 15 cptga       changepointGA   tidycptga       segment_cptga()     changepointG…

``` r
ls_coverage() |>
  dplyr::group_by(method) |>
  dplyr::summarize(
    models = paste(unique(model), collapse = ", "),
    penalties = paste(unique(penalty), collapse = ", ")
  ) |>
  dplyr::arrange(method) |>
  knitr::kable()
```

| method | models | penalties |
|:---|:---|:---|
| binseg | fit_meanvar | None, SIC, BIC, MBIC, AIC, HQC, Asymptotic, Manual, CROPS |
| coen | fit_nhpp | BMDL |
| cptga | NA | NA |
| ga | fit_arima, fit_lmshift, fit_lmshift_ar1, fit_meanshift_lnorm, fit_meanshift_norm, fit_meanshift_norm_ar1, fit_meanvar, fit_nhpp, fit_trendshift, fit_trendshift_ar1 | SIC, AIC, BIC, HQC, MBIC, MDL |
| ga-coen | fit_nhpp | BMDL |
| ga-shi | fit_meanshift_norm_ar1 | BIC |
| manual | fit_meanshift_norm | BIC |
| null | fit_meanshift_norm | BIC |
| pelt | fit_meanshift_norm, fit_meanvar | None, SIC, BIC, MBIC, AIC, HQC, Asymptotic, Manual, CROPS |
| random | fit_arima, fit_lmshift, fit_lmshift_ar1, fit_meanshift_lnorm, fit_meanshift_norm, fit_meanshift_norm_ar1, fit_meanvar, fit_nhpp, fit_trendshift, fit_trendshift_ar1 | SIC, AIC, BIC, HQC, MBIC, MDL |
| segmented | NA | NA |
| segneigh | fit_meanvar | None, SIC, BIC, MBIC, AIC, HQC, Asymptotic, Manual, CROPS |
| single-best | fit_meanvar | None, SIC, BIC, MBIC, AIC, HQC, Asymptotic, Manual, CROPS |
| strucchange | NA | NA |
| wbs | NA | NA |

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
    ##   Baumer B, Suárez Sierra B, Coen A, Taimal C (????). _tidychangepoint:
    ##   A Tidy Framework for Changepoint Detection Analysis_. R package
    ##   version 1.0.2.9000, <https://beanumber.github.io/tidychangepoint/>.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {tidychangepoint: A Tidy Framework for Changepoint Detection Analysis},
    ##     author = {Benjamin S. Baumer and Biviana Marcela {Suárez Sierra} and Arrigo Coen and Carlos A. Taimal},
    ##     note = {R package version 1.0.2.9000},
    ##     url = {https://beanumber.github.io/tidychangepoint/},
    ##   }
