
# tidychangepoint

<!-- badges: start -->
[![R-CMD-check](https://github.com/beanumber/tidychangepoint/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/beanumber/tidychangepoint/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Usage

To install `tidychangepoint`:

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
    ##   Facilitate Changepoint Detection Analysis in a Tidy Framework_. R
    ##   package version 0.0.1,
    ##   <https://beanumber.github.io/tidychangepoint/>.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {tidychangepoint: Facilitate Changepoint Detection Analysis in a Tidy Framework},
    ##     author = {Benjamin S. Baumer and Biviana Marcela {Suarez Sierra} and Arrigo Coen and Carlos A. Taimal},
    ##     note = {R package version 0.0.1},
    ##     url = {https://beanumber.github.io/tidychangepoint/},
    ##   }
