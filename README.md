
# tidychangepoint

<!-- badges: start -->
[![R-CMD-check](https://github.com/c-taimal/tidychangepoint/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/c-taimal/tidychangepoint/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Usage

``` r
remotes::install_github("beanumber/tidychangepoint")
```

``` r
library(tidychangepoint)
```

## Tidy methods for changepoint analysis

Consider the following time series:

``` r
plot(as.ts(DataCPSim))
```

![](README_files/figure-gfm/rlnorm-plot-1.png)<!-- -->

`tidychangepoint` allows you to use any number of algorithms for
detecting changepoints in univariate time series with a common,
`tidyverse`-compliant interface. The `segment()` function takes a
numeric vector that is coercible into a `ts` object, and a string
indicating the algorithm you wish you use. `segment()` always returns a
`tidycpt` object.

``` r
cpts <- segment(DataCPSim, method = "cpt-pelt")
```

    ## method: cpt-pelt

``` r
class(cpts)
```

    ## [1] "tidycpt"

Various methods are available for `tidycpt` objects. For example,
`as.ts()` returns the original data as `ts` object, and `changepoints()`
returns the set of changepoints.

``` r
changepoints(cpts)
```

    ## [1] 547 822 972

### `broom` Interface

`tidychangepoint` follows the design interface of the `broom` package.
Therefore, `augment()`, `tidy()` and `glance()` methods exists for
`tidycpt` objects.

- `augment()` returns a `tsibble` that is grouped according to the
  regions defined by the changepoints.

``` r
augment(cpts)
```

    ## # A tsibble: 1,096 x 3 [1]
    ## # Groups:    region [4]
    ##    index     y region 
    ##    <int> <dbl> <fct>  
    ##  1     1  35.5 [0,547)
    ##  2     2  29.0 [0,547)
    ##  3     3  35.6 [0,547)
    ##  4     4  33.0 [0,547)
    ##  5     5  29.5 [0,547)
    ##  6     6  25.4 [0,547)
    ##  7     7  28.8 [0,547)
    ##  8     8  50.3 [0,547)
    ##  9     9  24.9 [0,547)
    ## 10    10  58.9 [0,547)
    ## # ℹ 1,086 more rows

- `tidy()` returns a `tbl` that provides summary statistics for each
  region

``` r
tidy(cpts)
```

    ## # A tibble: 4 × 13
    ##   region   num_obs   min   max  mean    sd exceedances begin   end log_posterior
    ##   <chr>      <int> <dbl> <dbl> <dbl> <dbl> <named lis> <dbl> <int>         <dbl>
    ## 1 [0,547)      546  13.7  92.8  35.3  11.3 <int [11]>      0   547         -60.1
    ## 2 [547,82…     275  20.5 163.   58.1  19.3 <int [95]>    547   822        -198. 
    ## 3 [822,97…     150  39.2 215.   96.7  30.5 <int [129]>   822   972        -150. 
    ## 4 [972,1.…     125  67.2 299.  156.   49.6 <int [125]>   972  1096        -126. 
    ## # ℹ 3 more variables: logLik <dbl>, alpha <dbl>, beta <dbl>

- `glance()` returns a `tbl` that provides summary statistics for the
  model fit.

``` r
glance(cpts)
```

    ## # A tibble: 1 × 13
    ##   pkg     version algorithm test_stat  MBIC num_cpts num_cpts_max min_seg_length
    ##   <chr>   <chr>   <chr>     <chr>     <dbl>    <int>        <dbl>          <dbl>
    ## 1 change… 2.2.4   PELT      Normal     28.0        4          Inf              2
    ## # ℹ 5 more variables: nhpp_logLik <logLik>, nhpp_AIC <dbl>, nhpp_BIC <dbl>,
    ## #   nhpp_MBIC <dbl>, nhpp_BMDL <dbl>

### Other methods

The `plot()` method leverages `ggplot2` to provide an informative plot.

``` r
plot(cpts)
```

![](README_files/figure-gfm/pelt-plot-1.png)<!-- -->

The `diagnose()` function shows the empirical cumulative distribution of
the exceedances of the time series, as well as how that distribution is
modeled as a non-homogeneous Poisson process.

``` r
diagnose(cpts)
```

    ## Warning: Removed 2 rows containing missing values (`geom_line()`).
    ## Removed 2 rows containing missing values (`geom_line()`).
    ## Removed 2 rows containing missing values (`geom_line()`).

![](README_files/figure-gfm/diagnose-1.png)<!-- -->

## Algorithms

### From `changepoint`

The `segment()` function passes argument to the `cpt.meanvar()` function
from the `changepoint` package, and stores the resulting `cpt` object as
its `segmenter` object.

``` r
x <- changepoint::cpt.meanvar(DataCPSim, method = "PELT")
identical(x, cpts$segmenter)
```

    ## [1] TRUE

### Naive methods

`segment()` includes three options for computing changepoints using
naive methods, all of which return `lm` objects:

- `method = "null"`: returns null model with no changepoints

``` r
DataCPSim |>
  segment(method = "null") |>
  diagnose()
```

    ## method: null

    ## Warning: Removed 2 rows containing missing values (`geom_line()`).
    ## Removed 2 rows containing missing values (`geom_line()`).
    ## Removed 2 rows containing missing values (`geom_line()`).

![](README_files/figure-gfm/null-plot-1.png)<!-- -->

- `method = "manual"`: returns a model with changepoints specified
  manually using the `cpts` argument

``` r
DataCPSim |>
  segment(method = "manual", cpts = c(365, 826)) |>
  diagnose()
```

    ## method: manual

    ## Warning: Removed 2 rows containing missing values (`geom_line()`).
    ## Removed 2 rows containing missing values (`geom_line()`).
    ## Removed 2 rows containing missing values (`geom_line()`).

![](README_files/figure-gfm/manual-plot-1.png)<!-- -->

- `method = "single-best"`: returns the model with at most one
  changepoint that minimizes the log-likelihood

``` r
DataCPSim |>
  segment(method = "single-best") |>
  diagnose()
```

    ## method: single-best

    ## Warning: Removed 2 rows containing missing values (`geom_line()`).
    ## Removed 2 rows containing missing values (`geom_line()`).
    ## Removed 2 rows containing missing values (`geom_line()`).

![](README_files/figure-gfm/amoc-plot-1.png)<!-- -->

- `method = "random"`: returns the best model from a random collection

``` r
DataCPSim |>
  segment(method = "random", num_generations = 20) |>
  diagnose()
```

    ## method: random

    ## Warning: Removed 2 rows containing missing values (`geom_line()`).
    ## Removed 2 rows containing missing values (`geom_line()`).
    ## Removed 2 rows containing missing values (`geom_line()`).

![](README_files/figure-gfm/random-plot-1.png)<!-- -->

### Genetic BMDL

- `method = "cpt-gbmdl"`: implements the Genetic BMDL heuristic

``` r
# DataCPSim |>
#   segment(method = "cpt-gbmdl")
changepoints(lista_AG)
```

    ## [1] 548 830

``` r
diagnose(lista_AG)
```

    ## Warning: Removed 2 rows containing missing values (`geom_line()`).
    ## Removed 2 rows containing missing values (`geom_line()`).
    ## Removed 2 rows containing missing values (`geom_line()`).

![](README_files/figure-gfm/gbmdl-tidyplot-1.png)<!-- -->

Diagnostic plots are also available.

``` r
plot(lista_AG$segmenter)
```

    ## method: null

    ## method: single-best

    ## method: cpt-pelt

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](README_files/figure-gfm/plot-gbmdl-1.png)<!-- -->

``` r
diagnose(lista_AG$segmenter)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (`geom_bar()`).

![](README_files/figure-gfm/diagnostic-1.png)<!-- -->

## Citation

``` r
citation("tidychangepoint")
```

    ## To cite package 'tidychangepoint' in publications use:
    ## 
    ##   Taimal CA, Suárez-Sierra BM, Rivera JC (2023). "An Exploration of
    ##   Genetic Algorithms Operators for the Detection of Multiple
    ##   Change-Points of Exceedances Using Non-homogeneous Poisson Processes
    ##   and Bayesian Methods." In _Colombian Conference on Computing_,
    ##   230-258. Springer. doi:10.1007/978-3-031-47372-2_20
    ##   <https://doi.org/10.1007/978-3-031-47372-2_20>.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @InProceedings{,
    ##     title = {An Exploration of Genetic Algorithms Operators for the Detection of Multiple Change-Points of Exceedances Using Non-homogeneous Poisson Processes and Bayesian Methods},
    ##     author = {Carlos A Taimal and Biviana Marcela Suárez-Sierra and Juan Carlos Rivera},
    ##     booktitle = {Colombian Conference on Computing},
    ##     year = {2023},
    ##     pages = {230--258},
    ##     organization = {Springer},
    ##     doi = {10.1007/978-3-031-47372-2_20},
    ##   }
