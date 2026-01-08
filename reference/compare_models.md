# Compare various models or algorithms for a given changepoint set

Compare various models or algorithms for a given changepoint set

## Usage

``` r
compare_models(x, ...)

compare_algorithms(x, ...)
```

## Arguments

- x:

  A
  [tidycpt](https://beanumber.github.io/tidychangepoint/reference/tidycpt-class.md)
  object

- ...:

  currently ignored

## Value

A
[tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)

## Details

A
[tidycpt](https://beanumber.github.io/tidychangepoint/reference/tidycpt-class.md)
object has a set of changepoints returned by the algorithm that
segmented the time series. That changepoint set was obtained using a
specific model. Treating this changepoint set as fixed, the
`compare_models()` function fits several common changepoint models to
the time series and changepoint set, and returns the results of
[`glance()`](https://generics.r-lib.org/reference/glance.html).
Comparing the fits of various models could lead to improved
understanding.

Alternatively, `compare_algorithms()` runs several fast changepoint
detection algorithms on the original time series, and consolidates the
results.

## Examples

``` r
# Segment a times series using PELT
x <- segment(CET, method = "pelt")

# Compare models
compare_models(x)
#> # A tibble: 8 × 12
#>   pkg     version algorithm params num_cpts  rmse logLik   AIC   BIC  MBIC   MDL
#>   <chr>   <pckg_> <chr>     <list>    <int> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 tidych… 1.0.2   nhpp      <dbl>         5 0.577  -290.  616.  686.  651.  680.
#> 2 tidych… 1.0.2   splinesh… <dbl>         5 0.553  -303.  654.  747.  677.  724.
#> 3 tidych… 1.0.2   trendshi… <dbl>         5 0.564  -309.  655.  729.  689.  724.
#> 4 tidych… 1.0.2   meanshif… <dbl>         5 0.568  -311.  649.  699.  694.  711.
#> 5 tidych… 1.0.2   trendshi… <dbl>         5 0.572  -315.  666.  736.  701.  730.
#> 6 tidych… 1.0.2   meanvar   <NULL>        5 0.577  -318.  671.  737.  708.  731.
#> 7 tidych… 1.0.2   meanshif… <dbl>         5 0.577  -318.  661.  707.  708.  719.
#> 8 tidych… 1.0.2   meanvar   <NULL>        5 0.577  -318.  671.  737.  708.  731.
#> # ℹ 1 more variable: BMDL <dbl>

# Compare algorithms
compare_algorithms(x)
#> Seeding initial population with probability: 0.0161274134792387
#> # A tibble: 6 × 8
#>   pkg      version algorithm seg_params model_name criteria fitness elapsed_time
#>   <chr>    <pckg_> <chr>     <list>     <chr>      <chr>      <dbl> <drtn>      
#> 1 tidycha… 1.0.2   manual    <list [0]> meanshift… BIC         789. 0.003 secs  
#> 2 changep… 2.3     BinSeg    <list [1]> meanvar    MBIC        653. 0.005 secs  
#> 3 changep… 2.3     PELT      <list [1]> meanvar    MBIC       -Inf  0.008 secs  
#> 4 changep… 2.3     PELT      <list [1]> meanvar    MBIC       -Inf  0.008 secs  
#> 5 wbs      1.4.1   Wild Bin… <list [1]> meanshift… MBIC       -130. 0.023 secs  
#> 6 GA       3.2.4   Genetic   <list [1]> meanshift… BIC         698. 0.099 secs  
```
