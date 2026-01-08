# Manually segment a time series

Segment a time series by manually inputting the changepoint set

## Usage

``` r
segment_manual(x, tau, ...)
```

## Arguments

- x:

  A time series

- tau:

  a set of indices representing a changepoint set

- ...:

  arguments passed to
  [seg_cpt](https://beanumber.github.io/tidychangepoint/reference/seg_cpt.md)

## Value

A
[seg_cpt](https://beanumber.github.io/tidychangepoint/reference/seg_cpt.md)
object

## Details

Sometimes you want to see how a manually input set of changepoints
performs. This function takes a time series and a changepoint detection
set as inputs and returns a
[seg_cpt](https://beanumber.github.io/tidychangepoint/reference/seg_cpt.md)
object representing the segmenter. Note that by default
[`fit_meanshift_norm()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanshift.md)
is used to fit the model and [`BIC()`](https://rdrr.io/r/stats/AIC.html)
is used as the penalized objective function.

## Examples

``` r
# Segment a time series manually
segment_manual(CET, tau = c(84, 330))
#> List of 9
#>  $ data        : Time-Series [1:366] from 1 to 366: 8.87 9.1 9.78 9.52 8.63 9.34 8.29 9.86 8.52 9.51 ...
#>  $ pkg         : chr "tidychangepoint"
#>  $ base_class  : chr(0) 
#>  $ algorithm   : chr "manual"
#>  $ changepoints: num [1:2] 84 330
#>  $ fitness     : Named num 697
#>   ..- attr(*, "names")= chr "BIC"
#>  $ seg_params  : list()
#>  $ model_name  : chr "meanshift_norm"
#>  $ penalty     : chr "BIC"
#>  - attr(*, "class")= chr "seg_cpt"
segment_manual(CET, tau = NULL)
#> List of 9
#>  $ data        : Time-Series [1:366] from 1 to 366: 8.87 9.1 9.78 9.52 8.63 9.34 8.29 9.86 8.52 9.51 ...
#>  $ pkg         : chr "tidychangepoint"
#>  $ base_class  : chr(0) 
#>  $ algorithm   : chr "manual"
#>  $ changepoints: NULL
#>  $ fitness     : Named num 789
#>   ..- attr(*, "names")= chr "BIC"
#>  $ seg_params  : list()
#>  $ model_name  : chr "meanshift_norm"
#>  $ penalty     : chr "BIC"
#>  - attr(*, "class")= chr "seg_cpt"
```
