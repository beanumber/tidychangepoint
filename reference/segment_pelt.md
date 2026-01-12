# Segment a time series using the PELT algorithm

Segmenting functions for the PELT algorithm

## Usage

``` r
segment_pelt(x, model_fn = fit_meanvar, ...)
```

## Arguments

- x:

  A time series

- model_fn:

  A `character` or `name` coercible into a
  [fun_cpt](https://beanumber.github.io/tidychangepoint/reference/fun_cpt.md)
  function. See, for example,
  [`fit_meanshift_norm()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanshift.md).
  The default is
  [`fit_meanvar()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanvar.md).

- ...:

  arguments passed to
  [`changepoint::cpt.meanvar()`](https://rdrr.io/pkg/changepoint/man/cpt.meanvar.html)
  or
  [`changepoint::cpt.mean()`](https://rdrr.io/pkg/changepoint/man/cpt.mean.html)

## Value

A `cpt` object returned by
[`changepoint::cpt.meanvar()`](https://rdrr.io/pkg/changepoint/man/cpt.meanvar.html)
or
[`changepoint::cpt.mean()`](https://rdrr.io/pkg/changepoint/man/cpt.mean.html)

## Details

This function wraps either
[`changepoint::cpt.meanvar()`](https://rdrr.io/pkg/changepoint/man/cpt.meanvar.html)
or
[`changepoint::cpt.mean()`](https://rdrr.io/pkg/changepoint/man/cpt.mean.html).

## Examples

``` r
# Segment a time series using PELT
res <- segment_pelt(DataCPSim)
res
#> Class 'cpt' : Changepoint Object
#>        ~~   : S4 class containing 12 slots with names
#>               cpttype date version data.set method test.stat pen.type pen.value minseglen cpts ncpts.max param.est 
#> 
#> Created on  : Mon Jan 12 17:06:17 2026 
#> 
#> summary(.)  :
#> ----------
#> Created Using changepoint version 2.3 
#> Changepoint type      : Change in mean and variance 
#> Method of analysis    : PELT 
#> Test Statistic  : Normal 
#> Type of penalty       : MBIC with value, 27.99769 
#> Minimum Segment Length : 2 
#> Maximum no. of cpts   : Inf 
#> Changepoint Locations : 547 822 972 
str(res)
#> Formal class 'cpt' [package "changepoint"] with 12 slots
#>   ..@ data.set : Time-Series [1:1096] from 1 to 1096: 35.5 29 35.6 33 29.5 ...
#>   ..@ cpttype  : chr "mean and variance"
#>   ..@ method   : chr "PELT"
#>   ..@ test.stat: chr "Normal"
#>   ..@ pen.type : chr "MBIC"
#>   ..@ pen.value: num 28
#>   ..@ minseglen: num 2
#>   ..@ cpts     : int [1:4] 547 822 972 1096
#>   ..@ ncpts.max: num Inf
#>   ..@ param.est:List of 2
#>   .. ..$ mean    : num [1:4] 35.3 58.2 96.8 156.5
#>   .. ..$ variance: num [1:4] 127 371 921 2406
#>   ..@ date     : chr "Mon Jan 12 17:06:17 2026"
#>   ..@ version  : chr "2.3"

# Segment as time series while specifying a penalty function
segment_pelt(DataCPSim, penalty = "BIC")
#> Class 'cpt' : Changepoint Object
#>        ~~   : S4 class containing 12 slots with names
#>               cpttype date version data.set method test.stat pen.type pen.value minseglen cpts ncpts.max param.est 
#> 
#> Created on  : Mon Jan 12 17:06:17 2026 
#> 
#> summary(.)  :
#> ----------
#> Created Using changepoint version 2.3 
#> Changepoint type      : Change in mean and variance 
#> Method of analysis    : PELT 
#> Test Statistic  : Normal 
#> Type of penalty       : BIC with value, 20.99827 
#> Minimum Segment Length : 2 
#> Maximum no. of cpts   : Inf 
#> Changepoint Locations : 547 822 972 

# Segment a time series while specifying a meanshift normal model
segment_pelt(DataCPSim, model_fn = fit_meanshift_norm, penalty = "BIC")
#> Class 'cpt' : Changepoint Object
#>        ~~   : S4 class containing 12 slots with names
#>               cpttype date version data.set method test.stat pen.type pen.value minseglen cpts ncpts.max param.est 
#> 
#> Created on  : Mon Jan 12 17:06:17 2026 
#> 
#> summary(.)  :
#> ----------
#> Created Using changepoint version 2.3 
#> Changepoint type      : Change in mean 
#> Method of analysis    : PELT 
#> Test Statistic  : Normal 
#> Type of penalty       : BIC with value, 13.99884 
#> Minimum Segment Length : 1 
#> Maximum no. of cpts   : Inf 
#> Number of changepoints: 873 
```
