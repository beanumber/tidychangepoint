# Segment a time series using a variety of algorithms

A wrapper function that encapsulates various algorithms for detecting
changepoint sets in univariate time series.

## Usage

``` r
segment(x, method = "null", ...)

# S3 method for class 'tbl_ts'
segment(x, method = "null", ...)

# S3 method for class 'xts'
segment(x, method = "null", ...)

# S3 method for class 'numeric'
segment(x, method = "null", ...)

# S3 method for class 'ts'
segment(x, method = "null", ...)
```

## Arguments

- x:

  a numeric vector coercible into a
  [stats::ts](https://rdrr.io/r/stats/ts.html) object

- method:

  a character string indicating the algorithm to use. See Details.

- ...:

  arguments passed to methods

## Value

An object of class
[tidycpt](https://beanumber.github.io/tidychangepoint/reference/tidycpt-class.md).

## Details

Currently, `segment()` can use the following algorithms, depending on
the value of the `method` argument:

- `pelt`: Uses the PELT algorithm as implemented in
  [`segment_pelt()`](https://beanumber.github.io/tidychangepoint/reference/segment_pelt.md),
  which wraps either
  [`changepoint::cpt.mean()`](https://rdrr.io/pkg/changepoint/man/cpt.mean.html)
  or
  [`changepoint::cpt.meanvar()`](https://rdrr.io/pkg/changepoint/man/cpt.meanvar.html).
  The `segmenter` is of class `cpt`.

- `binseg`: Uses the Binary Segmentation algorithm as implemented by
  [`changepoint::cpt.meanvar()`](https://rdrr.io/pkg/changepoint/man/cpt.meanvar.html).
  The `segmenter` is of class `cpt`.

- `segneigh`: Uses the Segmented Neighborhood algorithm as implemented
  by
  [`changepoint::cpt.meanvar()`](https://rdrr.io/pkg/changepoint/man/cpt.meanvar.html).
  The `segmenter` is of class `cpt`.

- `single-best`: Uses the AMOC criteria as implemented by
  [`changepoint::cpt.meanvar()`](https://rdrr.io/pkg/changepoint/man/cpt.meanvar.html).
  The `segmenter` is of class `cpt`.

- `wbs`: Uses the Wild Binary Segmentation algorithm as implemented by
  [`wbs::wbs()`](https://rdrr.io/pkg/wbs/man/wbs.html). The `segmenter`
  is of class `wbs`.

- `strucchange`: Uses the segmented algorithm as implemented by
  [`strucchange::breakpoints()`](https://rdrr.io/pkg/strucchange/man/breakpoints.html).
  The `segmenter` is of class `breakpoints`.

- `segmented`: Uses the segmented algorithm as implemented by
  [`segmented::segmented()`](https://rdrr.io/pkg/segmented/man/segmented.html).
  The `segmenter` is of class `segmented`.

- `cptga`: Uses the Genetic algorithm implemented by
  [`segment_cptga()`](https://beanumber.github.io/tidychangepoint/reference/segment_cptga.md),
  which wraps
  [`changepointGA::cptga()`](https://rdrr.io/pkg/changepointGA/man/cptga.html).
  The `segmenter` is of class `tidycptga`.

- `ga`: Uses the Genetic algorithm implemented by
  [`segment_ga()`](https://beanumber.github.io/tidychangepoint/reference/segment_ga.md),
  which wraps
  [`GA::ga()`](https://github.com/luca-scr/GA/reference/ga.html). The
  `segmenter` is of class `tidyga`.

- `ga-shi`: Uses the genetic algorithm implemented by
  [`segment_ga_shi()`](https://beanumber.github.io/tidychangepoint/reference/segment_ga.md),
  which wraps
  [`segment_ga()`](https://beanumber.github.io/tidychangepoint/reference/segment_ga.md).
  The `segmenter` is of class `tidyga`.

- `ga-coen`: Uses Coen's heuristic as implemented by
  [`segment_ga_coen()`](https://beanumber.github.io/tidychangepoint/reference/segment_ga.md).
  The `segmenter` is of class `tidyga`. This implementation supersedes
  the following one.

- `coen`: Uses Coen's heuristic as implemented by
  [`segment_coen()`](https://beanumber.github.io/tidychangepoint/reference/segment_coen.md).
  The `segmenter` is of class
  [`seg_basket()`](https://beanumber.github.io/tidychangepoint/reference/seg_basket.md).
  Note that this function is deprecated.

- `random`: Uses a random basket of changepoints as implemented by
  [`segment_ga_random()`](https://beanumber.github.io/tidychangepoint/reference/segment_ga.md).
  The `segmenter` is of class `tidyga`.

- `manual`: Uses the vector of changepoints in the `tau` argument. The
  `segmenter` is of class
  [seg_cpt](https://beanumber.github.io/tidychangepoint/reference/seg_cpt.md)\`.

- `null`: The default. Uses no changepoints. The `segmenter` is of class
  [seg_cpt](https://beanumber.github.io/tidychangepoint/reference/seg_cpt.md).

## See also

[`changepoint::cpt.meanvar()`](https://rdrr.io/pkg/changepoint/man/cpt.meanvar.html),
[`wbs::wbs()`](https://rdrr.io/pkg/wbs/man/wbs.html),
[`GA::ga()`](https://github.com/luca-scr/GA/reference/ga.html),
[`segment_ga()`](https://beanumber.github.io/tidychangepoint/reference/segment_ga.md)

## Examples

``` r
# Segment a time series using PELT
segment(DataCPSim, method = "pelt")
#> ℹ A tidycpt object. Segmenter ↓
#> Class 'cpt' : Changepoint Object
#>        ~~   : S4 class containing 12 slots with names
#>               cpttype date version data.set method test.stat pen.type pen.value minseglen cpts ncpts.max param.est 
#> 
#> Created on  : Mon Jan 12 17:05:53 2026 
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
#> ℹ Model: A meanvar  model with 4 region(s).
#> → Each region has 2 parameter(s).
#> → The model has 0 global parameter(s).

# Segment a time series using PELT and the BIC penalty
segment(DataCPSim, method = "pelt", penalty = "BIC")
#> ℹ A tidycpt object. Segmenter ↓
#> Class 'cpt' : Changepoint Object
#>        ~~   : S4 class containing 12 slots with names
#>               cpttype date version data.set method test.stat pen.type pen.value minseglen cpts ncpts.max param.est 
#> 
#> Created on  : Mon Jan 12 17:05:54 2026 
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
#> ℹ Model: A meanvar  model with 4 region(s).
#> → Each region has 2 parameter(s).
#> → The model has 0 global parameter(s).

# Segment a time series using Binary Segmentation
segment(DataCPSim, method = "binseg", penalty = "BIC")
#> ℹ A tidycpt object. Segmenter ↓
#> Class 'cpt' : Changepoint Object
#>        ~~   : S4 class containing 14 slots with names
#>               cpts.full pen.value.full data.set cpttype method test.stat pen.type pen.value minseglen cpts ncpts.max param.est date version 
#> 
#> Created on  : Mon Jan 12 17:05:54 2026 
#> 
#> summary(.)  :
#> ----------
#> Created Using changepoint version 2.3 
#> Changepoint type      : Change in mean and variance 
#> Method of analysis    : BinSeg 
#> Test Statistic  : Normal 
#> Type of penalty       : BIC with value, 20.99827 
#> Minimum Segment Length : 2 
#> Maximum no. of cpts   : 5 
#> Changepoint Locations : 547 809 972 
#> Range of segmentations:
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]  809   NA   NA   NA   NA
#> [2,]  809  547   NA   NA   NA
#> [3,]  809  547  972   NA   NA
#> [4,]  809  547  972  822   NA
#> [5,]  809  547  972  822  813
#> 
#>  For penalty values: 1485.679 462.0479 160.3649 15.04514 15.04514 
#> ℹ Model: A meanvar  model with 4 region(s).
#> → Each region has 2 parameter(s).
#> → The model has 0 global parameter(s).

# Segment a time series using a random changepoint set
segment(DataCPSim, method = "random")
#> Seeding initial population with probability: 0.0063863343681642
#> ℹ A tidycpt object. Segmenter ↓
#> An object of class "ga"
#> 
#> Call:
#> GA::ga(type = "binary", fitness = obj_fun, nBits = n, population = ..1,     maxiter = 1)
#> 
#> Available slots:
#>  [1] "data"          "model_fn_args" "call"          "type"         
#>  [5] "lower"         "upper"         "nBits"         "names"        
#>  [9] "popSize"       "iter"          "run"           "maxiter"      
#> [13] "suggestions"   "population"    "elitism"       "pcrossover"   
#> [17] "pmutation"     "optim"         "fitness"       "summary"      
#> [21] "bestSol"       "fitnessValue"  "solution"     
#> ℹ Model: A meanshift_norm  model with 7 region(s).
#> → Each region has 1 parameter(s).
#> → The model has 1 global parameter(s).

# Segment a time series using a manually-specified changepoint set
segment(DataCPSim, method = "manual", tau = c(826))
#> ℹ A tidycpt object. Segmenter ↓
#> List of 9
#>  $ data        : Time-Series [1:1096] from 1 to 1096: 35.5 29 35.6 33 29.5 ...
#>  $ pkg         : chr "tidychangepoint"
#>  $ base_class  : chr(0) 
#>  $ algorithm   : chr "manual"
#>  $ changepoints: num 826
#>  $ fitness     : Named num 10571
#>   ..- attr(*, "names")= chr "BIC"
#>  $ seg_params  : list()
#>  $ model_name  : chr "meanshift_norm"
#>  $ penalty     : chr "BIC"
#>  - attr(*, "class")= chr "seg_cpt"
#> ℹ Model: A meanshift_norm  model with 2 region(s).
#> → Each region has 1 parameter(s).
#> → The model has 1 global parameter(s).

# Segment a time series using a null changepoint set
segment(DataCPSim)
#> ℹ A tidycpt object. Segmenter ↓
#> List of 9
#>  $ data        : Time-Series [1:1096] from 1 to 1096: 35.5 29 35.6 33 29.5 ...
#>  $ pkg         : chr "tidychangepoint"
#>  $ base_class  : chr(0) 
#>  $ algorithm   : chr "manual"
#>  $ changepoints: NULL
#>  $ fitness     : Named num 11503
#>   ..- attr(*, "names")= chr "BIC"
#>  $ seg_params  : list()
#>  $ model_name  : chr "meanshift_norm"
#>  $ penalty     : chr "BIC"
#>  - attr(*, "class")= chr "seg_cpt"
#> ℹ Model: A meanshift_norm  model with 1 region(s).
#> → Each region has 1 parameter(s).
#> → The model has 1 global parameter(s).
```
