# Convert, retrieve, or verify a segmenter object

Convert, retrieve, or verify a segmenter object

## Usage

``` r
as.segmenter(object, ...)

as.seg_cpt(object, ...)

# S3 method for class 'seg_basket'
as.seg_cpt(object, ...)

# S3 method for class 'seg_cpt'
as.seg_cpt(object, ...)

# S3 method for class 'tidycpt'
as.segmenter(object, ...)

# S3 method for class 'ga'
as.seg_cpt(object, ...)

# S3 method for class 'cpt'
as.seg_cpt(object, ...)

# S3 method for class 'cptga'
as.seg_cpt(object, ...)

# S3 method for class 'segmented'
as.seg_cpt(object, ...)

# S3 method for class 'breakpointsfull'
as.seg_cpt(object, ...)

# S3 method for class 'wbs'
as.seg_cpt(object, ...)

is_segmenter(object, ...)
```

## Arguments

- object:

  A
  [tidycpt](https://beanumber.github.io/tidychangepoint/reference/tidycpt-class.md)
  or `segmenter` object

- ...:

  Arguments passed to methods

## Value

- `as.segmenter()` returns the `segmenter` object of a `tidycpt` object.
  Note that this could be of any class, depending on the class returned
  by the segmenting function.

&nbsp;

- `as.seg_cpt()` returns a
  [seg_cpt](https://beanumber.github.io/tidychangepoint/reference/seg_cpt.md)
  object

&nbsp;

- `is_segmenter()` a `logical` vector of length 1

## Details

[tidycpt](https://beanumber.github.io/tidychangepoint/reference/tidycpt-class.md)
objects have a `segmenter` component (that is typically created by a
class to
[`segment()`](https://beanumber.github.io/tidychangepoint/reference/segment.md)).
The functions documented here are convenience utility functions for
working with the `segmenter` components. `as.segmenter()` is especially
useful in pipelines to avoid having to use the `$` or `[` notation for
subsetting.

`as.segmenter()` simply returns the segmenter of a `tidycpt` object.

`as.seg_cpt()` takes a wild-caught `segmenter` object of arbitrary class
and converts it into a
[seg_cpt](https://beanumber.github.io/tidychangepoint/reference/seg_cpt.md)
object.

`is_segmenter()` checks to see if a segmenter object implements all of
the S3 methods necessary to be considered a segmenter.

## See also

Other tidycpt-generics:
[`as.model()`](https://beanumber.github.io/tidychangepoint/reference/as.model.md),
[`changepoints()`](https://beanumber.github.io/tidychangepoint/reference/changepoints.md),
[`diagnose()`](https://beanumber.github.io/tidychangepoint/reference/diagnose.md),
[`fitness()`](https://beanumber.github.io/tidychangepoint/reference/fitness.md),
[`model_name()`](https://beanumber.github.io/tidychangepoint/reference/model_name.md)

Other segmenter-functions:
[`fitness()`](https://beanumber.github.io/tidychangepoint/reference/fitness.md),
[`model_args()`](https://beanumber.github.io/tidychangepoint/reference/model_args.md),
[`seg_params()`](https://beanumber.github.io/tidychangepoint/reference/seg_params.md)

## Examples

``` r
# Segment a time series using PELT
x <- segment(CET, method = "pelt")

# Return the segmenter component
x |>
  as.segmenter()
#> Class 'cpt' : Changepoint Object
#>        ~~   : S4 class containing 12 slots with names
#>               cpttype date version data.set method test.stat pen.type pen.value minseglen cpts ncpts.max param.est 
#> 
#> Created on  : Fri Jan  9 14:04:48 2026 
#> 
#> summary(.)  :
#> ----------
#> Created Using changepoint version 2.3 
#> Changepoint type      : Change in mean and variance 
#> Method of analysis    : PELT 
#> Test Statistic  : Normal 
#> Type of penalty       : MBIC with value, 23.61053 
#> Minimum Segment Length : 2 
#> Maximum no. of cpts   : Inf 
#> Changepoint Locations : 55 57 267 344 347 
  
# Note the class of this object could be anything
x |>
  as.segmenter() |>
  class()
#> [1] "cpt"
#> attr(,"package")
#> [1] "changepoint"
  
# Convert the segmenter into the standardized seg_cpt class
x |>
  as.segmenter() |>
  as.seg_cpt()
#> List of 9
#>  $ data        : Time-Series [1:366] from 1 to 366: 8.87 9.1 9.78 9.52 8.63 9.34 8.29 9.86 8.52 9.51 ...
#>  $ pkg         : chr "changepoint"
#>  $ base_class  : chr "cpt"
#>   ..- attr(*, "package")= chr "changepoint"
#>  $ algorithm   : chr "PELT"
#>  $ changepoints: int [1:5] 55 57 267 344 347
#>  $ fitness     : Named num -Inf
#>   ..- attr(*, "names")= chr "MBIC"
#>  $ seg_params  :List of 1
#>   ..$ :List of 3
#>   .. ..$ test_stat     : chr "Normal"
#>   .. ..$ num_cpts_max  : num Inf
#>   .. ..$ min_seg_length: num 2
#>  $ model_name  : chr "meanvar"
#>  $ penalty     : chr "MBIC"
#>  - attr(*, "class")= chr "seg_cpt"

# Is the segmenter valid?
x |>
  as.segmenter() |>
  is_segmenter()
#> [1] TRUE
```
