# Container class for `tidycpt` objects

Container class for `tidycpt` objects

## Value

A tidycpt object.

## Details

Every `tidycpt` object contains:

- `segmenter`: The object returned by the underlying changepoint
  detection algorithm. These can be of arbitrary class. Use
  [`as.segmenter()`](https://beanumber.github.io/tidychangepoint/reference/as.segmenter.md)
  to retrieve them.

- `model`: A model object inheriting from
  [mod_cpt](https://beanumber.github.io/tidychangepoint/reference/mod_cpt.md),
  as created by
  [`as.model()`](https://beanumber.github.io/tidychangepoint/reference/as.model.md)
  when called on the `segmenter`.

- `elapsed_time`: The clock time that passed while the algorithm was
  running.

- `time_index`: If available, the labels for the time indices of the
  time series.

## Examples

``` r
# Segment a time series using PELT
x <- segment(CET, method = "pelt")
class(x)
#> [1] "tidycpt"
str(x)
#> List of 4
#>  $ segmenter   :Formal class 'cpt' [package "changepoint"] with 12 slots
#>   .. ..@ data.set : Time-Series [1:366] from 1 to 366: 8.87 9.1 9.78 9.52 8.63 9.34 8.29 9.86 8.52 9.51 ...
#>   .. ..@ cpttype  : chr "mean and variance"
#>   .. ..@ method   : chr "PELT"
#>   .. ..@ test.stat: chr "Normal"
#>   .. ..@ pen.type : chr "MBIC"
#>   .. ..@ pen.value: num 23.6
#>   .. ..@ minseglen: num 2
#>   .. ..@ cpts     : int [1:6] 55 57 267 344 347 366
#>   .. ..@ ncpts.max: num Inf
#>   .. ..@ param.est:List of 2
#>   .. .. ..$ mean    : num [1:6] 8.79 9.44 9.17 9.62 10.52 ...
#>   .. .. ..$ variance: num [1:6] 0.368 0 0.366 0.256 0 ...
#>   .. ..@ date     : chr "Thu Jan  8 19:51:31 2026"
#>   .. ..@ version  : chr "2.3"
#>  $ model       :List of 6
#>   ..$ data         : Time-Series [1:366] from 1 to 366: 8.87 9.1 9.78 9.52 8.63 9.34 8.29 9.86 8.52 9.51 ...
#>   ..$ tau          : int [1:5] 55 57 267 344 347
#>   ..$ region_params: tibble [6 × 3] (S3: tbl_df/tbl/data.frame)
#>   .. ..$ region           : chr [1:6] "[1,55)" "[55,57)" "[57,267)" "[267,344)" ...
#>   .. ..$ param_mu         : num [1:6] 8.8 9.04 9.17 9.6 10.54 ...
#>   .. ..$ param_sigma_hatsq: Named num [1:6] 0.37429 0.16 0.36591 0.2457 0.00109 ...
#>   .. .. ..- attr(*, "names")= chr [1:6] "[1,55)" "[55,57)" "[57,267)" "[267,344)" ...
#>   ..$ model_params : NULL
#>   ..$ fitted_values: num [1:366] 8.8 8.8 8.8 8.8 8.8 ...
#>   ..$ model_name   : chr "meanvar"
#>   ..- attr(*, "class")= chr "mod_cpt"
#>  $ elapsed_time: 'difftime' num 0.00839376449584961
#>   ..- attr(*, "units")= chr "secs"
#>  $ time_index  : Date[1:366], format: "1659-01-01" "1660-01-01" ...
#>  - attr(*, "class")= chr "tidycpt"
```
