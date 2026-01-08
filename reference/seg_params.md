# Retrieve parameters from a segmenter

Retrieve parameters from a segmenter

## Usage

``` r
seg_params(object, ...)

# S3 method for class 'seg_cpt'
seg_params(object, ...)

# S3 method for class 'ga'
seg_params(object, ...)

# S3 method for class 'cpt'
seg_params(object, ...)

# S3 method for class 'cptga'
seg_params(object, ...)

# S3 method for class 'segmented'
seg_params(object, ...)

# S3 method for class 'wbs'
seg_params(object, ...)
```

## Arguments

- object:

  A `segmenter` object.

- ...:

  currently ignored

## Value

A named `list` of parameters with their values.

## Details

Most segmenting algorithms have parameters. This function retrieves an
informative set of those parameter values.

## See also

Other segmenter-functions:
[`as.segmenter()`](https://beanumber.github.io/tidychangepoint/reference/as.segmenter.md),
[`fitness()`](https://beanumber.github.io/tidychangepoint/reference/fitness.md),
[`model_args()`](https://beanumber.github.io/tidychangepoint/reference/model_args.md)

## Examples

``` r
# Segment a time series using PELT
x <- segment(CET, method = "pelt")
x |>
  as.segmenter() |>
  seg_params()
#> $test_stat
#> [1] "Normal"
#> 
#> $num_cpts_max
#> [1] Inf
#> 
#> $min_seg_length
#> [1] 2
#> 
```
