# Retrieve the arguments that a model-fitting function used

Retrieve the arguments that a model-fitting function used

## Usage

``` r
model_args(object, ...)

# Default S3 method
model_args(object, ...)

# S3 method for class 'seg_cpt'
model_args(object, ...)

# S3 method for class 'ga'
model_args(object, ...)

# S3 method for class 'cpt'
model_args(object, ...)

# S3 method for class 'cptga'
model_args(object, ...)

# S3 method for class 'segmented'
model_args(object, ...)

# S3 method for class 'wbs'
model_args(object, ...)
```

## Arguments

- object:

  A `segmenter` object.

- ...:

  currently ignored

## Value

A named `list` of arguments, or `NULL`

## Details

Every model is fit by a model-fitting function, and these functions
sometimes take arguments. `model_args()` recovers the arguments that
were passed to the model fitting function when it was called. These are
especially important when using a genetic algorithm.

## See also

Other model-fitting:
[`fit_arima()`](https://beanumber.github.io/tidychangepoint/reference/fit_arima.md),
[`fit_lmshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_lmshift.md),
[`fit_meanshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanshift.md),
[`fit_meanvar()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanvar.md),
[`fit_nhpp()`](https://beanumber.github.io/tidychangepoint/reference/fit_nhpp.md),
[`model_name()`](https://beanumber.github.io/tidychangepoint/reference/model_name.md),
[`new_fun_cpt()`](https://beanumber.github.io/tidychangepoint/reference/fun_cpt.md),
[`whomademe()`](https://beanumber.github.io/tidychangepoint/reference/whomademe.md)

Other segmenter-functions:
[`as.segmenter()`](https://beanumber.github.io/tidychangepoint/reference/as.segmenter.md),
[`fitness()`](https://beanumber.github.io/tidychangepoint/reference/fitness.md),
[`seg_params()`](https://beanumber.github.io/tidychangepoint/reference/seg_params.md)

## Examples

``` r
# Segment a time series using Coen's algorithm
x <- segment(CET, method = "ga-coen", maxiter = 3)
#> Seeding initial population with probability: 0.0327868852459016

# Recover the arguments passed to the model-fitting function
x |>
  as.segmenter() |>
  model_args()
#> $model_fn
#> [1] "nhpp"
#> 
#> $penalty_fn
#> [1] "BMDL"
#> 
  
```
