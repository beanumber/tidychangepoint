# Evaluate candidate changepoints sets

Evaluate candidate changepoints sets

## Usage

``` r
evaluate_cpts(x, ...)

# S3 method for class 'seg_basket'
evaluate_cpts(x, ...)

# S3 method for class 'list'
evaluate_cpts(x, .data, model_fn, ...)

# S3 method for class 'tbl_df'
evaluate_cpts(x, .data, model_fn, ...)
```

## Arguments

- x:

  An object to evaluate

- ...:

  arguments passed to methods

- .data:

  A time series

- model_fn:

  Name of the function to fit the model. See, for examples,
  [`fit_meanshift_norm()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanshift.md).

## Value

A
[tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
