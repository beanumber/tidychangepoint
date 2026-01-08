# Retrieve the name of the model that a segmenter or model used

Retrieve the name of the model that a segmenter or model used

## Usage

``` r
model_name(object, ...)

# Default S3 method
model_name(object, ...)

# S3 method for class 'character'
model_name(object, ...)

# S3 method for class 'mod_cpt'
model_name(object, ...)

# S3 method for class 'seg_basket'
model_name(object, ...)

# S3 method for class 'seg_cpt'
model_name(object, ...)

# S3 method for class 'tidycpt'
model_name(object, ...)

# S3 method for class 'ga'
model_name(object, ...)

# S3 method for class 'cpt'
model_name(object, ...)

# S3 method for class 'cptga'
model_name(object, ...)

# S3 method for class 'segmented'
model_name(object, ...)

# S3 method for class 'breakpointsfull'
model_name(object, ...)

# S3 method for class 'wbs'
model_name(object, ...)
```

## Arguments

- object:

  A `segmenter` object.

- ...:

  currently ignored

## Value

A `character` vector of length 1.

## Details

Every segmenter works by fitting a model to the data. `model_name()`
returns the name of a model that can be passed to
[`whomademe()`](https://beanumber.github.io/tidychangepoint/reference/whomademe.md)
to retrieve the model fitting function. These functions must begin with
the prefix `fit_`. Note that the model fitting functions exist in
`tidychangepoint` are are not necessarily the actual functions used by
the segmenter.

Models also implement `model_name()`.

## See also

Other model-fitting:
[`fit_arima()`](https://beanumber.github.io/tidychangepoint/reference/fit_arima.md),
[`fit_lmshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_lmshift.md),
[`fit_meanshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanshift.md),
[`fit_meanvar()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanvar.md),
[`fit_nhpp()`](https://beanumber.github.io/tidychangepoint/reference/fit_nhpp.md),
[`model_args()`](https://beanumber.github.io/tidychangepoint/reference/model_args.md),
[`new_fun_cpt()`](https://beanumber.github.io/tidychangepoint/reference/fun_cpt.md),
[`whomademe()`](https://beanumber.github.io/tidychangepoint/reference/whomademe.md)

Other tidycpt-generics:
[`as.model()`](https://beanumber.github.io/tidychangepoint/reference/as.model.md),
[`as.segmenter()`](https://beanumber.github.io/tidychangepoint/reference/as.segmenter.md),
[`changepoints()`](https://beanumber.github.io/tidychangepoint/reference/changepoints.md),
[`diagnose()`](https://beanumber.github.io/tidychangepoint/reference/diagnose.md),
[`fitness()`](https://beanumber.github.io/tidychangepoint/reference/fitness.md)

## Examples

``` r
# Segment a time series using PELT
x <- segment(CET, method = "pelt")

# Retrieve the name of the model from the segmenter
x |>
  as.segmenter() |>
  model_name()
#> [1] "meanvar"

# What function created the model? 
x |>
  model_name() |>
  whomademe()
#> function (x, tau, ...) 
#> {
#>     if (!is_valid_tau(tau, length(x))) {
#>         stop("Invalid changepoint set")
#>     }
#>     else {
#>         tau <- unique(tau)
#>     }
#>     regions <- split_by_tau(as.ts(x), tau)
#>     region_mods <- purrr::map(regions, ~fit_meanshift_norm(.x, 
#>         tau = NULL))
#>     fitted_values <- purrr::list_c(purrr::map(region_mods, ~c(fitted(.x))))
#>     region_params <- dplyr::mutate(purrr::list_rbind(purrr::map(region_mods, 
#>         purrr::pluck("region_params"))), region = names(regions))
#>     region_params$param_sigma_hatsq <- purrr::map_dbl(region_mods, 
#>         model_variance)
#>     mod_cpt(x <- as.ts(x), tau = tau, region_params = region_params, 
#>         model_params = c(), fitted_values = fitted_values, model_name = "meanvar")
#> }
#> <bytecode: 0x55ffb1b757d8>
#> <environment: namespace:tidychangepoint>
#> attr(,"model_name")
#> [1] "meanvar"
#> attr(,"class")
#> [1] "fun_cpt"
model_name(x$segmenter)
#> [1] "meanvar"

# Retrieve the name of the model from the model
x |>
  as.model() |>
  model_name()
#> [1] "meanvar"
  
```
