# Class for model-fitting functions

Class for model-fitting functions

## Usage

``` r
new_fun_cpt(x, ...)

validate_fun_cpt(x)

fun_cpt(x, ...)
```

## Arguments

- x:

  a `character` giving the name of a model-fitting function

- ...:

  currently ignored

## Value

A fun_cpt object.

## Details

All model-fitting functions must be registered through a call to
`fun_cpt()`.

All model-fitting functions must take at least three arguments:

- `x`: a time series,

- `tau`: a set of changepoint indices

- `...`: other arguments passed to methods

See
[`fit_meanshift_norm()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanshift.md),

## See also

Other model-fitting:
[`fit_arima()`](https://beanumber.github.io/tidychangepoint/reference/fit_arima.md),
[`fit_lmshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_lmshift.md),
[`fit_meanshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanshift.md),
[`fit_meanvar()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanvar.md),
[`fit_nhpp()`](https://beanumber.github.io/tidychangepoint/reference/fit_nhpp.md),
[`model_args()`](https://beanumber.github.io/tidychangepoint/reference/model_args.md),
[`model_name()`](https://beanumber.github.io/tidychangepoint/reference/model_name.md),
[`whomademe()`](https://beanumber.github.io/tidychangepoint/reference/whomademe.md)

## Examples

``` r
# Register a model-fitting function
f <- fun_cpt("fit_meanvar")

# Verify that it now has class `fun_cpt`
str(f)
#> function (x, tau, ...)  
#>  - attr(*, "model_name")= chr "meanvar"
#>  - attr(*, "class")= chr "fun_cpt"

# Use it
f(CET, 42)
#> ℹ Model: A meanvar  model with 2 region(s).
#> → Each region has 2 parameter(s).
#> → The model has 0 global parameter(s).
```
