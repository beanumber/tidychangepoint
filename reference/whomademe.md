# Recover the function that created a model

Recover the function that created a model

## Usage

``` r
whomademe(x, ...)
```

## Arguments

- x:

  A `character` giving the name of a model. To be passed to
  [`model_name()`](https://beanumber.github.io/tidychangepoint/reference/model_name.md).

- ...:

  currently ignored

## Value

A `function`

## Details

Model objects (inheriting from
[mod_cpt](https://beanumber.github.io/tidychangepoint/reference/mod_cpt.md))
know the name of the function that created them. `whomademe()` returns
that function.

## See also

Other model-fitting:
[`fit_arima()`](https://beanumber.github.io/tidychangepoint/reference/fit_arima.md),
[`fit_lmshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_lmshift.md),
[`fit_meanshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanshift.md),
[`fit_meanvar()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanvar.md),
[`fit_nhpp()`](https://beanumber.github.io/tidychangepoint/reference/fit_nhpp.md),
[`model_args()`](https://beanumber.github.io/tidychangepoint/reference/model_args.md),
[`model_name()`](https://beanumber.github.io/tidychangepoint/reference/model_name.md),
[`new_fun_cpt()`](https://beanumber.github.io/tidychangepoint/reference/fun_cpt.md)

## Examples

``` r
# Get the function that made a model
f <- whomademe(fit_meanshift_norm(CET, tau = 42))
str(f)
#> function (x, tau, ...)  
#>  - attr(*, "model_name")= chr "meanshift_norm"
#>  - attr(*, "class")= chr "fun_cpt"
```
