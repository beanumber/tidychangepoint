# Fit an ARIMA model

Fit an ARIMA model

## Usage

``` r
fit_arima(x, tau, ...)
```

## Arguments

- x:

  A time series

- tau:

  a set of indices representing a changepoint set

- ...:

  currently ignored

## Value

A
[mod_cpt](https://beanumber.github.io/tidychangepoint/reference/mod_cpt.md)
object.

## Details

Fits an ARIMA model using
[`stats::arima()`](https://rdrr.io/r/stats/arima.html).

## See also

[`changepointGA::ARIMA.BIC()`](https://rdrr.io/pkg/changepointGA/man/ARIMA.BIC.html)

Other model-fitting:
[`fit_lmshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_lmshift.md),
[`fit_meanshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanshift.md),
[`fit_meanvar()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanvar.md),
[`fit_nhpp()`](https://beanumber.github.io/tidychangepoint/reference/fit_nhpp.md),
[`model_args()`](https://beanumber.github.io/tidychangepoint/reference/model_args.md),
[`model_name()`](https://beanumber.github.io/tidychangepoint/reference/model_name.md),
[`new_fun_cpt()`](https://beanumber.github.io/tidychangepoint/reference/fun_cpt.md),
[`whomademe()`](https://beanumber.github.io/tidychangepoint/reference/whomademe.md)

## Examples

``` r
# Fit a mean-variance model
fit_arima(CET, tau = c(42, 330))
#> ℹ Model: A arima  model with 3 region(s).
#> → Each region has 2 parameter(s).
#> → The model has 0 global parameter(s).
```
