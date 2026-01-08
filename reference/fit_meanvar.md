# Fit a model for mean and variance

Fit a model for mean and variance

## Usage

``` r
fit_meanvar(x, tau, ...)
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

In a mean-variance model, both the means and variances are allowed to
vary across regions. Thus, this model fits a separate \\\mu_j\\ and
\\\sigma_j\\ for each region \\j\\.

## See also

[`changepoint::cpt.meanvar()`](https://rdrr.io/pkg/changepoint/man/cpt.meanvar.html)

Other model-fitting:
[`fit_arima()`](https://beanumber.github.io/tidychangepoint/reference/fit_arima.md),
[`fit_lmshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_lmshift.md),
[`fit_meanshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanshift.md),
[`fit_nhpp()`](https://beanumber.github.io/tidychangepoint/reference/fit_nhpp.md),
[`model_args()`](https://beanumber.github.io/tidychangepoint/reference/model_args.md),
[`model_name()`](https://beanumber.github.io/tidychangepoint/reference/model_name.md),
[`new_fun_cpt()`](https://beanumber.github.io/tidychangepoint/reference/fun_cpt.md),
[`whomademe()`](https://beanumber.github.io/tidychangepoint/reference/whomademe.md)

## Examples

``` r
# Fit a mean-variance model
fit_meanvar(CET, tau = c(42, 330))
#> ℹ Model: A meanvar  model with 3 region(s).
#> → Each region has 2 parameter(s).
#> → The model has 0 global parameter(s).
```
