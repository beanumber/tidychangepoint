# Fit a non-homogeneous Poisson process model to the exceedances of a time series.

Fit a non-homogeneous Poisson process model to the exceedances of a time
series.

## Usage

``` r
fit_nhpp(x, tau, ...)
```

## Arguments

- x:

  A time series

- tau:

  A vector of changepoints

- ...:

  currently ignored

## Value

An `nhpp` object, which inherits from
[mod_cpt](https://beanumber.github.io/tidychangepoint/reference/mod_cpt.md).

## Details

Any time series can be modeled as a non-homogeneous Poisson process of
the locations of the
[exceedances](https://beanumber.github.io/tidychangepoint/reference/exceedances.md)
of a threshold in the series. This function uses the
[BMDL](https://beanumber.github.io/tidychangepoint/reference/BMDL.md)
criteria to determine the best fit parameters for each region defined by
the changepoint set `tau`.

## See also

Other model-fitting:
[`fit_arima()`](https://beanumber.github.io/tidychangepoint/reference/fit_arima.md),
[`fit_lmshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_lmshift.md),
[`fit_meanshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanshift.md),
[`fit_meanvar()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanvar.md),
[`model_args()`](https://beanumber.github.io/tidychangepoint/reference/model_args.md),
[`model_name()`](https://beanumber.github.io/tidychangepoint/reference/model_name.md),
[`new_fun_cpt()`](https://beanumber.github.io/tidychangepoint/reference/fun_cpt.md),
[`whomademe()`](https://beanumber.github.io/tidychangepoint/reference/whomademe.md)

## Examples

``` r
# Fit an NHPP model using the mean as a threshold
fit_nhpp(DataCPSim, tau = 826)
#> ℹ Model: A nhpp  model with 2 region(s).
#> → Each region has 2 parameter(s).
#> → The model has 1 global parameter(s).

# Fit an NHPP model using other thresholds
fit_nhpp(DataCPSim, tau = 826, threshold = 20)
#> ℹ Model: A nhpp  model with 2 region(s).
#> → Each region has 2 parameter(s).
#> → The model has 1 global parameter(s).
fit_nhpp(DataCPSim, tau = 826, threshold = 200)
#> ℹ Model: A nhpp  model with 2 region(s).
#> → Each region has 2 parameter(s).
#> → The model has 1 global parameter(s).

# Fit an NHPP model using changepoints determined by PELT
fit_nhpp(DataCPSim, tau = changepoints(segment(DataCPSim, method = "pelt")))
#> ℹ Model: A nhpp  model with 4 region(s).
#> → Each region has 2 parameter(s).
#> → The model has 1 global parameter(s).
```
