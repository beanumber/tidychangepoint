# Fit an NHPP model to one specific region

Fit an NHPP model to one specific region

## Usage

``` r
fit_nhpp_region(exc, tau_left, tau_right, params = parameters_weibull(), ...)
```

## Arguments

- exc:

  Output from
  [`exceedances()`](https://beanumber.github.io/tidychangepoint/reference/exceedances.md)

- tau_left:

  left-most changepoint

- tau_right:

  right-most changepoint

- params:

  Output from
  [`parameters_weibull()`](https://beanumber.github.io/tidychangepoint/reference/iweibull.md)

- ...:

  arguments passed to
  [`stats::optim()`](https://rdrr.io/r/stats/optim.html)

## Value

Modified output from
[`stats::optim()`](https://rdrr.io/r/stats/optim.html).

## Details

This is an internal function not to be called by users. Use
[`fit_nhpp()`](https://beanumber.github.io/tidychangepoint/reference/fit_nhpp.md).

## See also

[`fit_nhpp()`](https://beanumber.github.io/tidychangepoint/reference/fit_nhpp.md)
