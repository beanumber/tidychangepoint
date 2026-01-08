# Fast implementation of meanshift model

Fast implementation of meanshift model

## Usage

``` r
fit_meanshift(x, tau, distribution = "norm", ...)

fit_meanshift_norm(x, tau, ...)

fit_meanshift_lnorm(x, tau, ...)

fit_meanshift_norm_ar1(x, tau, ...)
```

## Arguments

- x:

  A time series

- tau:

  a set of indices representing a changepoint set

- distribution:

  A character indicating the distribution of the data. Should match R
  distribution function naming conventions (e.g., "norm" for the Normal
  distribution, etc.)

- ...:

  arguments passed to [`stats::lm()`](https://rdrr.io/r/stats/lm.html)

## Value

A
[mod_cpt](https://beanumber.github.io/tidychangepoint/reference/mod_cpt.md)
object.

## Details

`fit_meanshift_norm()` returns the same model as
[`fit_lmshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_lmshift.md)
with the `deg_poly` argument set to 0. However, it is faster on large
changepoint sets.

`fit_meanshift_lnorm()` fit the meanshift model with the assumption of
log-normally distributed data.

`fit_meanshift_norm_ar1()` applies autoregressive errors.

## See also

Other model-fitting:
[`fit_arima()`](https://beanumber.github.io/tidychangepoint/reference/fit_arima.md),
[`fit_lmshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_lmshift.md),
[`fit_meanvar()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanvar.md),
[`fit_nhpp()`](https://beanumber.github.io/tidychangepoint/reference/fit_nhpp.md),
[`model_args()`](https://beanumber.github.io/tidychangepoint/reference/model_args.md),
[`model_name()`](https://beanumber.github.io/tidychangepoint/reference/model_name.md),
[`new_fun_cpt()`](https://beanumber.github.io/tidychangepoint/reference/fun_cpt.md),
[`whomademe()`](https://beanumber.github.io/tidychangepoint/reference/whomademe.md)

## Author

Xueheng Shi, Ben Baumer

## Examples

``` r
# Manually specify a changepoint set
tau <- c(365, 826)

# Fit the model
mod <- fit_meanshift_norm_ar1(DataCPSim, tau)

# View model parameters
logLik(mod)
#> 'log Lik.' -5195.452 (df=7)
deg_free(mod)
#> [1] 7

# Manually specify a changepoint set
cpts <- c(1700, 1739, 1988)
ids <- time2tau(cpts, as_year(time(CET)))

# Fit the model
mod <- fit_meanshift_norm(CET, tau = ids)

# Review model parameters
glance(mod)
#> # A tibble: 1 × 11
#>   pkg     version algorithm params num_cpts  rmse logLik   AIC   BIC  MBIC   MDL
#>   <chr>   <pckg_> <chr>     <list>    <int> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 tidych… 1.0.2   meanshif… <dbl>         3 0.576  -317.  651.  682.  681.  692.

# Fit an autoregressive model
mod <- fit_meanshift_norm_ar1(CET, tau = ids)

# Review model parameters
glance(mod)
#> # A tibble: 1 × 11
#>   pkg     version algorithm params num_cpts  rmse logLik   AIC   BIC  MBIC   MDL
#>   <chr>   <pckg_> <chr>     <list>    <int> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 tidych… 1.0.2   meanshif… <dbl>         3 0.567  -311.  640.  675.  668.  684.
```
