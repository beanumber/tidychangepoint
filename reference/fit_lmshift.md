# Regression-based model fitting

Regression-based model fitting

## Usage

``` r
fit_lmshift(x, tau, deg_poly = 0, ...)

fit_lmshift_ar1(x, tau, ...)

fit_trendshift(x, tau, ...)

fit_trendshift_ar1(x, tau, ...)
```

## Arguments

- x:

  A time series

- tau:

  a set of indices representing a changepoint set

- deg_poly:

  integer indicating the degree of the polynomial spline to be fit.
  Passed to [`stats::poly()`](https://rdrr.io/r/stats/poly.html).

- ...:

  arguments passed to [`stats::lm()`](https://rdrr.io/r/stats/lm.html)

## Value

A
[mod_cpt](https://beanumber.github.io/tidychangepoint/reference/mod_cpt.md)
object

## Details

These model-fitting functions use
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) to fit the
corresponding regression model to a time series, using the changepoints
specified by the `tau` argument. Each changepoint is treated as a
categorical fixed-effect, while the `deg_poly` argument controls the
degree of the polynomial that interacts with those fixed-effects. For
example, setting `deg_poly` equal to 0 will return the same model as
calling
[`fit_meanshift_norm()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanshift.md),
but the latter is faster for larger changepoint sets because it doesn't
have to fit all of the regression models.

Setting `deg_poly` equal to 1 fits the `trendshift` model.

- `fit_lmshift_ar1()`: will apply auto-regressive lag 1 errors

&nbsp;

- `fit_trendshift()`: will fit a line in each region

&nbsp;

- `fit_trendshift_ar1()`: will fit a line in each region and autoregress
  lag 1 errors

## See also

Other model-fitting:
[`fit_arima()`](https://beanumber.github.io/tidychangepoint/reference/fit_arima.md),
[`fit_meanshift()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanshift.md),
[`fit_meanvar()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanvar.md),
[`fit_nhpp()`](https://beanumber.github.io/tidychangepoint/reference/fit_nhpp.md),
[`model_args()`](https://beanumber.github.io/tidychangepoint/reference/model_args.md),
[`model_name()`](https://beanumber.github.io/tidychangepoint/reference/model_name.md),
[`new_fun_cpt()`](https://beanumber.github.io/tidychangepoint/reference/fun_cpt.md),
[`whomademe()`](https://beanumber.github.io/tidychangepoint/reference/whomademe.md)

## Examples

``` r
# Manually specify a changepoint set
tau <- c(365, 826)

# Fit the model
mod <- fit_lmshift(DataCPSim, tau)

# Retrieve model parameters
logLik(mod)
#> 'log Lik.' -5250.548 (df=6)
deg_free(mod)
#> [1] 6

# Manually specify a changepoint set
cpts <- c(1700, 1739, 1988)
ids <- time2tau(cpts, as_year(time(CET)))

# Fit the model
mod <- fit_lmshift(CET, tau = ids)

# View model parameters
glance(mod)
#> # A tibble: 1 × 11
#>   pkg     version algorithm params num_cpts  rmse logLik   AIC   BIC  MBIC   MDL
#>   <chr>   <pckg_> <chr>     <list>    <int> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 tidych… 1.0.3   meanshift <dbl>         3 0.576  -317.  651.  682.  681.  692.
glance(fit_lmshift(CET, tau = ids, deg_poly = 1))
#> # A tibble: 1 × 11
#>   pkg     version algorithm params num_cpts  rmse logLik   AIC   BIC  MBIC   MDL
#>   <chr>   <pckg_> <chr>     <list>    <int> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 tidych… 1.0.3   trendshi… <dbl>         3 0.538  -292.  608.  655.  630.  658.
glance(fit_lmshift_ar1(CET, tau = ids))
#> # A tibble: 1 × 11
#>   pkg     version algorithm params num_cpts  rmse logLik   AIC   BIC  MBIC   MDL
#>   <chr>   <pckg_> <chr>     <list>    <int> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 tidych… 1.0.3   meanshif… <dbl>         3 0.567  -311.  640.  675.  668.  684.
glance(fit_lmshift_ar1(CET, tau = ids, deg_poly = 1))
#> # A tibble: 1 × 11
#>   pkg     version algorithm params num_cpts  rmse logLik   AIC   BIC  MBIC   MDL
#>   <chr>   <pckg_> <chr>     <list>    <int> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 tidych… 1.0.3   trendshi… <dbl>         3 0.537  -291.  608.  658.  628.  661.
glance(fit_lmshift_ar1(CET, tau = ids, deg_poly = 2))
#> # A tibble: 1 × 11
#>   pkg     version algorithm params num_cpts  rmse logLik   AIC   BIC  MBIC   MDL
#>   <chr>   <pckg_> <chr>     <list>    <int> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 tidych… 1.0.3   splinesh… <dbl>         3 0.535  -290.  613.  680.  625.  675.

# Empty changepoint sets are allowed
fit_lmshift(CET, tau = NULL)
#> ℹ Model: A null  model with 1 region(s).
#> → Each region has 1 parameter(s).
#> → The model has 1 global parameter(s).

# Duplicate changepoints are removed
fit_lmshift(CET, tau = c(42, 42))
#> ℹ Model: A meanshift  model with 2 region(s).
#> → Each region has 1 parameter(s).
#> → The model has 1 global parameter(s).
```
