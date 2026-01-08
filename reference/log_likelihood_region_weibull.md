# Log-Likelihood functions for regions (Weibull)

Log-Likelihood functions for regions (Weibull)

## Usage

``` r
log_likelihood_region_weibull(t, tau_left, tau_right, theta)

log_prior_region_weibull(theta, params = parameters_weibull())

D_log_prior_region_weibull(theta, params = parameters_weibull())

D_log_likelihood_region_weibull(t, tau_left, tau_right, theta)
```

## Arguments

- t:

  vector of
  [`exceedances()`](https://beanumber.github.io/tidychangepoint/reference/exceedances.md)

- tau_left:

  Left endpoint of the region

- tau_right:

  Right endpoint of the region

- theta:

  numeric vector of parameters for the NHPP model

- params:

  Possibly modified output from
  [`parameters_weibull()`](https://beanumber.github.io/tidychangepoint/reference/iweibull.md)

## Value

A numeric vector
