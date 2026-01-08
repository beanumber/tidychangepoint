# Plot the intensity of an NHPP fit

Plot the intensity of an NHPP fit

## Usage

``` r
plot_intensity(x, ...)
```

## Arguments

- x:

  An NHPP `model` returned by
  [`fit_nhpp()`](https://beanumber.github.io/tidychangepoint/reference/fit_nhpp.md)

- ...:

  currently ignored

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object

## Examples

``` r
# Plot the estimated intensity function
plot_intensity(fit_nhpp(DataCPSim, tau = 826))


# Segment a time series using PELT
mod <- segment(bogota_pm, method = "pelt")

# Plot the estimated intensity function for the NHPP model using the 
# changepoints found by PELT
plot_intensity(fit_nhpp(bogota_pm, tau = changepoints(mod)))

```
