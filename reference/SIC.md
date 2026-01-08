# Schwarz information criterion

Schwarz information criterion

## Usage

``` r
SIC(object, ...)
```

## Arguments

- object:

  a fitted model object for which there exists a `logLik` method to
  extract the corresponding log-likelihood, or an object inheriting from
  class `logLik`.

- ...:

  optionally more fitted model objects.

## Value

The value of [`stats::BIC()`](https://rdrr.io/r/stats/AIC.html)

## See also

[`stats::BIC()`](https://rdrr.io/r/stats/AIC.html),
[`stats::AIC()`](https://rdrr.io/r/stats/AIC.html)

Other penalty-functions:
[`BMDL()`](https://beanumber.github.io/tidychangepoint/reference/BMDL.md),
[`HQC()`](https://beanumber.github.io/tidychangepoint/reference/HQC.md),
[`MBIC()`](https://beanumber.github.io/tidychangepoint/reference/MBIC.md),
[`MDL()`](https://beanumber.github.io/tidychangepoint/reference/MDL.md)

## Examples

``` r
# The SIC is just the BIC
SIC(fit_meanvar(CET, tau = NULL))
#> [1] 788.9703
BIC(fit_meanvar(CET, tau = NULL))
#> [1] 788.9703
```
