# Hannan–Quinn information criterion

Hannan–Quinn information criterion

## Usage

``` r
HQC(object, ...)

# Default S3 method
HQC(object, ...)

# S3 method for class 'logLik'
HQC(object, ...)
```

## Arguments

- object:

  any object from which a log-likelihood value, or a contribution to a
  log-likelihood value, can be extracted.

- ...:

  some methods for this generic function require additional arguments.

## Details

Computes the Hannan-Quinn information criterion for a model \\M\\ \$\$
HQC(\tau, M(y\|\hat{\theta}\_{\tau})) = 2k \cdot \ln{\ln{n}} - 2 \cdot
L_M(y\|\hat{\theta}\_\tau) \\, \$\$ where \\k\\ is the number of
parameters and \\n\\ is the number of observations.

## See also

[`stats::BIC()`](https://rdrr.io/r/stats/AIC.html),
[`stats::AIC()`](https://rdrr.io/r/stats/AIC.html)

Other penalty-functions:
[`BMDL()`](https://beanumber.github.io/tidychangepoint/reference/BMDL.md),
[`MBIC()`](https://beanumber.github.io/tidychangepoint/reference/MBIC.md),
[`MDL()`](https://beanumber.github.io/tidychangepoint/reference/MDL.md),
[`SIC()`](https://beanumber.github.io/tidychangepoint/reference/SIC.md)

## Examples

``` r
# Compute the HQC
HQC(fit_meanvar(CET, tau = NULL))
#> [1] 780.7158

HQC(fit_meanshift_norm_ar1(CET, tau = c(42, 330)))
#> [1] 635.8524
HQC(fit_trendshift(CET, tau = c(42, 81, 330)))
#> [1] 605.6614
```
