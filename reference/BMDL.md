# Bayesian Maximum Descriptive Length

Generic function to compute the Bayesian Maximum Descriptive Length for
a changepoint detection model.

## Usage

``` r
BMDL(object, ...)

# Default S3 method
BMDL(object, ...)

# S3 method for class 'nhpp'
BMDL(object, ...)
```

## Arguments

- object:

  any object from which a log-likelihood value, or a contribution to a
  log-likelihood value, can be extracted.

- ...:

  some methods for this generic function require additional arguments.

## Value

A `double` vector of length 1

## Details

Currently, the BMDL function is only defined for the NHPP model (see
[`fit_nhpp()`](https://beanumber.github.io/tidychangepoint/reference/fit_nhpp.md)).
Given a changepoint set \\\tau\\, the BMDL is: \$\$ BMDL(\tau, NHPP(y \|
\hat{\theta}\_\tau) = P\_{MDL}(\tau) - 2 \ln{ L\_{NHPP}(y \|
\hat{\theta}\_\tau) } - 2 \ln{ g(\hat{\theta}\_\tau) } \$\$ where
\\P\_{MDL}(\tau)\\ is the
[`MDL()`](https://beanumber.github.io/tidychangepoint/reference/MDL.md)
penalty.

## See also

Other penalty-functions:
[`HQC()`](https://beanumber.github.io/tidychangepoint/reference/HQC.md),
[`MBIC()`](https://beanumber.github.io/tidychangepoint/reference/MBIC.md),
[`MDL()`](https://beanumber.github.io/tidychangepoint/reference/MDL.md),
[`SIC()`](https://beanumber.github.io/tidychangepoint/reference/SIC.md)

## Examples

``` r
# Compute the BMDL
BMDL(fit_nhpp(DataCPSim, tau = NULL))
#> [1] 1454.918
BMDL(fit_nhpp(DataCPSim, tau = c(365, 830)))
#> [1] 1233.919
```
