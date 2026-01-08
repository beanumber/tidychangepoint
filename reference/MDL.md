# Maximum Descriptive Length

Generic function to compute the Maximum Descriptive Length for a
changepoint detection model.

## Usage

``` r
MDL(object, ...)

# Default S3 method
MDL(object, ...)

# S3 method for class 'logLik'
MDL(object, ...)
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

\$\$ P\_{MDL}(\tau) = \frac{a(\theta\_\tau)}{2} \cdot \sum\_{j=0}^m
\log{\left(\tau_j - \tau\_{j-1} \right)} + 2 \ln{m} + \sum\_{j=2}^m
\ln{\tau_j} + \left( 2 + b(\theta\_\tau) \right) \ln{n} \$\$ where
\\a(\theta)\\ is the number of parameters in \\\theta\\ that are fit in
each region, and \\b(\theta)\\ is the number of parameters fit to the
model as a whole.

These quantities should be
[`base::attributes()`](https://rdrr.io/r/base/attributes.html) of the
object returned by [`logLik()`](https://rdrr.io/r/stats/logLik.html).

## See also

Other penalty-functions:
[`BMDL()`](https://beanumber.github.io/tidychangepoint/reference/BMDL.md),
[`HQC()`](https://beanumber.github.io/tidychangepoint/reference/HQC.md),
[`MBIC()`](https://beanumber.github.io/tidychangepoint/reference/MBIC.md),
[`SIC()`](https://beanumber.github.io/tidychangepoint/reference/SIC.md)

## Examples

``` r
MDL(fit_meanshift_norm_ar1(CET, tau = c(42, 330)))
#> [1] 673.007
MDL(fit_trendshift(CET, tau = c(42, 81, 330)))
#> [1] 657.6599
```
