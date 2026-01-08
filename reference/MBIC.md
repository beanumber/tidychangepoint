# Modified Bayesian Information Criterion

Generic function to compute the Modified Bayesian Information Criterion
for a changepoint detection model.

## Usage

``` r
MBIC(object, ...)

# Default S3 method
MBIC(object, ...)

# S3 method for class 'logLik'
MBIC(object, ...)
```

## Arguments

- object:

  any object from which a log-likelihood value, or a contribution to a
  log-likelihood value, can be extracted.

- ...:

  some methods for this generic function require additional arguments.

## Value

A `double` vector of length 1

## References

Zhang and Seigmmund (2007) for MBIC:
[doi:10.1111/j.1541-0420.2006.00662.x](https://doi.org/10.1111/j.1541-0420.2006.00662.x)

## See also

[`stats::BIC()`](https://rdrr.io/r/stats/AIC.html)

Other penalty-functions:
[`BMDL()`](https://beanumber.github.io/tidychangepoint/reference/BMDL.md),
[`HQC()`](https://beanumber.github.io/tidychangepoint/reference/HQC.md),
[`MDL()`](https://beanumber.github.io/tidychangepoint/reference/MDL.md),
[`SIC()`](https://beanumber.github.io/tidychangepoint/reference/SIC.md)
