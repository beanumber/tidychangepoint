# Compute model variance

Compute model variance

## Usage

``` r
model_variance(object, ...)
```

## Arguments

- object:

  A model object implementing
  [`residuals()`](https://rdrr.io/r/stats/residuals.html) and
  [`nobs()`](https://rdrr.io/r/stats/nobs.html)

- ...:

  currently ignored

## Value

A `double` vector of length 1

## Details

Using the generic functions
[`residuals()`](https://rdrr.io/r/stats/residuals.html) and
[`nobs()`](https://rdrr.io/r/stats/nobs.html), this function computes
the variance of the residuals.

Note that unlike [`stats::var()`](https://rdrr.io/r/stats/cor.html), it
does not use \\n-1\\ as the denominator.
