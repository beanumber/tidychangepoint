# Vectors implementation for logLik

Vectors implementation for logLik

## Usage

``` r
# S3 method for class 'logLik.logLik'
vec_ptype2(x, y, ...)

# S3 method for class 'logLik.logLik'
vec_cast(x, to, ...)
```

## Arguments

- x, y:

  Vector types.

- ...:

  These dots are for future extensions and must be empty.

- to:

  Type to cast to. If `NULL`, `x` will be returned as is.

## Value

A [`stats::logLik()`](https://rdrr.io/r/stats/logLik.html) vector.

## See also

[`stats::logLik()`](https://rdrr.io/r/stats/logLik.html)

## Examples

``` r
a <- logLik(lm(mpg ~ disp, data = mtcars))
b <- logLik(lm(mpg ~ am, data = mtcars))
vec_ptype2(a, b)
#> 'log Lik.'  (df=3)
c(a, b)
#> [1] -82.10469 -95.24219
vec_cast(a, b)
#> 'log Lik.' -82.10469 (df=3)
```
