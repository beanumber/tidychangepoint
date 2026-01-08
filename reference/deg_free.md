# Retrieve the degrees of freedom from a `logLik` object

Retrieve the degrees of freedom from a `logLik` object

## Usage

``` r
deg_free(x)
```

## Arguments

- x:

  An object that implements a method for
  [`stats::logLik()`](https://rdrr.io/r/stats/logLik.html).

## Value

The `df` attribute of the
[`stats::logLik()`](https://rdrr.io/r/stats/logLik.html) of the given
object.

## Examples

``` r
# Retrieve the degrees of freedom model a changepoint model
DataCPSim |>
  segment() |>
  as.model() |>
  deg_free()
#> [1] 2
  
```
