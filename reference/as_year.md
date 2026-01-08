# Convert a date into a year

Convert a date into a year

## Usage

``` r
as_year(x)
```

## Arguments

- x:

  an object coercible into a
  [base::Date](https://rdrr.io/r/base/Dates.html). See
  [`base::as.Date()`](https://rdrr.io/r/base/as.Date.html).

## Value

A `character` vector representing the years of the input

## Examples

``` r
# Retrieve only the year
as_year("1988-01-01")
#> [1] "1988"
```
