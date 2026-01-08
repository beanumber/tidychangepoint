# Convert changepoint sets to time indices

Convert changepoint sets to time indices

## Usage

``` r
tau2time(tau, index)

time2tau(cpts, index)
```

## Arguments

- tau:

  a numeric vector of changepoint indices

- index:

  Index of times, typically returned by
  [`stats::time()`](https://rdrr.io/r/stats/time.html)

- cpts:

  Time series observation labels to be converted to indices

## Value

- `tau2time()`: a `character` of time labels

&nbsp;

- `time2tau()`: an `integer` vector of changepoint indices

## See also

[`stats::time()`](https://rdrr.io/r/stats/time.html),
[`as_year()`](https://beanumber.github.io/tidychangepoint/reference/as_year.md)

## Examples

``` r
# Recover the years from a set of changepoint indices
tau2time(c(42, 81, 330), index = as_year(time(CET)))
#> [1] "1700" "1739" "1988"

# Recover the changepoint set indices from the years
time2tau(c(1700, 1739, 1988), index = as_year(time(CET)))
#> [1]  42  81 330
```
