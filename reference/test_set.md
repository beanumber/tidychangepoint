# Simulate time series with known changepoint sets

Simulate time series with known changepoint sets

## Usage

``` r
test_set(n = 1, sd = 1, seed = NULL)
```

## Arguments

- n:

  Number of true changepoints in set

- sd:

  Standard deviation passed to
  [`stats::rnorm()`](https://rdrr.io/r/stats/Normal.html)

- seed:

  Value passed to
  [`base::set.seed()`](https://rdrr.io/r/base/Random.html)

## Value

A [`stats::ts()`](https://rdrr.io/r/stats/ts.html) object

## See also

[DataCPSim](https://beanumber.github.io/tidychangepoint/reference/DataCPSim.md)

## Examples

``` r
x <- test_set()
plot(x)

changepoints(x)
#> [1] 515
```
