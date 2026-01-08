# Simulated time series data

Randomly-generated time series data, using the
[`stats::rlnorm()`](https://rdrr.io/r/stats/Lognormal.html) function.

- For `rlnorm_ts_1`, there is one changepoint located at 826.

- For `rlnorm_ts_2`, there are two changepoints, located at 366 and 731.

- For `rlnorm_ts_3`, there are three changepoints, located at 548, 823,
  and 973.

## Usage

``` r
DataCPSim

rlnorm_ts_1

rlnorm_ts_2

rlnorm_ts_3
```

## Format

An object of class `numeric` of length 1096.

An object of class `ts` of length 1096.

An object of class `ts` of length 1096.

An object of class `ts` of length 1096.

## Details

- `DataCPSim`: Simulated time series of the same length as
  [bogota_pm](https://beanumber.github.io/tidychangepoint/reference/bogota_pm.md).

## See also

[bogota_pm](https://beanumber.github.io/tidychangepoint/reference/bogota_pm.md)

[`stats::ts()`](https://rdrr.io/r/stats/ts.html),
[`test_set()`](https://beanumber.github.io/tidychangepoint/reference/test_set.md)

## Examples

``` r
plot(rlnorm_ts_1)

plot(rlnorm_ts_2)

plot(rlnorm_ts_3)

changepoints(rlnorm_ts_1)
#> [1] 826
```
