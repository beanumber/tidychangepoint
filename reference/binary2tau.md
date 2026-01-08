# Convert changepoint sets to binary strings

Convert changepoint sets to binary strings

## Usage

``` r
binary2tau(x)

tau2binary(tau, n)
```

## Arguments

- x:

  A binary string that encodes a changepoint set. See
  [`GA::gabin_Population()`](https://github.com/luca-scr/GA/reference/ga_Population.html).

- tau:

  a numeric vector of changepoint indices

- n:

  the length of the original time series

## Value

- `binary2tau()`: an `integer` vector

&nbsp;

- `tau2binary()`: an `integer` vector of length `n`

## Details

In order to use
[`GA::ga()`](https://github.com/luca-scr/GA/reference/ga.html) in a
genetic algorithm, we need to encoude a changepoint set as a binary
string.

`binary2tau()` takes a binary string representation of a changepoint set
and converts it into a set of changepoint indices.

`tau2binary()` takes a set of changepoint indices the number of
observations in the time series and converts them into a binary string
representation of that changepoint set.

## Examples

``` r
# Recover changepoint set indices from binary strings
binary2tau(c(0, 0, 1, 0, 1))
#> [1] 3 5
binary2tau(round(runif(10)))
#> [1]  2  4  5  7  8 10

# Recover binary strings from changepoint set indices
tau2binary(c(7, 17), n = 24)
#>  [1] 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0
tau2binary(binary2tau(c(0, 0, 1, 1, 0, 1)), n = 6)
#> [1] 0 0 1 1 0 1
```
