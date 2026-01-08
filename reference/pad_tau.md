# Pad and unpad changepoint sets with boundary points

Pad and unpad changepoint sets with boundary points

## Usage

``` r
pad_tau(tau, n)

unpad_tau(padded_tau)

is_valid_tau(tau, n)

regions_tau(tau, n)

validate_tau(tau, n)
```

## Arguments

- tau:

  a numeric vector of changepoint indices

- n:

  the length of the original time series

- padded_tau:

  Output from `pad_tau()`

## Value

- `pad_tau()`: an `integer` vector that starts with 0 and ends in \\n\\.

&nbsp;

- `unpad_tau()`: an `integer` vector stripped of its first and last
  entries.

&nbsp;

- `is_valid_tau()`: a `logical` if all of the entries are between 2 and
  \\n-1\\.

&nbsp;

- `regions_tau()`: A
  [`base::factor()`](https://rdrr.io/r/base/factor.html)

&nbsp;

- `validate_tau()`: an `integer` vector with only the
  [`base::unique()`](https://rdrr.io/r/base/unique.html) entries between
  2 and \\n-1\\, inclusive.

## Details

If a time series contains \\n\\ observations, we label them from 1 to
\\n\\. Neither the 1st point nor the \\n\\th point can be a changepoint,
since the regions they create on one side would be empty. However, for
dividing the time series into non-empty segments, we start with 1, add
\\n+1\\, and then divide the half-open interval \\\[1, n+1)\\ into
half-open subintervals that define the regions.

`pad_tau()` ensures that 1 and \\n+1\\ are included.

`unpad_tau()` removes 1 and \\n+1\\, should they exist.

`is_valid_tau()` checks to see if the supplied set of changepoints is
valid

`validate_tau()` removes duplicates and boundary values.

## Examples

``` r
# Anything less than 2 is not allowed
is_valid_tau(0, length(DataCPSim))
#> [1] FALSE
is_valid_tau(1, length(DataCPSim))
#> [1] FALSE

# Duplicates are allowed
is_valid_tau(c(42, 42), length(DataCPSim))
#> [1] TRUE
is_valid_tau(826, length(DataCPSim))
#> [1] TRUE

# Anything greater than \eqn{n} (in this case 1096) is not allowed
is_valid_tau(1096, length(DataCPSim))
#> [1] FALSE
is_valid_tau(1097, length(DataCPSim))
#> [1] FALSE

# Always return a factor with half-open intervals on the right
regions_tau(c(42, 330), 1096)
#> [1] [1,42)     [42,330)   [330,1097)
#> Levels: [1,42) [330,1097) [42,330)
# Anything less than 2 is not allowed
validate_tau(0, length(DataCPSim))
#> numeric(0)
validate_tau(1, length(DataCPSim))
#> numeric(0)
validate_tau(826, length(DataCPSim))
#> [1] 826

# Duplicates are removed
validate_tau(c(826, 826), length(DataCPSim))
#> [1] 826

# Anything greater than \eqn{n} (in this case 1096) is not allowed
validate_tau(1096, length(DataCPSim))
#> numeric(0)
validate_tau(1097, length(DataCPSim))
#> numeric(0)

# Fix many problems
validate_tau(c(-4, 0, 1, 4, 5, 5, 824, 1096, 1097, 182384), length(DataCPSim))
#> [1]   4   5 824
```
