# Format the coefficients from a linear model as a tibble

Format the coefficients from a linear model as a tibble

## Usage

``` r
tbl_coef(mod, ...)
```

## Arguments

- mod:

  An `lm` model object

- ...:

  currently ignored

## Value

A
[tibble::tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
object containing the fitted coefficients.

## Examples

``` r
# Convert a time series into a data frame with indices
ds <- data.frame(y = as.ts(CET), t = 1:length(CET))

# Retrieve the coefficients from a null model
tbl_coef(lm(y ~ 1, data = ds))
#> # A tibble: 1 × 2
#>   region param_mu
#>   <chr>     <dbl>
#> 1 NA         9.29

# Retrieve the coefficients from a two changepoint model
tbl_coef(lm(y ~ (t >= 42) + (t >= 81), data = ds))
#> # A tibble: 3 × 2
#>   region  param_mu
#>   <chr>      <dbl>
#> 1 NA          8.68
#> 2 t >= 42     9.38
#> 3 t >= 81     9.37

# Retrieve the coefficients from a trendshift model
tbl_coef(lm(y ~ poly(t, 1, raw = TRUE) * (t >= 42) + poly(t, 1, raw = TRUE) * (t >= 81), data = ds))
#> # A tibble: 3 × 3
#>   region  param_mu param_beta1
#>   <chr>      <dbl>       <dbl>
#> 1 NA          9.25     -0.0273
#> 2 t >= 42     7.79      0.0535
#> 3 t >= 81     8.45     -0.0221

# Retrieve the coefficients from a quadratic model
tbl_coef(lm(y ~ poly(t, 2, raw = TRUE) * (t >= 42) + poly(t, 2, raw = TRUE) * (t >= 81), data = ds))
#> # A tibble: 3 × 4
#>   region  param_mu param_beta1 param_beta2
#>   <chr>      <dbl>       <dbl>       <dbl>
#> 1 NA          9.19     -0.0190   -0.000198
#> 2 t >= 42    11.0      -0.0646    0.00110 
#> 3 t >= 81     9.72      0.0745   -0.000870
```
