# Segment a time series using a genetic algorithm

Segmenting functions for various genetic algorithms

## Usage

``` r
segment_cptga(x, ...)
```

## Arguments

- x:

  A time series

- ...:

  arguments passed to
  [`changepointGA::cptga()`](https://rdrr.io/pkg/changepointGA/man/cptga.html)

## Value

A `tidycptga` object. This is just a
[`changepointGA::cptga()`](https://rdrr.io/pkg/changepointGA/man/cptga.html)
object with an additional slot for `data` (the original time series).

## Details

`segment_cptga()` uses the genetic algorithm in
[`changepointGA::cptga()`](https://rdrr.io/pkg/changepointGA/man/cptga.html)
to "evolve" a random set of candidate changepoint sets, using the
penalized objective function specified by `penalty_fn`. By default, the
normal `meanshift` model is fit (see
[`fit_meanshift_norm()`](https://beanumber.github.io/tidychangepoint/reference/fit_meanshift.md))
and the [BIC](https://rdrr.io/r/stats/AIC.html) penalty is applied.

## Examples

``` r
# \donttest{
# Segment a time series using a genetic algorithm
res <- segment_cptga(CET)
summary(res)
#>    Length     Class      Mode 
#>         1 tidycptga        S4 

# Segment a time series using changepointGA
x <- segment(CET, method = "cptga")
summary(x)
#> 
#> ── Summary of tidycpt object ───────────────────────────────────────────────────
#> → y: Contains 366 observations, ranging from 6.86  to 11.18  .
#> ℹ Segmenter (class tidycptga )
#> → A: Used the Genetic algorithm from the changepointGA  package.
#> → τ: Found 2 changepoint(s).
#> → f: Reported a fitness value of -370.22  using the BIC penalty.
#> ℹ Model
#> → M: Fit the arima  model.
#> → θ: Estimated 2 parameter(s), for each of 3 region(s).
changepoints(x)
#> [1]  42 330
# }
```
