# Plot GA information

Plot GA information

## Usage

``` r
# S3 method for class 'tidyga'
plot(x, ...)
```

## Arguments

- x:

  A `tidyga` object

- ...:

  currently ignored

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
# \donttest{
x <- segment(DataCPSim, method = "ga-coen", maxiter = 5)
#> Seeding initial population with probability: 0.0264598540145985
plot(x$segmenter)

# }
```
