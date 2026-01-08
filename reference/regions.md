# Extract the regions from a [tidycpt](https://beanumber.github.io/tidychangepoint/reference/tidycpt-class.md) object

Extract the regions from a
[tidycpt](https://beanumber.github.io/tidychangepoint/reference/tidycpt-class.md)
object

## Usage

``` r
regions(x, ...)

# S3 method for class 'mod_cpt'
regions(x, ...)

# S3 method for class 'tidycpt'
regions(x, ...)
```

## Arguments

- x:

  An object that has regions

- ...:

  Currently ignored

## Value

A [`base::factor()`](https://rdrr.io/r/base/factor.html) of intervals
indicating the region

## Examples

``` r
cpt <- fit_meanshift_norm(CET, tau = 330)
regions(cpt)
#> [1] [1,330)   [330,367)
#> Levels: [1,330) [330,367)
```
