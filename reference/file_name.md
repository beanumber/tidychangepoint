# Obtain a descriptive filename for a tidycpt object

Obtain a descriptive filename for a tidycpt object

## Usage

``` r
file_name(x, data_name_slug = "data")
```

## Arguments

- x:

  A
  [tidycpt](https://beanumber.github.io/tidychangepoint/reference/tidycpt-class.md)
  object

- data_name_slug:

  character string that will identify the data set used in the file name

## Value

A `character` string giving a unique file name.

## Details

`file_name()` generates a random, unique string indicating the algorithm
and
[`fitness()`](https://beanumber.github.io/tidychangepoint/reference/fitness.md)
for a
[tidycpt](https://beanumber.github.io/tidychangepoint/reference/tidycpt-class.md)
object.

## Examples

``` r
# Generate a unique name for the file
DataCPSim |>
  segment(method = "pelt") |>
  file_name()
#> [1] "data_PELT_9403_b42849422cbfd7c23a6ffe77d48389f8.rda"
```
