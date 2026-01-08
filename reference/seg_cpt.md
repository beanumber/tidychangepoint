# Base class for segmenters

Base class for segmenters

## Usage

``` r
new_seg_cpt(
  x = numeric(),
  pkg = character(),
  base_class = character(),
  algorithm = NA,
  changepoints = integer(),
  fitness = double(),
  seg_params = list(),
  model_name = "meanshift_norm",
  penalty = "BIC",
  ...
)

seg_cpt(x, ...)
```

## Arguments

- x:

  a numeric vector coercible into a
  [`stats::ts()`](https://rdrr.io/r/stats/ts.html) object

- pkg:

  name of the package providing the segmenter

- base_class:

  class of the underlying object

- algorithm:

  Algorithm used to find the changepoints

- changepoints:

  a possibly empty [`list()`](https://rdrr.io/r/base/list.html) of
  candidate changepoints

- fitness:

  A named `double` vector whose name reflects the penalty applied

- seg_params:

  a possibly empty [`list()`](https://rdrr.io/r/base/list.html) of
  segmenter parameters

- model_name:

  character indicating the model used to find the changepoints.

- penalty:

  character indicating the name of the penalty function used to find the
  changepoints.

- ...:

  currently ignored

## Value

A seg_cpt object.
