# Convert, retrieve, or verify a model object

Convert, retrieve, or verify a model object

## Usage

``` r
as.model(object, ...)

# Default S3 method
as.model(object, ...)

# S3 method for class 'tidycpt'
as.model(object, ...)

is_model(x, ...)
```

## Arguments

- object:

  A
  [tidycpt](https://beanumber.github.io/tidychangepoint/reference/tidycpt-class.md)
  object, typically returned by
  [`segment()`](https://beanumber.github.io/tidychangepoint/reference/segment.md)

- ...:

  currently ignored

- x:

  An object, typically returned by `fit_*()`

## Value

- `as.model()` returns a
  [mod_cpt](https://beanumber.github.io/tidychangepoint/reference/mod_cpt.md)
  model object

&nbsp;

- `is_model()` a `logical` vector of length 1

## Details

[tidycpt](https://beanumber.github.io/tidychangepoint/reference/tidycpt-class.md)
objects have a `model` component. The functions documented here are
convenience utility functions for working with the `model` components.
`as.model()` is especially useful in pipelines to avoid having to use
the `$` or `[` notation for subsetting.

When applied to a
[tidycpt](https://beanumber.github.io/tidychangepoint/reference/tidycpt-class.md)
object, `as.model()` simply returns the `model` component of that
object. However, when applied to a `segmenter` object, `as.model()`
attempts to converts that object into a
[mod_cpt](https://beanumber.github.io/tidychangepoint/reference/mod_cpt.md)
model object.

`is_model()` checks to see if a model object implements all of the S3
methods necessary to be considered a model.

## See also

Other tidycpt-generics:
[`as.segmenter()`](https://beanumber.github.io/tidychangepoint/reference/as.segmenter.md),
[`changepoints()`](https://beanumber.github.io/tidychangepoint/reference/changepoints.md),
[`diagnose()`](https://beanumber.github.io/tidychangepoint/reference/diagnose.md),
[`fitness()`](https://beanumber.github.io/tidychangepoint/reference/fitness.md),
[`model_name()`](https://beanumber.github.io/tidychangepoint/reference/model_name.md)

## Examples

``` r
# Segment a time series using PELT
x <- segment(CET, method = "pelt")

# Retrieve the model component
x |> 
  as.model()
#> ℹ Model: A meanvar  model with 6 region(s).
#> → Each region has 2 parameter(s).
#> → The model has 0 global parameter(s).

# Explicitly convert the segmenter to a model
x |>
  as.segmenter() |>
  as.model()
#> ℹ Model: A meanvar  model with 6 region(s).
#> → Each region has 2 parameter(s).
#> → The model has 0 global parameter(s).

# Is that model valid? 
x |>
  as.model() |>
  is_model()
#> [1] TRUE
  

# Fit a model directly, without using [segment()]
x <- fit_nhpp(CET, tau = 330)
is_model(x)
#> [1] TRUE
```
