# Retrieve the optimal fitness (or objective function) value used by an algorithm

Retrieve the optimal fitness (or objective function) value used by an
algorithm

## Usage

``` r
fitness(object, ...)

# S3 method for class 'seg_basket'
fitness(object, ...)

# S3 method for class 'seg_cpt'
fitness(object, ...)

# S3 method for class 'tidycpt'
fitness(object, ...)

# S3 method for class 'ga'
fitness(object, ...)

# S3 method for class 'cpt'
fitness(object, ...)

# S3 method for class 'cptga'
fitness(object, ...)

# S3 method for class 'lm'
fitness(object, ...)

# S3 method for class 'breakpointsfull'
fitness(object, ...)

# S3 method for class 'wbs'
fitness(object, ...)
```

## Arguments

- object:

  A `segmenter` object.

- ...:

  currently ignored

## Value

A named `double` vector with the fitness value.

## Details

Segmenting algorithms use a **fitness** metric, typically through the
use of a penalized objective function, to determine which changepoint
sets are more or less optimal. This function returns the value of that
metric for the changepoint set implied by the object provided.

## See also

Other tidycpt-generics:
[`as.model()`](https://beanumber.github.io/tidychangepoint/reference/as.model.md),
[`as.segmenter()`](https://beanumber.github.io/tidychangepoint/reference/as.segmenter.md),
[`changepoints()`](https://beanumber.github.io/tidychangepoint/reference/changepoints.md),
[`diagnose()`](https://beanumber.github.io/tidychangepoint/reference/diagnose.md),
[`model_name()`](https://beanumber.github.io/tidychangepoint/reference/model_name.md)

Other segmenter-functions:
[`as.segmenter()`](https://beanumber.github.io/tidychangepoint/reference/as.segmenter.md),
[`model_args()`](https://beanumber.github.io/tidychangepoint/reference/model_args.md),
[`seg_params()`](https://beanumber.github.io/tidychangepoint/reference/seg_params.md)

## Examples

``` r
# Segment a times series using a genetic algorithm
x <- segment(DataCPSim, method = "ga", maxiter = 10)

# Retrieve its fitness value
fitness(x)
#>      BIC 
#> 16257.74 

# \donttest{
# Segment a times series using a genetic algorithm
x <- segment(DataCPSim, method = "cptga")

# Retrieve its fitness value
fitness(x)
#>      BIC 
#> 6986.086 
# }
# Segment a time series using Segmented
x <- segment(DataCPSim, method = "selgmented")
#> No. of breakpoints: 2 .. 3 .. 4 .. 5 .. 6 .. 7 .. 8 .. 9 .. 10 .. 
#> 
#> BIC to detect no. of breakpoints:
#>        0        1        2        3        4        5        6        6 
#> 10727.65 10225.17 10207.73 10221.65 10214.40 10225.80 10231.34 10204.84 
#>        7        8        9 
#> 10219.03 10231.46 10245.46 
#> 
#> No. of selected breakpoints: 4  (2 breakpoint(s) removed due to small slope diff)

# Retrieve its fitness
fitness(x)
#>      BIC 
#> 10226.13 

# \donttest{
# Segment a time series using Segmented
x <- segment(DataCPSim, method = "strucchange")

# Retrieve its fitness
fitness(x)
#>      RSS 
#> 728167.9 
# }
# Segment a time series using Wild Binary Segmentation
x <- segment(DataCPSim, method = "wbs")

# Retrieve its fitness
fitness(x)
#>     MBIC 
#> 4189.719 
```
