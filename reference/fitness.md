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

# S3 method for class 'segmented'
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
#> 16441.07 

# \donttest{
# Segment a times series using a genetic algorithm
x <- segment(DataCPSim, method = "cptga")

# Retrieve its fitness value
fitness(x)
#>      BIC 
#> 6989.921 
# }
# Segment a time series using Segmented
x <- segment(DataCPSim, method = "segmented")

# Retrieve its fitness
fitness(x)
#>      MDL 
#> 10190.17 

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
