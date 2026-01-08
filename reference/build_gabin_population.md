# Initialize populations in genetic algorithms

Build an initial population set for genetic algorithms

## Usage

``` r
build_gabin_population(x, ...)

log_gabin_population(x, ...)
```

## Arguments

- x:

  a numeric vector coercible into a
  [stats::ts](https://rdrr.io/r/stats/ts.html) object

- ...:

  arguments passed to methods

## Value

A `function` that can be passed to the `population` argument of
[`GA::ga()`](https://github.com/luca-scr/GA/reference/ga.html) (through
[`segment_ga()`](https://beanumber.github.io/tidychangepoint/reference/segment_ga.md))

## Details

Genetic algorithms require a method for randomly generating initial
populations (i.e., a first generation). The default method used by
[`GA::ga()`](https://github.com/luca-scr/GA/reference/ga.html) for
changepoint detection is usually
[`GA::gabin_Population()`](https://github.com/luca-scr/GA/reference/ga_Population.html),
which selects candidate changepoints uniformly at random with
probability 0.5. This leads to an initial population with excessively
large candidate changepoint sets (on the order of \\n/2\\), which makes
the genetic algorithm slow.

- `build_gabin_population()` takes a `ts` object and runs several fast
  changepoint detection algorithms on it, then sets the initial
  probability to 3 times the average value of the size of the
  changepoint sets returned by those algorithms. This is a conservative
  guess as to the likely size of the optimal changepoint set.

- `log_gabin_population()` takes a `ts` object and sets the initial
  probability to the natural logarithm of the length of the time series.

## See also

[`GA::gabin_Population()`](https://github.com/luca-scr/GA/reference/ga_Population.html),
[`segment_ga()`](https://beanumber.github.io/tidychangepoint/reference/segment_ga.md)

## Examples

``` r
# Build a function to generate the population
f <- build_gabin_population(CET)

# Segment the time series using the population generation function
segment(CET, method = "ga", population = f, maxiter = 5)
#> Seeding initial population with probability: 0.0273224043715847
#> ℹ A tidycpt object. Segmenter ↓
#> An object of class "ga"
#> 
#> Call:
#> GA::ga(type = "binary", fitness = obj_fun, nBits = n, population = ..1,     maxiter = 5)
#> 
#> Available slots:
#>  [1] "data"          "model_fn_args" "call"          "type"         
#>  [5] "lower"         "upper"         "nBits"         "names"        
#>  [9] "popSize"       "iter"          "run"           "maxiter"      
#> [13] "suggestions"   "population"    "elitism"       "pcrossover"   
#> [17] "pmutation"     "optim"         "fitness"       "summary"      
#> [21] "bestSol"       "fitnessValue"  "solution"     
#> ℹ Model: A meanshift_norm  model with 5 region(s).
#> → Each region has 1 parameter(s).
#> → The model has 1 global parameter(s).
f <- log_gabin_population(CET)
segment(CET, method = "ga", population = f, maxiter = 10)
#> Seeding initial population with probability: 0.0161274134792387
#> ℹ A tidycpt object. Segmenter ↓
#> An object of class "ga"
#> 
#> Call:
#> GA::ga(type = "binary", fitness = obj_fun, nBits = n, population = ..1,     maxiter = 10)
#> 
#> Available slots:
#>  [1] "data"          "model_fn_args" "call"          "type"         
#>  [5] "lower"         "upper"         "nBits"         "names"        
#>  [9] "popSize"       "iter"          "run"           "maxiter"      
#> [13] "suggestions"   "population"    "elitism"       "pcrossover"   
#> [17] "pmutation"     "optim"         "fitness"       "summary"      
#> [21] "bestSol"       "fitnessValue"  "solution"     
#> ℹ Model: A meanshift_norm  model with 5 region(s).
#> → Each region has 1 parameter(s).
#> → The model has 1 global parameter(s).
```
