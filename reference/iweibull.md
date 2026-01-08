# Weibull distribution functions

Weibull distribution functions

## Usage

``` r
iweibull(x, shape, scale = 1)

mweibull(x, shape, scale = 1)

parameters_weibull(...)
```

## Arguments

- x:

  A numeric vector

- shape:

  Shape parameter for Weibull distribution. See
  [`stats::dweibull()`](https://rdrr.io/r/stats/Weibull.html).

- scale:

  Scale parameter for Weibull distribution. See
  [`stats::dweibull()`](https://rdrr.io/r/stats/Weibull.html).

- ...:

  currently ignored

## Value

A numeric vector

## Details

Intensity function for the Weibull distribution. \$\$ iweibull(x) =
\left( \frac{shape}{scale} \right) \cdot \left( \frac{x}{scale}
\right)^{shape - 1} \$\$

Mean intensity function for the Weibull distribution. \$\$ mweibull(x) =
\left( \frac{x}{scale} \right)^{shape} \$\$

`parameters_weibull()` returns a
[`list()`](https://rdrr.io/r/base/list.html) with two components:
`shape` and `scale`, each of which is a
[`list()`](https://rdrr.io/r/base/list.html) of distribution parameters.
These parameters are used to define the prior distributions for the
hyperparameters.

## See also

[`stats::dweibull()`](https://rdrr.io/r/stats/Weibull.html)

[`stats::dgamma()`](https://rdrr.io/r/stats/GammaDist.html)

## Examples

``` r
# Compute the intensities and plot them
iweibull(1, shape = 1, scale = 1)
#> [1] 1
plot(x = 1:10, y = iweibull(1:10, shape = 2, scale = 2))


# Compute various values of the distribution
mweibull(1, shape = 1, scale = 1)
#> [1] 1
plot(x = 1:10, y = mweibull(1:10, shape = 1, scale = 1))

plot(x = 1:10, y = mweibull(1:10, shape = 1, scale = 2))

plot(x = 1:10, y = mweibull(1:10, shape = 0.5, scale = 2))

plot(x = 1:10, y = mweibull(1:10, shape = 0.5, scale = 100))

plot(x = 1:10, y = mweibull(1:10, shape = 2, scale = 2))

plot(x = 1:10, y = mweibull(1:10, shape = 2, scale = 100))


# Generate prior distribution hyperparameters
parameters_weibull()
#> $shape
#> $shape$dist
#> [1] "gamma"
#> 
#> $shape$shape
#> [1] 1
#> 
#> $shape$rate
#> [1] 2
#> 
#> $shape$initial_value
#> [1] 0.1
#> 
#> $shape$lower_bound
#> [1] 1e-04
#> 
#> $shape$upper_bound
#> [1] 10
#> 
#> 
#> $scale
#> $scale$dist
#> [1] "gamma"
#> 
#> $scale$shape
#> [1] 3
#> 
#> $scale$rate
#> [1] 1.2
#> 
#> $scale$initial_value
#> [1] 0.5
#> 
#> $scale$lower_bound
#> [1] 1e-08
#> 
#> $scale$upper_bound
#> [1] 1e+05
#> 
#> 
```
