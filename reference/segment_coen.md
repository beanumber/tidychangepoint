# Algoritmo genético de Bayesian MDL a un paso

**\[deprecated\]** This implementation is deprecated. Please see
[`segment_ga_coen()`](https://beanumber.github.io/tidychangepoint/reference/segment_ga.md)

## Usage

``` r
segment_coen(
  x,
  num_generations = 50,
  nhpp_dist = c("W", "EW", "GGO", "MO", "GO")[1],
  vec_dist_a_priori = c("Gamma", "Gamma"),
  mat_phi = matrix(c(1, 3, 2, 1.2), ncol = 2),
  generation_size = 50,
  max_num_cp = 20,
  show_progress_bar = TRUE,
  ...
)
```

## Arguments

- x:

  an object coercible into a time series object via
  [`stats::as.ts()`](https://rdrr.io/r/stats/ts.html)

- num_generations:

  Number of generations to evolve

- nhpp_dist:

  toma valores en c("W","EW","GGO","MO","GO") y es el nombre de la
  función de tasa del NHPP

- vec_dist_a_priori:

  vector de los nobmres de las distribuciones a priori que se utilizan;
  eg c("Gamma","Gamma") y c("Gamma","Gamma","Gamma")

- mat_phi:

  matriz cuyos renglones tiene los parámetros de las distribuciones a
  priori; cada renglón tiene todos los parametros de una distribución

- generation_size:

  tamaño de las generaciones

- max_num_cp:

  el máximo número de rebases. Este parámetro se ocupa en particular
  para que todos los cromosomas quepan en una matriz.

- show_progress_bar:

  show the progress bar?

- ...:

  arguments passed to methods

## Value

A `cpt_gbmdl` object

## Examples

``` r
# \donttest{
x <- segment_coen(DataCPSim, num_generations = 5)
#>   |                                                                    |                                                            |   0%  |                                                                    |===============                                             |  25%  |                                                                    |==============================                              |  50%  |                                                                    |=============================================               |  75%  |                                                                    |============================================================| 100%
# }
```
