# Base class for changepoint models

Create changepoint detection model objects

## Usage

``` r
new_mod_cpt(
  x = numeric(),
  tau = integer(),
  region_params = tibble::tibble(),
  model_params = double(),
  fitted_values = double(),
  model_name = character(),
  ...
)

validate_mod_cpt(x)

mod_cpt(x, ...)
```

## Arguments

- x:

  a numeric vector coercible into a `ts` object

- tau:

  indices of the changepoint set

- region_params:

  A
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  with one row for each region defined by the changepoint set `tau`.
  Each variable represents a parameter estimated in that region.

- model_params:

  A numeric vector of parameters estimated by the model across the
  entire data set (not just in each region).

- fitted_values:

  Fitted values returned by the model on the original data set.

- model_name:

  A `character` vector giving the model's name.

- ...:

  currently ignored

## Value

A mod_cpt object

## Details

Changepoint detection models know how they were created, on what data
set, about the optimal changepoint set found, and the parameters that
were fit to the model. Methods for various generic reporting functions
are provided.

All changepoint detection models inherit from mod_cpt: the base class
for changepoint detection models. These models are created by one of the
`fit_*()` functions, or by
[`as.model()`](https://beanumber.github.io/tidychangepoint/reference/as.model.md).

## See also

[`as.model()`](https://beanumber.github.io/tidychangepoint/reference/as.model.md)

## Examples

``` r
cpt <- mod_cpt(CET)
str(cpt)
#> List of 6
#>  $ data         : Time-Series [1:366] from 1 to 366: 8.87 9.1 9.78 9.52 8.63 9.34 8.29 9.86 8.52 9.51 ...
#>  $ tau          : int(0) 
#>  $ region_params: tibble [0 × 0] (S3: tbl_df/tbl/data.frame)
#>  Named list()
#>  $ model_params : num(0) 
#>  $ fitted_values: num(0) 
#>  $ model_name   : chr(0) 
#>  - attr(*, "class")= chr "mod_cpt"
as.ts(cpt)
#> Time Series:
#> Start = 1 
#> End = 366 
#> Frequency = 1 
#>   [1]  8.87  9.10  9.78  9.52  8.63  9.34  8.29  9.86  8.52  9.51  9.02  8.96
#>  [13]  9.08  8.82  8.38  8.12  7.88  8.84  8.78  8.45  8.76  8.89  8.75  9.05
#>  [25]  8.49  7.95  9.16 10.15  8.99  7.86  8.56  8.94  8.17  7.73  8.47  7.67
#>  [37]  7.29  8.52  8.05  7.67  8.83  8.62  8.75  9.31  9.09  9.07  8.75  9.82
#>  [49]  9.41  9.68  8.74  9.49  9.42  9.14  8.64  9.44  9.44  8.38  9.04  9.29
#>  [61]  9.49  9.10  8.91  9.37  9.80  9.28  8.69  9.36  9.97  9.54  9.29 10.07
#>  [73]  9.90  9.69 10.50  9.82  9.57 10.33  9.95  9.84  9.21  6.86  9.32  8.38
#>  [85]  9.83  8.80  8.85  8.66  9.84  8.79  9.49  9.71  8.47  9.21  9.12  8.87
#>  [97]  8.58  8.78  8.97  8.98 10.03  9.85 10.02  9.61  8.95  8.73  8.55  8.66
#> [109]  8.70  8.95  8.81  8.53  8.58  9.17  9.29  9.08 10.10  9.01  9.11  9.20
#> [121] 10.39  9.10 10.20  8.01  9.30  7.82  8.56  8.25  9.29  9.19  8.90  9.44
#> [133]  9.27  9.17  9.11  9.86  8.69  8.99  9.01  9.60  7.88  9.28  9.63  8.98
#> [145]  9.09  9.59  9.00  9.84  8.69  8.87  8.97  8.78  9.70  8.21  8.73  7.78
#> [157]  9.08  7.89  8.89  9.89  9.27  8.57  9.56 10.07  8.41  9.32  9.77 10.09
#> [169]  9.52 10.32  8.18  8.73 10.13  9.50  9.52 10.51  9.57  8.88  8.86  8.10
#> [181]  8.71  8.52  8.75  9.26  9.11  8.61  8.30 10.16  9.27  9.43  9.33  9.10
#> [193]  9.18  9.82  8.42  9.34  8.09  9.08 10.11  9.16  9.64  7.89  9.15  9.21
#> [205]  9.69  8.86  9.72  9.68  9.03 10.40  9.61  9.02  9.07  9.76  9.03  9.33
#> [217]  9.48  9.53  9.19  9.26  7.44  9.09  8.58  9.48  9.04  9.85  8.58  8.74
#> [229]  8.30  8.24  9.02  8.77  8.51  8.18 10.01  9.32  8.70  9.34  9.44 10.10
#> [241]  9.71  9.60  9.15  8.88  9.33  9.02  9.16  9.47  8.88  9.28  8.59  9.19
#> [253] 10.09  9.37  9.83  9.90  8.96  9.21  8.55  9.53  8.53  9.58 10.51  8.70
#> [265]  9.11  9.29  9.20  9.74  9.24  9.59  9.06  9.47  9.02  9.41  9.86 10.03
#> [277]  9.74  9.35  9.59 10.21  9.70  9.06  9.12  9.11 10.05  9.59 10.29  9.46
#> [289]  9.65 10.03 10.64  9.43  9.30  9.12  9.87  9.26  9.33  8.87 10.06  9.45
#> [301] 10.52  9.75  9.95  8.61  8.52  9.48  8.99  9.47  9.64  9.33  9.32  9.60
#> [313]  9.72  9.22  9.57  9.55  9.94 10.00  9.40  9.35  8.74  9.35  9.19  9.78
#> [325] 10.06  9.72  8.86  8.75  9.03  9.74 10.46 10.59  9.50  9.81  9.48 10.23
#> [337] 10.49  9.20 10.51 10.30 10.61 10.25  9.96 10.59 10.52 10.52 10.52 10.95
#> [349] 10.59 10.06 10.23  8.95 10.80  9.81  9.69 11.04 10.40 10.41 10.67 10.77
#> [361] 10.42 10.84 10.35 11.18 11.13 10.96
changepoints(cpt)
#> integer(0)
```
