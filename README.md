
# BayesianMDLGA

<!-- badges: start -->
[![R-CMD-check](https://github.com/c-taimal/BayesianMDLGA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/c-taimal/BayesianMDLGA/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Usage

``` r
remotes::install_github("c-taimal/BayesianMDLGA")
```

``` r
library(BayesianMDLGA)
```

``` r
str(DataCPSim)
```

    ##  num [1:1096] 35.5 29 35.6 33 29.5 ...

``` r
str(DataCPSimRebases)
```

    ##  int [1:360] 20 51 60 221 226 233 271 308 354 415 ...

``` r
str(param)
```

    ## List of 33
    ##  $ nombre_datos           : chr "DataCPSimRebases"
    ##  $ frecuencia_datos       : num 7
    ##  $ n_datos                : chr "TODOS"
    ##  $ diarios0_rebases1      : num 0
    ##  $ valor_de_rebase        : num 63.2
    ##  $ r                      : num 50
    ##  $ k                      : num 50
    ##  $ penalty                : chr "BMDL"
    ##  $ max_num_cp             : num 20
    ##  $ prob_inicial           : num 0.06
    ##  $ prob_volado            : num 0.5
    ##  $ probs_muta             : num [1:3] 0.3 0.4 0.3
    ##  $ mutaciones             : num [1:3] -1 0 1
    ##  $ dist_extremos          : num 10
    ##  $ prob_para_sin_cp       : num 0.5
    ##  $ cp_real                : chr "sin cp_real"
    ##  $ quita_ini0_fin1        : num 0
    ##  $ probs_rank0_MDL1       : num 0
    ##  $ nombre_carpeta_pdf     : chr "Figures"
    ##  $ nombre_carpeta_RData   : chr "Data"
    ##  $ cuantos_mejores_cp_graf: num 100
    ##  $ my_data                : NULL
    ##  $ minimo_numero_de_cp    : num 1
    ##  $ probs_nuevos_muta0N    : num [1:3] 0.8 0.1 0.1
    ##  $ rf_type                : chr "W"
    ##  $ initial_val_optim      : num [1:2] 0.1 0.5
    ##  $ mat_low_upp            : num [1:2, 1:2] 1e-04 1e-08 1e+01 1e+05
    ##  $ vec_dist_a_priori      : chr [1:2] "Gamma" "Gamma"
    ##  $ mat_phi                : num [1:2, 1:2] 1 3 2 1.2
    ##  $ ajuste_bloque          : logi TRUE
    ##  $ print_progress_bar     : logi TRUE
    ##  $ print_progress_plots   : logi TRUE
    ##  $ value_set_seed         : num 123

``` r
str(pm_25)
```

    ## Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame': 1096 obs. of  1 variable:
    ##  $ PM2.5: num  393 277 303 336 329 201 237 235 292 276 ...
    ##  - attr(*, "spec")=List of 3
    ##   ..$ cols   :List of 1
    ##   .. ..$ PM2.5: list()
    ##   .. .. ..- attr(*, "class")= chr [1:2] "collector_number" "collector"
    ##   ..$ default: list()
    ##   .. ..- attr(*, "class")= chr [1:2] "collector_guess" "collector"
    ##   ..$ delim  : chr ","
    ##   ..- attr(*, "class")= chr "col_spec"
    ##  - attr(*, "problems")=<externalptr>

``` r
plot(DataCPSim)
```

![](README_files/figure-gfm/sim-plot-1.png)<!-- -->

``` r
summary(pm_25)
```

    ##      PM2.5      
    ##  Min.   : 13.0  
    ##  1st Qu.:207.0  
    ##  Median :300.0  
    ##  Mean   :288.3  
    ##  3rd Qu.:377.0  
    ##  Max.   :785.0

## Citation

``` r
citation("BayesianMDLGA")
```

    ## To cite package 'BayesianMDLGA' in publications use:
    ## 
    ##   Taimal CA, Suárez-Sierra BM, Rivera JC (2023). "An Exploration of
    ##   Genetic Algorithms Operators for the Detection of Multiple
    ##   Change-Points of Exceedances Using Non-homogeneous Poisson Processes
    ##   and Bayesian Methods." In _Colombian Conference on Computing_,
    ##   230-258. Springer. doi:10.1007/978-3-031-47372-2_20
    ##   <https://doi.org/10.1007/978-3-031-47372-2_20>.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @InProceedings{,
    ##     title = {An Exploration of Genetic Algorithms Operators for the Detection of Multiple Change-Points of Exceedances Using Non-homogeneous Poisson Processes and Bayesian Methods},
    ##     author = {Carlos A Taimal and Biviana Marcela Suárez-Sierra and Juan Carlos Rivera},
    ##     booktitle = {Colombian Conference on Computing},
    ##     year = {2023},
    ##     pages = {230--258},
    ##     organization = {Springer},
    ##     doi = {10.1007/978-3-031-47372-2_20},
    ##   }
