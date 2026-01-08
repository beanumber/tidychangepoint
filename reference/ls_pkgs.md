# Algorithmic coverage through tidychangepoint

Algorithmic coverage through tidychangepoint

## Usage

``` r
ls_models()

ls_pkgs()

ls_methods()

ls_penalties()

ls_cpt_penalties()

ls_coverage()
```

## Value

A [tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)
or `character`

## See also

[`segment()`](https://beanumber.github.io/tidychangepoint/reference/segment.md)

## Examples

``` r
# List all model-fitting functions
ls_models()
#>  [1] "fit_arima"              "fit_lmshift"            "fit_lmshift_ar1"       
#>  [4] "fit_meanshift_lnorm"    "fit_meanshift_norm"     "fit_meanshift_norm_ar1"
#>  [7] "fit_meanvar"            "fit_nhpp"               "fit_trendshift"        
#> [10] "fit_trendshift_ar1"    

# List packages supported by tidychangepoint
ls_pkgs()
#> # A tibble: 4 × 2
#>   pkg             version
#>   <chr>           <chr>  
#> 1 tidychangepoint 1.0.2  
#> 2 changepoint     2.3    
#> 3 wbs             1.4.1  
#> 4 GA              3.2.4  

# List methods supported by segment()
ls_methods()
#> # A tibble: 14 × 5
#>    method      pkg             segmenter_class helper              wraps        
#>    <chr>       <chr>           <chr>           <chr>               <chr>        
#>  1 pelt        changepoint     cpt             segment_pelt()      changepoint:…
#>  2 binseg      changepoint     cpt             NA                  changepoint:…
#>  3 segneigh    changepoint     cpt             NA                  changepoint:…
#>  4 single-best changepoint     cpt             NA                  changepoint:…
#>  5 wbs         wbs             wbs             NA                  wbs::wbs()   
#>  6 ga          GA              tidyga          segment_ga()        GA::ga()     
#>  7 ga-shi      GA              tidyga          segment_ga_shi()    segment_ga() 
#>  8 ga-coen     GA              tidyga          segment_ga_coen()   segment_ga() 
#>  9 coen        tidychangepoint seg_basket      segment_coen()      NA           
#> 10 random      GA              tidyga          segment_ga_random() segment_ga() 
#> 11 manual      tidychangepoint seg_cpt         segment_manual()    NA           
#> 12 null        tidychangepoint seg_cpt         segment_manual()    NA           
#> 13 segmented   segmented       segmented       NA                  segmented::s…
#> 14 cptga       changepointGA   tidycptga       segment_cptga()     changepointG…

# List penalty functions provided by tidychangepoint
ls_penalties()
#> [1] "SIC"  "AIC"  "BIC"  "HQC"  "MBIC" "MDL"  "BMDL"

# List penalty functions supported by changepoint
ls_cpt_penalties()
#> [1] "None"       "SIC"        "BIC"        "MBIC"       "AIC"       
#> [6] "HQC"        "Asymptotic" "Manual"     "CROPS"     

# List combinations of method, model, and penalty supported by tidychangepoint
ls_coverage()
#>          method                  model    penalty
#> 1          pelt     fit_meanshift_norm       None
#> 2          pelt            fit_meanvar       None
#> 3          pelt     fit_meanshift_norm        SIC
#> 4          pelt            fit_meanvar        SIC
#> 5          pelt     fit_meanshift_norm        BIC
#> 6          pelt            fit_meanvar        BIC
#> 7          pelt     fit_meanshift_norm       MBIC
#> 8          pelt            fit_meanvar       MBIC
#> 9          pelt     fit_meanshift_norm        AIC
#> 10         pelt            fit_meanvar        AIC
#> 11         pelt     fit_meanshift_norm        HQC
#> 12         pelt            fit_meanvar        HQC
#> 13         pelt     fit_meanshift_norm Asymptotic
#> 14         pelt            fit_meanvar Asymptotic
#> 15         pelt     fit_meanshift_norm     Manual
#> 16         pelt            fit_meanvar     Manual
#> 17         pelt     fit_meanshift_norm      CROPS
#> 18         pelt            fit_meanvar      CROPS
#> 19       binseg            fit_meanvar       None
#> 20     segneigh            fit_meanvar       None
#> 21  single-best            fit_meanvar       None
#> 22       binseg            fit_meanvar        SIC
#> 23     segneigh            fit_meanvar        SIC
#> 24  single-best            fit_meanvar        SIC
#> 25       binseg            fit_meanvar        BIC
#> 26     segneigh            fit_meanvar        BIC
#> 27  single-best            fit_meanvar        BIC
#> 28       binseg            fit_meanvar       MBIC
#> 29     segneigh            fit_meanvar       MBIC
#> 30  single-best            fit_meanvar       MBIC
#> 31       binseg            fit_meanvar        AIC
#> 32     segneigh            fit_meanvar        AIC
#> 33  single-best            fit_meanvar        AIC
#> 34       binseg            fit_meanvar        HQC
#> 35     segneigh            fit_meanvar        HQC
#> 36  single-best            fit_meanvar        HQC
#> 37       binseg            fit_meanvar Asymptotic
#> 38     segneigh            fit_meanvar Asymptotic
#> 39  single-best            fit_meanvar Asymptotic
#> 40       binseg            fit_meanvar     Manual
#> 41     segneigh            fit_meanvar     Manual
#> 42  single-best            fit_meanvar     Manual
#> 43       binseg            fit_meanvar      CROPS
#> 44     segneigh            fit_meanvar      CROPS
#> 45  single-best            fit_meanvar      CROPS
#> 46           ga              fit_arima        SIC
#> 47       random              fit_arima        SIC
#> 48           ga            fit_lmshift        SIC
#> 49       random            fit_lmshift        SIC
#> 50           ga        fit_lmshift_ar1        SIC
#> 51       random        fit_lmshift_ar1        SIC
#> 52           ga    fit_meanshift_lnorm        SIC
#> 53       random    fit_meanshift_lnorm        SIC
#> 54           ga     fit_meanshift_norm        SIC
#> 55       random     fit_meanshift_norm        SIC
#> 56           ga fit_meanshift_norm_ar1        SIC
#> 57       random fit_meanshift_norm_ar1        SIC
#> 58           ga            fit_meanvar        SIC
#> 59       random            fit_meanvar        SIC
#> 60           ga               fit_nhpp        SIC
#> 61       random               fit_nhpp        SIC
#> 62           ga         fit_trendshift        SIC
#> 63       random         fit_trendshift        SIC
#> 64           ga     fit_trendshift_ar1        SIC
#> 65       random     fit_trendshift_ar1        SIC
#> 66           ga              fit_arima        AIC
#> 67       random              fit_arima        AIC
#> 68           ga            fit_lmshift        AIC
#> 69       random            fit_lmshift        AIC
#> 70           ga        fit_lmshift_ar1        AIC
#> 71       random        fit_lmshift_ar1        AIC
#> 72           ga    fit_meanshift_lnorm        AIC
#> 73       random    fit_meanshift_lnorm        AIC
#> 74           ga     fit_meanshift_norm        AIC
#> 75       random     fit_meanshift_norm        AIC
#> 76           ga fit_meanshift_norm_ar1        AIC
#> 77       random fit_meanshift_norm_ar1        AIC
#> 78           ga            fit_meanvar        AIC
#> 79       random            fit_meanvar        AIC
#> 80           ga               fit_nhpp        AIC
#> 81       random               fit_nhpp        AIC
#> 82           ga         fit_trendshift        AIC
#> 83       random         fit_trendshift        AIC
#> 84           ga     fit_trendshift_ar1        AIC
#> 85       random     fit_trendshift_ar1        AIC
#> 86           ga              fit_arima        BIC
#> 87       random              fit_arima        BIC
#> 88           ga            fit_lmshift        BIC
#> 89       random            fit_lmshift        BIC
#> 90           ga        fit_lmshift_ar1        BIC
#> 91       random        fit_lmshift_ar1        BIC
#> 92           ga    fit_meanshift_lnorm        BIC
#> 93       random    fit_meanshift_lnorm        BIC
#> 94           ga     fit_meanshift_norm        BIC
#> 95       random     fit_meanshift_norm        BIC
#> 96           ga fit_meanshift_norm_ar1        BIC
#> 97       random fit_meanshift_norm_ar1        BIC
#> 98           ga            fit_meanvar        BIC
#> 99       random            fit_meanvar        BIC
#> 100          ga               fit_nhpp        BIC
#> 101      random               fit_nhpp        BIC
#> 102          ga         fit_trendshift        BIC
#> 103      random         fit_trendshift        BIC
#> 104          ga     fit_trendshift_ar1        BIC
#> 105      random     fit_trendshift_ar1        BIC
#> 106          ga              fit_arima        HQC
#> 107      random              fit_arima        HQC
#> 108          ga            fit_lmshift        HQC
#> 109      random            fit_lmshift        HQC
#> 110          ga        fit_lmshift_ar1        HQC
#> 111      random        fit_lmshift_ar1        HQC
#> 112          ga    fit_meanshift_lnorm        HQC
#> 113      random    fit_meanshift_lnorm        HQC
#> 114          ga     fit_meanshift_norm        HQC
#> 115      random     fit_meanshift_norm        HQC
#> 116          ga fit_meanshift_norm_ar1        HQC
#> 117      random fit_meanshift_norm_ar1        HQC
#> 118          ga            fit_meanvar        HQC
#> 119      random            fit_meanvar        HQC
#> 120          ga               fit_nhpp        HQC
#> 121      random               fit_nhpp        HQC
#> 122          ga         fit_trendshift        HQC
#> 123      random         fit_trendshift        HQC
#> 124          ga     fit_trendshift_ar1        HQC
#> 125      random     fit_trendshift_ar1        HQC
#> 126          ga              fit_arima       MBIC
#> 127      random              fit_arima       MBIC
#> 128          ga            fit_lmshift       MBIC
#> 129      random            fit_lmshift       MBIC
#> 130          ga        fit_lmshift_ar1       MBIC
#> 131      random        fit_lmshift_ar1       MBIC
#> 132          ga    fit_meanshift_lnorm       MBIC
#> 133      random    fit_meanshift_lnorm       MBIC
#> 134          ga     fit_meanshift_norm       MBIC
#> 135      random     fit_meanshift_norm       MBIC
#> 136          ga fit_meanshift_norm_ar1       MBIC
#> 137      random fit_meanshift_norm_ar1       MBIC
#> 138          ga            fit_meanvar       MBIC
#> 139      random            fit_meanvar       MBIC
#> 140          ga               fit_nhpp       MBIC
#> 141      random               fit_nhpp       MBIC
#> 142          ga         fit_trendshift       MBIC
#> 143      random         fit_trendshift       MBIC
#> 144          ga     fit_trendshift_ar1       MBIC
#> 145      random     fit_trendshift_ar1       MBIC
#> 146          ga              fit_arima        MDL
#> 147      random              fit_arima        MDL
#> 148          ga            fit_lmshift        MDL
#> 149      random            fit_lmshift        MDL
#> 150          ga        fit_lmshift_ar1        MDL
#> 151      random        fit_lmshift_ar1        MDL
#> 152          ga    fit_meanshift_lnorm        MDL
#> 153      random    fit_meanshift_lnorm        MDL
#> 154          ga     fit_meanshift_norm        MDL
#> 155      random     fit_meanshift_norm        MDL
#> 156          ga fit_meanshift_norm_ar1        MDL
#> 157      random fit_meanshift_norm_ar1        MDL
#> 158          ga            fit_meanvar        MDL
#> 159      random            fit_meanvar        MDL
#> 160          ga               fit_nhpp        MDL
#> 161      random               fit_nhpp        MDL
#> 162          ga         fit_trendshift        MDL
#> 163      random         fit_trendshift        MDL
#> 164          ga     fit_trendshift_ar1        MDL
#> 165      random     fit_trendshift_ar1        MDL
#> 166         wbs                   <NA>       <NA>
#> 167       cptga                   <NA>       <NA>
#> 168   segmented                   <NA>       <NA>
#> 169      ga-shi fit_meanshift_norm_ar1        BIC
#> 170     ga-coen               fit_nhpp       BMDL
#> 171        coen               fit_nhpp       BMDL
#> 172      manual     fit_meanshift_norm        BIC
#> 173        null     fit_meanshift_norm        BIC
```
