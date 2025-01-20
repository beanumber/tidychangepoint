# tidychangepoint 1.0.0

* `mlb_diffs` is now a `tsibble` with various statistics.
* Added `ls_*()` functions to list algorithms, models, and penalty functions 
usable in the package. 
* Added support for SIC (=BIC) and HQC penalty functions.
* Updated CET to include 2021-2024.
* Added `regions()` generic function
* Padding is always 1 and $n+1$, and intervals are always closed on the left and open on the right.
* `cut_inclusive()` is now `cut_by_tau()`. 
* Added `italy_grads` data set. 

# tidychangepoint 0.0.1

* Initial CRAN submission.
