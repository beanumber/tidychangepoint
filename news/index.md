# Changelog

## tidychangepoint 1.0.3

- Added support for models from the `strucchange` package.
- Added group aesthetic to plots (@rpruim)
- The dots (…) are passed more consistently

## tidychangepoint 1.0.2

CRAN release: 2025-10-16

- Updated use of `changepointGA::GA()` to
  [`changepointGA::cptga()`](https://rdrr.io/pkg/changepointGA/man/cptga.html)

## tidychangepoint 1.0.1

CRAN release: 2025-07-09

- Added preliminary support for ARIMA models.
- Added support for models from the `changepointGA` package.
- Added support for models from the `segmented` package.
- [`fitness.wbs()`](https://beanumber.github.io/tidychangepoint/reference/fitness.md)
  returns the MBIC penalty value.
- Added [`summary()`](https://rdrr.io/r/base/summary.html) methods for
  `tidycpt`, `seg_cpt`, and `mod_cpt` objects.
- Improved output from [`print()`](https://rdrr.io/r/base/print.html)
  methods.

## tidychangepoint 1.0.0

CRAN release: 2025-01-31

- `mlb_diffs` is now a `tsibble` with various statistics.
- Added `ls_*()` functions to list algorithms, models, and penalty
  functions usable in the package.
- Added support for SIC (=BIC) and HQC penalty functions.
- Updated CET to include 2021-2024.
- Added
  [`regions()`](https://beanumber.github.io/tidychangepoint/reference/regions.md)
  generic function
- Padding is always 1 and $n + 1$, and intervals are always closed on
  the left and open on the right.
- `cut_inclusive()` is now
  [`cut_by_tau()`](https://beanumber.github.io/tidychangepoint/reference/cut_by_tau.md).
- Added `italy_grads` data set.

## tidychangepoint 0.0.1

CRAN release: 2024-08-19

- Initial CRAN submission.
