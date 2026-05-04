## R CMD check results

0 errors | 0 warnings | 0 notes

- All checks are passing on R-release and R-devel on both Windows and Ubuntu.

- The following warnings have been resolved by replacing `changepointGA::ARIMA.BIC` with
  `changepointGA::arima_bic()`:

New result: WARNING
 Missing or unexported object: ‘changepointGA::ARIMA.BIC’
New result: WARNING
 Missing link(s) in Rd file 'fit_arima.Rd':
   ‘[changepointGA:ARIMA.BIC]{changepointGA::ARIMA.BIC()}’