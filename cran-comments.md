## R CMD check results

0 errors | 0 warnings | 0 notes

- All checks are passing on R-release and R-devel on both Windows and Ubuntu.
- On win-devel, I am seeing:

- checking examples ... [36s/36s] OK (36.2s)
  Examples with CPU (user + system) or elapsed time > 5s
            user system elapsed
  diagnose 5.595  0.049   5.663
  fitness  5.250  0.015   5.276
- checking examples with --run-donttest ... [109s/109s] OK (1m 49.3s)
  