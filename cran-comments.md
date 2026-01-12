## R CMD check results

0 errors | 0 warnings | 0 notes

- All checks are passing on R-release and R-devel on both Windows and Ubuntu.
- I am seeing:

- checking examples ... [36s/36s] OK (36.2s)
  Examples with CPU (user + system) or elapsed time > 5s
            user system elapsed
  diagnose 5.819  0.049   5.891
  
  I think this is because it is running example code under \donttest
  