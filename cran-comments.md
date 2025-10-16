## R CMD check results

0 errors | 0 warnings | 1 notes

- All checks are passing on R-release and R-devel on both Windows and Ubuntu.
- Fixed the WARNINGs caused by the update to `changepointGA`.
- On win-devel, I am seeing:

  * checking examples ... [54s] NOTE
  Examples with CPU (user + system) or elapsed time > 10s
           user system elapsed
  diagnose 5.95   0.28   11.28

  This is because it's running the examples that are in `\donttest{}`. I hope that's not a problem. 
  