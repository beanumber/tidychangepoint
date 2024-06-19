## R CMD check results

0 errors | 0 warnings | 1 note

- This is a new release.

- On R-release, everything looks good. 

- Some of the tests are failing on R-devel, but not on R-release. My best guess
  is that this is related to a change in behavior of `head()` and `tail()` in
  the development version of R, as noted [here](https://stat.ethz.ch/R-manual/R-devel/doc/html/NEWS.html),
  since I am using both functions on `ts` objects in the failing code. However, 
  I have yet to isolate and fix the bug. I'm tracking this issue [here](https://github.com/beanumber/tidychangepoint/issues/110).