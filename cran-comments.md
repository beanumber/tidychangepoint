## R CMD check results

0 errors | 0 warnings | 1 note

- This is a new release.

- All checks are passing on R-release and R-devel on both Windows and Ubuntu.

- We switched from the MIT license to the GPLv3 license. This was an oversight -- we had meant to use GPL all along. <https://github.com/beanumber/tidychangepoint/commit/02300d4a7d2d10bcc10369cb8e114ea8b52ee44d>
- We changed most of the `\dontrun` examples to `\donttest`, as per your guidance.
- We added return values for all documented functions. 
- The documentation has received a thorough set of revisions. 
