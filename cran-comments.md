## Submission comments
This release addresses some minor issues introduced by the recent evaluate 1.0.0 release. 
It also has some other unrelated (mostly minor) changes and bugfixes, see NEWS.md.

## Test environments
* Windows (winbuilder), R-release 4.4.1 (2024-06-14)
* Windows (winbuilder), R-devel (unstable) (2024-10-04 r87208)
* Windows (winbuilder), R-oldrelease 4.3.3 (2024-02-29)
* Windows (Github), R-release 4.4.1 (2024-06-14), 
* MacOs (Github), R-release 4.4.1 (2024-06-14)
* Ubuntu 22.04.5 (Github), R-release 4.4.1 (2024-06-14)
* Ubuntu 22.04.5 (Github), R-devel (unstable) (2024-10-04 r87208)
* Ubuntu 22.04.5 (Github), R 4.3.3 (2024-02-29)
* Ubuntu 22.04.5 (Github), R 4.2.3 (2023-03-15)
* Ubuntu 22.04.5 (Github), R 4.1.3 (2022-03-10)

## R CMD check results
0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problem
 * We failed to check 0 packages
 
### New problems
  * mverse currently fails due to changes, but this change was made in order to make it compatible with the recent evaluate 1.0.0 release.
  An issue has been submitted to mverse which notifies the package maintainers of this change: https://github.com/mverseanalysis/mverse/issues/54

