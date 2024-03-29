## Submission comments
This is a re-submission of v0.6.0 which was rejected due to failing R CMD check with _R_CHECK_DEPENDS_ONLY_=true.
This submission contains minor bug fixes, and improves support for the use of {future} for parallel computation

## Test environments
* Windows (winbuilder), R-devel (unstable) (2022-06-30 r82540)
* Windows (winbuilder), R-release 4.2.1 (2022-06-23)
* Windows (winbuilder), R-oldrelease 4.1.3 (2022-03-10)
* MacOs (github), R-release 4.2.1 (2022-06-23)
* Ubuntu 16.04 (github), R-release 4.2.1 (2022-06-23)

## R CMD check results
0 errors | 0 warnings | 1 notes
> checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Abhraneel Sarma <abhraneel@u.northwestern.edu>’
  Found the following (possibly) invalid URLs:
    URL: https://doi.org/10.1177/0956797612466416
      From: man/durante.Rd
            inst/doc/durante-multiverse-analysis.html
            README.md
      Status: 503
      Message: Service Unavailable
    URL: https://doi.org/10.1177/1745691616658637
      From: man/multiverse-package.Rd
            inst/doc/durante-multiverse-analysis.html
            inst/doc/visualising-multiverse.html
            README.md
      Status: 503
      Message: Service Unavailable
  Found the following (possibly) invalid DOIs:
    DOI: 10.1177/1745691616658637
      From: DESCRIPTION
      Status: Service Unavailable
      Message: 503
      
These DOIs are valid and will direct users to the correct webpage of the published article on the publisher's website


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

