## Test results
0 ERRORS | 0 WARNINGS | 1-3 NOTEs

### Test environments:
* Local Windows MRAN 3.4.3:
  - taxstats 0.0.4.1314
  - taxstats 0.0.5.1415
* Travis-CI: R 3.3, 3.4, and dev
* Appveyor: dev (ERROR as data.table was unavailable) and release.
* winbuilder: dev and release.

This is an update and resubmission to skip tests that do not run reliably on CRAN, following updates to dependencies. There was a single new NOTE indicating that the last submission was not long ago: this update was requested by CRAN. In addition, the UBSAN issues have been avoided by removing the dependency on RcppParallel entirely.

NOTES:
'Days since last update: 4'
  ==> Update requested by CRAN.
  
Possibly mis-spelled words in DESCRIPTION: ...
  ==> Spellings are correct: 'repos' and 'taxstats' cannot be quoted as they are within R code.

Suggests or Enhances not in mainstream repositories: ...
  ==> Normal due to taxstats dependency

## Note to CRAN: moderately-large vignette
The vignette is quite lengthy and, while it will run on CRAN, requires the installation of 'taxstats', a 58 MB source package, each time the package is checked. 


