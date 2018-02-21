## Test results
0 ERRORS | 0 WARNINGS | 1-3 NOTEs

### Test environments:
* Local Windows R 3.4.3
* Travis-CI: R 3.3, 3.4, and dev
* Appveyor: dev and release.
* winbuilder: dev and release.

This is an update to skip tests that do not appear to run reliably on CRAN, following updates to dependencies. There was a single new NOTE indicating that the last submission was not long ago: this update was requested by CRAN.

NOTES:
'Days since last update: 3'
  ==> Update requested by CRAN.
  
Possibly mis-spelled words in DESCRIPTION: ...
  ==> Spellings are correct: 'repos' and 'taxstats' cannot be quoted as they are within R code.

Suggests or Enhances not in mainstream repositories: ...
  ==> Normal due to taxstats dependency

GNU make is a SystemRequirements.
  ==> Required.

## Note to CRAN: moderately-large vignette
The vignette is quite lengthy and, while it will run on CRAN, requires the installation of 'taxstats', a 58 MB source package, each time the package is checked. 


