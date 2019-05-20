This is a package resubmission after archival.

The vignettes have been protected from accessing taxstats packages; the packages
will only be attempted to be installed if an environment variable is set.

## Test results
0 ERRORS | 0 WARNINGS | 1-2 NOTEs

### Test environments:
* Local Windows CRAN 3.6.0
* Travis-CI: Ubuntu 14.04. R 3.5, 3.6, and dev (r76539)
* Appveyor: dev (r76446) and release.
* winbuilder: dev (r76539) and release.



NOTES:

Package archival / previous violation of policy.
  ==> Vignettes using package 'taxstats' is now off by default
  ==> Installation of taxstats packages now requires manual setting of 
      environment variable
  
Possibly mis-spelled words in DESCRIPTION: ...
  ==> Spellings are correct: 'repos' and 'taxstats' cannot be quoted as they are
      within R code.
  
URLs in angle brackets:
  ==> Not appropriate since the URL is within R code.

Suggests or Enhances not in mainstream repositories: ...
  ==> Normal due to taxstats dependency

## Note to CRAN: moderately-large vignette
The vignette is quite lengthy and, if the environment variable is set,
installs package 'taxstats', a 58 MB source package,
each time the package is checked. 


