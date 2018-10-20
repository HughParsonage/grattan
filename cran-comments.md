## Test results
0 ERRORS | 0 WARNINGS | 1-2 NOTEs

### Test environments:
* Local Windows CRAN 3.5.1
* Travis-CI: Ubuntu 14.04. R 3.4, 3.5, and dev (r75443)
* Appveyor: dev (r75439) and release.
* winbuilder: dev (r75434) and release.



NOTES:
  
Possibly mis-spelled words in DESCRIPTION: ...
  ==> Spellings are correct: 'repos' and 'taxstats' cannot be quoted as they are within R code.
  
URLs in angle brackets:
  ==> Not appropriate since the URL is within R code.

Suggests or Enhances not in mainstream repositories: ...
  ==> Normal due to taxstats dependency

## Note to CRAN: moderately-large vignette
The vignette is quite lengthy and, while it will run on CRAN, requires the installation of 'taxstats', a 58 MB source package, each time the package is checked. 


