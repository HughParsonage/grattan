This is an update as requested by CRAN.  The previous version of this package
coincided with an update to the 'taxstats' package.  Due to an error on my
part, tests which required the updated version did not first check for version
sufficiency.  All tests and vignettes requiring the latest version have been
changed to skip if the version is insufficient; however, the 'taxstats' package
may require reinstallation on CRAN. 

The additional repository has been changed to include win-binaries on R 3.3, 3.4, and 3.5.

## Test environments
* local Windows install, CRAN 3.4.3 and R Under development (unstable) (2018-02-08 r74238)
* ubuntu 12.04 (on travis-ci), R devel, release, and old-release (R 3.3.3) <https://travis-ci.org/HughParsonage/grattan>
* Appveyor (devel and release)
* win-builder (devel and release)
* r-hub ('for CRAN')

* This is a package update to:
  - fix broken tests

## R CMD check results

0 errors | 0 warnings | 2 notes

* An error occurs on r-hub and appveyor because of a package
  required but not available: 'data.table' since a Windows
  binary is not available for r-devel.
* A NOTE occurs on win-builder but not other environments
  about the Additional repository not containing packages,
  yet the associated diagnostics do not match the repository.

The remaining NOTEs are unchanged from previous versions.

* The first NOTE is regarding CRAN incoming feasibility. 

  > Possibly mis-spelled words in DESCRIPTION:
  >   indices (17:77)
  
  The spelling is correct.
  
  The other notes are unchanged from the previous release.

* With respect to the 'taxstats' package not being a mainstream repository,
  its inclusion satisfies the CRAN repository policy, as in previous versions.
  
  The NOTE also recommends including the URL <https://hughparsonage.github.io/drat/> 
  in angle brackets; however, this NOTE is spurious as the URL is within R code.
  
* The second NOTE refers to GNU make as a SystemRequirements. 
  This is a modest requirement and is necessary to run RcppParallel.
  
In addition:

* There are two 'Additional issues' in which undefined behaviour is detected via `clang-UBSAN` and `gcc-UBSAN`. 
  These issues arise due to an issue with `RcppParallel`. 
  The maintainers of `RcppParallel` appears to fixed these issues in a development version 
  however this version is yet to reach CRAN.
  Accordingly, there are no changes in my package addressing these issues. 


