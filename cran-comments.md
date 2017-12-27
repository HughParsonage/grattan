## Test environments
* local Windows install, MRAN 3.4.2
* ubuntu 12.04 (on travis-ci), R devel and release <https://travis-ci.org/HughParsonage/grattan>
* win-builder (devel) <https://win-builder.r-project.org/Mer51d990tOv/00check.log>

* This is a package update to:
  - fix vignettes to comply with CRAN policy
  - reflect recent data

## R CMD check results

0 errors | 0 warnings | 2 notes

* The first NOTE is with respect to the 'taxstats' package not being a mainstream repository;
  its inclusion satisfies the CRAN repository policy, as in previous versions.
  
  The NOTE also recommends including the URL <https://hughparsonage.github.io/drat/> in angle brackets;
  however, this NOTE is spurious as the URL is within R code.
  
* The second NOTE refers to GNU make as a SystemRequirements. 
  This is a modest requirement and is necessary to run RcppParallel.
  
In addition:

* There are two 'Additional issues' in which undefined behaviour is detected via `clang-UBSAN` and `gcc-UBSAN`. 
  These issues arise due to an issue with `RcppParallel`. 
  The maintainers of `RcppParallel` appears to fixed these issues in a development version 
  (downstream of <https://github.com/RcppCore/RcppParallel/pull/48>); 
  however this version is yet to reach CRAN.
  Accordingly, there are no changes in my package addressing these issues. 


