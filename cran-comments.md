## Test environments
* local Windows install, MRAN 3.4.1 and R devel
* ubuntu 12.04 (on travis-ci), R devel and release <https://travis-ci.org/HughParsonage/grattan>
* win-builder (devel) <https://win-builder.r-project.org/3bn6Fx98KPZ2/00check.log>

## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a package update to reflect updated data

* The first NOTE is with respect to the 'taxstats' package not being a mainstream repository;
  its inclusion satisfies the CRAN repository policy, as in previous versions.
  
  The NOTE also recommends including the URL <https://hughparsonage.github.io/drat/> in angle brackets;
  however, this NOTE is spurious as the URL is within R code.
  
* The second NOTE refers to GNU make as a SystemRequirements. 
  This is a modest requirement and is necessary to run RcppParallel.


