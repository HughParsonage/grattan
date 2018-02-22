grattan
=======

Australian Tax Policy Analysis

NEWS
====

1.5.3.2
-------

### 2018-02-01

-   Labour force data
-   CRAN update to fix failure update of suggested packages.

1.5.3.1
-------

### 2018-01-22

### New features:

-   New function `model_income_tax` which attempts to provide every lever of the income tax system that is visible from the tax office's sample files. Users can model the sample file by changing single parameters to observe the effect on tax collections.
-   `small_business_tax_offset`: Include the small business tax offset as a standalone function and within `income_tax`.

### Other user-visible changes

-   `project` and `project_to` no longer require `fy.year.of.sample.file`. However, they expect the supplied `data.frame` to be compatible with the sample file provided. Failling to provide a sample file with the expected number of rows or not providing a sample file with a valid number of rows is a warning, which can be silenced by `check_fy_sample_file = FALSE`.

### Data:

-   Update labour force data to November 2017
-   Internal projection tables have been updated for the latest (2014-15) sample file.

### Other changes

-   `mgcv` was used but not declared in Suggests: Thanks to BDR for reporting.
-   (internal) Extend `prohibit_vector_recycling` to return the maximum permissible length of a list of vectors.

1.5.2.5
-------

### 2017-11-16

-   Update wage data to 2017-Q3
-   Update labour force data to 2017-09
-   (internal) The `lf_trend` internal data table used to report the labour force in thousands of persons, as the ABS does. This seemed a bit strange, so now `obsValue` uses integers (i.e. just the labour force).
-   Vignettes now install `taxstats` to a temporary directory if not already installed, rather than the user or system's library.

### 2017-10-27

-   Update CPI data
-   Fix wage data

1.5.2.3
-------

### 2017-10-21

-   Update labour-force data

1.5.2.0
-------

### 2017-10-19

-   New internal C++ functions for `income_tax`, and related functions
-   BTO function now uses tax scales from the *Income Tax Regulations*

1.5.1.2
-------

### 2017-10-15

-   Optional argument `age` in `income_tax` now `NULL` rather than `42`.
    The default argument continues to result in SAPTO being not applied if `.dots.ATO`. However, if `.dots.ATO` is supplied (and the age variable has not been removed from it), the individuals' SAPTO eligibility is determined by the age variable in `.dots.ATO`, rather than setting each individual's SAPTO to 0.

### 2017-08-30

-   Update labour force data. Avoid segfault in separate package in unit test.
-   Added a `NEWS.md` file to track changes to the package.

### 2017-08-16

-   Update wage, CPI, labour force data

### 2017-07-02

-   Update wage and labour force data
-   Fix breaking build due to change in dplyr API

CRAN Notes
==========

This is an update as requested by CRAN. The previous version of this package coincided with an update to the 'taxstats' package. Due to an error on my part, tests which required the updated version did not first check for version sufficiency. All tests and vignettes requiring the latest version have been changed to skip if the version is insufficient; however, the 'taxstats' package may require reinstallation on CRAN.

Test environments
-----------------

-   local Windows install, CRAN 3.4.3
-   ubuntu 12.04 (on travis-ci), R devel, release, and old-release (R 3.3.3) <https://travis-ci.org/HughParsonage/grattan>
-   win-builder (devel)

-   This is a package update to:
-   fix broken tests

R CMD check results
-------------------

0 errors | 0 warnings | 2 notes

-   The first NOTE is regarding CRAN incoming feasibility.

> Possibly mis-spelled words in DESCRIPTION: indices (17:77)

The spelling is correct.

The other notes are unchanged from the previous release.

-   With respect to the 'taxstats' package not being a mainstream repository, its inclusion satisfies the CRAN repository policy, as in previous versions.

The NOTE also recommends including the URL <https://hughparsonage.github.io/drat/> in angle brackets; however, this NOTE is spurious as the URL is within R code.

-   The second NOTE refers to GNU make as a SystemRequirements. This is a modest requirement and is necessary to run RcppParallel.

In addition:

-   There are two 'Additional issues' in which undefined behaviour is detected via `clang-UBSAN` and `gcc-UBSAN`. These issues arise due to an issue with `RcppParallel`. The maintainers of `RcppParallel` appears to fixed these issues in a development version however this version is yet to reach CRAN. Accordingly, there are no changes in my package addressing these issues.
