-   [grattan](#grattan)
-   [Overview](#overview)
    -   [`income_tax`](#income_tax)
        -   [With sample files](#with-sample-files)
        -   [Modelling changes to personal income tax](#modelling-changes-to-personal-income-tax)
    -   [`project`](#project)
-   [NEWS](#news)
    -   [1.6.0.0](#section)
        -   [2018-05-08](#section-1)
    -   [1.5.3.6](#section-2)
        -   [2018-02-21](#section-3)
    -   [1.5.3.1](#section-4)
        -   [2018-01-22](#section-5)
        -   [New features:](#new-features)
        -   [Other user-visible changes](#other-user-visible-changes)
        -   [Data:](#data)
        -   [Other changes](#other-changes)
    -   [1.5.2.5](#section-6)
        -   [2017-11-16](#section-7)
        -   [2017-10-27](#section-8)
    -   [1.5.2.3](#section-9)
        -   [2017-10-21](#section-10)
    -   [1.5.2.0](#section-11)
        -   [2017-10-19](#section-12)
    -   [1.5.1.2](#section-13)
        -   [2017-10-15](#section-14)
        -   [2017-08-30](#section-15)
        -   [2017-08-16](#section-16)
        -   [2017-07-02](#section-17)
-   [CRAN Notes](#cran-notes)
    -   [Test results](#test-results)
        -   [Test environments:](#test-environments)
    -   [Note to CRAN: moderately-large vignette](#note-to-cran-moderately-large-vignette)

grattan
=======

Australian Tax Policy Analysis

Overview
========

``` r
install.packages("grattan")
```

``` r
library(grattan)
```

    ## Last change: model_income_tax.R at 2018-06-14 16:47:31 (49 mins ago).

`income_tax`
------------

Calculates the income tax for a given taxable income and financial year:

``` r
income_tax(50e3, "2015-16")
```

    ## [1] 8547

### With sample files

`income_tax` is designed to work well with the ATO's sample files. You can obtain the sample files from my repo:

``` r
# install.packages("taxstats", repos = "https://hughparsonage.github.io/tax-drat")
library(taxstats)

library(data.table) 
library(magrittr)
```

Simply pass the sample file to `.dots.ATO` and the complexities of things like Medicare levy and the Seniors and Pensioners Tax Offset are handled for you. For example:

``` r
s1314 <- as.data.table(sample_file_1314)
s1314 %>%
  .[, tax := income_tax(Taxable_Income, "2013-14", .dots.ATO = s1314)] %>%
  .[, .(Taxable_Income, tax)]
```

    ##         Taxable_Income       tax
    ##      1:           4800     0.000
    ##      2:         126122 36503.970
    ##      3:          39742  4655.410
    ##      4:         108123 29574.355
    ##      5:          85957 21040.445
    ##     ---                         
    ## 258770:          24462  1111.710
    ## 258771:          37055  3701.525
    ## 258772:          45024  6530.520
    ## 258773:           5134     0.000
    ## 258774:          46368  7007.640

### Modelling changes to personal income tax

While `income_tax` is designed to inflexibly return the tax payable as legislated, `model_income_tax` is designed to calculate income tax when changes are made. For example,

``` r
s1314 %>%
  # reduce top threshold from 180,000 to 150,000
  model_income_tax(ordinary_tax_thresholds = c(0, 18200, 37000, 80000, 
                                               150e3), 
                   baseline_fy = "2013-14") %>%
  .[, .(Taxable_Income, baseline_tax, new_tax)]
```

    ##         Taxable_Income baseline_tax   new_tax
    ##      1:           4800            0     0.000
    ##      2:         126122        36503 36503.970
    ##      3:          39742         4655  4655.410
    ##      4:         108123        29574 29574.355
    ##      5:          85957        21040 21040.445
    ##     ---                                      
    ## 258770:          24462         1111  1111.710
    ## 258771:          37055         3701  3701.525
    ## 258772:          45024         6530  6530.520
    ## 258773:           5134            0     0.000
    ## 258774:          46368         7007  7007.640

`project`
---------

Given a sample file, we can project forward a number of years

``` r
s1617 <- project(s1314, h = 3L)
```

or to a particular financial year

``` r
s1718 <- project_to(s1314, "2017-18")
```

Together with `model_income_tax`, this allows us to make point-predictions of future years. The function `revenue_foregone` prettily prints the resultant revenue:

``` r
sample_file_1314 %>%
  project_to("2018-19") %>%
  model_income_tax(ordinary_tax_thresholds = c(0, 18200, 37000, 87000, 
                                               150e3), 
                   baseline_fy = "2017-18") %>%
  revenue_foregone
```

    ## [1] "$1.7 billion"

NEWS
====

1.6.0.0
-------

### 2018-05-08

-   Never-legislated Medicare levy change in 2019-20 has been reverted
-   Budget 2018:
-   `model_income_tax()` no longer coerces `WEIGHT` to integer.
-   New arguments to support Budget 2018:
    -   `lito_multi` Permits multiple pieces to the linear offset.
    -   `Budget2018_lamington` The Low And Middle Income Tax Offset proposed in the Budget 2018 budget.
    -   `Budget2018_lito_202223` The proposed change to LITO from 2022-23.
    -   `Budget2018_watr` The offset proposed by the Opposition the Budget Reply.
    -   `sbto_discount` Allows modification of the small business tax offset.
    -   `clear_tax_cols` By default, old tax columns are deleted.
    -   `warn_upper_thresholds` If changed to `FALSE` allows the automatic changes to be applied without warning.
-   New functions:
-   `progressivity()` for Gini-based measure of the progressivity of income tax
-   `revenue_foregone()` as a convenenience for returning the revenue foregone from a modelled sample file.

-   Routine changes:
-   ABS data updated as of 2018-05-21.

1.5.3.6
-------

### 2018-02-21

-   Labour force data and wage price index updated to 2018-02-21.
-   Update as requested to fix failing unit tests relying on non-standard packages.

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

Test results
------------

0 ERRORS | 0 WARNINGS | 1-2 NOTEs

### Test environments:

-   Local Windows CRAN 3.5.0 and R-devel (r74750):
-   Travis-CI: Ubuntu 14.04. R 3.4, 3.5, and dev (r74786)
-   Appveyor: dev (r74750) and release.
-   winbuilder: dev (r74786) and release.

NOTES:

Possibly mis-spelled words in DESCRIPTION: ... ==&gt; Spellings are correct: 'repos' and 'taxstats' cannot be quoted as they are within R code.

Suggests or Enhances not in mainstream repositories: ... ==&gt; Normal due to taxstats dependency

Note to CRAN: moderately-large vignette
---------------------------------------

The vignette is quite lengthy and, while it will run on CRAN, requires the installation of 'taxstats', a 58 MB source package, each time the package is checked.
