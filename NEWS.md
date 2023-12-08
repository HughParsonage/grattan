## 2023.1.2

* `-Wformat` issue for CRAN

## 2023.1

* `sapto` now operates on the `on_sapto_cd` variable from Alife

## 2023.0

* `_inflator` functions have been moved to grattanInflator, breaking (for example)
  `cpi_inflator_general_date`  which are now `cpi_inflator`.

## 2.0.0.0

### Breaking changes

* `income_tax` no longer accepts `family_status`, `n_dependants`, 
  `allow.forecasts` and `.debug`

* `model_income_tax` Arguments prefixed `Budget_` and `lito_` are deprecated and
  now `offsets = set_offsets` are preferred.  However, using `System` with
  `income_tax` is likely to be just as convenient.
  
* `lito` and `lmito` no longer accept varying parameters, but `lmito` is now exported

* `sapto_rcpp`, `income_tax_sapto`, `new_income_tax`, `new_medicare_levy`, 
  and `new_sapto` have now been removed. Use `System`.

* The transfer functions have been removed as they became unreliable / difficult to maintain.
  See repository `hughparsonage/grattanTransfers` for possible future development
  - `age_pension`
  - `carer_payment`
  - `child_care_subsidy`
  - `energy_supplement`
  - `family_tax_benefit`
  - `model_child_care_subsidy`
  - `model_rent_assistance`
  - `newstart_allowance`
  - `pension_supplement`
  - `rent_assistance`
  - `student_repayment`
  - `unemployment_benefit`
  - `youth_allowance`
  - `youth_unemployment`
  

* The `grattan v2` package will be more focused on income tax with other elements
  being spun off into other packages. In particular, `_inflator` functions
  and related forecasting methods, functions essentially accessing ABS data,
  and functions relating to tranfers will be put in other packages, and imported
  later. Functions that are made available in the `grattan` NAMESPACE may have
  changes to their API when this happens.

### API changes

* `income_tax` now uses `System` to define the tax system

### Other changes

* `model_income_tax` does not throw an error for incorrect specification
  of `lito_` arguments (which are deprecated)
* `age_grouper` now works with long vectors containing missing values

## 1.9.0.10

* New data changes to 2022
* `rent_assistance()` example no longer works because of outdated data.


### Breaking changes anticipated in 2.0.0.0

* `income_tax` and `medicare_levy` no longer accept `family_status` as an input, since it was giving
  misleading results. Use `.dots.ATO` with appropriate variables to define the 
  spouse's income or the number of children.
* `income_tax` gives a slightly different error message when an invalid 
  financial year is passed. Previously "not in correct form", now "not a valid financial year".
* `useABSConnection = TRUE` is no longer supported because of ABS server issues   


## 1.9.0.8

* Maintenance release for the new financial year. 
  

## 1.9.0.0

### New features

* 2017-18 sample file now contemplated as an input to `project` 
* `project` now has `r_super_balance` to project super balances by a user-supplied
  factor, rather than a hard-coded 1.05.
* Modelling superannuation changes now accepts contributions taxes relative to 
  marginal rates.

### Data
* Data updated to 2020-06-30

### Internal:

* `package:rsdmx` is now Suggested, since a dependency has been orphaned.

## 1.8.0.1

### Bug fixes
* Fixed issue with stringsAsFactors = FALSE being inconsistently applied with R 4.0

### Data

* Update wage, CPI, and labour force data





## 1.7.1.4
* Fixed minor error that affected tax liability calculations for 2010-11 and 2011-12 financial years

## 1.7.1.3
* `grattan`'s tax modelling functions now work with the 2016-17 ATO sample file 

## 1.7.1.1
* `grattan` now depends on R 3.5.0 due to serialization format version 3 becoming the default
  in R 3.6.0.
* `cpi_inflator` fails more gracefully when the ABS's website is not available

## 1.7.1.0

### Bug fixes
* `income_tax` now gives consistent results modulo the existence of completely empty 
  columns that are inputs for `sapto` (#158)
* `income_tax` will work if `.dots.ATO` is a non-data.table data.frame.
* `model_income_tax`:
  - Correctly imputes SAPTO vs non-SAPTO family thresholds when both provided.
  - `sapto_rcpp` is now more careful about passing length-one vectors to vectorized C++
    functions.
* `project` now correctly prioritizes `excl_vars` over variables with otherwise predefined 
  uprating mechanisms (such as `Sw_amt`).

### New functions:
* `awote` for weekly earnings

### Enhancements:
* `age_grouper` can now have a custom first label prefix, and is much faster
  when `length(age)` is large.
* `income_tax` now emits a warning when both age and `.dots.ATO` are provided, 
  indicating that `age` will be ignored.
* The data has been updated to 2019-02-23.
  

### Internal
* `mutate_ntile` and `weighted_ntile` now use the `hutils` equivalents. This 
  broke 3 unit tests because of the specific phrasing of some error messages.
* The vignette requires pandoc > 2.4. Some chunks have been refactored to avoid
  excess memory usage.

## 1.7.0.0

### Bug fixes

* Fixed failing interaction between temporary budget repair levy and small business tax offset in 2016-17.
* `small_business_tax_offset()` is now always positive, fixing the original misinterpretation of the legislation whereby negative business income resulted in a negative offset.
* `*_inflator` functions now return correct results for non-standard but supported financial years.
* `inflator` no longer fails when `to_fy` is length > 1 and unordered.

### New features

* `mutate_ntile` and `mutate_weighted_ntile` for adding quantile columns
* New welfare functions (usable for the 2015-16 financial year)
    - `age_pension`, 
    - `carer_payment`
    - `carers_allowance`
    - `energy_supplement`
    - `family_tax_benefit`
    - `newstart_allowance`
    - `pension_supplement`
    - `rent_assistance` the Commonwealth Rent Assistance
    - `model_rent_assistance` as experimental function for modelling changes to rent assistance.
    - `youth_allowance()` now available, though limited
* `compare_avg_tax_rates`: create a difference in average tax rates between multiple models and a baseline tax, by percentile.
* `install_taxstats()` as a convenient means to install the non-CRAN `taxstats` dependency.

### Enhancements

* `prohibit_vector_recycling()` and friends return more informative error messages.
* Added default values to the following functions:
    - `income_tax`, `income_tax_sapto`: the default value for fy.year is the current financial year
    - `cpi_inflator`, `lf_inflator_fy`, `wage_inflator`: if both from_fy and to_fy are missing, the default values become the previous and current financial years respectively. If only one of the two are missing, an error appears.
* `income_tax` is about twice as fast since 1.6.0.0: 1.5-2.0s down from 3.0-3.7s on the 100% population (13 million)
* `inflator` and `cpi_inflator`, `lf_inflator_fy`, and `wage_inflator` are now much faster when either `from_fy` or `to_fy` have more than 100,000 elements:

```r
set.seed(19952010)
from_fys <- sample(yr2fy(1995:2010), size = 1e6, replace = TRUE)
microbenchmark(cpi_inflator(from_fy = from_fys, to_fy = "2015-16"))
# Old
Unit: seconds
                                                expr      min      lq     mean   median       uq
 cpi_inflator(from_fy = from_fys, to_fy = "2015-16") 1.519483 1.54438 1.550628 1.549735 1.554507
      max neval
 1.661502   100
 
# New
Unit: milliseconds
                                                expr      min       lq     mean   median       uq
 cpi_inflator(from_fy = from_fys, to_fy = "2015-16") 40.71753 41.94061 47.93162 42.93946 48.08461
      max neval
 191.3497   100
```

### Potentially breaking changes

* `yr2fy(x)` no longer works for x = 1900L, 
  despite a unit test, for the sake of performance.
  
```
  #> Last change: NAMESPACE at 2018-08-19 14:47:14 (4 mins ago).
  #> Unit: milliseconds
  #>       expr min  lq mean median  uq max neval cld
  #>   yr2fy(z)  75  88   98     90 101 161   100  a 
  #>  .yr2fy(z) 274 286  298    297 302 359   100   b
```
  
  Use `yr2fy(x, assume1901_2100 = FALSE)` if you need the old behaviour.


### Misc/Internal

* `taxstats1516` is now a suggested dependency.



## 1.6.0.0
### 2018-05-08
* Never-legislated Medicare levy change in 2019-20 has been reverted
* Budget 2018:
  * `model_income_tax()` no longer coerces `WEIGHT` to integer.
  * New arguments to support Budget 2018:
    * `lito_multi` Permits multiple pieces to the linear offset.
    * `Budget2018_lamington` The Low And Middle Income Tax Offset proposed in the Budget 2018 budget.
    * `Budget2018_lito_202223` The proposed change to LITO from 2022-23.
    * `Budget2018_watr` The offset proposed by the Opposition the Budget Reply.
    * `sbto_discount` Allows modification of the small business tax offset.
    * `clear_tax_cols` By default, old tax columns are deleted.
    * `warn_upper_thresholds` If changed to `FALSE` allows the automatic changes to be applied without warning.
* New functions:
  * `progressivity()` for Gini-based measure of the progressivity of income tax
  * `revenue_foregone()` as a convenenience for returning the revenue foregone from a modelled sample file.
  
* Routine changes:
  * ABS data updated as of 2018-05-21.

## 1.5.3.6
### 2018-02-21
* Labour force data and wage price index updated to 2018-02-21.
* Update as requested to fix failing unit tests relying on non-standard packages.

## 1.5.3.1
### 2018-01-22

### New features:
* New function `model_income_tax` which attempts to provide every lever of the income tax system that is visible from the tax office's sample files. Users can model the sample file by changing single parameters to observe the effect on tax collections.
* `small_business_tax_offset`: Include the small business tax offset as a standalone function and within `income_tax`.

### Other user-visible changes
* `project` and `project_to` no longer require `fy.year.of.sample.file`. However, they expect the supplied `data.frame` to be compatible with the sample file provided. Failling to provide a sample file with the expected number of rows or not providing a sample file with a valid number of rows is a warning, which can be silenced by `check_fy_sample_file = FALSE`. 

### Data:
* Update labour force data to November 2017
* Internal projection tables have been updated for the latest (2014-15) sample file.

### Other changes
* `mgcv` was used but not declared in Suggests: Thanks to BDR for reporting.
* (internal) Extend `prohibit_vector_recycling` to return the maximum permissible length of a list of vectors.

## 1.5.2.5
### 2017-11-16
* Update wage data to 2017-Q3
* Update labour force data to 2017-09
* (internal) The `lf_trend` internal data table used to report the labour force in thousands of persons, as the ABS does. This seemed a bit strange, so now `obsValue` uses integers (i.e. just the labour force). 
* Vignettes now install `taxstats` to a temporary directory if not already installed, rather than the user or system's library.

### 2017-10-27
* Update CPI data
* Fix wage data

## 1.5.2.3
### 2017-10-21
* Update labour-force data

## 1.5.2.0
### 2017-10-19
* New internal C++ functions for `income_tax`, and related functions
* BTO function now uses tax scales from the *Income Tax Regulations*

## 1.5.1.2
### 2017-10-15
* Optional argument `age` in `income_tax` now `NULL` rather than `42`.  
The default argument continues to result in SAPTO being not applied if `.dots.ATO`.
However, if `.dots.ATO` is supplied (and the age variable has not been removed from it),
the individuals' SAPTO eligibility is determined by the age variable in `.dots.ATO`, rather
than setting each individual's SAPTO to 0.

### 2017-08-30
* Update labour force data. Avoid segfault in separate package in unit test.
* Added a `NEWS.md` file to track changes to the package.

### 2017-08-16
* Update wage, CPI, labour force data

### 2017-07-02
* Update wage and labour force data
* Fix breaking build due to change in dplyr API





